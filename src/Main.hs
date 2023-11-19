{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Config
import Config.File
import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary hiding (drop)
import Data.Default.Class
import Data.Int
import Data.List
import Data.Maybe
import Data.String.Utils (strip)
import Data.Time
import Data.Time.ISO8601
import Data.X509.CertificateStore
import Data.X509.Validation
import ESCommand
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.TLS
import Network.TLS.Extra.Cipher
import Network.URI
import System.IO
import System.X509
import System.Environment (lookupEnv)
import Text.Printf (printf)
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as DCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as SP

withMaybeLogFile :: Maybe FilePath -> ((B.ByteString -> IO ()) -> IO a) -> IO a
withMaybeLogFile Nothing go = go (const $ return ())
withMaybeLogFile (Just fp) go = withFile fp AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  go (B.hPutStr h)

defaultBaseUri :: URI
defaultBaseUri = URI
    { uriScheme = "http:"
    , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName = "localhost"
        , uriPort    = ":9200"
        }
    , uriPath = ""
    , uriQuery = ""
    , uriFragment = ""
    }

data DeploymentDetails = DeploymentDetails DeploymentResourceDetails deriving (Show, Eq)

instance FromJSON DeploymentDetails where
    parseJSON = withObject "DeploymentDetails" $ \v -> DeploymentDetails <$> v .: "resources"

data DeploymentResourceDetails = DeploymentResourceDetails [DeploymentClusterDetails] deriving (Show, Eq)

instance FromJSON DeploymentResourceDetails where
    parseJSON = withObject "DeploymentResourceDetails" $ \v -> DeploymentResourceDetails <$> v .: "elasticsearch"

data DeploymentClusterDetails = DeploymentClusterDetails
    { deploymentClusterRefId  :: String
    , deploymentClusterId     :: String
    , deploymentClusterRegion :: String
    , deploymentClusterInfo   :: DeploymentClusterInfo
    } deriving (Show, Eq)

instance FromJSON DeploymentClusterDetails where
    parseJSON = withObject "DeploymentClusterDetails" $ \v -> DeploymentClusterDetails
        <$> v .: "ref_id"
        <*> v .: "id"
        <*> v .: "region"
        <*> v .: "info"

data DeploymentClusterInfo = DeploymentClusterInfo { deploymentClusterInfoName :: String } deriving (Show, Eq)

instance FromJSON DeploymentClusterInfo where
    parseJSON = withObject "DeploymentClusterInfo" $ \v -> DeploymentClusterInfo <$> v .: "cluster_name"

data HeapDumpDetails = HeapDumpDetails
    { heapDumpInstanceId  :: String
    , heapDumpSize        :: Integer
    , heapDumpType        :: String
    , heapDumpStatus      :: String
    , heapDumpCaptureTime :: String
    } deriving (Show, Eq)

instance FromJSON HeapDumpDetails where
    parseJSON = withObject "HeapDumpDetails" $ \v -> HeapDumpDetails
        <$> v .:  "instance_id"
        <*> v .:? "size" .!= 0
        <*> v .:  "type"
        <*> v .:  "status"
        <*> v .:  "captured"

data RefHeapDumps = RefHeapDumps String [HeapDumpDetails] deriving (Show, Eq)

instance FromJSON RefHeapDumps where
    parseJSON = withObject "RefHeapDumps" $ \v -> RefHeapDumps
        <$> v .: "ref_id"
        <*> v .: "heap_dumps"

data HeapDumpsResponse = HeapDumpsResponse [RefHeapDumps] deriving (Show, Eq)

instance FromJSON HeapDumpsResponse where
    parseJSON = withObject "HeapDumpsResponse" $ \v -> HeapDumpsResponse
        <$> v .: "elasticsearch"

parseHeapDumpsResponse :: BL.ByteString -> HeapDumpsResponse
parseHeapDumpsResponse = either error id . eitherDecode

main :: IO ()
main = withConfig $ \config@Config
    { esGeneralConfig    = GeneralConfig{..}
    , esConnectionConfig = ConnectionConfig{..}
    } -> do

    (certStore, verifyCert) <- case esCertificateVerificationConfig of
        DefaultCertificateVerificationConfig -> (,True)  <$> getSystemCertificateStore
        NoCertificateVerificationConfig      -> (,False) <$> getSystemCertificateStore
        CustomCertificateVerificationConfig certStorePath -> do
            maybeCertStore <- readCertificateStore certStorePath
            case maybeCertStore of
                Just certStore -> return (certStore, True)
                Nothing -> error $ "failed to read certificate store from " ++ certStorePath

    let hostName = case esEndpointConfig of
            DefaultEndpoint               -> "localhost"
            URIEndpoint baseURI           -> fromMaybe "" $ fmap uriRegName $ uriAuthority baseURI
            CloudDeploymentEndpoint apiRoot _ _ -> case uriAuthority apiRoot of
                Just URIAuth{..} -> uriRegName
                Nothing -> error $ "could not extract hostname from URI '" ++ show apiRoot ++ "'"
            ServerlessProjectEndpoint apiRoot _ -> case uriAuthority apiRoot of
                Just URIAuth{..} -> uriRegName
                Nothing -> error $ "could not extract hostname from URI '" ++ show apiRoot ++ "'"

        clientParams = (defaultParamsClient hostName B.empty)
            { clientSupported = def
                { supportedCiphers = ciphersuite_default
                }
            , clientShared = def
                { sharedCAStore = certStore
                }
            , clientHooks = def
                { onServerCertificate = if verifyCert then validateDefault else \_ _ _ _ -> return []
                }
            }
        httpClientSettings = mkManagerSettings (TLSSettings clientParams) Nothing

    manager <- newManager httpClientSettings { managerResponseTimeout = responseTimeoutNone }

    applyCredentials <- let credFromString = T.encodeUtf8 . T.pack in case esCredentialsConfig of
        NoCredentials                          -> return id
        BasicCredentials userString passString -> return $ applyBasicAuth (credFromString userString) (credFromString passString)
        ApiKeyCredentials apiKeyEnvVar         -> do
            maybeApiKey <- lookupEnv apiKeyEnvVar
            case maybeApiKey of
                Nothing -> error $ "Environment variable '" ++ apiKeyEnvVar ++ "' not set (maybe run set-cloud-env)"
                Just apiKey ->
                    return $ \req -> req
                        { requestHeaders
                            = (hAuthorization,         credFromString $ "ApiKey " ++ apiKey)
                            : ("X-Management-Request", "true")
                            : requestHeaders req
                        }
        MacOsKeyringCredentials service account -> do
            apiKey <- SP.readProcess "security" ["find-generic-password", "-s", service, "-a", account, "-w"] ""
            return $ \req -> req
                { requestHeaders
                    = (hAuthorization,         credFromString $ "ApiKey " ++ strip apiKey)
                    : ("X-Management-Request", "true")
                    : requestHeaders req
                }

    baseURI <- case esEndpointConfig of
        DefaultEndpoint                                    -> return defaultBaseUri
        URIEndpoint baseURI                                -> return baseURI
        CloudDeploymentEndpoint apiRoot deploymentIdString maybeDeploymentRef -> do
            let deploymentURIPrefix = show apiRoot ++ "deployments/"
                deploymentId = if deploymentURIPrefix `isPrefixOf` deploymentIdString
                                then drop (length deploymentURIPrefix) deploymentIdString
                                else deploymentIdString
                lookupDeploymentRef = do
                    let initReq = parseURIRequest "deployment metadata" $ apiRoot ~// "api/v1/deployments" ~. deploymentId
                    getDeploymentResponse <- httpLbs (applyCredentials initReq) manager
                    when (responseStatus getDeploymentResponse /= ok200) $ error $ "failed to get deployment details: " ++ show getDeploymentResponse
                    case eitherDecode' (responseBody getDeploymentResponse) of
                        Left msg -> error msg
                        Right (DeploymentDetails (DeploymentResourceDetails clusters)) -> case clusters of
                            [] -> error $ "no clusters found for deployment " ++ deploymentId
                            [DeploymentClusterDetails{..}] -> return deploymentClusterRefId
                            _ -> do
                                putStrLn $ "deployment " ++ deploymentId ++ " has " ++ show (length clusters) ++ " clusters, choose from the following:"
                                forM_ clusters $ \DeploymentClusterDetails{..} -> putStrLn
                                    $ "--deployment "
                                    ++ deploymentId
                                    ++ "--deployment-ref "
                                    ++ deploymentClusterRefId
                                    ++ " # cluster id "
                                    ++ deploymentClusterId
                                    ++ ", name "
                                    ++ show (deploymentClusterInfoName deploymentClusterInfo)
                                putStrLn ""
                                error "deployment has multiple clusters"
            deploymentRef <- maybe lookupDeploymentRef return maybeDeploymentRef
            return $ unwrapURI $ apiRoot ~// "api/v1/deployments" ~/ deploymentId ~/ "elasticsearch" ~/ deploymentRef ~/ "proxy"

        ServerlessProjectEndpoint apiRoot projectId -> do
            let getProjectType [] = error $ "could not determine type of project " ++ projectId ++ " at " ++ show apiRoot
                getProjectType (pt:pts) = do
                    let initReq = parseURIRequest "project metadata" $ apiRoot ~// "api/v1/admin/serverless/projects" ~/ pt ~. projectId
                    getDeploymentResponse <- httpLbs (applyCredentials initReq {method="GET"}) manager
                    if
                        | responseStatus getDeploymentResponse == ok200       -> return pt
                        | responseStatus getDeploymentResponse == notFound404 -> getProjectType pts
                        | otherwise                                           -> error $ show getDeploymentResponse
            projectType <- getProjectType ["security", "observability", "elasticsearch"]
            return $ unwrapURI $ apiRoot ~// "api/v1/admin/serverless/projects" ~/ projectType ~/ projectId ~/ "_proxy"

    when esSaveConnectionConfig $
        if esEndpointConfig == DefaultEndpoint && esCredentialsConfig == NoCredentials
            then error "no connection config given, nothing to save"
            else do
                configFileName <- saveConfigFile (esConnectionConfig config) {esEndpointConfig = URIEndpoint baseURI}
                putStrLn $ "# Saved connection config to '" ++ configFileName ++ "'"

    case esOneShotCommand of
        Just HeapDumpList -> do
            let captureURI = let s = "../../../heap_dumps"
                                 r = fromMaybe (error s) $ parseRelativeReference s
                             in show $ r `relativeTo` baseURI
                initReq = fromMaybe (error captureURI) $ parseRequest captureURI
                req = applyCredentials initReq
            putStrLn $ "# curl --silent -XGET"
                ++ curlCertificateVerificationOption esCertificateVerificationConfig
                ++ curlCredentialsOption             False esCredentialsConfig
                ++ " "
                ++ show captureURI
            HeapDumpsResponse refHeapDumps <- parseHeapDumpsResponse . responseBody <$> httpLbs req manager
            forM_ refHeapDumps $ \(RefHeapDumps refId heapDumps) -> forM_ heapDumps $ \HeapDumpDetails{..} ->
                let s = printf "../../%s/instances/%s/heap_dump/_download" refId heapDumpInstanceId
                    r = fromMaybe (error s) $ parseRelativeReference s
                    c = "curl -OJ"
                        ++ curlCertificateVerificationOption esCertificateVerificationConfig
                        ++ curlCredentialsOption             False esCredentialsConfig
                        ++ " "
                        ++ show (r `relativeTo` baseURI)
                in putStrLn $ printf "%s %12d %s" heapDumpCaptureTime heapDumpSize c

        Just (ThreadDumpNode nodeType nodeIndex) -> do
            let captureURI = let s = printf "../instances/%s-%010d/thread_dump/_capture" (nodeTypeInstancePrefix nodeType) nodeIndex
                                 r = fromMaybe (error s) $ parseRelativeReference s
                             in show $ relativeTo r baseURI
                initReq = fromMaybe (error captureURI) $ parseRequest captureURI
                req = applyCredentials initReq
                    { method = T.encodeUtf8 "POST"
                    }
            putStrLn $ "# curl --silent --compressed -XPOST"
                ++ curlCertificateVerificationOption esCertificateVerificationConfig
                ++ curlCredentialsOption             False esCredentialsConfig
                ++ " "
                ++ show captureURI
            withResponse req manager $ \response -> let
                body = responseBody response
                go = do
                    chunk <- body
                    when (B.length chunk /= 0) $ do
                        B.hPutStr stdout chunk
                        go
                in go

        Nothing -> do
            unless esOnlyResponse $ do
                putStrLn $ "# Server URI: " ++ show baseURI
                putStrLn $ ""

            withMaybeLogFile esLogFile $ \writeLog ->
                runConduit
                     $  sourceHandle stdin
                     .| conduitParser esCommand
                     .| DCL.map snd
                     .| awaitForever (runCommand baseURI config applyCredentials manager)
                     .| awaitForever (\(consoleEntry, logEntry) -> liftIO $ do
                            putStrLn consoleEntry
                            writeLog $ T.encodeUtf8 $ T.pack $ logEntry ++ "\n")

(~.) :: Maybe URI -> String -> Maybe URI
maybeBaseUri ~. relPath = do
    baseUri <- maybeBaseUri
    relRef  <- parseRelativeReference $ relPath
    return $ relRef `relativeTo` baseUri

(~/) :: Maybe URI -> String -> Maybe URI
base ~/ relPath = base ~. (relPath ++ "/")

(~//) :: URI -> String -> Maybe URI
base ~// relPath = Just base ~/ relPath

unwrapURI :: Maybe URI -> URI
unwrapURI Nothing  = error "could not parse URI"
unwrapURI (Just u) = u

parseURIRequest :: String -> Maybe URI -> Request
parseURIRequest context maybeURI = either contextError id $ do
    uri <- maybe (Left "missing URI given") Right maybeURI
    maybe (Left "failed to parse request") Right $ parseRequest $ show uri
    where contextError m = error $ context ++ ": " ++ m

prettyStringFromJson :: ToJSON a => a -> String
prettyStringFromJson v
    = T.unpack $ T.decodeUtf8 $ BL.toStrict
        $ Data.Aeson.Encode.Pretty.encodePretty' aesonPrettyConfig v
    where
    aesonPrettyConfig = Data.Aeson.Encode.Pretty.defConfig
        { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Spaces 2 }

escapeShellQuoted :: Char -> String
escapeShellQuoted c
    | c == '\n' = "\\n"
    | c == '\'' = "\\'"
    | otherwise = [c]

nodeTypeInstancePrefix :: NodeType -> String
nodeTypeInstancePrefix NodeTypeInstance   = "instance"
nodeTypeInstancePrefix NodeTypeTiebreaker = "tiebreaker"

curlCertificateVerificationOption :: CertificateVerificationConfig -> String
curlCertificateVerificationOption NoCertificateVerificationConfig                     = " --insecure"
curlCertificateVerificationOption (CustomCertificateVerificationConfig certStorePath) = " --cacert '" ++ certStorePath ++ "'"
curlCertificateVerificationOption _                                                   = ""

curlCredentialsOption :: Bool -> CredentialsConfig -> String
curlCredentialsOption _                   NoCredentials = ""
curlCredentialsOption esShowCurlPassword (BasicCredentials userString passString)  = " --user '" ++ userString ++ ":" ++ (if esShowCurlPassword then (passString ++ "'") else "'$(cat escli_config.json | jq -r .credentials.pass)")
curlCredentialsOption _                  (ApiKeyCredentials apiKeyEnvVar)          = " --header \"Authorization: ApiKey ${" ++ apiKeyEnvVar ++ "}\" --header \"X-Management-Request: true\""
curlCredentialsOption _                  (MacOsKeyringCredentials service account) = " --header \"Authorization: ApiKey $(security find-generic-password -s "
                                                                                   ++ show service ++ " -a " ++ show account ++ " -w)\" --header \"X-Management-Request: true\""

runCommand :: URI -> Config -> (Request -> Request) -> Manager -> ESCommand -> ConduitT ESCommand (String, String) IO ()
runCommand
    baseURI
    Config
        { esGeneralConfig    = GeneralConfig{..}
        , esConnectionConfig = ConnectionConfig{..}
        }
    applyCredentials
    manager
    ESCommand{..}

    = do

    let absUri = maybe (error "Bad URI") (show . (`relativeTo` baseURI)) (parseURIReference httpPath)
        initReq = fromMaybe (error "Bad URI") $ parseRequest absUri

        BuilderWithLength bodyBuilder bodyLength = builderFromBody cmdBody

        maybeContentTypeHeader = case cmdBody of
                []  -> []
                [_] -> [(hContentType, "application/json")]
                _   -> [(hContentType, "application/x-ndjson")]

        req = applyCredentials initReq
            { method = httpVerb
            , requestHeaders = maybeContentTypeHeader
            , requestBody = RequestBodyBuilder bodyLength bodyBuilder
            }

        httpVerbString = T.unpack $ T.decodeUtf8 httpVerb
        resolvedUriString = getUri req `relativeFrom` baseURI
        tellBoth l = tell (l,l)
        tellLn l = tellBoth l >> tellBoth "\n"

    before <- liftIO getCurrentTime

    unless esOnlyResponse $ yield $ execWriter $ do
        unless esHideHeadings $ do
            tellLn $ "# " ++ replicate 40 '='
            tellLn   "# Request: "
        tellLn $ httpVerbString <> " " <> show resolvedUriString
        case cmdBody of
            [v] -> tellLn $ prettyStringFromJson v
            _   -> forM_ cmdBody $ tellLn . T.unpack . T.decodeUtf8 . BL.toStrict . encode
        unless esHideTiming $
            tellLn $ "# at " ++ formatISO8601Millis before
        unless esHideCurlEquivalent $ tellLn $ execWriter $ do
            tell "# curl --silent --compressed"
            tell $ curlCertificateVerificationOption esCertificateVerificationConfig
            tell $ curlCredentialsOption             esShowCurlPassword esCredentialsConfig
            case maybeContentTypeHeader of
                []    | httpVerbString == "GET"  -> return ()
                (_:_) | httpVerbString == "POST" -> return ()
                _                                -> tell $ " -X" ++ httpVerbString
            tell $ " '" ++ absUri ++ "'"
            case maybeContentTypeHeader of
                [] -> return ()
                ((_, ct):_) -> tell $ " -H 'Content-type: " ++ T.unpack (T.decodeUtf8 ct) ++ "'"
            when (bodyLength > 0) $ do
                tell " --data-binary $'"
                tell $ concatMap escapeShellQuoted $ concatMap (T.unpack . T.decodeUtf8) $ BL.toChunks $ B.toLazyByteString bodyBuilder
                tell "'"

    response <- liftIO $ httpLbs req manager
    after <- liftIO getCurrentTime

    if esOnlyResponse
      then liftIO $ BL.hPutStr stdout $ responseBody response
      else yield $ execWriter $ do
        unless esHideHeadings $ do
            tellLn $ "# " ++ replicate 40 '-'
            tellLn "# Response: "
        unless esHideStatusCode $
            tellLn $ "# " ++ show (statusCode    $ responseStatus response)
                    ++ " "  ++ T.unpack (T.decodeUtf8 $ statusMessage $ responseStatus response)

        unless esHideDeprecationWarnings $
            forM_ [ headerContent | (headerName, headerContent) <- responseHeaders response, headerName == "Warning" ] $ \headerContent ->
                tellLn $ "# WARNING: " ++ (T.unpack $ T.decodeUtf8 headerContent)

        let linesFromJsonBody b = case (eitherDecode b :: Either String Value) of
                Left er -> ["JSON parse error: " ++ show er, show b]
                Right bv -> Prelude.lines $ prettyStringFromJson bv
            linesFromPlainBody b = map (T.unpack . T.decodeUtf8 . BL.toStrict) $ BL.split 0x0a b

        case lookup hContentType (responseHeaders response) of
            Nothing -> tellLn "# No content-type returned"
            Just ct -> case mapContentMedia [("application/json", linesFromJsonBody), ("text/plain", linesFromPlainBody)] ct of
                Nothing -> tellLn $ "# Unknown content-type: " ++ show ct
                Just linesFn -> if esMaxResponseLines < 0
                    then forM_ (linesFn $ responseBody response) $ \l -> tellLn $ "# " ++ l
                    else go esMaxResponseLines 0 (linesFn $ responseBody response)
                        where   go _ skipped [] = do
                                    when (0 < skipped) $ tell ("# ... (" ++ show (skipped::Int) ++ " lines skipped)\n", "")
                                    return ()
                                go linesRemaining skipped (l:ls) = if 0 < (linesRemaining::Int)
                                    then do
                                        tellLn $ "# " ++ l
                                        go (linesRemaining - 1) skipped ls
                                    else do
                                        tell ("", "# " ++ l ++ "\n")
                                        go 0 (skipped + 1) ls


        unless esHideTiming $ do
            tellLn $ "# at " ++ formatISO8601Millis after
            tellLn $ "# (" ++ show (diffUTCTime after before) ++ " elapsed)"

data BuilderWithLength = BuilderWithLength B.Builder !Int64

instance Semigroup BuilderWithLength where
    BuilderWithLength b1 l1 <> BuilderWithLength b2 l2
        = BuilderWithLength (b1 <> b2) (l1 + l2)

instance Monoid BuilderWithLength where
    mempty = BuilderWithLength mempty 0

jsonWithLength :: ToJSON a => a -> BuilderWithLength
jsonWithLength v = BuilderWithLength (B.lazyByteString bs) (BL.length bs)
  where bs = encode v

newlineWithLength :: BuilderWithLength
newlineWithLength = BuilderWithLength (B.word8 0x0a) 1

builderFromBody :: [Value] -> BuilderWithLength
builderFromBody []  = mempty
builderFromBody [v] = jsonWithLength v
builderFromBody vs  = mconcat $ map ((<> newlineWithLength) . jsonWithLength) vs
