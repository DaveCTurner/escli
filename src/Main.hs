{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Config
import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Default.Class
import Data.Int
import Data.Maybe
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
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as DCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

withMaybeLogFile :: Maybe FilePath -> ((B.ByteString -> IO ()) -> IO a) -> IO a
withMaybeLogFile Nothing go = go (const $ return ())
withMaybeLogFile (Just fp) go = withFile fp AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  go (B.hPutStr h)

main :: IO ()
main = withConfig $ \config@Config{esGeneralConfig=GeneralConfig{..},..} -> do

    putStrLn $ "# Server URI: " ++ show esBaseURI
    putStrLn $ ""

    (certStore, verifyCert) <- case esCertificateVerificationConfig of
        DefaultCertificateVerificationConfig -> (,True)  <$> getSystemCertificateStore
        NoCertificateVerificationConfig      -> (,False) <$> getSystemCertificateStore
        CustomCertificateVerificationConfig certStorePath -> do
            maybeCertStore <- readCertificateStore certStorePath
            case maybeCertStore of
                Just certStore -> return (certStore, True)
                Nothing -> error $ "failed to read certificate store from " ++ certStorePath

    let hostName = fromMaybe "" $ fmap uriRegName $ uriAuthority esBaseURI

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

    withMaybeLogFile esLogFile $ \writeLog ->
        runConduit
             $  sourceHandle stdin
             .| conduitParser esCommand
             .| DCL.map snd
             .| awaitForever (runCommand config manager)
             .| awaitForever (\logEntry -> liftIO $ do
                    putStrLn logEntry
                    writeLog $ T.encodeUtf8 $ T.pack $ logEntry ++ "\n")

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

runCommand :: Config -> Manager -> ESCommand -> ConduitT ESCommand String IO ()
runCommand Config{esGeneralConfig=GeneralConfig{..},..} manager ESCommand{..} = do
    let absUri = maybe (error "Bad URI") (show . (`relativeTo` esBaseURI)) (parseURIReference httpPath)
        initReq = fromMaybe (error "Bad URI") $ parseRequest absUri

        BuilderWithLength bodyBuilder bodyLength = builderFromBody cmdBody

        maybeContentTypeHeader = case cmdBody of
                []  -> []
                [_] -> [(hContentType, "application/json")]
                _   -> [(hContentType, "application/x-ndjson")]

        withCredentials = maybe id applyCredentials esCredentials
          where
          applyCredentials (userString, passString) = applyBasicAuth (credFromString userString) (credFromString passString)
          credFromString = T.encodeUtf8 . T.pack

        req = withCredentials initReq
            { method = httpVerb
            , requestHeaders = maybeContentTypeHeader
            , requestBody = RequestBodyBuilder bodyLength bodyBuilder
            }

        httpVerbString = T.unpack $ T.decodeUtf8 httpVerb
        resolvedUriString = getUri req `relativeFrom` esBaseURI
        tellLn l = tell l >> tell "\n"

    before <- liftIO getCurrentTime

    yield $ execWriter $ do
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
            tell "# curl --compressed"
            case esCertificateVerificationConfig of
                NoCertificateVerificationConfig                   -> tell " -k"
                CustomCertificateVerificationConfig certStorePath -> tell $ " --cacert '" ++ certStorePath ++ "'"
                _                                                 -> return ()
            case esCredentials of
                Nothing -> return ()
                Just (userString, passString) -> tell $ " -u '" ++ userString ++ ":" ++ (if esShowCurlPassword then passString else "<REDACTED>") ++ "'"
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

    yield $ execWriter $ do
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
                Just linesFn -> forM_ (linesFn $ responseBody response) $ \l -> tellLn $ "# " ++ l

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
