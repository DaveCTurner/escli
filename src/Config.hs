{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( Config(..)
    , GeneralConfig(..)
    , CertificateVerificationConfig(..)
    , CredentialsConfig(..)
    , ConnectionConfig(..)
    , EndpointConfig(..)
    , withConfig
    , configFileName
    ) where

import Options.Applicative
import Network.URI
import System.FilePath
import System.Directory
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

configFileName :: FilePath
configFileName = "escli_config.json"

data GeneralConfig = GeneralConfig
    { esHideTiming              :: Bool
    , esHideHeadings            :: Bool
    , esHideStatusCode          :: Bool
    , esHideCurlEquivalent      :: Bool
    , esShowCurlPassword        :: Bool
    , esHideDeprecationWarnings :: Bool
    , esSaveConnectionConfig    :: Bool
    , esMaxResponseLines        :: Int
    , esLogFile                 :: Maybe FilePath
    } deriving (Show, Eq)

data CertificateVerificationConfig
    = DefaultCertificateVerificationConfig
    | NoCertificateVerificationConfig
    | CustomCertificateVerificationConfig FilePath
    deriving (Show, Eq)

certificateVerificationConfigParser :: Parser CertificateVerificationConfig
certificateVerificationConfigParser
   = (CustomCertificateVerificationConfig <$> strOption
        (  long "certificate-store"
        <> help "Location of certificate store"
        <> metavar "FILE"))
   <|> (flag' NoCertificateVerificationConfig
        (  long "insecurely-bypass-certificate-verification"
        <> help "Do not perform certificate verification"))
   <|> pure DefaultCertificateVerificationConfig

generalConfigParser :: Parser GeneralConfig
generalConfigParser = GeneralConfig
    <$> switch
        (  long "hide-timing"
        <> help "Hide timing information")
    <*> switch
        (  long "hide-headings"
        <> help "Hide request/response headings")
    <*> switch
        (  long "hide-status"
        <> help "Hide HTTP status code")
    <*> switch
        (  long "hide-curl-equivalent"
        <> help "Hide `curl`-equivalent command")
    <*> switch
        (  long "show-curl-password"
        <> help "Show password in `curl`-equivalent command")
    <*> switch
        (  long "hide-deprecation-warnings"
        <> help "Hide deprecation warnings")
    <*> switch
        (  long "save"
        <> help ("Save connection config to '" ++ configFileName ++ "' for future invocations"))
    <*> (flag' (-1)
            (  long "no-max-response-lines"
            <> help "Show an unlimited number of lines of response")
        <|> option auto
            (  long "max-response-lines"
            <> help "Maximum number of lines of response to show"
            <> metavar "LINES"
            <> showDefault
            <> value 40))
    <*> optional (strOption
        (  long "log-file"
        <> help "File in which to record output"
        <> metavar "FILE"))

data CredentialsConfig
    = NoCredentials
    | BasicCredentials  String String
    | ApiKeyCredentials String
    deriving (Show, Eq)

credentialsConfigParser :: Parser CredentialsConfig
credentialsConfigParser
    = (BasicCredentials
        <$> strOption
            (  long "username"
            <> help "Elasticsearch username, for security-enabled clusters"
            <> metavar "USERNAME")
        <*> strOption
            (  long "password"
            <> help "Elasticsearch password, for security-enabled clusters"
            <> metavar "PASSWORD"))
    <|> (ApiKeyCredentials
        <$> strOption
            (  long "api-key"
            <> help "Environment variable holding API key"
            <> metavar "ENVVAR"))
    <|> pure NoCredentials

instance FromJSON CredentialsConfig where
    parseJSON = withObject "CredentialsConfig" $ \v -> do
        credType <- v .: "type"
        case credType of
            "apikey" -> ApiKeyCredentials <$> v .: "var"
            "basic"  -> BasicCredentials  <$> v .: "user" <*> v .: "pass"
            _        -> fail $ "unknown credentials type '" ++ credType ++ "'"

instance ToJSON CredentialsConfig where
    toJSON (ApiKeyCredentials var) = object ["type" .= ("apikey" :: String), "var" .= var]
    toJSON (BasicCredentials  user pass) = object ["type" .= ("basic"  :: String), "user" .= user, "pass" .= pass]
    toJSON v = error $ "saving credentials " ++ show v ++ " not supported"

data EndpointConfig
    = DefaultEndpoint
    | URIEndpoint             URI
    | CloudDeploymentEndpoint URI String (Maybe String)
    deriving (Show, Eq)

data ConnectionConfig = ConnectionConfig
    { esEndpointConfig                :: EndpointConfig
    , esCredentialsConfig             :: CredentialsConfig
    } deriving (Show, Eq)

uriEndpointConfigParser :: Parser ConnectionConfig
uriEndpointConfigParser = ConnectionConfig
    <$> (URIEndpoint <$> option (maybeReader parseAbsoluteURI)
        (  long "server"
        <> help "Base HTTP URI of the Elasticsearch server"
        <> metavar "ADDR")
        <|> pure DefaultEndpoint)
    <*> credentialsConfigParser

cloudDeploymentEndpointConfigParser :: Parser ConnectionConfig
cloudDeploymentEndpointConfigParser = buildConnectionConfig
    <$> strOption
        (  long "deployment"
        <> help "Cloud deployment ID"
        <> metavar "DEPLOYMENT-ID")
    <*> optional (strOption
        (  long "deployment-ref"
        <> help "Cloud deployment reference"
        <> metavar "REF-ID"))
    <*> strOption
        (  long "cloud-api-root"
        <> help "URL of root Cloud API endpoint"
        <> metavar "URL"
        <> value "https://admin.found.no/"
        <> showDefault)
    <*> strOption
        (  long "api-key"
        <> help "Environment variable holding API key"
        <> metavar "ENVVAR"
        <> value "ADMIN_EC_API_KEY"
        <> showDefault)
    where
        buildConnectionConfig deploymentId deploymentRefId apiRootString apiKeyVar = case parseAbsoluteURI apiRootString of
            Just apiRoot -> ConnectionConfig
                { esEndpointConfig    = CloudDeploymentEndpoint apiRoot deploymentId deploymentRefId
                , esCredentialsConfig = ApiKeyCredentials apiKeyVar
                }
            Nothing -> error $ "could not parse API root URL '" ++ apiRootString ++ "'"

instance FromJSON ConnectionConfig where
    parseJSON = withObject "ConnectionConfig" $ \v -> ConnectionConfig
        <$> (uriFromString =<< (v .: "baseuri"))
        <*> (v .: "credentials")
        where
            uriFromString :: String -> Aeson.Parser EndpointConfig
            uriFromString s = case parseAbsoluteURI s of
                Just u  -> pure $ URIEndpoint u
                Nothing -> fail $ "could not parse URI '" ++ s ++ "'"

instance ToJSON ConnectionConfig where
    toJSON ConnectionConfig{esEndpointConfig = URIEndpoint esBaseURI,..} = object ["baseuri" .= show esBaseURI, "credentials" .= esCredentialsConfig]
    toJSON cc = error $ "saving connection config " ++ show cc ++ " not supported"

data Config = Config
    { esConnectionConfig              :: ConnectionConfig
    , esGeneralConfig                 :: GeneralConfig
    , esCertificateVerificationConfig :: CertificateVerificationConfig
    } deriving (Show, Eq)

configParser :: Parser Config
configParser = Config
    <$> (cloudDeploymentEndpointConfigParser <|> uriEndpointConfigParser)
    <*> generalConfigParser
    <*> certificateVerificationConfigParser

configParserInfo :: ParserInfo Config
configParserInfo = info (configParser <**> helper)
    (fullDesc
        <> progDesc "Interact with Elasticsearch from the shell"
        <> header "escli - Interact with Elasticsearch from the shell")

findConfigFile :: IO (Maybe FilePath)
findConfigFile = go =<< getCurrentDirectory
  where
    go d = do
        let candidatePath = d </> configFileName
            parentPath = takeDirectory d
        foundIt <- doesFileExist candidatePath
        if foundIt
            then return (Just candidatePath)
            else if parentPath == d
                then return Nothing
                else go parentPath

withConfig :: (Config -> IO a) -> IO a
withConfig go = do
    argsConfig <- execParser configParserInfo
    config <- if esConnectionConfig argsConfig == ConnectionConfig DefaultEndpoint NoCredentials
        then do
            maybeConfigFilePath <- findConfigFile
            case maybeConfigFilePath of
                Nothing -> return argsConfig
                Just configFilePath -> do
                    fileConfigOrError <- eitherDecodeFileStrict' configFilePath
                    case fileConfigOrError of
                        Left msg -> error msg
                        Right fileConfig -> do
                            putStrLn $ "# Loaded connection config from '" ++ configFilePath ++ "'"
                            return argsConfig {esConnectionConfig = fileConfig}
        else return argsConfig
    go config
