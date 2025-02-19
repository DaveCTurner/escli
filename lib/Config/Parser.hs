{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Parser
    ( configParserInfo
    , ConfigParserContext(..)
    ) where

import Config
import Options.Applicative
import Network.URI

data ConfigParserContext = ConfigParserContext
  { configParserContextFileName      :: String
  , configParserContextApiRoot       :: String
  , configParserContextApiKeyService :: Maybe String
  } deriving (Show, Eq)

configParserInfo :: ConfigParserContext -> ParserInfo Config
configParserInfo configParserContext = info (configParser configParserContext <**> helper)
    (fullDesc
        <> progDesc "Interact with Elasticsearch from the shell"
        <> header "escli - Interact with Elasticsearch from the shell")

configParser :: ConfigParserContext -> Parser Config
configParser configParserContext = Config
    <$> (   cloudDeploymentEndpointConfigParser   configParserContext
        <|> serverlessProjectEndpointConfigParser configParserContext
        <|> uriEndpointConfigParser
        )
    <*> generalConfigParser configParserContext

cloudApiRootParser :: ConfigParserContext -> Parser URI
cloudApiRootParser ConfigParserContext{..} = uriFromString <$> strOption
    (  long "cloud-api-root"
    <> help "URL of root Cloud API endpoint (${ENV_URL} by default)"
    <> metavar "URL"
    <> value configParserContextApiRoot
    <> showDefault)
    where
    uriFromString s = case parseAbsoluteURI s of
        Nothing -> error $ "could not parse API root URL " ++ show s
        Just u  -> u

cloudDeploymentEndpointConfigParser :: ConfigParserContext -> Parser ConnectionConfig
cloudDeploymentEndpointConfigParser configParserContext = buildConnectionConfig
    <$> strOption
        (  long "deployment"
        <> help "Cloud deployment ID"
        <> metavar "DEPLOYMENT-ID")
    <*> optional (strOption
        (  long "deployment-ref"
        <> help "Cloud deployment reference"
        <> metavar "REF-ID"))
    <*> cloudApiRootParser configParserContext
    <*> contextCredentialsConfigParser configParserContext
    where
        buildConnectionConfig deploymentId deploymentRefId apiRoot credentialsConfig = ConnectionConfig
            { esEndpointConfig    = CloudDeploymentEndpoint apiRoot deploymentId deploymentRefId
            , esCredentialsConfig = credentialsConfig
            , esCertificateVerificationConfig = DefaultCertificateVerificationConfig
            }

serverlessProjectEndpointConfigParser :: ConfigParserContext -> Parser ConnectionConfig
serverlessProjectEndpointConfigParser configParserContext = buildConnectionConfig
    <$> strOption
        (  long "project"
        <> help "Cloud serverless project ID"
        <> metavar "PROJECT-ID")
    <*> cloudApiRootParser configParserContext
    <*> contextCredentialsConfigParser configParserContext
    where
        buildConnectionConfig projectId apiRoot credentialsConfig = ConnectionConfig
            { esEndpointConfig    = ServerlessProjectEndpoint apiRoot projectId
            , esCredentialsConfig = credentialsConfig
            , esCertificateVerificationConfig = DefaultCertificateVerificationConfig
            }

uriEndpointConfigParser :: Parser ConnectionConfig
uriEndpointConfigParser = ConnectionConfig
    <$> (URIEndpoint <$> option (maybeReader parseAbsoluteURI)
        (  long "server"
        <> help "Base HTTP URI of the Elasticsearch server"
        <> metavar "ADDR")
        <|> pure DefaultEndpoint)
    <*> credentialsConfigParser
    <*> certificateVerificationConfigParser

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
    <|> macOsKeyringCredentialsConfigParser
    <|> pure NoCredentials

macOsKeyringCredentialsConfigParser :: Parser CredentialsConfig
macOsKeyringCredentialsConfigParser = MacOsKeyringCredentials
    <$> strOption
        (  long "mac-os-keyring-service"
        <> help "Name of service to look up in MacOS keyring"
        <> metavar "SERVICE")
    <*> optional (strOption
        (  long "mac-os-keyring-account"
        <> help "Name of account to look up in MacOS keyring"
        <> metavar "ACCOUNT"))

contextCredentialsConfigParser :: ConfigParserContext -> Parser CredentialsConfig
contextCredentialsConfigParser ConfigParserContext{..} = macOsKeyringCredentialsConfigParser
    <|> case configParserContextApiKeyService of
            Nothing -> empty
            Just keyringService -> pure $ MacOsKeyringCredentials keyringService Nothing

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

generalConfigParser :: ConfigParserContext -> Parser GeneralConfig
generalConfigParser ConfigParserContext{..} = GeneralConfig
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
        (  long "only-response"
        <> help "Only emit raw response")
    <*> switch
        (  long "save"
        <> help ("Save connection config to '" ++ configParserContextFileName ++ "' for future invocations"))
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
    <*> optional oneShotCommandConfigParser

oneShotCommandConfigParser :: Parser OneShotCommandConfig
oneShotCommandConfigParser
    =   (ThreadDumpNode NodeTypeInstance <$> option auto
            (  long "thread-dump"
            <> help "Capture thread dump of node"
            <> metavar "INSTANCE-ID"))
    <|> (ThreadDumpNode NodeTypeTiebreaker <$> option auto
            (  long "thread-dump-tiebreaker"
            <> help "Capture thread dump of tiebreaker node"
            <> metavar "INSTANCE-ID"))
    <|> (flag' HeapDumpList
            (  long "heap-dumps"
            <> help "List available heap dumps"))
