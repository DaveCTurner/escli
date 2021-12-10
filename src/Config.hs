{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), GeneralConfig(..), CertificateVerificationConfig(..), CredentialsConfig(..), withConfig) where

import Options.Applicative
import Network.URI

data GeneralConfig = GeneralConfig
    { esHideTiming              :: Bool
    , esHideHeadings            :: Bool
    , esHideStatusCode          :: Bool
    , esHideCurlEquivalent      :: Bool
    , esShowCurlPassword        :: Bool
    , esHideDeprecationWarnings :: Bool
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
            (  long "apikey"
            <> help "Environment variable holding API key"
            <> metavar "ENVVAR"))
    <|> pure NoCredentials

data Config = Config
    { esBaseURI                       :: URI
    , esCredentialsConfig             :: CredentialsConfig
    , esGeneralConfig                 :: GeneralConfig
    , esCertificateVerificationConfig :: CertificateVerificationConfig
    } deriving (Show, Eq)

configParser :: Parser Config
configParser = Config
    <$> option (maybeReader parseAbsoluteURI)
        (  long "server"
        <> help "Base HTTP URI of the Elasticsearch server"
        <> showDefault
        <> value URI
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
        <> metavar "ADDR")
    <*> credentialsConfigParser
    <*> generalConfigParser
    <*> certificateVerificationConfigParser

configParserInfo :: ParserInfo Config
configParserInfo = info (configParser <**> helper)
    (fullDesc
        <> progDesc "Interact with Elasticsearch from the shell"
        <> header "escli - Interact with Elasticsearch from the shell")

withConfig :: (Config -> IO a) -> IO a
withConfig go = go =<< execParser configParserInfo
