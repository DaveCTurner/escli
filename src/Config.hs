{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), withConfig) where

import Options.Applicative
import Network.URI

data Config = Config
    { esBaseURI                 :: URI
    , esHideTiming              :: Bool
    , esHideHeadings            :: Bool
    , esHideStatusCode          :: Bool
    , esHideCurlEquivalent      :: Bool
    , esHideDeprecationWarnings :: Bool
    , esNoVerifyCert            :: Bool
    , esLogFile                 :: Maybe FilePath
    , esCredentials             :: Maybe (String, String)
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
    <*> switch 
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
        (  long "hide-deprecation-warnings"
        <> help "Hide deprecation warnings")
    <*> switch 
        (  long "insecurely-bypass-certificate-verification"
        <> help "Do not perform certificate verification")
    <*> optional (strOption
        (  long "log-file"
        <> help "File in which to record output"
        <> metavar "FILE"))
    <*> optional ((,)
        <$> strOption
            (  long "username"
            <> help "Elasticsearch username, for security-enabled clusters"
            <> metavar "USERNAME")
        <*> strOption
            (  long "password"
            <> help "Elasticsearch password, for security-enabled clusters"
            <> metavar "PASSWORD"))

configParserInfo :: ParserInfo Config
configParserInfo = info (configParser <**> helper)
    (fullDesc
        <> progDesc "Interact with Elasticsearch from the shell"
        <> header "escli - Interact with Elasticsearch from the shell")

withConfig :: (Config -> IO a) -> IO a
withConfig go = go =<< execParser configParserInfo
