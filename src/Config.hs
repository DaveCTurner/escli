{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), withConfig) where

import Options.Applicative
import Data.Monoid
import Network.URI

data Config = Config
    { esBaseURI            :: URI
    , esHideTiming         :: Bool
    , esHideHeadings       :: Bool
    , esHideStatusCode     :: Bool
    , esHideCurlEquivalent :: Bool
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

configParserInfo :: ParserInfo Config
configParserInfo = info (configParser <**> helper)
    (fullDesc
        <> progDesc "Interact with Elasticsearch from the shell"
        <> header "escli - Interact with Elasticsearch from the shell")

withConfig :: (Config -> IO a) -> IO a
withConfig go = go =<< execParser configParserInfo