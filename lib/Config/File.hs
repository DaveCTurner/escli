{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.File
    ( withConfig
    , saveConfigFile
    ) where

import Config
import Config.Parser
import Control.Monad (unless)
import Data.Aeson
import Data.Maybe
import Options.Applicative
import Network.URI
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import qualified Data.Aeson.Types as Aeson

configFileName :: FilePath
configFileName = "escli_config.json"

newtype JsonCredentials = JsonCredentials { unJsonCredentials :: CredentialsConfig }

instance FromJSON JsonCredentials where
    parseJSON = withObject "JsonCredentials" $ \v -> JsonCredentials <$> do
        credType <- v .: "type"
        case credType of
            "apikey"         -> ApiKeyCredentials       <$> v .: "var"
            "mac-os-keyring" -> MacOsKeyringCredentials <$> v .: "service" <*> v .: "account"
            "basic"          -> BasicCredentials        <$> v .: "user"    <*> v .: "pass"
            _                -> fail $ "unknown credentials type '" ++ credType ++ "'"

instance ToJSON JsonCredentials where
    toJSON (JsonCredentials (MacOsKeyringCredentials service account)) = object ["type" .= ("mac-os-keyring" :: String), "service" .= service, "account" .= account]
    toJSON (JsonCredentials (ApiKeyCredentials       var))             = object ["type" .= ("apikey" :: String),         "var"     .= var]
    toJSON (JsonCredentials (BasicCredentials        user pass))       = object ["type" .= ("basic"  :: String),         "user"    .= user, "pass" .= pass]
    toJSON (JsonCredentials v) = error $ "saving credentials " ++ show v ++ " not supported"

newtype JsonConnection = JsonConnection { unJsonConnection :: ConnectionConfig }

instance FromJSON JsonConnection where
    parseJSON = withObject "ConnectionConfig" $ \v -> JsonConnection <$> (ConnectionConfig
        <$> (uriFromString      =<< (v .: "baseuri"))
        <*> (unJsonCredentials <$> (v .: "credentials"))
        <*> (maybe DefaultCertificateVerificationConfig CustomCertificateVerificationConfig <$> (v .:! "certFile")))
        where
            uriFromString :: String -> Aeson.Parser EndpointConfig
            uriFromString s = case parseAbsoluteURI s of
                Just u  -> pure $ URIEndpoint u
                Nothing -> fail $ "could not parse URI '" ++ s ++ "'"

instance ToJSON JsonConnection where
    toJSON (JsonConnection ConnectionConfig{esEndpointConfig = URIEndpoint esBaseURI,..}) = object $
        [ "baseuri"     .= show esBaseURI
        , "credentials" .= JsonCredentials esCredentialsConfig
        ] ++ (case esCertificateVerificationConfig of
                DefaultCertificateVerificationConfig -> []
                CustomCertificateVerificationConfig file -> ["certFile" .= file]
                _ -> error ("saving cert verification config " ++ show esCertificateVerificationConfig ++ " not supported"))
    toJSON (JsonConnection cc) = error $ "saving connection config " ++ show cc ++ " not supported"

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
    envUrlEnvVar <- lookupEnv "ENV_URL"
    argsConfig <- execParser $ configParserInfo ConfigParserContext
        { configParserContextFileName = configFileName
        , configParserContextApiRoot  = fromMaybe "unset" envUrlEnvVar
        }
    config <- if esConnectionConfig argsConfig == ConnectionConfig DefaultEndpoint NoCredentials DefaultCertificateVerificationConfig
        then do
            maybeConfigFilePath <- findConfigFile
            case maybeConfigFilePath of
                Nothing -> return argsConfig
                Just configFilePath -> do
                    fileConfig <- either error unJsonConnection <$> eitherDecodeFileStrict' configFilePath
                    unless (esOnlyResponse $ esGeneralConfig argsConfig) $ putStrLn $ "# Loaded connection config from '" ++ configFilePath ++ "'"
                    return argsConfig {esConnectionConfig = fileConfig}
        else return argsConfig
    go config

saveConfigFile :: ConnectionConfig -> IO FilePath
saveConfigFile connectionConfig = do
    encodeFile configFileName $ JsonConnection connectionConfig
    return configFileName