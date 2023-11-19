{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.File
    ( withConfig
    , configFileName
    ) where

import Config
import Config.Parser
import Control.Monad (unless)
import Options.Applicative
import Network.URI
import System.FilePath
import System.Directory
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

configFileName :: FilePath
configFileName = "escli_config.json"

instance FromJSON CredentialsConfig where
    parseJSON = withObject "CredentialsConfig" $ \v -> do
        credType <- v .: "type"
        case credType of
            "apikey"         -> ApiKeyCredentials       <$> v .: "var"
            "mac-os-keyring" -> MacOsKeyringCredentials <$> v .: "service" <*> v .: "account"
            "basic"          -> BasicCredentials        <$> v .: "user"    <*> v .: "pass"
            _                -> fail $ "unknown credentials type '" ++ credType ++ "'"

instance ToJSON CredentialsConfig where
    toJSON (MacOsKeyringCredentials service account) = object ["type" .= ("mac-os-keyring" :: String), "service" .= service, "account" .= account]
    toJSON (ApiKeyCredentials       var)             = object ["type" .= ("apikey" :: String),         "var"     .= var]
    toJSON (BasicCredentials        user pass)       = object ["type" .= ("basic"  :: String),         "user"    .= user, "pass" .= pass]
    toJSON v = error $ "saving credentials " ++ show v ++ " not supported"

instance FromJSON ConnectionConfig where
    parseJSON = withObject "ConnectionConfig" $ \v -> ConnectionConfig
        <$> (uriFromString =<< (v .: "baseuri"))
        <*> (v .: "credentials")
        <*> (maybe DefaultCertificateVerificationConfig CustomCertificateVerificationConfig <$> (v .:! "certFile"))
        where
            uriFromString :: String -> Aeson.Parser EndpointConfig
            uriFromString s = case parseAbsoluteURI s of
                Just u  -> pure $ URIEndpoint u
                Nothing -> fail $ "could not parse URI '" ++ s ++ "'"

instance ToJSON ConnectionConfig where
    toJSON ConnectionConfig{esEndpointConfig = URIEndpoint esBaseURI,..} = object $
        [ "baseuri"     .= show esBaseURI
        , "credentials" .= esCredentialsConfig
        ] ++ (case esCertificateVerificationConfig of
                DefaultCertificateVerificationConfig -> []
                CustomCertificateVerificationConfig file -> ["certFile" .= file]
                _ -> error ("saving cert verification config " ++ show esCertificateVerificationConfig ++ " not supported"))
    toJSON cc = error $ "saving connection config " ++ show cc ++ " not supported"

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
    argsConfig <- execParser $ configParserInfo ConfigParserContext
        { configParserContextFileName = configFileName
        }
    config <- if esConnectionConfig argsConfig == ConnectionConfig DefaultEndpoint NoCredentials DefaultCertificateVerificationConfig
        then do
            maybeConfigFilePath <- findConfigFile
            case maybeConfigFilePath of
                Nothing -> return argsConfig
                Just configFilePath -> do
                    fileConfigOrError <- eitherDecodeFileStrict' configFilePath
                    case fileConfigOrError of
                        Left msg -> error msg
                        Right fileConfig -> do
                            unless (esOnlyResponse $ esGeneralConfig argsConfig) $ putStrLn $ "# Loaded connection config from '" ++ configFilePath ++ "'"
                            return argsConfig {esConnectionConfig = fileConfig}
        else return argsConfig
    go config
