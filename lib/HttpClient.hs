{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module HttpClient
    ( HttpClient()
    , runHttpClient
    , buildHttpClient
    , runRequestWith
    ) where

import Config
import CredentialsUtils
import Data.Aeson
import Data.ByteString.UTF8 as BSU
import Data.Default.Class
import Data.Maybe
import Data.String.Utils (strip)
import Data.X509.CertificateStore
import Data.X509.Validation
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.TLS
import Network.TLS.Extra.Cipher
import Network.URI
import System.X509
import System.Environment (lookupEnv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as SP

newtype HttpClient = HttpClient { runHttpClient :: forall a. (Request -> (Response BodyReader -> IO a) -> IO a) }

buildHttpClient :: URI -> ConnectionConfig -> IO HttpClient
buildHttpClient defaultBaseUri esConnectionConfig@ConnectionConfig{..} = do
    manager          <- buildManager          defaultBaseUri esConnectionConfig
    applyCredentials <- buildApplyCredentials                esCredentialsConfig
    return HttpClient { runHttpClient = \request -> withResponse (applyCredentials request) manager }

buildManager :: URI -> ConnectionConfig -> IO Manager
buildManager defaultBaseUri ConnectionConfig{..} = do
    let validateNone = \_ _ _ _ -> return []
    (certStore, verifyCert) <- case esCertificateVerificationConfig of
        DefaultCertificateVerificationConfig -> (,validateDefault)  <$> getSystemCertificateStore
        NoCertificateVerificationConfig      -> (,validateNone)     <$> getSystemCertificateStore
        CustomCertificateVerificationConfig certStorePath -> do
            maybeCertStore <- readCertificateStore certStorePath
            case maybeCertStore of
                Just certStore -> return (certStore, validateDefault)
                Nothing -> error $ "failed to read certificate store from " ++ certStorePath

    let clientParams = (defaultParamsClient (hostName defaultBaseUri esEndpointConfig) B.empty)
            { clientSupported = def { supportedCiphers    = ciphersuite_default }
            , clientShared    = def { sharedCAStore       = certStore }
            , clientHooks     = def { onServerCertificate = verifyCert }
            }
        httpClientSettings = mkManagerSettings (TLSSettings clientParams) Nothing

    newManager httpClientSettings { managerResponseTimeout = responseTimeoutNone }

hostName :: URI -> EndpointConfig -> String
hostName defaultBaseUri = hostNameFromURI . \case
    DefaultEndpoint                       -> defaultBaseUri
    URIEndpoint               baseURI     -> baseURI
    CloudDeploymentEndpoint   apiRoot _ _ -> apiRoot
    ServerlessProjectEndpoint apiRoot _   -> apiRoot

hostNameFromURI :: URI -> String
hostNameFromURI uri = fromMaybe (error $ "could not extract hostname from URI " ++ show uri) $ uriRegName <$> uriAuthority uri

buildApplyCredentials :: CredentialsConfig -> IO (Request -> Request)
buildApplyCredentials = \case
    NoCredentials                           -> return id
    BasicCredentials{..} -> return $ applyBasicAuth (credFromString esCredentialsBasicUser) (credFromString esCredentialsBasicPassword)
    ApiKeyCredentials{..}          -> do
        maybeApiKey <- lookupEnv esCredentialsApiKeyEnvVar
        case maybeApiKey of
            Nothing -> error $ "Environment variable '" ++ esCredentialsApiKeyEnvVar ++ "' not set (maybe run set-cloud-env?)"
            Just apiKey ->
                return $ \req -> req
                    { requestHeaders
                        = (hAuthorization,         credFromString $ "ApiKey " ++ apiKey)
                        : ("X-Management-Request", "true")
                        : requestHeaders req
                    }
    MacOsKeyringCredentials{..} -> do
        keyringContentsString <- SP.readProcess "security" (macOsKeyringLookupArgs esCredentialsKeyringService esCredentialsKeyringAccount) ""
        let apiKey = case eitherDecodeStrict $ BSU.fromString keyringContentsString of
                            Left msg -> error $ "failed to decode keyring value as JSON: " ++ msg
                            Right (KeyringEntry k) -> k
        return $ \req -> req
            { requestHeaders
                = (hAuthorization,         credFromString $ "ApiKey " ++ strip apiKey)
                : ("X-Management-Request", "true")
                : requestHeaders req
            }
    where credFromString = T.encodeUtf8 . T.pack

newtype KeyringEntry = KeyringEntry { unKeyringEntry :: String } deriving (Show, Eq)

instance FromJSON KeyringEntry where
    parseJSON = withObject "KeyringEntry" $ \v -> KeyringEntry <$> v .: "key"

runRequestWith :: HttpClient -> Request -> IO (Response BL.ByteString)
runRequestWith httpClient request = runHttpClient httpClient request $ \response -> do
    bss <- brConsume $ responseBody response
    return response { responseBody = BL.fromChunks bss }
