{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config
    ( Config(..)
    , GeneralConfig(..)
    , CertificateVerificationConfig(..)
    , CredentialsConfig(..)
    , ConnectionConfig(..)
    , EndpointConfig(..)
    , OneShotCommandConfig(..)
    , NodeType(..)
    ) where

import Network.URI

data Config = Config
    { esConnectionConfig              :: ConnectionConfig
    , esGeneralConfig                 :: GeneralConfig
    } deriving (Show, Eq)

data ConnectionConfig = ConnectionConfig
    { esEndpointConfig                :: EndpointConfig
    , esCredentialsConfig             :: CredentialsConfig
    , esCertificateVerificationConfig :: CertificateVerificationConfig
    } deriving (Show, Eq)

data EndpointConfig
    = DefaultEndpoint
    | URIEndpoint               URI
    | CloudDeploymentEndpoint   URI String (Maybe String)
    | ServerlessProjectEndpoint URI String
    deriving (Show, Eq)

data CredentialsConfig
    = NoCredentials
    | BasicCredentials        { esCredentialsBasicUser :: String, esCredentialsBasicPassword :: String }
    | ApiKeyCredentials       { esCredentialsApiKeyEnvVar :: String }
    | MacOsKeyringCredentials { esCredentialsKeyringService :: String, esCredentialsKeyringAccount :: String }
    deriving (Show, Eq)

data CertificateVerificationConfig
    = DefaultCertificateVerificationConfig
    | NoCertificateVerificationConfig
    | CustomCertificateVerificationConfig FilePath
    deriving (Show, Eq)

data GeneralConfig = GeneralConfig
    { esHideTiming              :: Bool
    , esHideHeadings            :: Bool
    , esHideStatusCode          :: Bool
    , esHideCurlEquivalent      :: Bool
    , esShowCurlPassword        :: Bool
    , esHideDeprecationWarnings :: Bool
    , esOnlyResponse            :: Bool
    , esSaveConnectionConfig    :: Bool
    , esMaxResponseLines        :: Int
    , esLogFile                 :: Maybe FilePath
    , esOneShotCommand          :: Maybe OneShotCommandConfig
    } deriving (Show, Eq)

data OneShotCommandConfig
    = ThreadDumpNode NodeType Int
    | HeapDumpList
    deriving (Show, Eq)

data NodeType
    = NodeTypeInstance
    | NodeTypeTiebreaker
    deriving (Show, Eq)
