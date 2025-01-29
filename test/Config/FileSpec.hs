{-# LANGUAGE OverloadedStrings #-}

module Config.FileSpec where

import Config
import Config.File
import Data.Aeson
import Data.Aeson.Types
import Network.URI
import Test.Hspec

spec :: Spec
spec = do
    describe "handles BasicCredentials" $
        [ "type" .= ("basic" :: String)
        , "user" .= ("USER" :: String)
        , "pass" .= ("PASS" :: String)
        ] `shouldParseCredentialsAs` BasicCredentials "USER" "PASS"

    describe "handles ApiKeyCredentials" $
        [ "type" .= ("apikey" :: String)
        , "var"  .= ("var" :: String)
        ] `shouldParseCredentialsAs` ApiKeyCredentials "var"

    describe "handles MacOsKeyringCredentials with account" $
        [ "type"    .= ("mac-os-keyring" :: String)
        , "service" .= ("KEYRING_SERVICE" :: String)
        , "account" .= ("KEYRING_ACCOUNT" :: String)
        ] `shouldParseCredentialsAs` MacOsKeyringCredentials "KEYRING_SERVICE" (Just "KEYRING_ACCOUNT")

    describe "handles MacOsKeyringCredentials without account" $
        [ "type"    .= ("mac-os-keyring" :: String)
        , "service" .= ("KEYRING_SERVICE" :: String)
        ] `shouldParseCredentialsAs` MacOsKeyringCredentials "KEYRING_SERVICE" Nothing

uriEndpoint :: String -> EndpointConfig
uriEndpoint uriString = case parseURI uriString of
    Nothing -> error $ "invalid URI string " ++ show uriString
    Just u -> URIEndpoint u

shouldParseAs :: Value -> ConnectionConfig -> Spec
shouldParseAs input expected = do
    it "parses as expected" $ parseJsonConnection input `shouldBe` expected
    it "renders as expected" $
        toJSON (JsonConnection expected) `shouldBe` input
    it "round-trips the JSON"  $ toJSON (JsonConnection $ parseJsonConnection input) `shouldBe` input
    it "round-trips the value" $ parseJsonConnection (toJSON $ JsonConnection expected) `shouldBe` expected

parseJsonConnection :: Value -> ConnectionConfig
parseJsonConnection v = case fromJSON v of
    Error msg -> error $ "JSON parse failure: " ++ msg
    Success jc -> unJsonConnection jc

shouldParseCredentialsAs :: [Pair] -> CredentialsConfig -> Spec
shouldParseCredentialsAs credentialsJson expectedCredentials =
    object
        [ "baseuri"     .= baseUriString
        , "credentials" .= object credentialsJson
        ] `shouldParseAs` ConnectionConfig
            { esEndpointConfig                = uriEndpoint baseUriString
            , esCredentialsConfig             = expectedCredentials
            , esCertificateVerificationConfig = DefaultCertificateVerificationConfig
            }
    where
        baseUriString :: String
        baseUriString = "https://example.org"
