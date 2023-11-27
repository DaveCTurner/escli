module Config.ParserSpec where

import Config
import Config.Parser
import Network.URI
import Options.Applicative
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk
import Test.Hspec

spec :: Spec
spec = do
    it "accepts no args" $
        "" `optsShouldYield` defaultConfig

    describe "general config" $ do

        generalConfigFlag "--hide-timing"                (\s -> s {esHideTiming              = True})
        generalConfigFlag "--hide-headings"              (\s -> s {esHideHeadings            = True})
        generalConfigFlag "--hide-status"                (\s -> s {esHideStatusCode          = True})
        generalConfigFlag "--hide-curl-equivalent"       (\s -> s {esHideCurlEquivalent      = True})
        generalConfigFlag "--show-curl-password"         (\s -> s {esShowCurlPassword        = True})
        generalConfigFlag "--hide-deprecation-warnings"  (\s -> s {esHideDeprecationWarnings = True})
        generalConfigFlag "--only-response"              (\s -> s {esOnlyResponse            = True})
        generalConfigFlag "--save"                       (\s -> s {esSaveConnectionConfig    = True})
        generalConfigFlag "--no-max-response-lines"      (\s -> s {esMaxResponseLines        = -1  })
        generalConfigFlag "--max-response-lines 42"      (\s -> s {esMaxResponseLines        = 42  })
        generalConfigFlag "--log-file escli.log"         (\s -> s {esLogFile                 = Just "escli.log"})
        generalConfigFlag "--thread-dump 123"            (\s -> s {esOneShotCommand          = Just $ ThreadDumpNode NodeTypeInstance   123})
        generalConfigFlag "--thread-dump-tiebreaker 456" (\s -> s {esOneShotCommand          = Just $ ThreadDumpNode NodeTypeTiebreaker 456})
        generalConfigFlag "--heap-dumps"                 (\s -> s {esOneShotCommand          = Just HeapDumpList})

    describe "connection config" $ do

        describe "certificate verification" $ do
            connectionConfig ""                                             (\s -> s { esCertificateVerificationConfig = DefaultCertificateVerificationConfig })
            connectionConfig "--certificate-store cafile.pem"               (\s -> s { esCertificateVerificationConfig = CustomCertificateVerificationConfig "cafile.pem" })
            connectionConfig "--insecurely-bypass-certificate-verification" (\s -> s { esCertificateVerificationConfig = NoCertificateVerificationConfig })

        connectionConfig "" (\s -> s {esEndpointConfig = DefaultEndpoint })

        describe "URI endpoints" $ do

            connectionConfig "--server https://example.org:1234/" (\s -> s {esEndpointConfig = URIEndpoint $ _u "https://example.org:1234/" })

            connectionConfig "--server https://example.org:1234/ --username testuser --password testpass" (\s -> s
                { esEndpointConfig    = URIEndpoint $ _u "https://example.org:1234/"
                , esCredentialsConfig = BasicCredentials "testuser" "testpass"
                })

            connectionConfig "--server https://example.org:1234/ --api-key API_KEY_ENV_VAR" (\s -> s
                { esEndpointConfig    = URIEndpoint $ _u "https://example.org:1234/"
                , esCredentialsConfig = ApiKeyCredentials "API_KEY_ENV_VAR"
                })

            connectionConfig "--server https://example.org:1234/ --mac-os-keyring-service SRVC --mac-os-keyring-account ACCT" (\s -> s
                { esEndpointConfig    = URIEndpoint $ _u "https://example.org:1234/"
                , esCredentialsConfig = MacOsKeyringCredentials "SRVC" "ACCT"
                })

        describe "Cloud deployment endpoints" $ do

            connectionConfig "--deployment abcdef" (\s -> s
                { esEndpointConfig    = CloudDeploymentEndpoint testApiRoot "abcdef" Nothing
                , esCredentialsConfig = ApiKeyCredentials "API_KEY"
                })

            connectionConfig "--deployment abcdef --deployment-ref es-ref" (\s -> s
                { esEndpointConfig    = CloudDeploymentEndpoint testApiRoot "abcdef" (Just "es-ref")
                , esCredentialsConfig = ApiKeyCredentials "API_KEY"
                })

            connectionConfig "--deployment abcdef --api-key OTHER_API_KEY" (\s -> s
                { esEndpointConfig    = CloudDeploymentEndpoint testApiRoot "abcdef" Nothing
                , esCredentialsConfig = ApiKeyCredentials "OTHER_API_KEY"
                })

            connectionConfig "--deployment abcdef --cloud-api-root http://api.example.org/other/test/root" (\s -> s
                { esEndpointConfig    = CloudDeploymentEndpoint (_u "http://api.example.org/other/test/root") "abcdef" Nothing
                , esCredentialsConfig = ApiKeyCredentials "API_KEY"
                })

    describe "validation" $ do

        invalid "--definitely-not-a-valid-option" "Invalid option `--definitely-not-a-valid-option'"
        invalid "--max-response-lines"            "The option `--max-response-lines` expects an argument."
        invalid "--username"                      "The option `--username` expects an argument."
        invalid "--password"                      "The option `--password` expects an argument."

        invalid "--server https://example.org:1234/ --username testuser"                                      "Missing: --password PASSWORD"
        invalid "--server https://example.org:1234/ --password testpass"                                      "Missing: --username USERNAME"
        invalid "--server https://example.org:1234/ --mac-os-keyring-account ACCT"                            "Missing: --mac-os-keyring-service SERVICE"
        invalid "--server https://example.org:1234/ --mac-os-keyring-service SRVC"                            "Missing: --mac-os-keyring-account ACCOUNT"
        invalid "--server https://example.org:1234/ --username testuser --password testpass --api-key APIKEY" "Invalid option `--api-key'"
        invalid "--server https://example.org:1234/ --api-key APIKEY --username testuser"                     "Invalid option `--username'"

        invalid "--deployment abcdef --username foo"                               "Invalid option `--username'"
        invalid "--deployment abcdef --mac-os-keyring-account ACCT"                "Invalid option `--mac-os-keyring-account'"
        invalid "--deployment abcdef --certificate-store cafile.pem"               "Invalid option `--certificate-store'"
        invalid "--deployment abcdef --insecurely-bypass-certificate-verification" "Invalid option `--insecurely-bypass-certificate-verification'"

invalid :: String -> String -> SpecWith (Arg Expectation)
invalid arg msg = it ("rejects " ++ show arg ++ " with " ++ show msg) $ arg `optsShouldFailWith` msg

_u :: String -> URI
_u uri = maybe (error $ "could not parse URI " ++ show uri) id $ parseAbsoluteURI uri

connectionConfig :: String -> (ConnectionConfig -> ConnectionConfig) -> SpecWith (Arg Expectation)
connectionConfig arg f = it ("accepts " ++ arg) $ arg `optsShouldYield` (defaultConfig {esConnectionConfig = f (esConnectionConfig defaultConfig)})

generalConfigFlag :: String -> (GeneralConfig -> GeneralConfig) -> SpecWith (Arg Expectation)
generalConfigFlag arg f = it ("accepts " ++ arg) $ arg `optsShouldYield` (defaultConfig {esGeneralConfig = f (esGeneralConfig defaultConfig)})

defaultConfig :: Config
defaultConfig = Config
    { esConnectionConfig = ConnectionConfig
        { esEndpointConfig                = DefaultEndpoint
        , esCredentialsConfig             = NoCredentials
        , esCertificateVerificationConfig = DefaultCertificateVerificationConfig
        }
    , esGeneralConfig = GeneralConfig
        { esHideTiming              = False
        , esHideHeadings            = False
        , esHideStatusCode          = False
        , esHideCurlEquivalent      = False
        , esShowCurlPassword        = False
        , esHideDeprecationWarnings = False
        , esOnlyResponse            = False
        , esSaveConnectionConfig    = False
        , esMaxResponseLines        = 40
        , esLogFile                 = Nothing
        , esOneShotCommand          = Nothing
        }
    }

optsShouldYield :: String -> Config -> Expectation
optsShouldYield args expectedConfig = case execParserPure defaultPrefs (configParserInfo testConfigParserContext) (words args) of
    Success actualConfig -> actualConfig `shouldBe` expectedConfig
    Failure failure      -> error $ show failure
    CompletionInvoked cr -> error $ show cr

optsShouldFailWith :: String -> String -> Expectation
optsShouldFailWith args expectedMessage = case execParserPure defaultPrefs (configParserInfo testConfigParserContext) (words args) of
    Success actualConfig -> error $ show actualConfig
    Failure (ParserFailure failureFn) -> verifyFailure $ failureFn "escli"
    CompletionInvoked cr -> error $ show cr

    where
    verifyFailure :: (ParserHelp, a, b) -> Expectation
    verifyFailure (ParserHelp{helpError=Chunk maybeDoc}, _, _) = do
        maybe (error "no error message") (($ "") . displayS . renderCompact) maybeDoc `shouldBe` expectedMessage

testConfigParserContext :: ConfigParserContext
testConfigParserContext = ConfigParserContext
    { configParserContextFileName = "test_escli.json"
    , configParserContextApiRoot  = "http://example.org/escli/api/root"
    }

testApiRoot :: URI
testApiRoot = _u $ configParserContextApiRoot testConfigParserContext
