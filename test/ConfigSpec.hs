module ConfigSpec where

import Options.Applicative
import Test.Hspec
import Config
import Config.Parser

spec :: Spec
spec = describe "ConfigSpec" $ do
    it "accepts no args" $
        [] `optsShouldYield` Config
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

optsShouldYield :: [String] -> Config -> Expectation
optsShouldYield args expectedConfig = case execParserPure defaultPrefs (configParserInfo testConfigParserContext) args of
    Success actualConfig -> actualConfig `shouldBe` expectedConfig
    Failure failure      -> error $ show failure
    CompletionInvoked cr -> error $ show cr

    where
    testConfigParserContext :: ConfigParserContext
    testConfigParserContext = ConfigParserContext
        { configParserContextFileName = "test_escli.json"
        , configParserContextApiRoot  = "http://example.org/escli/api/root"
        }
