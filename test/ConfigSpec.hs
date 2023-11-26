module ConfigSpec where

import Options.Applicative
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk
import Test.Hspec
import Config
import Config.Parser

spec :: Spec
spec = describe "ConfigSpec" $ do
    it "accepts no args" $
        "" `optsShouldYield` defaultConfig

    it "rejects unknown options" $
        "--definitely-not-a-valid-option" `optsShouldFailWith` "Invalid option `--definitely-not-a-valid-option'"

    it "requires an argument to --max-response-lines" $
        "--max-response-lines" `optsShouldFailWith` "The option `--max-response-lines` expects an argument."

    it "requires an argument to --username" $
        "--username" `optsShouldFailWith` "The option `--username` expects an argument."

    it "requires an argument to --password" $
        "--password" `optsShouldFailWith` "The option `--password` expects an argument."

optsShouldYield :: String -> Config -> Expectation
optsShouldYield args expectedConfig = case execParserPure defaultPrefs (configParserInfo testConfigParserContext) (words args) of
    Success actualConfig -> actualConfig `shouldBe` expectedConfig
    Failure failure      -> error $ show failure
    CompletionInvoked cr -> error $ show cr

    where
    testConfigParserContext :: ConfigParserContext
    testConfigParserContext = ConfigParserContext
        { configParserContextFileName = "test_escli.json"
        , configParserContextApiRoot  = "http://example.org/escli/api/root"
        }

optsShouldFailWith :: String -> String -> Expectation
optsShouldFailWith args expectedMessage = case execParserPure defaultPrefs (configParserInfo testConfigParserContext) (words args) of
    Success actualConfig -> error $ show actualConfig
    Failure (ParserFailure failureFn) -> verifyFailure $ failureFn "escli"
    CompletionInvoked cr -> error $ show cr

    where
    testConfigParserContext :: ConfigParserContext
    testConfigParserContext = ConfigParserContext
        { configParserContextFileName = "test_escli.json"
        , configParserContextApiRoot  = "http://example.org/escli/api/root"
        }

    verifyFailure :: (ParserHelp, a, b) -> Expectation
    verifyFailure (ParserHelp{helpError=Chunk maybeDoc}, _, _) = do
        maybe (error "no error message") (($ "") . displayS . renderCompact) maybeDoc `shouldBe` expectedMessage

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