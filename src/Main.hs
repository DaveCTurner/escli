{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Conduit.Binary
import Config
import ESCommand
import Data.Conduit.Attoparsec
import Network.URI
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Data.Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Int
import qualified Data.Aeson.Encode.Pretty
import Data.Time
import Data.Time.ISO8601
import Network.HTTP.Types.Status
import Network.HTTP.Media

main :: IO ()
main = withConfig $ \config -> do

    putStrLn $ "# Server URI: " ++ show (esBaseURI config)
    putStrLn $ ""
    manager <- newManager defaultManagerSettings
      { managerResponseTimeout = responseTimeoutNone }

    runConduit
         $  sourceHandle stdin
         .| conduitParser esCommand
         .| DCL.map snd
         .| awaitForever (runCommand config manager)
         .| awaitForever (liftIO . putStrLn)

prettyStringFromJson :: ToJSON a => a -> String
prettyStringFromJson v
    = T.unpack $ T.decodeUtf8 $ BL.toStrict
        $ Data.Aeson.Encode.Pretty.encodePretty' aesonPrettyConfig v
    where
    aesonPrettyConfig = Data.Aeson.Encode.Pretty.defConfig
        { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Spaces 2 }

runCommand :: Config -> Manager -> ESCommand -> ConduitT ESCommand String IO ()
runCommand Config{..} manager ESCommand{..} = do
    let initReq = fromMaybe (error "Bad URI") $ do
            uriRef <- parseURIReference httpPath
            let absUri = uriRef `relativeTo` esBaseURI
            parseRequest $ show absUri

        BuilderWithLength bodyBuilder bodyLength = builderFromBody cmdBody

        req = initReq
            { method = httpVerb
            , requestHeaders = case cmdBody of
                []  -> []
                [_] -> [(hContentType, "application/json")]
                _   -> [(hContentType, "application/x-ndjson")]
            , requestBody = RequestBodyBuilder bodyLength bodyBuilder
            }

        httpVerbString = T.unpack $ T.decodeUtf8 httpVerb
        resolvedUriString = getUri req `relativeFrom` esBaseURI

    unless esHideHeadings $ do
        yield $ "# " ++ replicate 40 '='
        yield "# Request: "
    yield $ httpVerbString <> " " <> show resolvedUriString
    case cmdBody of
        [v] -> yield $ prettyStringFromJson v
        _   -> forM_ cmdBody $ yield . T.unpack . T.decodeUtf8 . BL.toStrict . encode
    before <- liftIO getCurrentTime
    unless esHideTiming $
        yield $ "# at " ++ formatISO8601Millis before
    yield ""

    response <- liftIO $ httpLbs req manager
    after <- liftIO getCurrentTime
    unless esHideHeadings $ do
        yield $ "# " ++ replicate 40 '-'
        yield "# Response: "
    yield $ "# " ++ show (statusCode    $ responseStatus response)
            ++ " "  ++ T.unpack (T.decodeUtf8 $ statusMessage $ responseStatus response)

    let linesFromJsonBody b = case (eitherDecode b :: Either String Value) of
            Left er -> ["JSON parse error: " ++ show er, show b]
            Right bv -> Prelude.lines $ prettyStringFromJson bv
        linesFromPlainBody b = map (T.unpack . T.decodeUtf8 . BL.toStrict) $ BL.split 0x0a b

    case lookup hContentType (responseHeaders response) of
        Nothing -> yield "# No content-type returned"
        Just ct -> case mapContentMedia [("application/json", linesFromJsonBody), ("text/plain", linesFromPlainBody)] ct of
            Nothing -> yield $ "# Unknown content-type: " ++ show ct
            Just linesFn -> forM_ (linesFn $ responseBody response) $ \l -> yield $ "# " ++ l
    unless esHideTiming $ do
        yield $ "# at " ++ formatISO8601Millis after
        yield $ "# (" ++ show (diffUTCTime after before) ++ " elapsed)"
    yield ""

data BuilderWithLength = BuilderWithLength B.Builder !Int64

instance Monoid BuilderWithLength where
    mempty = BuilderWithLength mempty 0
    mappend (BuilderWithLength b1 l1) (BuilderWithLength b2 l2)
        = BuilderWithLength (b1 <> b2) (l1 + l2)

jsonWithLength :: ToJSON a => a -> BuilderWithLength
jsonWithLength v = BuilderWithLength (B.lazyByteString bs) (BL.length bs)
  where bs = encode v

newlineWithLength :: BuilderWithLength
newlineWithLength = BuilderWithLength (B.word8 0x0a) 1

builderFromBody :: [Value] -> BuilderWithLength
builderFromBody []  = mempty
builderFromBody [v] = jsonWithLength v
builderFromBody vs  = mconcat $ map ((<> newlineWithLength) . jsonWithLength) vs
