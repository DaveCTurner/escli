{-# LANGUAGE LambdaCase #-}

module ESCommand (ESCommand(..), esCommand) where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as B
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

data ESCommand = ESCommand
    { httpVerb :: B.ByteString
    , httpPath :: String
    , cmdBody  :: [Value]
    } deriving (Show, Eq)

esCommand :: AP.Parser ESCommand
esCommand = AP.skipMany skipNewline >> ESCommand
    <$> AP.choice (map (AP.string . T.encodeUtf8 . T.pack) $ words "GET POST PUT DELETE HEAD")
    <*  AP.takeWhile1 (== 0x20)
    <*> (T.unpack . T.decodeUtf8With T.lenientDecode <$> AP.takeWhile1 (>= 0x20))
    <*  skipNewline
    <*> AP.sepBy jsonNotNewline skipNewline
    <*  (skipNewline <|> AP.endOfInput)

    where
    skipNewline :: AP.Parser ()
    skipNewline = do
        void $ optional $ do
            void $ AP.word8 0x23
            AP.takeWhile (/= 0x0a)
        void $ AP.word8 0x0a

    jsonNotNewline :: AP.Parser Value
    jsonNotNewline = AP.peekWord8 >>= \case
        Just w | w >= 0x20 -> json
        _                  -> fail "jsonNotNewline"
