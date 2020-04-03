{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web.MarkovBot.Status
  ( Status(..)
  , isRetweet
  , loadFromTweetJS
) where

import Data.Aeson ((.:), FromJSON (parseJSON), Value, withObject)
import qualified Data.Aeson as JSON (decode')
import qualified Data.Aeson.Types as JSON (parseEither)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P (string, parseOnly, takeLazyByteString)
import Data.ByteString (ByteString)
import Data.Text (isPrefixOf, replace, Text)
import Data.Vector (Vector)

newtype Tweet
  = Tweet
  { status :: Status
  }

instance FromJSON Tweet where
  parseJSON = withObject "Tweet" $ \o -> do
    status <- o .: "tweet"
    pure Tweet { status }

newtype Status = Status
  { text :: Text
  } deriving (Show)

instance FromJSON Status where
  parseJSON = withObject "status" $ \o -> do
    escapedText <- o .: "full_text"
    let text = unescapeEntities escapedText
    pure Status { text }

tweetJSP :: Parser Value
tweetJSP = do
  _ <- P.string "window.YTD.tweet.part0 = "
  bs <- P.takeLazyByteString
  let mvalue = JSON.decode' bs
  case mvalue of
    Nothing -> fail "Could not parse JSON"
    Just x -> pure x

loadFromTweetJS :: ByteString -> Either String (Vector Status)
loadFromTweetJS bs = do
  values <- P.parseOnly tweetJSP bs
  tweets <- JSON.parseEither parseJSON values
  let statuses = fmap getStatus tweets
  pure statuses
  where
    getStatus Tweet { status } = status

unescapeEntities :: Text -> Text
unescapeEntities = replace "&lt;" "<" . replace "&gt;" ">" . replace "&amp;" "&"

isRetweet :: Status -> Bool
isRetweet Status { text } = "RT @" `isPrefixOf` text
