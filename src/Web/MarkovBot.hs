{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MarkovBot(
    getTwInfoFromEnv
  , textSourceFromRemoteTweetJS
  , textSourceFromTweetJS
  , postPoemWithTable
) where

import Control.Exception (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit (newManager, simpleHttp, tlsManagerSettings)
import Web.Authenticate.OAuth (def, newCredential, oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (call, setCredential, TWInfo, TwitterError(TwitterErrorResponse), TwitterErrorMessage(..), twitterOAuth, update)
import Web.MarkovBot.MarkovChain (generatePoem, Table)
import Web.MarkovBot.Status (Status(..), isRetweet, loadFromTweetJS)
import System.Environment (lookupEnv)

type URL = String

getTwInfoFromEnv :: IO (Maybe TWInfo)
getTwInfoFromEnv = do
    let keys = [
                 "MARKOV_BOT_CONSUMER_KEY"
               , "MARKOV_BOT_CONSUMER_SECRET"
               , "MARKOV_BOT_ACCESS_TOKEN"
               , "MARKOV_BOT_ACCESS_TOKEN_SECRET"
               ]
    [consumerKey, consumerSecret, accessToken, accessTokenSecret] <- map (fmap BS.pack) <$> traverse lookupEnv keys
    let tokens = buildOAuth <$> consumerKey <*> consumerSecret
        credential = newCredential <$> accessToken <*> accessTokenSecret
    return $ setCredential <$> tokens <*> credential <*> pure def
    where buildOAuth key secret = twitterOAuth {
                                                 oauthConsumerKey = key
                                               , oauthConsumerSecret = secret
                                               }

textSourceFromRemoteTweetJS :: URL -> IO Text
textSourceFromRemoteTweetJS = fmap textSourceFromStatusesVector . statusesFromRemoteTweetJS

textSourceFromTweetJS :: FilePath -> IO Text
textSourceFromTweetJS = fmap textSourceFromStatusesVector . statusesFromTweetJS

decodeTweetJS :: ByteString -> V.Vector Status
decodeTweetJS bs =
  case loadFromTweetJS bs of
      Left e -> error e
      Right x -> x

statusesFromTweetJS :: FilePath -> IO (V.Vector Status)
statusesFromTweetJS path = decodeTweetJS <$> BS.readFile path

statusesFromRemoteTweetJS :: URL -> IO (V.Vector Status)
statusesFromRemoteTweetJS url = do
  lbs <- simpleHttp url
  let bs = BL.toStrict lbs
      statuses = decodeTweetJS bs
  pure statuses

textSourceFromStatusesVector :: V.Vector Status -> Text
textSourceFromStatusesVector = T.intercalate "\n"
                  . V.toList
                  . V.map getText
                  . V.filter (not . shouldReject)
  where
    getText Status { text } = text
    shouldReject status =
      let Status { text } = status
       in T.isInfixOf "http" text
       || T.isInfixOf "@" text
       || isRetweet status

postPoemWithTable :: TWInfo -> Table -> IO ()
postPoemWithTable twInfo table = flip catch retry $ do
    text <- T.take 140 <$> generatePoem table
    T.putStrLn text
    manager <- newManager tlsManagerSettings
    !_ <- call twInfo manager (update text)
    return ()
  where
    -- retry if the status is a duplicate
    retry (TwitterErrorResponse _ _ [TwitterErrorMessage { twitterErrorCode = 187 }]) = postPoemWithTable twInfo table
    retry e = print e
