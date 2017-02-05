{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Web.MarkovBot(
    getTwInfoFromEnv
  , textSourceFromRemoteTweetsCSV
  , textSourceFromTweetsCSV
  , postPoemWithTable
) where

import Control.Exception (catch)
import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv (decode, HasHeader(..))
import Data.List (intercalate, isInfixOf)
import qualified Data.Vector as V
import qualified Data.Text as T
import Network.HTTP.Conduit (
    newManager
  , tlsManagerSettings
  )
import Network.HTTP.Conduit (
    newManager
  , tlsManagerSettings
  )
import Network.Wreq (get, responseBody)
import Web.Authenticate.OAuth (def, newCredential, oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (call, setCredential, TWInfo, TwitterError(TwitterErrorResponse), TwitterErrorMessage(..), twitterOAuth, update)
import Web.MarkovBot.MarkovChain (generatePoem, Table)
import Web.MarkovBot.Status (Status(..), statusIsRetweet)
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

textSourceFromRemoteTweetsCSV :: URL -> IO String
textSourceFromRemoteTweetsCSV = fmap textSourceFromStatusesVector . statusesFromRemoteTweetsCSV

textSourceFromTweetsCSV :: FilePath -> IO String
textSourceFromTweetsCSV = fmap textSourceFromStatusesVector . statusesFromTweetsCSV

decodeTweetsCSV :: BL.ByteString -> V.Vector Status
decodeTweetsCSV = either (error "Failed to parse CSV file") id . decode HasHeader

statusesFromTweetsCSV :: FilePath -> IO (V.Vector Status)
statusesFromTweetsCSV path = decodeTweetsCSV <$> BL.readFile path

statusesFromRemoteTweetsCSV :: URL -> IO (V.Vector Status)
statusesFromRemoteTweetsCSV url = decodeTweetsCSV <$> ((^. responseBody) <$> get url)

textSourceFromStatusesVector :: V.Vector Status -> String
textSourceFromStatusesVector = intercalate "\n"
                  . V.toList
                  . V.map statusText
                  . V.filter (not . isInfixOf "http" . statusText)
                  . V.filter (notElem '@' . statusText)
                  . V.filter (not . statusIsRetweet)

postPoemWithTable :: TWInfo -> Table -> IO ()
postPoemWithTable twInfo table = flip catch retry $ do
    text <- T.pack . take 140 <$> generatePoem table
    manager <- newManager tlsManagerSettings
    !status <- call twInfo manager (update text)
    return ()
  where
    -- retry if the status is a duplicate
    retry (TwitterErrorResponse _ _ [TwitterErrorMessage { twitterErrorCode = 187 }]) = postPoemWithTable twInfo table
    retry e = print e
