{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Web.MarkovBot(
    getTwInfoFromEnv
  , textSourceFromTweetsCSV
  , postPoemWithTable
) where

import Control.Monad.Trans.Resource (runResourceT)
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
import Web.Authenticate.OAuth (def, newCredential, oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (call, setCredential, TWInfo, twitterOAuth, update)
import Web.MarkovBot.MarkovChain (generatePoem, Table)
import Web.MarkovBot.Status (Status(..), statusIsRetweet)
import System.Environment (lookupEnv)

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

textSourceFromTweetsCSV :: FilePath -> IO String
textSourceFromTweetsCSV = fmap textSourceFromStatusesVector . statusesFromTweetsCSV

statusesFromTweetsCSV :: FilePath -> IO (V.Vector Status)
statusesFromTweetsCSV path = either (error "Failed to parse CSV file") id . decode HasHeader <$> BL.readFile path

textSourceFromStatusesVector :: V.Vector Status -> String
textSourceFromStatusesVector = intercalate "\n"
                  . V.toList
                  . V.map statusText
                  . V.filter (not . isInfixOf "http" . statusText)
                  . V.filter (notElem '@' . statusText)
                  . V.filter (not . statusIsRetweet)

postPoemWithTable :: TWInfo -> Table -> IO ()
postPoemWithTable twInfo table = do
    text <- T.pack <$> generatePoem table
    manager <- newManager tlsManagerSettings
    !status <- runResourceT $ call twInfo manager (update text)
    return ()
