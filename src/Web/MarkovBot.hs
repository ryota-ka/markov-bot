{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Web.MarkovBot(
    getTwInfoFromEnv
  , textSourceFromFilePath
  , postPoemWithSource
) where

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import Data.List (intercalate, isInfixOf)
import qualified Data.Text as T
import Network.HTTP.Conduit (
    newManager
  , tlsManagerSettings
  )
import Web.Authenticate.OAuth (def, newCredential, oauthConsumerKey, oauthConsumerSecret)
import Web.Twitter.Conduit (call, setCredential, TWInfo, twitterOAuth, update)
import Web.MarkovBot.MarkovChain (generatePoem)
import Web.MarkovBot.Status (Status(..), statusForCSVRecord, statusIsRetweet)
import System.Environment (lookupEnv)
import Text.CSV (CSV, parseCSVFromFile)

getTwInfoFromEnv :: IO (Maybe TWInfo)
getTwInfoFromEnv = do
    let keys = [
                 "MARKOV_BOT_CONSUMER_KEY"
               , "MARKOV_BOT_CONSUMER_SECRET"
               , "MARKOV_BOT_ACCESS_TOKEN"
               , "MARKOV_BOT_ACCESS_TOKEN_SECRET"
               ]
    [consumerKey, consumerSecret, accessToken, accessTokenSecret] <- map (fmap B.pack) <$> traverse lookupEnv keys
    let tokens = buildOAuth <$> consumerKey <*> consumerSecret
        credential = newCredential <$> accessToken <*> accessTokenSecret
    return $ setCredential <$> tokens <*> credential <*> pure def
    where buildOAuth key secret = twitterOAuth {
                                                 oauthConsumerKey = key
                                               , oauthConsumerSecret = secret
                                               }

textSourceFromFilePath :: FilePath -> IO String
textSourceFromFilePath path =
    textSourceFromTweetsCSV . either (error "Failed to parse tweets.csv") id <$> parseCSVFromFile path

textSourceFromTweetsCSV :: CSV -> String
textSourceFromTweetsCSV = intercalate "\n"
                  . map statusText
                  . filter (not . isInfixOf "http" . statusText )
                  . filter (notElem '@' . statusText)
                  . filter (not . statusIsRetweet)
                  . map statusForCSVRecord
                  . init
                  . tail

postPoemWithSource :: TWInfo -> String -> IO ()
postPoemWithSource twInfo src = do
    text <- T.pack <$> generatePoem src
    manager <- newManager tlsManagerSettings
    !status <- runResourceT $ call twInfo manager (update text)
    return ()
