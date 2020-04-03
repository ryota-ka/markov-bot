{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import Options.Applicative
import System.Exit (die, exitFailure)
import Web.MarkovBot (getTwInfoFromEnv, textSourceFromRemoteTweetJS, textSourceFromTweetJS, postPoemWithTable)
import Web.MarkovBot.MarkovChain (buildTable)

data Options = Options
  { optionsTweetJS :: String
  , optionsOrder :: Int
  } deriving Show

tweetJSP :: Parser FilePath
tweetJSP = strOption $
    long "tweet-js"
 <> metavar "FILE_OR_URL"
 <> help "Path or URL for tweet.js"

orderP :: Parser Int
orderP = option auto $
    long "order"
 <> metavar "INT"
 <> help "Order of Markov chain"
 <> showDefault
 <> value 3

optionsP :: Parser Options
optionsP = Options <$> tweetJSP <*> orderP

execute :: Options -> IO ()
execute opts =
    let tweetJS = optionsTweetJS opts
        order = optionsOrder opts
        isURL = "http" `isPrefixOf` tweetJS
        loadTweetJS =
            if isURL
                then textSourceFromRemoteTweetJS
                else textSourceFromTweetJS
     in do
        unless (order > 0) $ die "order must be greater than 0"
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- loadTweetJS tweetJS >>= buildTable order
        postPoemWithTable twInfo table

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
