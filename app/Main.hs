{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import Web.MarkovBot (getTwInfoFromEnv, textSourceFromRemoteTweetsCSV, textSourceFromTweetsCSV, postPoemWithTable)
import Web.MarkovBot.MarkovChain (buildTable)

data Options = Options
  { optionsTweetsCSV :: String
  , optionsOrder :: Int
  } deriving Show

tweetsCSVP :: Parser FilePath
tweetsCSVP = strOption $
    long "tweets-csv"
 <> metavar "FILE_OR_URL"
 <> help "Path or URL for tweets.csv"

orderP :: Parser Int
orderP = option auto $
    long "order"
 <> metavar "INT"
 <> help "Order of Markov chain"
 <> showDefault
 <> value 3

optionsP :: Parser Options
optionsP = Options <$> tweetsCSVP <*> orderP

execute :: Options -> IO ()
execute opts =
    let tweetsCSV = optionsTweetsCSV opts
        order = optionsOrder opts
        isURL = "http" `isPrefixOf` tweetsCSV
        loadTweetsCSV =
            if isURL
                then textSourceFromRemoteTweetsCSV
                else textSourceFromTweetsCSV
     in do
        unless (order > 0) $ die "order must be greater than 0"
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- loadTweetsCSV tweetsCSV >>= buildTable order
        postPoemWithTable twInfo table

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
