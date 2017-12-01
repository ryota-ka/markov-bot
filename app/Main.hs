{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (when)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Web.MarkovBot (getTwInfoFromEnv, textSourceFromRemoteTweetsCSV, textSourceFromTweetsCSV, postPoemWithTable)
import Web.MarkovBot.MarkovChain (buildTable)

data Options = Options
  {
    optionsTweetsCSV :: String
  } deriving Show

tweetsCSVP :: Parser FilePath
tweetsCSVP = strOption (
     long "tweets-csv"
  <> metavar "FILE"
  <> help "Path for tweets.csv"
                          )

optionsP :: Parser Options
optionsP = Options <$> tweetsCSVP

execute :: Options -> IO ()
execute opts =
    let tweetsCSV = optionsTweetsCSV opts
        isURL = "http" `isPrefixOf` tweetsCSV
     in do
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- bool textSourceFromTweetsCSV textSourceFromRemoteTweetsCSV isURL tweetsCSV >>= buildTable
        postPoemWithTable twInfo table

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
