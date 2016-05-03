{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time (getCurrentTime, utctDayTime)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Web.MarkovBot (getTwInfoFromEnv, textSourceFromTweetsCSV, postPoemWithTable)
import Web.MarkovBot.MarkovChain (buildTable)

data Options = Options
  {
    optionsInterval  :: Int
  , optionsTweetsCSV :: String
  } deriving Show

intervalP :: Parser Int
intervalP = option auto (
    long "interval"
 <> metavar "SECONDS"
 <> help "The time between status updates (in seconds)"
 <> value 1800
                        )

tweetsCSVP :: Parser FilePath
tweetsCSVP = strOption (
     long "tweets-csv"
  <> metavar "FILE"
  <> help "Path for tweets.csv"
                          )

optionsP :: Parser Options
optionsP = Options <$> intervalP <*> tweetsCSVP

execute :: Options -> IO ()
execute opts =
    let interval = optionsInterval opts
        tweetsCSV = optionsTweetsCSV opts
     in do
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- textSourceFromTweetsCSV tweetsCSV >>= buildTable
        let go = (== 0) . (`mod` interval) . floor . utctDayTime <$> getCurrentTime
             >>= flip when (postPoemWithTable twInfo table)
              >> threadDelay 1000000
              >> go
            in go

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
