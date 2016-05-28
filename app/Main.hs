{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Bool (bool)
import Data.List (isPrefixOf)
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime, UTCTime)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Web.MarkovBot (getTwInfoFromEnv, textSourceFromRemoteTweetsCSV, textSourceFromTweetsCSV, postPoemWithTable)
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

waitUntil :: UTCTime -> IO ()
waitUntil t = getCurrentTime >>= threadDelay . floor .  (* 1000 ^ 2) . diffUTCTime t

execute :: Options -> IO ()
execute opts =
    let interval = optionsInterval opts
        tweetsCSV = optionsTweetsCSV opts
        isURL = "http" `isPrefixOf` tweetsCSV
     in do
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- bool textSourceFromTweetsCSV textSourceFromRemoteTweetsCSV isURL tweetsCSV >>= buildTable
        now <- getCurrentTime
        let schedule = map (flip addUTCTime now . toEnum . (* interval) . (* 1000 ^ 4)) $ [1 ..]
        flip fix schedule $ \loop (t:ts) -> postPoemWithTable twInfo table >> waitUntil t >> loop ts

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
