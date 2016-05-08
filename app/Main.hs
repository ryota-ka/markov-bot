{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Bool (bool)
import Data.List (isPrefixOf)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Posix.Time (epochTime)
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

execute :: Options -> IO ()
execute opts =
    let interval = optionsInterval opts
        tweetsCSV = optionsTweetsCSV opts
        isURL = "http" `isPrefixOf` tweetsCSV
     in do
        !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
        !table <- bool textSourceFromTweetsCSV textSourceFromRemoteTweetsCSV isURL tweetsCSV >>= buildTable
        !start <- (\t -> head . filter ((== 0) . (`mod` interval)) . map (+ t) $ [0..]) . fromEnum <$> epochTime
        (* 1000000) . flip subtract start . fromEnum <$> epochTime >>= threadDelay
        fix $ \loop -> postPoemWithTable twInfo table >> threadDelay (interval * 1000000) >> loop

main :: IO ()
main = execParser (info (helper <*> optionsP) fullDesc) >>= execute
