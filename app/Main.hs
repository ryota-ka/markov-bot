{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time (getCurrentTime, utctDayTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Web.MarkovBot

main :: IO ()
main = do
    !twInfo <- getTwInfoFromEnv >>= maybe (putStrLn "environment variables are not set" >> exitFailure) return
    src <- getArgs >>= return . head >>= textSourceFromFilePath
    let go = (== 0) . (`mod` 1800) . floor . utctDayTime <$> getCurrentTime
         >>= flip when (postPoemWithSource twInfo src)
          >> threadDelay 1000000
          >> go
        in go
