module Web.MarkovBot.MarkovChain (
    generatePoem
) where

import Prelude hiding (Word)
import Control.Monad (join)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Data.List (intercalate)
import Text.MeCab

data Word = Begin | Middle String | End deriving (Eq, Show)

fromWord :: Word -> String
fromWord (Middle x) = x
fromWord _ = ""

fromWords :: [Word] -> String
fromWords = concatMap fromWord

toWords :: [String] -> [Word]
toWords [] = []
toWords xs = Begin : init (convert xs)
  where
    convert :: [String] -> [Word]
    convert [] = []
    convert (x:xs) = if x == "\n"
                     then End : Begin : convert xs
                     else Middle x : convert xs

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample xs = return <$> randomOne xs
  where
    randomOne xs = (xs !!) <$> randomRIO (0, length xs - 1)

generateTable :: Int -> [Word] -> [[Word]]
generateTable _ [] = []
generateTable n xxs@(x:xs)
  | length xxs < n = []
  | End `elem` init part = generateTable n xs
  | otherwise = part : generateTable n xs
  where
    part = take n xxs

initialWords :: [[Word]] -> IO (Maybe [Word])
initialWords table = sample (filter f table)
  where
    f (x:xs) = x == Begin

nextWords :: [[Word]] -> [Word] -> IO (Maybe [Word])
nextWords _ [] = return Nothing
nextWords table prefix = sample (filter f table)
  where
    f xs = init xs == prefix

chainWords :: [[Word]] -> [Word] -> String -> IO String
chainWords table prefix chain = do
  next <- nextWords table prefix
  if last prefix == End
  then return chain
  else case next of
    Nothing -> return chain
    Just ws -> chainWords table (tail ws) (chain ++ maybe "" fromWord (last <$> next))

generatePoem :: String -> IO String
generatePoem source = do
  mecab <- new ["mecab", "-l0"]
  nodeLines <- mapM (parseToNodes mecab) (lines source)
  let wordLines = map (filter (not . null) . map nodeSurface) nodeLines
  let allWords = intercalate ["\n"] wordLines

  let table = generateTable 3 (toWords allWords)

  begin <- initialWords table
  case begin of
    Nothing -> return ""
    Just ws -> chainWords table (tail ws) $ fromWords ws
