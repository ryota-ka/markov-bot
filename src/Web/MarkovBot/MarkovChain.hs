module Web.MarkovBot.MarkovChain (
    buildTable
  , generatePoem
  , Table
) where

import Prelude hiding (Word)
import Control.Monad (join)
import qualified Data.Map as M
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Data.List (intercalate)
import Text.MeCab

data Word = Begin | Middle String | End deriving (Eq, Ord, Show)
data Rose a = Rose a a [Rose a] deriving (Show)

type Table = M.Map (Word, Word) [Word]

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

sample :: [a] -> IO a
sample [] = error "empty list"
sample xs = (xs !!) <$> randomRIO (0, length xs - 1)

generateTable :: [Word] -> Table
generateTable (a:b:c:xs)
  | a == End || b == End = generateTable rest
  | otherwise = let table = generateTable rest in prepend (a, b) c table
    where prepend key value map = M.insert key (value : (fromMaybe [] (M.lookup key map))) map
          rest = b:c:xs
generateTable _ = M.empty

nextWords :: Table -> (Word, Word) -> [Word]
nextWords table key = fromMaybe [] $ M.lookup key table

buildRoseTree :: Table -> (Word, Word) -> Rose Word
buildRoseTree table (a, b) = Rose a b $ map (\w -> buildRoseTree table (b, w)) (nextWords table (a, b))

randomlyWalkRoseTree :: Rose Word -> IO [Word]
randomlyWalkRoseTree rose =
    let f yieldedValues (Rose a b roses) =
          if null roses || a == End || b == End
             then pure (yieldedValues, Rose a b [])
             else sample roses >>= f (b:yieldedValues)
        in fst <$> f [] rose

buildTable :: String -> IO Table
buildTable source = do
    mecab <- new ["mecab", "-l0"]
    nodeLines <- mapM (parseToNodes mecab) (lines source)

    let wordLines = map (filter (not . null) . map nodeSurface) nodeLines
        allWords = intercalate ["\n"] wordLines
        table = generateTable (toWords allWords)

    return table

generatePoem :: Table -> IO String
generatePoem table = do
    (_, first) <- sample . filter ((== Begin) . fst) . M.keys $ table
    let tree = buildRoseTree table (Begin, first)
    fromWords . reverse <$> randomlyWalkRoseTree tree
