{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.MarkovBot.MarkovChain
    ( buildTable
    , generatePoem
    , Table
    ) where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude hiding (Word, words)
import System.Random (randomRIO)
import Text.MeCab

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

data Word = Begin | Middle Text | End deriving (Eq, Ord, Show)
data Rose a = Rose (Vector a) (Vector (Rose a))

type Table = Map (Vector Word) (Vector Word)

class Samplable f where
    sample :: forall a. f a -> IO a
    sample xs = getAt xs <$> randomRIO (0, getLength xs - 1)
    getAt :: f a -> Int -> a
    getLength :: f a -> Int

instance Samplable [] where
    getAt = (!!)
    getLength = length

instance Samplable Vector where
    getAt = (V.!)
    getLength = V.length

fromWord :: Word -> Text
fromWord (Middle x) = x
fromWord _ = ""

fromWords :: Vector Word -> Text
fromWords words = V.foldl (T.append) "" $ V.map fromWord words

toWords :: [Text] -> Vector Word
toWords [] = V.empty
toWords words = V.fromList $ Begin : init (convert words)
  where
    convert :: [Text] -> [Word]
    convert [] = []
    convert (x:xs) = if x == "\n"
                     then End : Begin : convert xs
                     else Middle x : convert xs

generateTable :: Int -> Vector Word -> Table
generateTable order words
  | V.length words < order = M.empty
  | any (== End) (V.take order words) = generateTable order (V.unsafeTail words)
  | otherwise =
    let table = generateTable order (V.unsafeTail words)
        in M.insertWith (V.++) (V.take order words) (V.singleton (words V.! order)) table

lookupNextWordCandidates :: Table -> Vector Word -> Vector Word
lookupNextWordCandidates table key = fromMaybe V.empty $ M.lookup key table

buildRoseTree :: Table -> Vector Word -> Rose Word
buildRoseTree table seed = Rose seed $ V.map (\w -> buildRoseTree table (V.unsafeTail seed `V.snoc` w)) (lookupNextWordCandidates table seed)

generateWordPathByRandomlyWalkingRoseTree :: Rose Word -> IO (Vector Word)
generateWordPathByRandomlyWalkingRoseTree rose@(Rose words _) =
    let go path (Rose words children) =
          if null children || all (== End) words
             then pure path
             else sample children >>= go (path `V.snoc` V.unsafeLast words)
        in (V.unsafeInit words V.++) <$> go V.empty rose

buildTable :: Int -> Text -> IO Table
buildTable order source = do
    mecab <- new ["mecab", "-l0"]
    nodeLines <- mapM (parseToNodes mecab) (T.lines source)

    let wordLines = map (filter (not . T.null) . map nodeSurface) nodeLines
        allWords = intercalate ["\n"] wordLines
        table = generateTable order (toWords allWords)

    pure table

generatePoem :: Table -> IO Text
generatePoem table = do
    seed <- sample <$> filter ((== Begin) . V.unsafeHead) . M.keys $ table
    let tree = buildRoseTree table seed
    fromWords <$> generateWordPathByRandomlyWalkingRoseTree tree
