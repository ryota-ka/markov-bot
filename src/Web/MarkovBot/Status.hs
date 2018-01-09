module Web.MarkovBot.Status(
    Status(..)
  , statusIsRetweet
) where

import Data.Bool (bool)
import qualified Data.ByteString.Char8 as B
import Data.Csv ((.!), FromField, FromRecord, parseField, parseRecord, Record)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Text.Read (readMaybe)

data Status = Status {
    statusTweetId                  :: Int
  , statusInReplyToStatusId        :: Maybe Int
  , statusInReplyToUserId          :: Maybe Int
  , statusTimestamp                :: UTCTime
  , statusSource                   :: Text
  , statusText                     :: Text
  , statusRetweetedStatusId        :: Maybe Int
  , statusRetweetedStatusUserId    :: Maybe Int
  , statusRetweetedStatusTimestamp :: Maybe UTCTime
  , statusExpandedUrls             :: Text
  } deriving (Show)

instance FromField UTCTime where
    parseField s =
        case readMaybe (B.unpack s) of
          Nothing -> fail "failed to parse UTCTime"
          Just t  -> pure t

instance FromRecord Status where
    parseRecord v = Status
                 <$> v .! 0
                 <*> v .! 1
                 <*> v .! 2
                 <*> v .! 3
                 <*> v .! 4
                 <*> v .! 5
                 <*> v .! 6
                 <*> v .! 7
                 <*> v .! 8
                 <*> v .! 9

statusIsRetweet :: Status -> Bool
statusIsRetweet status
    = case statusRetweetedStatusId status of
           (Just _) -> True
           _        -> False
