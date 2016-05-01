module Web.MarkovBot.Status(
    Status(..)
  , statusForCSVRecord
  , statusIsRetweet
) where

import Data.Bool (bool)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Text.CSV (Record)
import Text.Read (readMaybe)

data Status = Status {
    statusTweetId                  :: Int
  , statusInReplyToStatusId        :: Maybe Int
  , statusInReplyToUserId          :: Maybe Int
  , statusTimestamp                :: UTCTime
  , statusSource                   :: String
  , statusText                     :: String
  , statusRetweetedStatusId        :: Maybe Int
  , statusRetweetedStatusUserId    :: Maybe Int
  , statusRetweetedStatusTimestamp :: Maybe UTCTime
  , statusExpandedUrls             :: String
  } deriving Show

statusForCSVRecord :: Record -> Status
statusForCSVRecord (a:b:c:d:e:f:g:h:i:j:_)
    = let a' = read a
          b' = pack b
          c' = pack c
          d' = read d
          e' = e
          f' = f
          g' = pack g
          h' = pack h
          i' = readMaybe i
          j' = j
       in Status {
                   statusTweetId                  = a'
                 , statusInReplyToStatusId        = b'
                 , statusInReplyToUserId          = c'
                 , statusTimestamp                = d'
                 , statusSource                   = e'
                 , statusText                     = f'
                 , statusRetweetedStatusId        = g'
                 , statusRetweetedStatusUserId    = h'
                 , statusRetweetedStatusTimestamp = i'
                 , statusExpandedUrls             = j'
                 }
    where pack x = bool (readMaybe x) Nothing (null x)

statusIsRetweet :: Status -> Bool
statusIsRetweet status
    = case statusRetweetedStatusId status of
           (Just _) -> True
           _        -> False
