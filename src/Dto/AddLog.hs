module Dto.AddLog
  ( AddLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.Types
import Model.DateTime

data AddLogDto = AddLogDto
  { fid    :: EFID
  , amount :: Grams
  , day    :: Text
  , minute :: Minute
  }
  deriving Show

instance ToJSON AddLogDto where
  toJSON (AddLogDto { fid, amount, day, minute }) = object
    [ "t" .= ("Add" :: String)
    , "fid" .= fid
    , "amount" .= amount
    , "day" .= day
    , "minute" .= minute
    ]

  toEncoding (AddLogDto { fid, amount, day, minute }) = pairs
    (  "t"
    .= ("Add" :: String)
    <> "fid"
    .= fid
    <> "amount"
    .= amount
    <> "day"
    .= day
    <> "minute"
    .= minute
    )
