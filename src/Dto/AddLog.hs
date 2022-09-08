module Dto.AddLog
  ( AddLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Model.Types

data AddLogDto = AddLogDto
  { fid    :: Id
  , amount :: Amount
  , day    :: Text
  , minute :: Minute
  }
  deriving (Show, Generic)

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
