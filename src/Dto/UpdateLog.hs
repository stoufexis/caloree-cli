module Dto.UpdateLog
  ( ModifyLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.DateTime                 ( Inteval )
import           Model.Types                    ( Grams
                                                , EFID
                                                )

data ModifyLogDto = ModifyLogDto
  { fid       :: Maybe EFID
  , newAmount :: Grams
  , day       :: Text
  , minute    :: Inteval
  }
  deriving Show

instance ToJSON ModifyLogDto where
  toJSON (ModifyLogDto { fid, newAmount, day, minute }) = object
    [ "t" .= ("Modify" :: String)
    , "fid" .= fid
    , "newAmount" .= newAmount
    , "day" .= day
    , "minute" .= minute
    ]

  toEncoding (ModifyLogDto { fid, newAmount, day, minute }) = pairs
    (  "t"
    .= ("Modify" :: String)
    <> "fid"
    .= fid
    <> "newAmount"
    .= newAmount
    <> "day"
    .= day
    <> "minute"
    .= minute
    )
