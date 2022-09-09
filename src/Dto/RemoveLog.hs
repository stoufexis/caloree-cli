module Dto.RemoveLog
  ( RemoveLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.Types                    ( EFID
                                                , Inteval
                                                )

data RemoveLogDto = RemoveLogDto
  { fid    :: Maybe EFID
  , day    :: Text
  , minute :: Inteval
  }

instance ToJSON RemoveLogDto where
  toJSON (RemoveLogDto { fid, day, minute }) = object
    [ "t" .= ("Remove" :: String)
    , "fid" .= fid
    , "day" .= day
    , "minute" .= minute
    ]

  toEncoding (RemoveLogDto { fid, day, minute }) = pairs
    (  "t"
    .= ("Remove" :: String)
    <> "fid"
    .= fid
    <> "day"
    .= day
    <> "minute"
    .= minute
    )
