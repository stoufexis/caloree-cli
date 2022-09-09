module Dto.UndoLog
  ( UndoLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.Types
import Model.DateTime

data UndoLogDto = UndoLogDto
  { fid    :: Maybe EFID
  , day    :: Text
  , minute :: Inteval
  , times  :: Int
  }
  deriving Show

instance ToJSON UndoLogDto where
  toJSON (UndoLogDto { fid, day, minute, times }) = object
    [ "t" .= ("Undo" :: String)
    , "fid" .= fid
    , "day" .= day
    , "minute" .= minute
    , "times" .= times
    ]

  toEncoding (UndoLogDto { fid, day, minute, times }) = pairs
    (  "t"
    .= ("Undo" :: String)
    <> "fid"
    .= fid
    <> "day"
    .= day
    <> "minute"
    .= minute
    <> "times"
    .= times
    )
