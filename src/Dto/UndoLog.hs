module Dto.UndoLog
  ( UndoLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )

data UndoLogDto = UndoLogDto
  { day   :: Text
  , times :: Integer
  }
  deriving Show

instance ToJSON UndoLogDto where
  toJSON (UndoLogDto { day, times }) =
    object ["t" .= ("Undo" :: String), "day" .= day, "times" .= times]

  toEncoding (UndoLogDto { day, times }) =
    pairs ("t" .= ("Undo" :: String) <> "day" .= day <> "times" .= times)
