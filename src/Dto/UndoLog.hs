module Dto.UndoLog
  ( UndoLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.Types                    ( UndoTimes )

data UndoLogDto = UndoLogDto
  { day   :: Text
  , times :: UndoTimes
  }
  deriving Show

instance ToJSON UndoLogDto where
  toJSON (UndoLogDto { day, times }) =
    object ["t" .= ("Undo" :: String), "day" .= day, "times" .= times]

  toEncoding (UndoLogDto { day, times }) =
    pairs ("t" .= ("Undo" :: String) <> "day" .= day <> "times" .= times)
