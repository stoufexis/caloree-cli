module Dto.RemoveLog
  ( RemoveLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )

data RemoveLogDto = RemoveLogDto
  { day :: Text
  , num :: Integer
  }

instance ToJSON RemoveLogDto where
  toJSON (RemoveLogDto { day, num }) =
    object ["t" .= ("Remove" :: String), "day" .= day, "num" .= num]

  toEncoding (RemoveLogDto { day, num }) =
    pairs ("t" .= ("Remove" :: String) <> "day" .= day <> "num" .= num)
