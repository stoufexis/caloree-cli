module Dto.UpdateLog
  ( ModifyLogDto(..)
  ) where
import           Data.Aeson.Types
import           Data.Text                      ( Text )
import           Model.Types                    ( Grams )

data ModifyLogDto = ModifyLogDto
  { newAmount :: Grams
  , day       :: Text
  , num       :: Integer
  }
  deriving Show

instance ToJSON ModifyLogDto where
  toJSON (ModifyLogDto { newAmount, day, num }) = object
    [ "t" .= ("Modify" :: String)
    , "newAmount" .= newAmount
    , "day" .= day
    , "num" .= num
    ]

  toEncoding (ModifyLogDto { newAmount, day, num }) = pairs
    (  "t"
    .= ("Modify" :: String)
    <> "newAmount"
    .= newAmount
    <> "day"
    .= day
    <> "num"
    .= num
    )
