module Dto.AddLog
  ( AddLogDto(..)
  , FID(..)
  ) where
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Model.Types                    ( Amount
                                                , Id
                                                , Minute
                                                )
newtype FID = FID {food_id :: Id} deriving (Show, Generic)

instance ToJSON FID
instance FromJSON FID

data AddLogDto = AddLogDto
  { t      :: Text
  , fid    :: FID
  , amount :: Amount
  , day    :: Text
  , minute :: Minute
  }
  deriving (Show, Generic)

instance ToJSON AddLogDto
instance FromJSON AddLogDto
