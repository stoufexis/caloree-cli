{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.FoodRequest
  ( getFoods
  , getFood
  ) where
import           Control.Monad.RWS              ( MonadIO
                                                , MonadReader
                                                )
import           Data.Aeson                     ( FromJSON )
import           Http.Common                    ( reqUnsecure )
import           Model.Config                   ( AppConfig )
import           Model.Food                     ( Food )
import           Model.FoodPreview              ( FoodPreview )
import           Model.Types                    ( Amount(Amount)
                                                , Description(..)
                                                , Id(Id)
                                                , PageLimit(..)
                                                )
import           Network.HTTP.Req

baseGetFood
  :: (FromJSON b, MonadIO f, MonadReader AppConfig f) => Option 'Http -> f b
baseGetFood params =
  fmap responseBody $ reqUnsecure GET (/: "food") NoReqBody jsonResponse params

getFoods
  :: (MonadReader AppConfig m, MonadIO m)
  => (Description, PageLimit)
  -> m [FoodPreview]
getFoods (Description d, PageLimit { page = p, limit = l }) =
  baseGetFood $ "description" =: d <> "page" =: p <> "limit" =: l


getFood :: (MonadReader AppConfig m, MonadIO m) => (Id, Amount) -> m [Food]
getFood (Id i, Amount amount) =
  fmap pure $ baseGetFood $ "food_id" =: i <> "amount" =: amount


