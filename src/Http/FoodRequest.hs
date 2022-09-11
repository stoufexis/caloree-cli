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
import           Model.Types
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )
import           Typeclass.WithDefault          ( def )

baseGetFood
  :: (FromJSON b, MonadIO f, MonadReader AppConfig f) => Option 'Http -> f b
baseGetFood params =
  fmap responseBody $ reqUnsecure GET (/: "food") NoReqBody jsonResponse params

getFoods
  :: (MonadReader AppConfig m, MonadIO m)
  => Description
  -> Maybe Page
  -> Maybe Limit
  -> m [FoodPreview]
getFoods d p l = baseGetFood $ qparam d <> qparam (def p) <> qparam (def l)


getFood
  :: (MonadReader AppConfig m, MonadIO m) => Id -> Maybe Grams -> m [Food]
getFood (Id i) a =
  fmap pure $ baseGetFood $ qparam (EFID $ Right $ Id i) <> qparam (def a)


