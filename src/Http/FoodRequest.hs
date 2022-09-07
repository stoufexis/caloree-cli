{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.FoodRequest
  ( getFoods
  ) where
import           Control.Monad.RWS              ( MonadReader, MonadIO )
import           Http.Common                    ( reqUnsecure )
import           Model.Config                   ( AppConfig )
import           Model.FoodPreview              ( FoodPreview )
import           Model.Types                    ( Description(..)
                                                , PageLimit(..)
                                                )
import           Network.HTTP.Req

getFoods
  :: (MonadReader AppConfig m, MonadIO m)
  => (Description, PageLimit)
  -> m [FoodPreview]
getFoods (Description d, PageLimit { page = p, limit = l }) =
  fmap responseBody $ reqUnsecure GET "food" NoReqBody params
  where params = "description" =: d <> "page" =: p <> "limit" =: l
