{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.CustomFoodRequest
  ( getCustomFood
  , getCustomFoods
  , addCustomFood
  , deleteCustomFood
  ) where
import           Control.Monad.Cont             ( MonadIO )
import           Control.Monad.RWS              ( MonadReader )
import           Dto.CustomFoodDto              ( CustomFoodDto(..) )
import           Http.Common                    ( reqUnsecure )
import           Model.Config                   ( AppConfig )
import           Model.CustomFood               ( CustomFood )
import           Model.CustomFoodPreview        ( CustomFoodPreview )
import           Model.Nutrients                ( Nutrients )
import           Model.Types
import           Network.HTTP.Req
import           Typeclass.AsQueryParam         ( AsQueryParam(qparam) )

getCustomFoods
  :: (MonadReader AppConfig m, MonadIO m)
  => Description
  -> PageLimit
  -> m [CustomFoodPreview]
getCustomFoods d pl = fmap responseBody request
 where
  path    = (/: "custom-food")
  request = reqUnsecure GET path NoReqBody jsonResponse params
  params  = qparam d <> qparam pl

getCustomFood
  :: (MonadReader AppConfig m, MonadIO m) => Id -> Amount -> m [CustomFood]
getCustomFood (Id i) (Amount _) = fmap (pure . responseBody) request
 where
  path x = x /: "custom-food" /~ i
  request = reqUnsecure GET path NoReqBody jsonResponse mempty

addCustomFood
  :: (MonadReader AppConfig m, MonadIO m) => Description -> Nutrients -> m ()
addCustomFood d n = request >> pure ()
 where
  body    = ReqBodyJson CustomFoodDto { description = d, nutrients = n }
  request = reqUnsecure POST (/: "custom-food") body ignoreResponse mempty

deleteCustomFood :: (MonadReader AppConfig m, MonadIO m) => Id -> m ()
deleteCustomFood (Id i) = request >> pure ()
 where
  path x = x /: "custom-food" /~ i
  request = reqUnsecure DELETE path NoReqBody ignoreResponse mempty
