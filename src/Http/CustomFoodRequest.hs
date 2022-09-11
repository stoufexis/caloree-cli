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
import           Typeclass.WithDefault          ( def )

getCustomFoods
  :: (MonadReader AppConfig m, MonadIO m)
  => Description
  -> Maybe Page
  -> Maybe Limit
  -> m [CustomFoodPreview]
getCustomFoods d p l = fmap responseBody request
 where
  request = reqUnsecure GET (/: "custom-food") NoReqBody jsonResponse params
  params  = qparam d <> qparam (def p) <> qparam (def l)

getCustomFood
  :: (MonadReader AppConfig m, MonadIO m) => Id -> Maybe Grams -> m CustomFood
getCustomFood (Id i) _ = fmap responseBody request
 where
  path x = x /: "custom-food" /~ i
  request = reqUnsecure GET path NoReqBody jsonResponse mempty

addCustomFood
  :: (MonadReader AppConfig m, MonadIO m) => Description -> Nutrients -> m ()
addCustomFood description nutrients = request >> pure ()
 where
  body    = ReqBodyJson $ CustomFoodDto description nutrients
  request = reqUnsecure POST (/: "custom-food") body ignoreResponse mempty

deleteCustomFood :: (MonadReader AppConfig m, MonadIO m) => Id -> m ()
deleteCustomFood (Id i) = request >> pure ()
 where
  path x = x /: "custom-food" /~ i
  request = reqUnsecure DELETE path NoReqBody ignoreResponse mempty
