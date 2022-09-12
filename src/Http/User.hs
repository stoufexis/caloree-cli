{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Http.User
  ( getTargetNutrients
  , upsertNutrients
  ) where

import           Control.Monad.Cont             ( MonadIO )
import           Control.Monad.RWS              ( MonadReader )
import           Http.Common                    ( reqUnsecure )
import           Model.Config                   ( AppConfig(..) )
import           Model.Nutrients                ( Nutrients(..) )
import           Model.User
import           Network.HTTP.Req

getTargetNutrients :: (MonadReader AppConfig m, MonadIO m) => m Nutrients
getTargetNutrients = fmap (nutrients . responseBody)
  $ reqUnsecure GET (/: "user") NoReqBody jsonResponse mempty

upsertNutrients :: (MonadReader AppConfig m, MonadIO m) => Nutrients -> m ()
upsertNutrients n =
  reqUnsecure POST (/: "user") (ReqBodyJson n) ignoreResponse mempty >> pure ()

