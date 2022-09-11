{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Http.User
  ( getTargetNutrients
  ) where

import           Control.Monad.Cont             ( MonadIO )
import           Control.Monad.RWS              ( MonadReader )
import           Model.Config                   ( AppConfig(..) )
import           Model.Nutrients                ( Nutrients(..) )

getTargetNutrients :: (MonadReader AppConfig m, MonadIO m) => m Nutrients
getTargetNutrients = pure Nutrients { energy  = 3000
                                    , protein = 200
                                    , carbs   = 300
                                    , fat     = 80
                                    , fiber   = 30
                                    }
