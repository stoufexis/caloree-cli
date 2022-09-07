module Typeclass.Requested
  ( Requested(..)
  ) where
import           Control.Monad.RWS
import           Http.FoodRequest
import           Model.Config
import           Model.FoodPreview
import           Model.Types

class Requested m p a | p -> a where
  request :: p -> m [a]

instance (MonadReader AppConfig m, MonadIO m) => Requested m (Description, PageLimit) FoodPreview where
  request = getFoods
