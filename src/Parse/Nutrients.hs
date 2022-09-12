module Parse.Nutrients
  ( parseUpdateNutrients
  ) where
import           Model.Command                  ( Command(UpdateTargets) )
import           Options.Applicative
import           Parse.Common                   ( nutrientsOption )

parseUpdateNutrients :: Mod CommandFields Command
parseUpdateNutrients = command "nutrients" $ info
  (UpdateTargets <$> nutrientsOption)
  (fullDesc <> progDesc "Update target nutrients")
