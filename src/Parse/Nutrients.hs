module Parse.Nutrients
  ( parseUpdateNutrients
  ) where
import           Model.Command                  ( Command(UpdateTargets) )
import           Options.Applicative
import           Typeclass.Parsed               ( ParsedMandatory(parserM) )

parseUpdateNutrients :: Mod CommandFields Command
parseUpdateNutrients = command "nutrients" $ info
  (UpdateTargets <$> parserM)
  (fullDesc <> progDesc "Update target nutrients")
