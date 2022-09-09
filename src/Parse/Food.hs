module Parse.Food
  () where
import           Model.Command                  ( Command(SearchFood) )
import           Options.Applicative
import           Parse.Common                   ( descriptionOption
                                                , limitOption
                                                , pageOption
                                                , verbosityOption
                                                )

searchFood :: Mod CommandFields Command
searchFood = command
  "search"
  (info
    (   SearchFood
    <$> verbosityOption
    <*> descriptionOption
    <*> pageOption
    <*> limitOption
    )
    commandDescription
  )
  where commandDescription = fullDesc <> progDesc "Print a greeting for TARGET"

