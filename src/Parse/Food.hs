module Parse.Food
  ( parseFoodCommands
  ) where
import           Model.Command                  ( Command(SearchFood, ViewFood)
                                                )
import           Options.Applicative
import           Parse.Common                   ( descriptionOption
                                                , idFoodViewOption
                                                , limitOption
                                                , pageOption
                                                , verbosityOption
                                                , viewFoodAmountOption
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
 where
  commandDescription = fullDesc
    <> progDesc "Search foods matching the given filters in the database"

viewFood :: Mod CommandFields Command
viewFood = command
  "view"
  (info
    (ViewFood <$> verbosityOption <*> idFoodViewOption <*> viewFoodAmountOption)
    commandDescription
  )
 where
  commandDescription =
    fullDesc <> progDesc "View nutrients of food of a specific amount"

parseFoodCommands :: ParserInfo Command
parseFoodCommands = info (subparser (searchFood <> viewFood))
                         commandDescription
 where
  commandDescription =
    fullDesc <> progDesc "Commands for searching and viewing foods"


