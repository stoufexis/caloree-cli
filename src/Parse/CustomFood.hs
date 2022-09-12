module Parse.CustomFood
  ( parseCustomFoodCommands
  ) where
import           Model.Command                  ( Command
                                                  ( AddCustomFood
                                                  , DeleteCustomFood
                                                  , SearchCustomFood
                                                  , ViewCustomFood
                                                  )
                                                )
import           Options.Applicative
import           Parse.Common                   ( descriptionOption
                                                , idFoodViewOption
                                                , limitOption
                                                , nutrientsOption
                                                , pageOption
                                                , verbosityOption
                                                , viewFoodAmountOption
                                                )

addCustomFood :: Mod CommandFields Command
addCustomFood = command "add" $ info
  (AddCustomFood <$> descriptionOption <*> nutrientsOption)
  (fullDesc <> progDesc "Create new custom food")

deleteCustomFood :: Mod CommandFields Command
deleteCustomFood = command
  "delete"
  (info (DeleteCustomFood <$> idFoodViewOption) commandDescription)
  where commandDescription = fullDesc <> progDesc "Delete a custom food"

searchCustomFood :: Mod CommandFields Command
searchCustomFood = command
  "search"
  (info
    (   SearchCustomFood
    <$> verbosityOption
    <*> descriptionOption
    <*> pageOption
    <*> limitOption
    )
    commandDescription
  )
 where
  commandDescription =
    fullDesc <> progDesc "Search your custom foods matching the given filters"

viewCustomFood :: Mod CommandFields Command
viewCustomFood = command
  "view"
  (info
    (   ViewCustomFood
    <$> verbosityOption
    <*> idFoodViewOption
    <*> viewFoodAmountOption
    )
    commandDescription
  )
 where
  commandDescription =
    fullDesc <> progDesc "View nutrients of food of a specific amount"

parseCustomFoodCommands :: Mod CommandFields Command
parseCustomFoodCommands = command "custom" $ info
  (hsubparser
    (viewCustomFood <> searchCustomFood <> addCustomFood <> deleteCustomFood)
  )
  commandDescription
 where
  commandDescription =
    fullDesc
      <> progDesc
           "Commands for searching, viewing, deleting and adding custom foods"
      <> header "Search foods"

