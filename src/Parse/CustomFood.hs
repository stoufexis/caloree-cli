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
import           Model.Nutrients                ( Nutrients(Nutrients) )
import           Options.Applicative
import           Parse.Common                   ( descriptionOption
                                                , idFoodViewOption
                                                , limitOption
                                                , pageOption
                                                , verbosityOption
                                                , viewFoodAmountOption
                                                )

addCustomFood :: Mod CommandFields Command
addCustomFood = command
  "add"
  (info
    (   acf
    <$> descriptionOption
    <*> energy
    <*> protein
    <*> carbs
    <*> fat
    <*> fiber
    )
    commandDescription
  )
 where
  acf d e p c f = AddCustomFood d . Nutrients e p c f

  commandDescription = fullDesc <> progDesc "Create new custom food"

  energy =
    option (auto @Float)
      $  long "energy"
      <> short 'e'
      <> metavar "ENERGY"
      <> help "Energy of custom food"

  protein =
    option (auto @Float)
      $  long "protein"
      <> short 'p'
      <> metavar "PROTEIN"
      <> help "Protein of custom food"

  carbs =
    option (auto @Float) $ long "carbs" <> short 'c' <> metavar "CARBS" <> help
      "Carbs of custom food"

  fat = option (auto @Float) $ long "fat" <> metavar "FAT" <> help
    "Fat of custom food"

  fiber = option (auto @Float) $ long "fiber" <> metavar "FIBER" <> help
    "Fiber of custom food"

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

