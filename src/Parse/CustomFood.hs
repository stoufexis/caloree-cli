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
import           Typeclass.Parsed

addCustomFood :: Mod CommandFields Command
addCustomFood = command "add" $ info
  (AddCustomFood <$> parserM <*> parserM)
  (fullDesc <> progDesc "Create new custom food")

deleteCustomFood :: Mod CommandFields Command
deleteCustomFood = command "delete" $ info
  (DeleteCustomFood <$> parserM)
  (fullDesc <> progDesc "Delete a custom food")

searchCustomFood :: Mod CommandFields Command
searchCustomFood = command "search" $ info
  (SearchCustomFood <$> parserO <*> parserO <*> parserO <*> parserO)
  (fullDesc <> progDesc "Search your custom foods matching the given filters")

viewCustomFood :: Mod CommandFields Command
viewCustomFood = command "view" $ info
  (ViewCustomFood <$> parserO <*> parserM <*> parserO)
  (fullDesc <> progDesc "View nutrients of food of a specific amount")

parseCustomFoodCommands :: Mod CommandFields Command
parseCustomFoodCommands = command "custom" $ info
  (  hsubparser
  $  viewCustomFood
  <> searchCustomFood
  <> addCustomFood
  <> deleteCustomFood
  )
  (fullDesc <> progDesc
    "Commands for searching, viewing, deleting and adding custom foods"
  )

