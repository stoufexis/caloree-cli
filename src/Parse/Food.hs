module Parse.Food
  ( parseFoodCommands
  ) where
import           Model.Command                  ( Command(SearchFood, ViewFood)
                                                , Verbosity(Minimal)
                                                )
import           Model.Types                    ( EntryNum(EntryNum)
                                                , Limit(Limit)
                                                , Page(Page)
                                                )
import           Options.Applicative
import           Typeclass.Parsed

searchFood :: Mod CommandFields Command
searchFood = command "search" $ info
  (makeSearchFood <$> parserO <*> parserO <*> parserO <*> parserO <*> parserO)
  (  fullDesc
  <> progDesc "Search foods matching the given filters in the database"
  )
 where
  makeSearchFood (Just (EntryNum n)) _ d _ _ =
    SearchFood (Just Minimal) d (Just $ Page n) (Just $ Limit 1)
  makeSearchFood Nothing v d p l = SearchFood v d p l

viewFood :: Mod CommandFields Command
viewFood = command "view" $ info
  (ViewFood <$> parserO <*> parserM <*> parserO)
  (fullDesc <> progDesc "View nutrients of food of a specific amount")

parseFoodCommands :: Mod CommandFields Command
parseFoodCommands = command "food" $ info
  (hsubparser $ searchFood <> viewFood)
  (fullDesc <> progDesc "Commands for searching and viewing foods")



