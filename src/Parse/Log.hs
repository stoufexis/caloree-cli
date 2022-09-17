module Parse.Log
  ( parseLogCommands
  ) where
import           Model.Command
import           Model.Types
import           Options.Applicative
import           Typeclass.Parsed

updateLog :: Mod CommandFields Command
updateLog = command "update" $ info
  (UpdateLog <$> parserM <*> parserO <*> parserM)
  (fullDesc <> progDesc "Create new custom food")

deleteLog :: Mod CommandFields Command
deleteLog = command "delete" $ info
  (RemoveLog <$> parserO <*> parserM)
  (fullDesc <> progDesc "Create new custom food")

undoLog :: Mod CommandFields Command
undoLog = command "undo" $ info
  (UndoLog <$> parserO <*> parserO)
  (fullDesc <> progDesc "Create new custom food")

viewLog :: Mod CommandFields Command
viewLog = command "view" $ info
  (   makeViewLog
  <$> parserO
  <*> parserO
  <*> parserO
  <*> parserM
  <*> parserO
  <*> parserO
  )
  (fullDesc <> progDesc "View all logs matching filters")

 where
  makeViewLog r (Just (EntryNum n)) v f _ _ =
    ViewLog r v f (Just $ Page n) (Just $ Limit 1)
  makeViewLog r Nothing v d p l = ViewLog r v d p l

addLog :: Mod CommandFields Command
addLog = command "add" $ info
  (AddLog <$> parserM <*> parserO <*> parserO <*> parserM)
  (fullDesc <> progDesc "Log a food at an amount")

parseLogCommands :: Mod CommandFields Command
parseLogCommands = command "log" $ info
  (hsubparser $ addLog <> viewLog <> updateLog <> deleteLog <> undoLog)
  (fullDesc <> progDesc "Commands for viewing and manipulating logs")
