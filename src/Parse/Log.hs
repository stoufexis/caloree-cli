module Parse.Log
  ( parseLogCommands
  ) where
import           Model.Command
import           Model.Types
import           Options.Applicative
import           Parse.Common

updateLog :: Mod CommandFields Command
updateLog = command "update" $ info
  (UpdateLog <$> logFiltersOption <*> addFoodAmountOption)
  (fullDesc <> progDesc "Create new custom food")

deleteLog :: Mod CommandFields Command
deleteLog = command "delete" $ info
  (RemoveLog <$> logFiltersOption)
  (fullDesc <> progDesc "Create new custom food")

undoLog :: Mod CommandFields Command
undoLog = command "undo" $ info
  (UndoLog <$> logFiltersOption <*> times)
  (fullDesc <> progDesc "Create new custom food")
 where
  times =
    option (auto @Int) $ long "times" <> short 'n' <> metavar "TIMES" <> help
      "Will undo this many times"

viewLog :: Mod CommandFields Command
viewLog = command
  "view"
  (info
    (   makeViewLog
    <$> timeRoundOption
    <*> resultNum
    <*> verbosityOption
    <*> logFiltersOption
    <*> pageOption
    <*> limitOption
    )
    (fullDesc <> progDesc "View all logs matching filters")
  )
 where
  makeViewLog r (Just n) _ f _ _ =
    ViewLog r (Just Minimal) f (Just $ Page n) (Just $ Limit 1)
  makeViewLog r Nothing v d p l = ViewLog r v d p l

addLog :: Mod CommandFields Command
addLog = command
  "add"
  (info
    (   AddLog
    <$> addFoodAmountOption
    <*> dateOption
    <*> timeOption
    <*> efidOptionMandatory
    )
    (fullDesc <> progDesc "Log a food at an amount")
  )


parseLogCommands :: Mod CommandFields Command
parseLogCommands = command "log" $ info
  (hsubparser $ addLog <> viewLog <> updateLog <> deleteLog <> undoLog)
  (fullDesc <> progDesc "Commands for viewing and manipulating logs")

