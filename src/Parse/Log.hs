module Parse.Log
  ( parseLogCommands
  ) where
import           Model.Command                  ( Command
                                                  ( AddLog
                                                  , RemoveLog
                                                  , UndoLog
                                                  , UpdateLog
                                                  , ViewLog
                                                  )
                                                )
import           Options.Applicative
import           Parse.Common                   ( addFoodAmountOption
                                                , dateOption
                                                , efidOptionMandatory
                                                , limitOption
                                                , logFiltersOption
                                                , pageOption
                                                , timeOption
                                                , verbosityOption
                                                )

updateLog :: Mod CommandFields Command
updateLog = command
  "update"
  (info (UpdateLog <$> logFiltersOption <*> addFoodAmountOption) description)
  where description = fullDesc <> progDesc "Create new custom food"

deleteLog :: Mod CommandFields Command
deleteLog = command "delete"
                    (info (RemoveLog <$> logFiltersOption) description)
  where description = fullDesc <> progDesc "Create new custom food"

undoLog :: Mod CommandFields Command
undoLog = command "undo"
                  (info (UndoLog <$> logFiltersOption <*> times) description)
 where
  description = fullDesc <> progDesc "Create new custom food"
  times =
    option (auto @Int) $ long "times" <> short 'n' <> metavar "TIMES" <> help
      "Will undo this many times"

viewLog :: Mod CommandFields Command
viewLog = command
  "view"
  (info
    (   ViewLog
    <$> verbosityOption
    <*> logFiltersOption
    <*> pageOption
    <*> limitOption
    )
    description
  )
  where description = fullDesc <> progDesc "View all logs matching filters"

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
  description
 where
  description =
    fullDesc <> progDesc "Commands for viewing and manipulating logs" <> header
      "Search foods"

