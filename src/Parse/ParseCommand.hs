module Parse.ParseCommand
  ( parseCommand
  ) where
import           Control.Monad.Cont             ( MonadIO(liftIO) )
import           Model.Command                  ( Command )
import           Options.Applicative            ( execParser
                                                , helper
                                                , hsubparser
                                                , idm
                                                , info
                                                )
import           Parse.CustomFood               ( parseCustomFoodCommands )
import           Parse.Food                     ( parseFoodCommands )
import           Parse.Log                      ( parseLogCommands )
import           Parse.Nutrients                ( parseUpdateNutrients )


parseCommand :: MonadIO m => m Command
parseCommand = liftIO $ execParser (info (helper <*> subcommands) idm)
 where
  subcommands =
    hsubparser
      $  parseLogCommands
      <> parseCustomFoodCommands
      <> parseFoodCommands
      <> parseUpdateNutrients

