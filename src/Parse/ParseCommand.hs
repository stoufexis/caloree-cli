{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse.ParseCommand
  ( parseCommand
  ) where
import           Control.Applicative
import           Control.Monad.Cont             ( MonadIO(liftIO) )
import           Model.Command                  ( Command(ViewLog)
                                                , LogFilters(LogFilters)
                                                )
import           Options.Applicative
import           Parse.CustomFood               ( parseCustomFoodCommands )
import           Parse.Food                     ( parseFoodCommands )
import           Parse.Log                      ( parseLogCommands )
import           Parse.Nutrients                ( parseUpdateNutrients )


defaultCommand :: Parser Command
defaultCommand = pure $ ViewLog Nothing Nothing logFilters Nothing Nothing
 where
  logFilters = LogFilters Nothing Nothing Nothing

parseCommand :: MonadIO m => m Command
parseCommand = liftIO (execParser $ info commands idm)
 where
  commands = helper <*> (subcommands <|> defaultCommand)

  subcommands =
    hsubparser
      $  parseLogCommands
      <> parseCustomFoodCommands
      <> parseFoodCommands
      <> parseUpdateNutrients

