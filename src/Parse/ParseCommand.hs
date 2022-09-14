{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse.ParseCommand
  ( parseCommand
  ) where
import           Control.Applicative
import           Control.Monad.Cont             ( MonadIO(liftIO) )
import           Model.Command                  ( Command
                                                  ( SearchBothFood
                                                  , ViewLog
                                                  )
                                                , LogFilters(LogFilters)
                                                )
import           Model.DateTime                 ( Inteval(Inteval) )
import           Options.Applicative
import           Parse.Common                   ( descriptionArgument )
import           Parse.CustomFood               ( parseCustomFoodCommands )
import           Parse.Food                     ( parseFoodCommands )
import           Parse.Log                      ( parseLogCommands )
import           Parse.Nutrients                ( parseUpdateNutrients )


defaultCommand :: Parser Command
defaultCommand = pure $ ViewLog Nothing Nothing logFilters Nothing Nothing
 where
  logFilters = LogFilters interval Nothing Nothing
  interval   = Inteval Nothing Nothing Nothing

defaultCommandSearch :: Parser Command
defaultCommandSearch =
  SearchBothFood Nothing
    <$> descriptionArgument
    <*> pure Nothing
    <*> pure Nothing

parseCommand :: MonadIO m => m Command
parseCommand = liftIO (execParser $ info commands idm)
 where
  commands =
    helper <*> (subcommands <|> defaultCommandSearch <|> defaultCommand)

  subcommands =
    hsubparser
      $  parseLogCommands
      <> parseCustomFoodCommands
      <> parseFoodCommands
      <> parseUpdateNutrients

