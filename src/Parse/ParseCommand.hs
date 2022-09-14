{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse.ParseCommand
  ( parseCommand
  ) where
import           Control.Applicative
import           Control.Monad.Cont             ( MonadIO(liftIO) )
import           Model.Command                  ( Command
                                                  ( AddLog
                                                  , SearchBothFood
                                                  , ViewLog
                                                  )
                                                , LogFilters(LogFilters)
                                                )
import           Model.DateTime                 ( Inteval(Inteval) )
import           Model.Types                    ( Description(Description)
                                                , EFID(EFID)
                                                , Grams(Grams)
                                                , Id(Id)
                                                )
import           Options.Applicative
import           Parse.Common                   ( descriptionOption )
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
 where
  descriptionArgument = fmap Description $ strArgument (metavar "DESCRIPTION")

defaultCommandAdd :: Parser Command
defaultCommandAdd =
  AddLog <$> amount <*> pure Nothing <*> pure Nothing <*> efid
 where
  amount   = argument (fmap Grams auto) (metavar "AMOUNT")
  efid     = fmap readEfid $ strArgument (metavar "FOOD_ID/CUSTOM_FOOD_ID")
  readEfid = \case
    '*' : i -> EFID $ Left $ Id $ read i
    i       -> EFID $ Right $ Id $ read i

parseCommand :: MonadIO m => m Command
parseCommand = liftIO (execParser $ info commands idm)
 where
  commands =
    helper
      <*> (   subcommands
          <|> defaultCommandSearch
          <|> defaultCommandAdd
          <|> defaultCommand
          )

  subcommands =
    hsubparser
      $  parseLogCommands
      <> parseCustomFoodCommands
      <> parseFoodCommands
      <> parseUpdateNutrients

