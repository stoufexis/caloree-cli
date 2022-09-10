module Parse.ParseCommand
  ( parseCommand
  ) where
import           Options.Applicative            ( execParser
                                                , helper
                                                , hsubparser
                                                , idm
                                                , info
                                                )
import           Parse.CustomFood               ( parseCustomFoodCommands )
import           Parse.Food                     ( parseFoodCommands )
import           Parse.Log                      ( parseLogCommands )


parseCommand :: IO ()
parseCommand = execParser (info (helper <*> subcommands) idm) >>= print
 where
  subcommands =
    hsubparser
      $  parseLogCommands
      <> parseCustomFoodCommands
      <> parseFoodCommands

