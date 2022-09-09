module ParseCommand
  ( app
  ) where
import qualified Data.Text                     as T
import           Model.Types                    ( Description(Description) )
import           Options.Applicative            ( execParser )
import           Options.Applicative.Builder
import           Text.Read                      ( readEither )

maybeOpt :: Read a => ReadM (Maybe a)
maybeOpt = eitherReader $ \x -> fmap Just (readEither x)

descOpt :: ReadM Description
descOpt = eitherReader (Right . Description . T.pack)

searchFoodCommand :: Mod CommandFields (Maybe Int, Description)
searchFoodCommand = command "search"
                            (info aa (progDesc "Search food database"))
 where
  aa              = (,) <$> verbosityOption <*> descOption

  verbosityOption = option
    (maybeOpt @Int)
    (  long "verbosity"
    <> short 'v'
    <> metavar "VERBOSITY"
    <> help "Choose the amount of output verbosity"
    <> value Nothing
    )

  descOption = option
    descOpt
    (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help
      "Description that will be used to search"
    )

app :: IO ()
app = execParser (info (subparser searchFoodCommand) mempty) >>= print
-- appp :: IO ()
-- appp = execParser (info aa mempty) >>= print
--  where
--   aa = subparser $ command
--     "log"
--     (info (subparser (command "start" (info iOpt mempty))) mempty)

--   iOpt :: Parser (Maybe Int)
--   iOpt = option
--     maybeOpt
--     (  long "lines"
--     <> short 'n'
--     <> metavar "K"
--     <> help "Output the last K lines"
--     <> value Nothing
--     )

