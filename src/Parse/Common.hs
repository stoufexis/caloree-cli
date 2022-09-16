{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse.Common
  ( readMDesc
  , readMMaybe
  , readMEnum
  , verbosityOption
  , descriptionOption
  , pageOption
  , limitOption
  , idFoodViewOption
  , viewFoodAmountOption
  , logFiltersOption
  , addFoodAmountOption
  , dateOption
  , timeOption
  , efidOptionMandatory
  , resultNum
  , timeRoundOption
  , nutrientsOption
  , descriptionArgument
  , efidArgument
  , gramsArgument
  ) where
import qualified Data.Text                     as T
import           Data.Text                      ( split )
import           Model.Command                  ( LogFilters(LogFilters) )
import           Model.DateTime
import           Model.Nutrients
import           Model.Types
import           Options.Applicative
import           Text.Read                      ( readEither )


readMMaybe :: Read a => (a -> b) -> ReadM (Maybe b)
readMMaybe f = eitherReader $ \x -> fmap (Just . f) (readEither x)

readMDesc :: ReadM Description
readMDesc = eitherReader (Right . Description . T.pack)

readMEnum :: Enum a => ReadM (Maybe a)
readMEnum = fmap (fmap toEnum) (readMMaybe id)

readMId :: ReadM Id
readMId = eitherReader (fmap Id . readEither)

verbosityOption :: Parser (Maybe Verbosity)
verbosityOption = option
  readMEnum
  (  long "verbosity"
  <> short 'v'
  <> metavar "VERBOSITY"
  <> help "Choose the amount of output verbosity"
  <> value Nothing
  )

descriptionArgument :: Parser Description
descriptionArgument = fmap Description $ strArgument (metavar "DESCRIPTION")

descriptionOption :: Parser Description
descriptionOption = fmap Description $ strOption
  (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help
    "Description to be used for searching"
  )

pageOption :: Parser (Maybe Page)
pageOption = option
  (readMMaybe Page)
  (  long "page"
  <> short 'p'
  <> metavar "PAGE"
  <> help "Page of results to show"
  <> value Nothing
  )

limitOption :: Parser (Maybe Limit)
limitOption = option
  (readMMaybe Limit)
  (  long "limit"
  <> short 'l'
  <> metavar "LIMIT"
  <> help "Limit results to"
  <> value Nothing
  )

idFoodViewOption :: Parser Id
idFoodViewOption =
  option readMId (long "id" <> short 'i' <> metavar "ID" <> help "Id of a food")

gramsOpt :: ReadM a -> Mod OptionFields a -> Parser a
gramsOpt f v = option
  f
  (  long "amount"
  <> short 'a'
  <> metavar "AMOUNT"
  <> help "Amount of food in grams"
  <> v
  )

gramsArgument :: Parser Grams
gramsArgument = argument (fmap Grams auto) (metavar "AMOUNT")

viewFoodAmountOption :: Parser (Maybe Grams)
viewFoodAmountOption = gramsOpt (readMMaybe Grams) (value Nothing)

addFoodAmountOption :: Parser Grams
addFoodAmountOption = gramsOpt (fmap Grams auto) mempty

parseDate :: (Date -> a) -> ReadM a
parseDate f = eitherReader (\x -> parse x $ split delimiters $ T.pack x)
 where
  parse x = \case
    [y, m, d] ->
      fmap f
        $   Date
        <$> readEither (T.unpack y)
        <*> readEither (T.unpack m)
        <*> readEither (T.unpack d)

    _ -> Left x

  delimiters = \case
    '-' -> True
    _   -> False

parseTime :: (Time -> a) -> ReadM a
parseTime f = eitherReader (\x -> parse x $ split delimiters $ T.pack x)
 where
  parse x = \case
    [h, m] ->
      fmap f $ Time <$> readEither (T.unpack h) <*> readEither (T.unpack m)

    _ -> Left x

  delimiters = \case
    ':' -> True
    _   -> False


dateOption :: Parser (Maybe Date)
dateOption = option
  (parseDate Just)
  (  long "day"
  <> short 'd'
  <> metavar "DAY"
  <> help "Day in the form of `YYYY-MM-DD`"
  <> value Nothing
  )

efidOptions :: Mod OptionFields a
efidOptions =
  long "id" <> short 'i' <> metavar "CUSTOM_FOOD_ID/FOOD_ID" <> help
    "Id referencing a food"

parseEfid :: ReadM EFID
parseEfid = eitherReader $ \case
  '-' : i -> fmap (EFID . Left . Id) $ readEither i
  i       -> fmap (EFID . Right . Id) $ readEither i

efidOptionMandatory :: Parser EFID
efidOptionMandatory = option parseEfid efidOptions

efidOptionOptional :: Parser (Maybe EFID)
efidOptionOptional =
  option (fmap Just parseEfid) (efidOptions <> value Nothing)

efidArgument :: Parser EFID
efidArgument = argument parseEfid (metavar "FOOD_ID/CUSTOM_FOOD_ID")

timeOption :: Parser (Maybe Time)
timeOption = option
  (parseTime Just)
  (  short 't'
  <> long "time"
  <> metavar "TIME"
  <> help "Time in the form `HH:MM`"
  <> value Nothing
  )

timeRoundOption :: Parser (Maybe TimeRound)
timeRoundOption = option
  (fmap (Just . TimeRound) auto)
  (  short 'r'
  <> long "round"
  <> metavar "ROUND"
  <> help "Present logs with time rounded to `r`"
  <> value Nothing
  )

logFiltersOption :: Parser LogFilters
logFiltersOption =
  LogFilters <$> intervalOpt <*> dateOption <*> efidOptionOptional
 where
  intervalOpt = makeInterval <$> begin <*> end

  makeInterval (Just b) (Just e) = Just $ Inteval b e
  makeInterval (Just b@Time { hour, minute }) Nothing =
    Just $ Inteval b $ Time hour $ minute + 1
  makeInterval _ _ = Nothing

  begin = option
    (fmap Just $ parseTime id)
    (  short 'b'
    <> long "begin"
    <> metavar "BEGIN"
    <> help "Time to start range in the form `HH:MM`"
    <> value Nothing
    )

  end = option
    (fmap Just $ parseTime id)
    (  long "end"
    <> short 'e'
    <> metavar "END"
    <> help "Time to start range in the form `HH:MM`"
    <> value Nothing
    )


resultNum :: Parser (Maybe Integer)
resultNum = option
  (readMMaybe id)
  (short 'n' <> help "Equivelant to -v 0 -l 1 -p `n`" <> value Nothing)

nutrientsOption :: Parser Nutrients
nutrientsOption = Nutrients <$> energy <*> protein <*> carbs <*> fat <*> fiber
 where
  energy =
    option (fmap Kcal auto)
      $  long "energy"
      <> short 'e'
      <> metavar "ENERGY"
      <> help "Energy of custom food"

  protein =
    option (fmap Grams auto)
      $  long "protein"
      <> short 'p'
      <> metavar "PROTEIN"
      <> help "Protein of custom food"

  carbs =
    option (fmap Grams auto)
      $  long "carbs"
      <> short 'c'
      <> metavar "CARBS"
      <> help "Carbs of custom food"

  fat = option (fmap Grams auto) $ long "fat" <> metavar "FAT" <> help
    "Fat of custom food"

  fiber = option (fmap Grams auto) $ long "fiber" <> metavar "FIBER" <> help
    "Fiber of custom food"
