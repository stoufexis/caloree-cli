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
  ) where
import qualified Data.Text                     as T
import           Data.Text                      ( split )
import           Model.Command                  ( LogFilters(LogFilters) )
import           Model.DateTime                 ( Date(Date)
                                                , Group(Group)
                                                , Inteval(Inteval)
                                                , Minute(Minute)
                                                , Offset(Offset)
                                                , Time(Time), TimeRound (TimeRound)
                                                )
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

viewFoodAmountOption :: Parser (Maybe Grams)
viewFoodAmountOption = option
  (readMMaybe Grams)
  (  long "amount"
  <> short 'a'
  <> metavar "AMOUNT"
  <> help "Nutrients of food per this amount"
  <> value Nothing
  )

addFoodAmountOption :: Parser Grams
addFoodAmountOption = option
  (fmap Grams auto)
  (long "amount" <> short 'a' <> metavar "AMOUNT" <> help "Amount of food")

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
    '-' -> True
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

fidOptPartial :: Mod OptionFields a
fidOptPartial = long "fid" <> metavar "FOOD_ID" <> help "Id referencing a food"

cfidOptPartial :: Mod OptionFields a
cfidOptPartial =
  long "cfid" <> metavar "CUSTOM_FOOD_ID" <> help "Id referencing a custom food"

efidOptionOptional :: Parser (Maybe EFID)
efidOptionOptional = makeEfid <$> cfidOpt <*> fidOpt
 where
  makeEfid (Just cfid) _          = Just $ EFID $ Left cfid
  makeEfid _           (Just fid) = Just $ EFID $ Right fid
  makeEfid _           _          = Nothing

  fidOpt  = option (readMMaybe Id) (fidOptPartial <> value Nothing)
  cfidOpt = option (readMMaybe Id) (cfidOptPartial <> value Nothing)

efidOptionMandatory :: Parser EFID
efidOptionMandatory = fmap EFID $ cfidOpt <|> fidOpt
 where
  fidOpt  = option (fmap (Right . Id) auto) fidOptPartial
  cfidOpt = option (fmap (Left . Id) auto) cfidOptPartial

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
  intervalOpt = Inteval <$> window <*> group <*> offset
  window      = option
    (readMMaybe Minute)
    (  long "window"
    <> short 'w'
    <> metavar "WINDOW"
    <> help "Window results by minutes"
    <> value Nothing
    )

  group = option
    (readMMaybe Group)
    (  long "group"
    <> short 'g'
    <> metavar "GROUP"
    <> help "Group of results to show"
    <> value Nothing
    )

  offset = option
    (readMMaybe Offset)
    (  long "offset"
    <> short 'o'
    <> metavar "offset"
    <> help "Offset results by this amount of minutes"
    <> value Nothing
    )

resultNum :: Parser (Maybe Integer)
resultNum = option
  (readMMaybe id)
  (short 'n' <> help "Equivelant to -v 0 -l 1 -p `n`" <> value Nothing)
