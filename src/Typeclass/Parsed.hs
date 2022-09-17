{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Typeclass.Parsed
  ( ParsedMandatory(..)
  , ParsedOptional(..)
  , ParsedArgument(..)
  ) where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text
                                                , split
                                                )
import qualified Data.Text                     as T
import           Fmt
import           Model.Command                  ( LogFilters(LogFilters) )
import           Model.DateTime
import           Model.Nutrients                ( Nutrients(Nutrients) )
import           Model.Types
import           Options.Applicative
import           Text.Read                      ( readEither )
import           Typeclass.Formatted
import           Typeclass.WithDefault

class ParsedMandatory a where
  parserM  :: Parser a

class ParsedOptional a where
  parserO  :: Parser (Maybe a)

class ParsedArgument a where
  parserA  :: Parser a

instance ParsedOptional Verbosity where
  parserO =
    option readMEnum
      $  long "verbosity"
      <> short 'v'
      <> metavar "VERBOSITY"
      <> helpDefaultText (Proxy @Verbosity) "Output verbosity. One of 0,1,2."
      <> value Nothing

instance ParsedMandatory Description where
  parserM = fmap Description $ strOption (descriptionMeta $ help . pretty)

instance ParsedOptional Description where
  parserO = option
    (fmap Just readMDesc)
    (descriptionMeta (helpDefaultText $ Proxy @Description) <> value Nothing)

instance ParsedOptional Page where
  parserO =
    option (readMMaybe Page)
      $  long "page"
      <> short 'p'
      <> metavar "PAGE"
      <> helpDefaultText (Proxy @Page) "Page of results to show."
      <> value Nothing

instance ParsedOptional Limit where
  parserO =
    option (readMMaybe Limit)
      $  long "limit"
      <> short 'l'
      <> metavar "LIMIT"
      <> helpDefaultText (Proxy @Limit) "Limit results to. "
      <> value Nothing

instance ParsedMandatory Id where
  parserM = option
    readMId
    (long "id" <> short 'i' <> metavar "ID" <> help "Id of a food")

instance ParsedArgument Grams where
  parserA = argument (fmap Grams auto) (metavar "AMOUNT")

instance ParsedOptional Grams where
  parserO =
    gramsOpt (readMMaybe Grams) (helpDefaultText $ Proxy @Grams) (value Nothing)

instance ParsedMandatory Grams where
  parserM = gramsOpt (fmap Grams auto) (help . pretty) mempty

instance ParsedOptional Date where
  parserO = option (parseDate Just) (dateMeta helpf <> value Nothing)
    where helpf x = help $ pretty x <> " Defaults to current date."

instance ParsedMandatory Date where
  parserM = option (parseDate id) (dateMeta $ help . pretty)

instance ParsedMandatory EFID where
  parserM = option parseEfid efidMeta

instance ParsedOptional EFID where
  parserO = option (fmap Just parseEfid) (efidMeta <> value Nothing)

instance ParsedArgument EFID where
  parserA = argument parseEfid (metavar "FOOD_ID/CUSTOM_FOOD_ID")

instance ParsedOptional Time where
  parserO = option
    (parseTime Just)
    (  short 't'
    <> long "time"
    <> metavar "TIME"
    <> help "Time in the form `HH:MM`. Defaults to current time."
    <> value Nothing
    )

instance ParsedOptional TimeRound where
  parserO =
    option (fmap (Just . TimeRound) auto)
      $  short 'r'
      <> long "round"
      <> metavar "ROUND"
      <> helpDefaultText (Proxy @TimeRound)
                         "Present logs with time rounded to `r`."
      <> value Nothing

instance ParsedMandatory LogFilters where
  parserM = LogFilters <$> intervalOpt <*> parserO <*> parserO
   where
    intervalOpt = makeInterval <$> begin <*> end

    makeInterval (Just b) (Just e) = Just $ Inteval b e
    makeInterval (Just b@Time { hour, minute }) Nothing =
      Just $ Inteval b $ Time hour $ minute + 1
    makeInterval _ _ = Nothing

    begin =
      option (fmap Just $ parseTime id)
        $  short 'b'
        <> long "begin"
        <> metavar "BEGIN"
        <> helpDefaultText (Proxy @Inteval)
                           "Time to start range in the form `HH:MM`."
        <> value Nothing


    end =
      option (fmap Just $ parseTime id)
        $  long "end"
        <> short 'e'
        <> metavar "END"
        <> helpDefaultText (Proxy @Inteval)
                           "Time to end range in the form `HH:MM`."
        <> value Nothing

instance ParsedMandatory Nutrients where
  parserM = Nutrients <$> energy <*> protein <*> carbs <*> fat <*> fiber
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

instance ParsedOptional UndoTimes where
  parserO = option
    (readMMaybe UndoTimes)
    (  short 'u'
    <> helpDefaultText (Proxy @UndoTimes) "Undo this many times"
    <> metavar "UNDO_TIMES"
    <> value Nothing
    )

instance ParsedMandatory EntryNum where
  parserM =
    option (fmap EntryNum auto)
      $  short 'n'
      <> long "num"
      <> metavar "NUM"
      <> help "Entry number"

instance ParsedOptional EntryNum where
  parserO =
    option (readMMaybe EntryNum)
      $  short 'n'
      <> long "num"
      <> metavar "NUM"
      <> help "Entry number"
      <> value Nothing

helpDefaultText
  :: forall f a a'
   . (WithDefault a, Formatted a)
  => Proxy a
  -> Text
  -> Mod f a'
helpDefaultText _ t =
  help $ t |+ " Default value: " +| formatted (withDefault @a)

readMMaybe :: Read a => (a -> b) -> ReadM (Maybe b)
readMMaybe f = eitherReader $ \x -> fmap (Just . f) (readEither x)

readMDesc :: ReadM Description
readMDesc = eitherReader (Right . Description . T.pack)

readMEnum :: Enum a => ReadM (Maybe a)
readMEnum = fmap (fmap toEnum) (readMMaybe id)

readMId :: ReadM Id
readMId = eitherReader (fmap Id . readEither)

descriptionMeta :: (Text -> Mod OptionFields a) -> Mod OptionFields a
descriptionMeta hf =
  long "description" <> short 'd' <> metavar "DESCRIPTION" <> hf
    "Description to be used for searching"

gramsOpt
  :: ReadM a -> (Text -> Mod OptionFields a) -> Mod OptionFields a -> Parser a
gramsOpt f helpf v =
  option f
    $  long "amount"
    <> short 'a'
    <> metavar "AMOUNT"
    <> helpf "Grams of food."
    <> v

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

dateMeta :: (Text -> Mod OptionFields a) -> Mod OptionFields a
dateMeta hf = long "day" <> short 'd' <> metavar "DAY" <> hf
  "Day in the form of `YYYY-MM-DD`."

efidMeta :: Mod OptionFields a
efidMeta = long "id" <> short 'i' <> metavar "CUSTOM_FOOD_ID/FOOD_ID" <> help
  "Id referencing a food"

parseEfid :: ReadM EFID
parseEfid = eitherReader $ \case
  'c' : i -> fmap (EFID . Left . Id) $ readEither i
  'f' : i -> fmap (EFID . Right . Id) $ readEither i
  i       -> Left i
