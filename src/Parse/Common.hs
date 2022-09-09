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
  ) where
import qualified Data.Text                     as T
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
idFoodViewOption = option
  readMId
  (long "id" <> short 'i' <> metavar "ID" <> help "Id of the food to be viewed")

viewFoodAmountOption :: Parser (Maybe Amount)
viewFoodAmountOption = option
  (readMMaybe Amount)
  (  long "amount"
  <> short 'a'
  <> metavar "AMOUNT"
  <> help "Nutrients of Food per this amount"
  <> value Nothing
  )


