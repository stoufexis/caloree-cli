module ParseCommand
  () where
import           Options.Applicative            ( long
                                                , strOption
                                                )
import           Options.Applicative.Builder


-- instance WithDefault Inteval where
--   withDefault = Inteval (Just withDefault, Just withDefault, Just withDefault)

-- instance WithDefault Group where
--   withDefault = Group 0

-- instance WithDefault Minute where
--   withDefault = Minute 1440

-- instance WithDefault Offset where
--   withDefault = Offset 0

-- instance WithDefault PageLimit where
--   withDefault = PageLimit { page = 0, limit = 25 }

-- instance WithDefault Verbosity where
--   withDefault = Normal

-- instance WithDefault Amount where
--   withDefault = Amount 100

data D = OTT Int Bool String | OTT' Int Bool

app = error ""
 where
  iOpt = strOption
    (long "number" <> short 'n' <> metavar "NUMBER" <> value "out.txt" <> help
      "Write output to FILE"
    )

