{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Typeclass.Tabled
  ( Tabled(..)
  ) where
import           Colonnade                      ( Colonnade
                                                , Headed
                                                )
import           Data.Text                      ( Text )
import           Data.Vector                    ( Vector
                                                , indexed
                                                )
import           Model.Types                    ( Verbosity )
import           RenderTable                    ( renderTable )

class Tabled a where
  colonnade :: Verbosity -> Colonnade Headed (Int, a) Text
  table :: Verbosity -> Vector a -> Text

  default table :: Verbosity -> Vector a -> Text
  table v = renderTable (colonnade v) . indexed

