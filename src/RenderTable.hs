module RenderTable
  ( renderTable
  ) where

import           Colonnade
import qualified Colonnade.Encode              as E
import           Data.Text
import qualified Data.Text                     as T
import           Fmt

renderTable
  :: Foldable f
  => Colonnade Headed a Text -- ^ columnar encoding
  -> f a -- ^ rows
  -> Text
renderTable col xs = T.unlines
  [ divider
  , mconcat
    [ E.headerMonoidalFull
      sizedCol
      (\(E.Sized msz (Headed h)) -> case msz of
        Just sz -> "| " +| rightPad sz ' ' h |+ " "
        Nothing -> ""
      )
    , "|"
    ]
  , renderBody sizedCol xs
  ]
 where
  sizedCol = E.sizeColumns T.length xs col

  divider  = mconcat
    [ E.headerMonoidalFull
      sizedCol
      (\(E.Sized msz _) -> case msz of
        Just sz -> "+" <> hyphens (sz + 2)
        Nothing -> ""
      )
    , "+"
    ]

renderBody
  :: Foldable f => Colonnade (E.Sized (Maybe Int) Headed) a Text -> f a -> Text
renderBody sizedCol xs = mconcat [divider, rowContents, divider]
 where
  divider = mconcat
    [ E.headerMonoidalFull
      sizedCol
      (\(E.Sized msz _) -> case msz of
        Just sz -> "+" <> hyphens (sz + 2)
        Nothing -> ""
      )
    , "+\n"
    ]

  rowContents = foldMap
    (\x -> mconcat
      [ E.rowMonoidalHeader
        sizedCol
        (\(E.Sized msz _) c -> case msz of
          Nothing -> ""
          Just sz -> "| " +| rightPad sz ' ' c |+ " "
        )
        x
      , "|\n"
      ]
    )
    xs

hyphens :: Int -> Text
hyphens n = T.replicate n "-"

rightPad :: Int -> Char -> Text -> Text
rightPad m a xs = xs <> T.unfoldrN (m - T.length xs) (const $ Just (a, ())) ()

