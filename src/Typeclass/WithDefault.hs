module Typeclass.WithDefault
  ( WithDefault(..)
  , def
  ) where

class WithDefault a where
  withDefault :: a

def :: WithDefault a => Maybe a -> a
def (Just x) = x
def Nothing  = withDefault
