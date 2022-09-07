module Model.User
  ( User(..)
  ) where

newtype User = User
  { username :: String
  }
