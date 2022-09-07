{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Model.Config
  ( AppConfig(..)
  ) where
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )

data AppConfig = AppConfig
  { host     :: Text
  , port     :: Int
  , username :: ByteString
  , password :: ByteString
  }