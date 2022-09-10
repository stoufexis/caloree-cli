module Main (main) where

import Lib
import Parse.ParseCommand (parseCommand)

main :: IO ()
main = parseCommand
