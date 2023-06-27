module Main (main) where

import Untyped.Parser
import Untyped.Interpreter
import CLI

main :: IO ()
main = buildMain parse interpret
