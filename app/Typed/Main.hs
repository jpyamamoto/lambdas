module Main (main) where

import Typed.Parser
import Typed.Interpreter
import CLI

main :: IO ()
main = buildMain parse interpret
