module Main (main) where

import SystemF.Parser
import SystemF.Interpreter
import CLI

main :: IO ()
main = buildMain parse interpret
