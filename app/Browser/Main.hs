module Main (main) where

import GHC.JS.Foreign.Callback
import GHC.JS.Prim

import qualified Untyped.Parser as UP
import qualified Untyped.Interpreter as UI
import qualified Typed.Parser as TP
import qualified Typed.Interpreter as TI
import qualified SystemF.Parser as SFP
import qualified SystemF.Interpreter as SFI
import qualified Browser as B


foreign import javascript "((f) => { untyped = f })"
  setUntyped :: Callback (JSVal -> IO JSVal) -> IO ()

foreign import javascript "((f) => { typed = f })"
  setTyped :: Callback (JSVal -> IO JSVal) -> IO ()

foreign import javascript "((f) => { systemF = f })"
  setSystemF :: Callback (JSVal -> IO JSVal) -> IO ()

runUntyped :: String -> String
runUntyped = B.buildAPI UP.parse UI.interpret

runTyped :: String -> String
runTyped = B.buildAPI TP.parse TI.interpret

runSystemF :: String -> String
runSystemF = B.buildAPI SFP.parse SFI.interpret

pack :: (String -> String) -> JSVal -> IO JSVal
pack f = return . toJSString . f . fromJSString

main :: IO ()
main = do
  untyped <- syncCallback1' (pack runUntyped)
  typed <- syncCallback1' (pack runTyped)
  systemF <- syncCallback1' (pack runSystemF)

  setUntyped untyped
  setTyped typed
  setSystemF systemF
