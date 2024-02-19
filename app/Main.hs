module Main where
import Parser
import Control.Monad.State

type InnerState = StateT Int
type OuterState = StateT String

nestedState :: OuterState (InnerState IO) ()
nestedState = do
  modify (++ " outer")
  lift $ modify (+ 1)

main :: IO ()
main = do
  outer <- runStateT nestedState "initial"
  inner <- runStateT (fst outer) 0
  print inner
  print $ snd outer


