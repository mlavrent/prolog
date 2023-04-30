module Interpret.Interpreter where

import Control.Monad.State.Lazy (State)

class Interpretable p where
  interpret :: State s p
