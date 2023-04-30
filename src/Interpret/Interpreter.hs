module Interpret.Interpreter where

import Control.Monad.State (State, runState)
import Interpret.ProgramState (HasInitialState, initialState)

class Program p s r where
  -- interpretting a program is a computation from an init state to a final state and result value
  interpret :: p -> State s r

  -- if the program state has a defined initial state, we can run a program using that as the initial state
  interpretInit :: (HasInitialState s) => p -> (r, s)
  interpretInit program = runState (interpret program) initialState
