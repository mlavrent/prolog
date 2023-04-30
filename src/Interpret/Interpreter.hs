module Interpret.Interpreter where
import Control.Monad.State (State)

class Program p where
  -- interpretting a program is a computation from an init state to a final state and result value
  interpret :: p s o -> State s o
