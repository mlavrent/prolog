module Interpret.ProgramState where

class HasInitialState s where
  initialState :: s
