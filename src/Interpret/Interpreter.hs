module Interpret.Interpreter where

class Interpretable p where
  interpret :: State p
