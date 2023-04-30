{-# LANGUAGE MultiParamTypeClasses #-}
module Parse.Parser where

class Parsable toks ast where
  parse :: toks -> ast
