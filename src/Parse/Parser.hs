{-# LANGUAGE MultiParamTypeClasses #-}
module Parse.Parser where

class Parsable tok ast where
  parse :: [tok] -> ast
