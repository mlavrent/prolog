module Parse.Tokenizer where

class Token t where
  -- TODO: we probably want to provide default implementation of tokenize, but require something lower-level to be defined
  tokenize :: String -> [t]
