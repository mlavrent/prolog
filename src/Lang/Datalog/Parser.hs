{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Lang.Datalog.Parser where

import Parse.Parser (Parsable, parse)
import Lang.Datalog.Tokenizer (DatalogToken)
import Lang.Datalog.Ast (Program)

instance Parsable [DatalogToken i v] (Program i v) where
  parse = undefined

