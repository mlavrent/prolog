{-# LANGUAGE MultiParamTypeClasses #-}
module Lang.Datalog.Datalog where

import Interpret.Interpreter (Program)
import Parse.Parser (Parsable, parse)
import Lang.Datalog.Tokens (DatalogToken)
import qualified Lang.Datalog.Ast as Ast

-- Wrappers around the tokenized and parsed program to carry extra context (e.g. filename) if needed
data DatalogToks i v = DatalogToks { tokens :: [DatalogToken i v] }
data DatalogProgram i v = DatalogProgram { program :: Ast.Program i v }

instance Parsable (DatalogToks i v) (DatalogProgram i v) where
  parse = undefined -- TODO:

-- instance Program (DatalogProgram i v)
