{-# LANGUAGE MultiParamTypeClasses #-}
module Lang.Datalog.Datalog where

import Interpret.Interpreter (Program (interpret))
import Parse.Parser (Parsable (parse))
import Lang.Datalog.Tokens (DatalogToken)
import qualified Lang.Datalog.Ast as Ast
import Interpret.ProgramState (HasInitialState (initialState))
import Lang.Datalog.Ast (Clause)

-- Wrappers around the tokenized and parsed program to carry extra context (e.g. filename) if needed
newtype DatalogToks i v = DatalogToks { tokens :: [DatalogToken i v] }
newtype Datalog i v = Datalog { program :: Ast.Program i v }

instance Parsable (DatalogToks i v) (Datalog i v) where
  parse = undefined -- TODO:


data DatalogState i v = ProgramState
  { importedReqs :: [Ast.Id i]
  , clauses :: [Clause i v] }

instance HasInitialState (DatalogState i v) where
  initialState = ProgramState [] []

instance Program (Datalog i v) (DatalogState i v) String where
  interpret p = undefined
