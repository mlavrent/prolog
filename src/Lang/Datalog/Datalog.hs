{-# LANGUAGE MultiParamTypeClasses #-}
module Lang.Datalog.Datalog where

import Interpret.Interpreter (Interpretable (interpret))
import Interpret.ProgramState (HasInitialState (initialState))
import Parse.Parser (Parsable (parse))
import Lang.Datalog.Tokens (DatalogToken)
import qualified Lang.Datalog.Ast as Ast
import Util.State (mapStateAndVal)
import Control.Arrow ((***))

-- Wrappers around the tokenized and parsed program to carry extra context (e.g. filename) if needed
newtype DatalogToks i v = DatalogToks { tokens :: [DatalogToken i v] }
newtype Datalog i v = Datalog { program :: Ast.Program i v }

instance Parsable (DatalogToks i v) (Datalog i v) where
  parse = undefined -- TODO:


newtype DatalogState i v = DatalogState
  { clauses :: [Ast.Clause i v] }

toDatalogState :: Ast.ProgramState i v -> DatalogState i v
toDatalogState (Ast.ProgramState is) = DatalogState is

toProgramState :: DatalogState i v -> Ast.ProgramState i v
toProgramState (DatalogState is) = Ast.ProgramState is

instance HasInitialState (DatalogState i v) where
  initialState = DatalogState []

-- todo: define how results should look and mapping b/w them
data DatalogResult i v = Result

toDatalogResult :: Ast.ProgramResult i v -> DatalogResult i v
toDatalogResult _ = undefined


instance Interpretable (Datalog i v) (DatalogState i v) (DatalogResult i v) where
  interpret (Datalog p) = mapStateAndVal (toDatalogResult *** toDatalogState) toProgramState (interpret p)
