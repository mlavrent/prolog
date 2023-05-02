{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Lang.Datalog.Ast where

import Interpret.Interpreter (Interpretable (interpret))
import Control.Monad.State (modify, MonadState (get))
import Util.Maybe (mcons)

-- The AST below uses the syntax for Datalog as described here:
-- https://docs.racket-lang.org/datalog/datalog.html
newtype Program i v = Program [Statement i v]

data Statement i v
  = Assertion (Clause i v)
  | Retraction (Clause i v)
  | Query (Literal i v)

data Clause i v
  = Rule (Literal i v) [Literal i v]
  | Fact (Literal i v)

data Literal i v
  = PredicateApp (Id i) [Term i v]
  | BinaryPredicateApp BinaryPredicate (Term i v) (Term i v)

data BinaryPredicate = EqPred | NeqPred

data Term i v
  = Variable v
  | Constant (Value i)

data Value i
  = IdValue (Id i)
  | AtomConst String
  | StringConst String
  | IntConst Integer
  | BoolConst Bool

newtype Id i = Id i

-- Define how to interpret the sub-trees of the Ast
newtype ProgramState i v = ProgramState
  { clauses :: [Clause i v] }

-- the list here is kept in reverse to how it was generated and should be printed
type ProgramResult i v = [QueryResult i v]
data QueryResult i v = Result -- todo: flesh out how this should look

instance Interpretable (Program i v) (ProgramState i v) [QueryResult i v] where
  interpret (Program []) = return []
  interpret (Program (s:ss)) = do
    sResult <- interpret s
    ssResult <- interpret (Program ss)
    return (mcons sResult ssResult)

instance Interpretable (Statement i v) (ProgramState i v) (Maybe (QueryResult i v)) where
  interpret (Assertion c) = do
    _ <- modify (addAssertion c)
    return Nothing
  interpret (Retraction c) = do
    _ <- modify (retractAssertion c)
    return Nothing
  interpret (Query l) = do
    pState <- get
    return . Just $ evalQuery pState l

addAssertion :: Clause i v -> ProgramState i v -> ProgramState i v
addAssertion = undefined

retractAssertion :: Clause i v -> ProgramState i v -> ProgramState i v
retractAssertion = undefined

-- We implement a bottom-up evaluation (i.e. forward reasoning) approach here
evalQuery :: ProgramState i v -> Literal i v -> QueryResult i v
evalQuery = undefined -- todo: define query evaluation
