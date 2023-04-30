module Lang.Datalog.Ast where

-- Define the tokens for tokenizing the program
data DatalogToken i v
  = LeftParen
  | RightParent
  | Period
  | Tilde
  | QuestionMark
  | ColonDash
  | EqSign
  | NeqSign
  | VarTok v
  | IdTok i
  | StringTok String
  | IntTok Integer
  | BoolTok Bool

-- The AST below uses the syntax for Datalog as described here:
-- https://docs.racket-lang.org/datalog/datalog.html
newtype Program i v = Program [Statement i v]

data Statement i v
  = Assertion (Clause i v)
  | Retraction (Clause i v)
  | Query (Literal i v)
  | Requirement (Id i)

data Clause i v
  = Rule (Literal i v) [Literal i v]
  | Fact (Literal i v)

data Literal i v
  = PredicateApp (Id i) [Term i v]
  | ExternalPredicateApp (Id i) [Term i v]
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
