module Lang.Datalog.Tokens where

import Parse.Tokenizer (Token, tokenize)

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

instance Token (DatalogToken i v) where
  tokenize = undefined -- TODO:
