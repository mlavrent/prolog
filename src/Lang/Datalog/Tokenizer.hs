module Lang.Datalog.Tokenizer where

import Parse.Tokenizer (Token, tokenize)

data DatalogToken i v
  = LeftParen
  | RightParent
  | Period
  | Tilde
  | QuestionMark
  | ColonDash
  | EqSign
  | NeqSign
  | Variable v
  | Identifier i
  | StringTok String
  | IntTok Integer
  | BoolTok Bool

-- TODO: implement tokenization
instance Token (DatalogToken i v) where
  tokenize = undefined
