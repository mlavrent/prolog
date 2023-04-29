module Main (main) where

import Lang.Datalog.Ast ()
import Lang.Datalog.Parser ()
import Lang.Datalog.Tokenizer ()
import Lang.Datalog.Interpreter ()

main :: IO ()
main = do
  putStrLn "hello world"
