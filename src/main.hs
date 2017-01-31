module Main where
import Types
import Parse
import Eval
import Math
import Control.Monad.Except

main = forever $ putStrLn . evalStmt =<< getLine

evalStmt input = case eval =<< readExpr input of
    Right ok -> show ok
    Left err -> show err

