module Main where
import Types
import Parse
import Eval
import Math
import Control.Monad.Except

loopM f = f >> loopM f 
main = loopM $ putStrLn . evalStmt =<< getLine

evalStmt input = case eval =<< readExpr input of
    Right ok -> show ok
    Left err -> show err

