module Eval (eval) where
import Types
import Math
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = apply func =<< traverse eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives = mathPrimitives ++ typePrimitives

typePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
typePrimitives = [("symbol?", testAll isSymbol)
                 ,("list?", testAll isList)
                 ,("string?", testAll isString)
                 ,("number?", testAll isNumber)
                 ,("char?", testAll isChar)
                 ,("bool?", testAll isBool)]

testAll :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
testAll f = return . Bool . all f
isSymbol (Atom _) = True
isSymbol _ = False
isList (List _) = True
isList (DottedList _ _) = True
isList _ = False
isNumber (Number _) = True
isNumber _ = False
isString (String _) = True
isString _ = False
isChar (Character _) = True
isChar _ = False
isBool (Bool _) = True
isBool _ = False

foldm1 :: (a -> a -> ThrowsError a) -> [a] -> ThrowsError a
foldm1 f (x:y:xs) = go (f x y) xs
  where go acc (x:xs) = do cur <- acc
                           go (f cur x) xs
        go acc [] = acc
foldm1 f _= throwError $ NumArgs 2 []

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool
