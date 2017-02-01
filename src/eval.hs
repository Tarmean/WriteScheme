module Eval (eval) where
import Types
import Math
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
     do result <- eval pred
        case result of
             Bool False -> eval alt
             otherwise  -> eval conseq
eval (List (Atom func : args)) = apply func =<< traverse eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives = mathPrimitives ++ typePrimitives ++ boolPrimitives ++ listPrimitives

typePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
typePrimitives = [("symbol?", testAll isSymbol)
                 ,("list?", testAll isList)
                 ,("string?", testAll isString)
                 ,("number?", testAll isNumber)
                 ,("char?", testAll isChar)
                 ,("bool?", testAll isBool)
                 ,("eqv?", eqv)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv ls@((Number _):(Number _):_)           = numEq ls
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrimitives = [("car", car), ("cdr", cdr), ("cons", cons)]
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

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

-- foldm1 :: (a -> a -> ThrowsError a) -> [a] -> ThrowsError a
-- foldm1 f (x:y:xs) = go (f x y) xs
--   where go acc (x:xs) = do cur <- acc
--                            go (f cur x) xs
--         go acc [] = acc
-- foldm1 f _= throwError $ NumArgs 2 []

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

boolPrimitives = [("&&", foldm1 $ boolOp (&&)), ("||", foldm1 $ boolOp (||))]
boolOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
boolOp f (Bool a) (Bool b) = return $ Bool (a `f` b)
boolOp f (Bool _) r = throwError $ TypeMismatch "Bool" r
boolOp f l _ = throwError $ TypeMismatch "Bool" l

