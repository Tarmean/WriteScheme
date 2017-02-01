{-# Language Rank2Types #-}
module Math (mathPrimitives, foldm1, numEq) where
import Types
import Data.Ratio (numerator, denominator)
import Data.Complex
import Data.List ( foldl1')
import Control.Monad.Except

typeof :: LispVal -> NumberType
typeof (Number (Complex _)) = ComplexType
typeof (Number (Real _)) = RealType
typeof (Number (Rational _)) = RationalType
typeof (Number (Integer _)) = IntType
typeof _ = NaNType

liftEq :: (forall a . Eq a => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
liftEq f a b = case typeof a `max` typeof b of
  ComplexType  -> return . Bool $ toComplex a `f` toComplex b
  RealType     -> return . Bool $ toReal a `f` toReal b
  RationalType -> return . Bool $ toFrac a `f` toFrac b
  IntType      -> return . Bool $ toInt a `f` toInt b
  NaNType  -> nanErr a b "Number"

liftOrd :: (forall a . Ord a => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
liftOrd f a b = case typeof a `max` typeof b of
  ComplexType  -> nanErr a b "Comparable Number"
  RealType     -> return . Bool $ toReal a `f` toReal b
  RationalType -> return . Bool $ toFrac a `f` toFrac b
  IntType      -> return . Bool $ toInt a `f` toInt b
  NaNType  -> nanErr a b "Number"
liftFrac :: (forall a . Fractional a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftFrac f a b = case typeof a `max` typeof b `max` RationalType of
  ComplexType  -> return . Number . Complex $ toComplex a `f` toComplex b
  RealType     -> return . Number . Real $ toReal a `f` toReal b
  RationalType -> return . Number . Rational $ toFrac a `f` toFrac b
  NaNType  -> nanErr a b "Number"
liftNum :: (forall a . Num a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftNum f a b = case typeof a `max` typeof b of
  ComplexType  -> return . Number . Complex $ toComplex a `f` toComplex b
  RealType     -> return . Number . Real $ toReal a `f` toReal b
  RationalType -> return . Number . Rational $ toFrac a `f` toFrac b
  IntType      -> return . Number . Integer $ toInt a `f` toInt b
  NaNType  -> nanErr a b "Number"
nanErr :: LispVal -> LispVal -> String -> ThrowsError LispVal
nanErr (Number _) r s = throwError $ TypeMismatch s r
nanErr r _ s = throwError $ TypeMismatch s r
liftIntegral ::  (Integer->Integer->Integer) -> LispVal -> LispVal -> ThrowsError LispVal
liftIntegral f (Number (Integer a)) (Number (Integer b)) =  return . Number . Integer $ b `f` a
liftIntegral _ l r =  nanErr l r "Integer"
toComplex (Number (Complex n)) = n
toComplex (Number (Real n)) = n:+0
toComplex (Number (Rational n)) = fromRational n:+0
toComplex (Number (Integer n)) = fromInteger n:+0
toReal (Number (Real n)) = n
toReal (Number (Rational n)) = fromRational n
toReal (Number (Integer n)) = fromInteger n
toFrac (Number (Rational n)) = n
toFrac (Number (Integer n)) = fromInteger n
toInt (Number (Integer n)) = n

foldm1 :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
foldm1 f (x:xs) = go (return x) xs
  where go acc (x:xs) = do cur <- acc
                           go (f cur x) xs
        go acc [] = acc
foldm1 f r = throwError $ NumArgs 1 r

boolMonoid :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
boolMonoid op ls@(x:y:xs) = go ls
  where 
    go :: [LispVal] -> ThrowsError LispVal
    go (x:y:xs) = bAnd <$> op x y <*> go (y:xs)
    go _ = return . Bool $ True
    bAnd (Bool a) (Bool b) = Bool (a && b)
boolMonoid _ ls = throwError $ NumArgs 2 ls

cmpOp :: (forall a . Ord a => a -> a -> Bool) ->  [LispVal] -> ThrowsError LispVal
cmpOp op  = boolMonoid $ liftOrd op
numEq ::  [LispVal] -> ThrowsError LispVal
numEq = boolMonoid $ liftEq (==)
numericBinop :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
numericBinop = foldm1

mathPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
mathPrimitives = [("+", numericBinop $liftNum (+))
                 ,("-", numericBinop $liftNum (-))
                 ,("*", numericBinop $liftNum (*))
                 ,("/", numericBinop $liftFrac (/))
                 ,("mod", numericBinop $ liftIntegral mod)
                 ,("quotient", numericBinop $ liftIntegral quot)
                 ,("remainder", numericBinop $ liftIntegral rem)
                 ,("<", cmpOp (<))
                 ,(">", cmpOp (>))
                 ,("==", numEq)
                 ,("/=", cmpOp (/=))
                 ,("<=", cmpOp (<=))
                 ,(">=", cmpOp (>=))]
