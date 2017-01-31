{-# Language Rank2Types #-}
{-# Language ExistentialQuantification #-}
module Math (mathPrimitives, foldm1, getWrapper) where
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
typeof _ = NotANumType

data Box = forall a . Fractional a => Box ((LispVal -> a), a -> LispVal)
getWrapper :: forall a .Num a => NumberType -> Box
getWrapper ComplexType = Box (toComplex, Number . Complex)
getWrapper RealType = Box (toReal, Number . Real)
getWrapper RationalType = Box (toReal, Number . Real)
-- getWrapper IntType = Box (toInt, Number . Integer)
liftOrd :: (forall a . Ord a => a -> a -> Bool) -> LispVal -> LispVal -> ThrowsError LispVal
liftOrd f a b = case typeof a `max` typeof b of
  ComplexType  -> nanErr a b "Comparable Number" -- return . Bool $ toComplex a `f` toComplex b
  RealType     -> return . Bool $ toReal a `f` toReal b
  RationalType -> return . Bool $ toFrac a `f` toFrac b
  IntType      -> return . Bool $ toInt a `f` toInt b
  NotANumType  -> nanErr a b "Number"
liftFrac :: (forall a . Fractional a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftFrac op a b= 
  case high of
   Box (from, to) -> return . to $ from a `op` from b
  -- | high == NotANumType = nanErr a b "Number"
  -- | otherwise = 
  where 
    high = getWrapper $ typeof a `max` typeof b `max` RationalType
    -- (From from, To to) = high
liftNum :: (forall a . Num a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftNum f a b = case typeof a `max` typeof b of
  ComplexType  -> return . Number . Complex $ toComplex a `f` toComplex b
  RealType     -> return . Number . Real $ toReal a `f` toReal b
  RationalType -> return . Number . Rational $ toFrac a `f` toFrac b
  IntType      -> return . Number . Integer $ toInt a `f` toInt b
  NotANumType  -> nanErr a b "Number"
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

-- foldm2 :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
-- foldm2 f (x:y:xs) = go (f x y) xs
--   where go acc (x:xs) = do cur <- acc
--                            go (f cur x) xs
--         go acc [] = acc
-- foldm2 f r= throwError $ NumArgs 2 r

cmpOp :: (forall a . Ord a => a -> a -> Bool) ->  [LispVal] -> ThrowsError LispVal
cmpOp op [x, y] = (liftOrd op) x y
cmpOp _ ls = throwError $ NumArgs 2 ls
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
                 ,("==", cmpOp (==))
                 ,("/=", cmpOp (/=))
                 ,("<=", cmpOp (<=))
                 ,(">=", cmpOp (>=))]
