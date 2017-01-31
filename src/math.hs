{-# Language Rank2Types #-}
module Math (mathPrimitives, foldm1) where
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

liftFrac :: (forall a . Fractional a => a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftFrac f a b = case typeof a `max` typeof b `max` RationalType of
  ComplexType  -> return . Number . Complex $ toComplex a `f` toComplex b
  RealType     -> return . Number . Real $ toReal a `f` toReal b
  RationalType -> return . Number . Rational $ toFrac a `f` toFrac b
  NotANumType  -> nanErr a b "Number"
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

foldm1 :: (a -> a -> ThrowsError a) -> [a] -> ThrowsError a
foldm1 f [] = throwError $ NumArgs 1 []
foldm1 f (x:xs) = go (return x) xs
  where go acc (x:xs) = do cur <- acc
                           go (f cur x) xs
        go acc [] = acc

numericBinop :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
numericBinop = foldm1

mathPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
mathPrimitives = [("+", numericBinop $liftNum (+))
                 ,("-", numericBinop $liftNum (-))
                 ,("*", numericBinop $liftNum (*))
                 ,("/", numericBinop $liftFrac (/))
                 ,("mod", numericBinop $ liftIntegral mod)
                 ,("quotient", numericBinop $ liftIntegral quot)
                 ,("remainder", numericBinop $ liftIntegral rem)]
