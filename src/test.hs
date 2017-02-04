{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE  MultiParamTypeClasses #-}


import Control.Applicative
import Data.Complex
import Data.List

-- Enum for our numbers with first class phantom type
-- The constructors come with a type constraint on a, e.g. TypeEnum a = (forall a . a ~ Integer) Integer | ...
data TypeEnum a where
    Integer  :: TypeEnum Integer
    Rational :: TypeEnum Rational
    Real     :: TypeEnum Double
    Complex  :: TypeEnum (Complex Double)

doMath ::  (forall b . Num b => b -> b -> b) -> Number -> Number -> Number
doMath f (Number w1 a) (Number w2 b)
  | Just Eql <- compareTypes w1 w2 = case matchNum w1 of NumDict -> Number w1 $ f  a          b
  | Just Sub <- castTypes w1 w2    = case matchNum w2 of NumDict -> Number w2 $ f (coerce a)  b
  | Just Sub <- castTypes w2 w1    = case matchNum w1 of NumDict -> Number w1 $ f  a         (coerce b)
class Subtype a b where coerce :: a -> b
instance Num a => Subtype Integer a where coerce = fromInteger
instance Fractional a => Subtype Rational a where coerce = fromRational
instance Subtype Double (Complex Double) where coerce i = i :+ 0
data CoercionDict l r where Sub :: Subtype l r => CoercionDict l r
castTypes :: TypeEnum a -> TypeEnum b -> Maybe (CoercionDict a b)
castTypes Integer Integer = Just Sub
castTypes Integer Rational = Just Sub
castTypes Integer Real = Just Sub
castTypes Integer Complex = Just Sub
castTypes Rational Rational = Just Sub
castTypes Rational Real = Just Sub
castTypes Rational Complex = Just Sub
castTypes Real Complex = Just Sub
castTypes _ _ = Nothing


class Representable a where
  repr :: TypeEnum a
instance Representable Integer where repr = Integer
instance Representable Rational where repr = Rational
instance Representable Double where repr = Real
instance Representable (Complex Double) where repr = Complex
pack :: Representable a => a -> Number
pack = Number repr
data EqualityWitness a b where
  Eql :: EqualityWitness a a
compareTypes :: TypeEnum a -> TypeEnum b -> Maybe (EqualityWitness a b)
compareTypes Integer Integer = Just Eql
compareTypes Rational Rational = Just Eql
compareTypes Real Real = Just Eql
compareTypes Complex Complex = Just Eql
compareTypes _ _ = Nothing
cast :: EqualityWitness a b ->  a -> b
cast Eql a = a
unpack :: Representable a => Number -> Maybe a
unpack (Number tf i) =  flip cast i <$> compareTypes tf repr


-- Existential type so we can handle all numbers together. GADT's are closed so we can retrieve the actual type by
-- pattern matching on the ype enum.
-- Two reasons the actual data is stored here instea of the enum:
--     * Reconstructing a Number from an existential value is easier since we don't need a typeclass that maps Type -> TypeEnum
--     * pattern matching on TypeEnum is less awkward without ignoring the payload all the time
data Number where Number :: forall a . TypeEnum a -> a -> Number

instance Show Number where
    show (Number Integer  i) = "I "++show i
    show (Number Rational i) = "R "++show i
    show (Number Real     i) = "F "++show i
    show (Number Complex  i) = "C "++show i

-- We have to define some arbitrary ordering for our number types to get the directions we cast in
-- Actually defining an Ord instance for Number seems like overkill since this is the only place we
-- would use it, though, so we just ride on the int one. If we were writing an edsl we could use type
-- families to define the mappings and get nice error messages for illegal casts at compile time, though.
ordering :: TypeEnum a -> Int
ordering Integer  = 0 -- lowest
ordering Rational = 1
ordering Real     = 2
ordering Complex  = 3 -- highest
higherPred :: forall dict . (forall a . TypeEnum a -> dict) -> Number -> Number -> dict
higherPred f (Number e1 _) (Number e2 _) = if ordering e1 >= ordering e2 then f e1 else f e2

-- This method promotes any Number to some type a. We only call this to upcast so a partial method is fine,
-- returning an either/maybe or using type families would work otherwise.
to :: TypeEnum a -> Number -> a
to Complex (Number Complex n) = n
to Complex (Number Real n) =  n:+0
to Complex (Number Rational n) = fromRational n:+0
to Complex (Number Integer n) = fromInteger n:+0
to Real (Number Real n) = n
to Real (Number Rational n) = fromRational n
to Real (Number Integer n) = fromInteger n
to Rational (Number Integer  n) = fromInteger n
to Rational (Number Rational n) = n
to Integer (Number Integer n) = n


-- We use a seperate type to capture type class instances so we can share more code between instances.
-- The segment where we actually have existential numbers is surprisingly small.
data NumDict a where NumDict :: forall a . Num a => NumDict a
matchNum :: forall a . TypeEnum a -> NumDict a
matchNum  Integer  = NumDict
matchNum  Rational = NumDict
matchNum  Real     = NumDict
matchNum  Complex  = NumDict
-- liftNum :: (forall a. Num a => a -> a -> a) -> Number -> Number -> Number
-- liftNum f a b = case higherPred matchNum a b of
--   NumDict d -> Number d $ to d a `f` to d b

data FractDict where FractDict :: forall a . Fractional a => TypeEnum a -> FractDict
matchFract :: forall a . TypeEnum a -> FractDict
matchFract Integer  = FractDict Rational
matchFract Rational = FractDict Rational
matchFract Real     = FractDict Real
matchFract Complex  = FractDict Complex
liftFract :: (forall a. Fractional a => a -> a -> a) -> Number -> Number -> Number
liftFract f a b = case higherPred matchFract a b of
     FractDict d -> Number d $ to d a `f` to d b
