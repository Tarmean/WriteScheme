module Types (LispVal(..), Number(..), NumberType(..), LispError(..), ThrowsError, throwError) where
import Data.Complex
import Data.Array
import Data.Ratio (numerator, denominator)
import Control.Monad.Except
import Text.Parsec(ParseError)
data LispVal = Atom String
             | List [LispVal]
             | Array (Array Int LispVal)
             | DottedList [LispVal] LispVal
             | Number Number
             | String String
             | Character Char
             | Bool Bool

data Number = Integer Integer | Rational Rational | Real Double | Complex (Complex Double)
data NumberType = IntType | RationalType | RealType | ComplexType | NotANumType deriving (Eq, Ord, Show)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwords (show <$> found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
instance Show LispError where show = showError
type ThrowsError = Either LispError
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

instance Show LispVal where
  show (Atom name) = name
  show (List ls) = "(" ++ showL ls ++ ")"
  show (DottedList ls l) = "(" ++ showL ls ++ "." ++ show l ++ ")"
  show (Number (Rational n)) = show (numerator n) ++ "/" ++ show (denominator n)
  show (Number n) = show n
  show (String s) = show s
  show (Character c) = "#\\"++[c]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
instance Show Number where
  show (Integer i) = show i
  show (Rational r) = show r
  show (Real d) = show d
  show (Complex c) = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showL = unwords . fmap show
