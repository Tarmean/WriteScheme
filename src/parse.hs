module Parse (readExpr) where
import Types
import Text.Parsec
import Text.Parsec.String
import Data.Char(digitToInt)
import Data.List (foldl')
import Data.Complex
import Data.Array

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse (parseExpr<*eof) "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
      char '"'
      content <- many (escapedCharacter <|> normalChar)
      char '"'
      return $ String content
  where 
    normalChar = noneOf "\""
    escapedCharacter = do char '\\'
                          c <- anyChar
                          case lookup c escapeMappings of
                            Just r -> return r
                            _ -> fail $ "Illegal escaped character: \\" ++ [c]
    escapeMappings = zip "nrt\"\\" "\n\r\t\"\\"


parseAtom :: Parser LispVal
parseAtom = do 
      first <- letter <|> symbol
      rest <- many (letter <|> digit <|> symbol)
      return $ Atom (first:rest)
parseBool :: Parser LispVal
parseBool =  pTrue <|> pFalse
  where pTrue = try (string "#t") >> return (Bool True)
        pFalse = try (string "#f") >> return (Bool False)
parseChar :: Parser LispVal
parseChar = try (string "#\\") >> (try (many1 letter)<|>fmap (:[])anyChar) >>= (fmap Character) . lookupChar
  where
    lookupChar "" = return ' '
    lookupChar "space" = return ' '
    lookupChar "newline" = return '\n'
    lookupChar [c] = return c
    lookupChar other = fail $ "Illegal Character: \\#" ++ other

parseNumber :: Parser LispVal
parseNumber = Number <$> (try parseComplex <|> try parseFloat <|> try parseRational <|> parseInteger)

pSign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id

parseInteger :: Parser Number
parseInteger = Integer <$> pNatural
 
parseComplex :: Parser Number
parseComplex = do
  real <- pNum <*spaces <* char '+' <* spaces
  imaginary <- pNum
  char 'i'
  return $ Complex $ real :+ imaginary
  where pNum = (realToFrac <$> try pFloat) <|> (fromIntegral <$> try (pInt 10 digit))
        

parseFloat = Real <$> pFloat
pFloat = do
  leading <- pInt 10 digit
  parseTrailing leading
parseTrailing leading = do
  trailing <- parseLower
  exp <- option 0 parseExp
  return $ (realToFrac leading + trailing) * realToFrac (10 ^^ exp)
parseExp = oneOf "eE" >> pInt 10 digit
parseLower = do
 char '.'
 digits <- many1 digit
 return $ foldr step 0 digits
 where step cur acc = (acc + realToFrac (digitToInt cur)) / 10
  
parseRational :: Parser Number
parseRational = do
  sign <- pSign
  x1 <- pInt 10 digit
  char '/'
  x2 <- pNat 10 digit
  return . Rational $ toRational x1/toRational x2

pNatural :: Parser Integer
pNatural = pOct <|> pDec <|> pHex
  where
    pOct = try (string "#o") >> pInt 8 octDigit
    pDec = try (optional $ string "#d") >> pInt 10 digit
    pHex = try (string "#x") >> pInt 16 hexDigit
pInt :: Integer -> Parser Char -> Parser Integer
pInt i c = pSign <*> pNat i c
pNat :: Integer -> Parser Char -> Parser Integer
pNat base validDigits = readInt base <$> many1 validDigits 
readInt :: Integer -> String -> Integer
readInt base = foldl' step 0
  where step acc cur = acc * base + toInteger (digitToInt cur)

parseList :: Parser LispVal
parseList = between (char '(' >> spaces) (char ')') $ do
  start <- sepEndBy parseExpr spaces
  trailing <- optionMaybe $ char '.' >> spaces >> parseExpr
  case trailing of
    Just t -> return $ DottedList start t
    Nothing -> return $ List start
parseArray :: Parser LispVal
parseArray = between (try $ string "#(" >> spaces) (spaces >> char ')') $ do
  exprs <- sepEndBy parseExpr spaces
  return $ Array (listArray (0, length exprs - 1) exprs)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuote = do quoted <- char '`' >> parseExpr
                     return $ List [Atom "quasiquote", quoted]
parseUnquote = do quoted <- char ',' >> parseExpr
                  return $ List [Atom "unquote", quoted]

parseExpr :: Parser LispVal
parseExpr = (parseAtom
         <|> parseChar
         <|> parseArray
         <|> parseBool
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseUnquote
         <|> parseQuasiQuote
         <|> parseList) <* spaces
