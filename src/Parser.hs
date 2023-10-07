module Parser (tensorFromString) where

import AutoDiff
import Text.Parsec
import Prelude hiding (exponent)

type Parser a = Parsec String () a

tensorFromString :: String -> Either ParseError Tensor
tensorFromString input = parse expr "f =" input

expr :: Parser Tensor
expr = try sumExpr <|> term <?> "expr"

sumOp :: Parser BinOp
sumOp = (string "+" >> return Add) <|> (string "-" >> return Sub)

sumExpr :: Parser Tensor
sumExpr = do
            tl <- term;
            spaces;
            op <- sumOp;
            spaces;
            tr <- expr;
            return (BinExpr op tl tr)

term :: Parser Tensor
term = try productExpr <|> factor <?> "term"

productOp :: Parser BinOp
productOp = (string "*" >> return Mult) <|> (string "/" >> return Div)

productExpr :: Parser Tensor
productExpr = do
                tl <- factor;
                spaces;
                op <- productOp;
                spaces;
                tr <- term;
                return (BinExpr op tl tr)

factor :: Parser Tensor
factor = do
          tl <- base;
          mt <- optionMaybe (spaces >> string "^" >> spaces >> base);
          case mt of
            Just tr -> return (BinExpr Pow tl tr)
            _       -> return tl

base :: Parser Tensor
base = spaces >> (try nestedExpr <|> try call <|> try var <|> real <?> "base")

nestedExpr :: Parser Tensor
nestedExpr = do
               _ <- string "(";
               spaces;
               e <- expr;
               spaces;
               _ <- string ")";
               spaces;
               return e

call :: Parser Tensor
call = do
        fn <- prefix;
        spaces;
        e  <- base;
        return (UnaryExpr fn e)

prefix :: Parser UnaryOp
prefix = (string "-" >> return Negate) <|>
         (string "log" >> return Log) <|>
         (string "exp" >> return Exp) <|>
         (string "sin" >> return Sin) <|>
         (string "cos" >> return Cos) <|>
         (string "tan" >> return Tan)

var :: Parser Tensor
var = do
        name <- many letter;
        _    <- string "@";
        val  <- try negatableValue;
        return (Var name val)

-- hack: variables should only support fully-resolved scalar values
negatableValue :: Parser Tensor
negatableValue = do
            fn <- maybeNegated;
            spaces;
            e  <- real;
            case fn of
              Just op -> return (UnaryExpr op e)
              _       -> return e

maybeNegated :: Parser (Maybe UnaryOp)
maybeNegated = optionMaybe (string "-" >> return Negate)

-- parse floating point scalars; negation handled by caller
real :: Parser Tensor
real = characteristic <> mantissa  <> exponent >>= \s -> spaces >> return (Const (read s))

digits :: Parser String
digits = many digit

characteristic :: Parser String
characteristic = digits

mantissa :: Parser String
mantissa = option "" (string "." <> digits)

exponent :: Parser String
exponent = option "" (string "e" <> sign <> digits)

sign :: Parser String
sign = option "" (string "+" <|> string "-")
