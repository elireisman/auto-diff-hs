module Parser (tensorFromString) where

import AutoDiff
import Text.Parsec
import Prelude hiding (exponent)

type Parser a = Parsec String () a

tensorFromString :: String -> Either ParseError Tensor
tensorFromString input = parse expr "f =" input

expr :: Parser Tensor
expr = (try sumExpr) <|> term <?> "expr"

sumOp :: Parser BinOp
sumOp = (string "+" >> return Add) <|> (string "-" >> return Sub)

sumExpr :: Parser Tensor
sumExpr = do
            tl <- term;
            spaces;
            op <- sumOp;
            spaces;
            tr <- term;
            return (BinExpr op tl tr)

term :: Parser Tensor
term = (try productExpr) <|> factor <?> "term"

productOp :: Parser BinOp
productOp = (string "*" >> return Mult) <|> (string "/" >> return Div)

productExpr :: Parser Tensor
productExpr = do
                tl <- factor;
                spaces;
                op <- productOp;
                spaces;
                tr <- factor;
                return (BinExpr op tl tr)

factor :: Parser Tensor
factor = do
          tl <- base;
          mt <- optionMaybe (spaces >> string "^" >> spaces >> base);
          case mt of
            Just tr -> return (BinExpr Pow tl tr)
            _       -> return tl

base :: Parser Tensor
base = spaces >> (try (string "(" >> spaces >> expr >>= \e -> (spaces >> string ")" >> spaces) >> return e) <|> try call <|> try var <|> real <?> "base")

call :: Parser Tensor
call = do
        fn <- prefix;
        spaces;
        e  <- base;
        return (UnaryExpr fn e)

prefix :: Parser UnaryOp
prefix = do
           prefix <- (string "-" <|> string "log" <|> string "exp");
           case prefix of
             "-"       -> return Negate
             "log"     -> return Log
             "exp"     -> return Exp

var :: Parser Tensor
var = do
        name <- try (many letter);
        string "@";
        val <- try negatableValue <|> real;
        return (Var name val)

-- hack, only needed while we limit vars to simple scalar values
negatableValue :: Parser Tensor
negatableValue = do
            fn <- prefix;
            spaces;
            e  <- real;
            return (UnaryExpr fn e)

real :: Parser Tensor
real = characteristic <> mantissa  <> exponent >>= \s -> spaces >> return (Const (read s))

digits :: Parser String
digits = many (oneOf "0123456789")

characteristic :: Parser String
characteristic = digits

mantissa :: Parser String
mantissa = option "" (string "." <> digits)

exponent :: Parser String
exponent = option "" (string "e" <> sign <> digits)

sign :: Parser String
sign = option "" (string "+" <|> string "-")
