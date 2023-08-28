module Parser where

import AutoDiff
import Text.Parsec
import Prelude hiding (exponent)

type Parser a = Parsec String () a

tensorFromString :: String -> Either ParseError Tensor
tensorFromString input = parse expr "f = ..." input

expr :: Parser Tensor
expr = sumExpr <|> term <?> "expr fail"

sumExpr :: Parser Tensor
sumExpr = try addExpr <|> try subExpr

addExpr :: Parser Tensor
addExpr = term >>= \tl -> spaces >> string "+" >> spaces >> factor >>= \tr -> return (BinExpr Add tl tr)

subExpr :: Parser Tensor
subExpr = term >>= \tl -> spaces >> string "-" >> spaces >> factor >>= \tr -> return (BinExpr Sub tl tr)

term :: Parser Tensor
term = productExpr <|> factor <?> "term fail"

productExpr :: Parser Tensor
productExpr = try multExpr <|> try divExpr

multExpr :: Parser Tensor
multExpr = factor >>= \tl -> spaces >> string "*" >> spaces >> base >>= \tr -> return (BinExpr Mult tl tr)

divExpr :: Parser Tensor
divExpr = factor >>= \tl -> spaces >> string "/" >> spaces >> base >>= \tr -> return (BinExpr Div tl tr)

factor :: Parser Tensor
factor = do
          tl <- try base;
          mt <- optionMaybe (string "^" >> arg);
          case mt of
            Just tr -> return (BinExpr Pow tl tr)
            _       -> return tl

base :: Parser Tensor
base = (string "(" >> expr >>= \e -> string ")" >> return e) <|> call <|> var <|> value <?> "base fail"

arg :: Parser Tensor
arg = (string "(" >> expr >>= \e -> string ")" >> return e) <|> call <|> var <|> value <?> "exponent fail"

call :: Parser Tensor
call = do
        fn <- try prefix;
        spaces;
        e  <- expr;
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
        val <- negatableValue <?> "var fail";
        return (Var name val)

-- hack, only needed while we limit vars to simple scalar values
negatableValue :: Parser Tensor
negatableValue = (do
        fn <- try prefix;
        spaces;
        e  <- try value;
        return (UnaryExpr fn e))

value :: Parser Tensor
value = characteristic <> mantissa  <> exponent >>= \s -> return (Const (read s))

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

