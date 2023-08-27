module Main where

import AutoDiff

-- shared vars for examples
x = Var "x" (Const 3)
y = Var "y" (Const 5)

numerator   = BinExpr Sub (Const 12) (BinExpr Mult x e2y)
denominator = BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx))
e2y         = UnaryExpr Exp y
e2nx        = UnaryExpr Exp (UnaryExpr Negate x)

-- test examples
example1 = eval z2 where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y

example2 = diff z2 "y" where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y

example3 = diff z "x" where
  z           = BinExpr Div numerator denominator

example4 = diff z "y" where
  z           = BinExpr Div numerator denominator

-- execute the tests
main :: IO ()
main = do
  putStrLn $ "evaluate '(x + y) * y)' at x=3, y=5: " ++ show example1
  putStrLn $ "differentiate '(x + y) * y' with respect to 'y' at x=3, y=5: " ++ show example2
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'x' at x=3, y=5: " ++ show example3
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'y' at x=3, y=5: " ++ show example4
