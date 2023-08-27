module Main (main) where

import Test.HUnit
import AutoDiff

-- define Tensors and functions to test
x = Var "x" (Const 3)
y = Var "y" (Const 5)

simple = BinExpr Mult (BinExpr Add x y) y

e2y         = UnaryExpr Exp y
e2nx        = UnaryExpr Exp (UnaryExpr Negate x)
numerator   = BinExpr Sub (Const 12) (BinExpr Mult x e2y)
denominator = BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx))
complex     = BinExpr Div numerator denominator

-- define test cases
testSimpleEval = TestCase (assertEqual "evaluate (x + y) * y " (eval simple) 40.0)
testSimpleDiffX = TestCase (assertEqual "differentiate (x + y) * y with respect to x " (diff simple "x") 5.0)
testSimpleDiffY = TestCase (assertEqual "differentiate (x + y) * y with respect to y " (diff simple "y") 13.0)

testComplexEval = TestCase (assertEqual "evaluate (12 - x * e^y) / (45 + x * y * e^(-x)) " (eval complex) (-9.470376512538714))
testComplexDiffX = TestCase (assertEqual "differentiate (12 - x * e^y) / (45 + x * y * e^(-x)) with respect to x " (diff complex "x") (-3.347297773010692))
testComplexDiffY = TestCase (assertEqual "differentiate (12 - x * e^y) / (45 + x * y * e^(-x)) with respect to y " (diff complex "y") (-9.70176956641438))

tests = TestList [
  TestLabel "evaluate (x + y) * y " testSimpleEval,
  TestLabel "differentiate (x + y) * y with respect to x" testSimpleDiffX,
  TestLabel "differentiate (x + y) * y with respect to y" testSimpleDiffY,
  TestLabel "evaluate (12 - x * e^y) / (45 + x * y * e^(-x)) " testComplexEval,
  TestLabel "differentiate (12 - x * e^y) / (45 + x * y * e^(-x)) with respect to x " testComplexDiffX,
  TestLabel "differentiate (12 - x * e^y) / (45 + x * y * e^(-x)) with respect to y " testComplexDiffY]

main :: IO Counts
main = do
  runTestTT tests 
