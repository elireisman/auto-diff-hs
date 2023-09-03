module Main where

import AutoDiff
import Parser

-- evaluate @s@
evalF :: String -> String
evalF s = "f = " ++ s ++
  "\n" ++ "parsed: " ++ (show t) ++
  "\n" ++ "evaluated: " ++ (show $ eval t)
  where
    t = case (tensorFromString s) of
          Left  parseErr     -> TError (show parseErr)
          Right parsedTensor -> parsedTensor

-- differentiate @s@ with respect to var @v@
differentiateF :: String -> String -> String
differentiateF s v = "f = " ++ s ++
  "\n" ++ "parsed: " ++ (show t) ++
  "\n" ++ "differentiated: f'(" ++ v ++ ") = " ++ (show $ diff t v)
  where
    t = case (tensorFromString s) of
          Left  parseErr     -> TError (show parseErr)
          Right parsedTensor -> parsedTensor


-- stub: execute a couple examples
main :: IO ()
main = do
  -- define expression to be evaluated and differentiated at x=3, y=5
  let f = "(12 - (x@3 * exp(y@5))) / (45 + (x@3 * y@5 * exp(-x@3)))"
  putStrLn $ evalF f
  putStrLn $ differentiateF f "x"
  putStrLn $ differentiateF f "y"
