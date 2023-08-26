-- supported expression forms; TODO: parse text inputs into these
data BinOp = Pow | Mult | Div | Add | Sub deriving (Eq, Show)

data UnaryOp = Negate | Log | Exp deriving (Eq, Show)

data Tensor = Const Double
            | Var String Tensor
            | UnaryExpr UnaryOp Tensor
            | BinExpr BinOp Tensor Tensor
            deriving (Show)


-- traverse the Tensor tree and calculate the expression's scalar value
resolve :: Tensor -> Double
resolve t = case t of
  (Const d)         -> d
  (Var _ t)         -> resolve t
  (UnaryExpr uop t) -> case uop of
                         Negate -> resolve (BinExpr Sub (Const 0) t)
                         Log    -> log (resolve t)
                         Exp    -> exp (resolve t)
  (BinExpr bop tl tr)  -> case bop of
                         Pow  -> (resolve tl) ** (resolve tr)
                         Mult -> (resolve tl) * (resolve tr)
                         Div  -> (resolve tl) / (resolve tr)
                         Add  -> (resolve tl) + (resolve tr)
                         Sub  -> (resolve tl) - (resolve tr)


-- traverse the Tensor tree and resolve the partial derivative of expression's scalar value
diff :: Tensor -> String -> Double
diff t wrt = case (t, wrt) of
  ((Const d), _)             -> 0
  ((Var name t), wrt)        -> if name == wrt then 1 else diff t wrt
  ((UnaryExpr uop t), wrt)   -> case uop of
                                 Negate ->  diff (BinExpr Sub (Const 0) t) wrt
                                 Log    -> (1 / (resolve t)) * (diff t wrt)
                                 Exp    -> exp (resolve t)
  ((BinExpr bop tl tr), wrt) -> case bop of
                                 Pow  -> v * (diff tl wrt) ** (v-1) where v = diff tr wrt
                                 Mult -> ((diff tl wrt) * (resolve tr)) + ((diff tr wrt) * (resolve tl))
                                 Div  -> (((diff tl wrt) * (resolve tr)) - ((diff tr wrt) * (resolve tl))) / ((resolve tr) ** 2)
                                 Add  -> (diff tl wrt) + (diff tr wrt)
                                 Sub  -> (diff tl wrt) - (diff tr wrt)

-- test drive it
example1 = resolve z where
  z = BinExpr Mult (BinExpr Add x y) y
  x = Var "x" (Const 3)
  y = Var "y" (Const 5)

example2 = diff z2 "y" where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y
  x  = Var "x" (Const 3)
  y  = Var "y" (Const 5)

example3 = diff z "x" where
  z    = BinExpr Div (BinExpr Sub (Const 12) (BinExpr Mult x e2y)) (BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx)))
  e2y  = UnaryExpr Exp y
  e2nx = UnaryExpr Exp (UnaryExpr Negate x)
  x    = Var "x" (Const 3)
  y    = Var "y" (Const 5)

example4 = diff z "y" where
  z    = BinExpr Div (BinExpr Sub (Const 12) (BinExpr Mult x e2y)) (BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx)))
  e2y  = UnaryExpr Exp y
  e2nx = UnaryExpr Exp (UnaryExpr Negate x)
  x    = Var "x" (Const 3)
  y    = Var "y" (Const 5)

-- execute the tests
main = do 
  putStrLn $ "calculate '(x + y) * y)' at x=3, y=5: " ++ show example1
  putStrLn $ "differentiate '(x + y) * y' with respect to 'y' at x=3, y=5: " ++ show example2
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'x' at x=3, y=5: " ++ show example3
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'y' at x=3, y=5: " ++ show example4
