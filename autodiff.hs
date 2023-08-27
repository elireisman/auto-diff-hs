-- supported expression forms; TODO: parse text inputs into these
data BinOp = Pow | Mult | Div | Add | Sub deriving (Eq, Show)

data UnaryOp = Negate | Log | Exp deriving (Eq, Show)

data Tensor = Const Double
            | Var String Tensor
            | UnaryExpr UnaryOp Tensor
            | BinExpr BinOp Tensor Tensor
            deriving (Show)


-- traverse the Tensor tree and calculate the expression's scalar value
eval :: Tensor -> Double
eval t = case t of
  (Const d)         -> d
  (Var _ t)         -> eval t
  (UnaryExpr uop t) -> case uop of
                         Negate -> 0 - (eval t)
                         Log    -> log (eval t)
                         Exp    -> exp (eval t)
  (BinExpr bop tl tr)  -> case bop of
                         Pow  -> (eval tl) ** (eval tr)
                         Mult -> (eval tl) * (eval tr)
                         Div  -> (eval tl) / (eval tr)
                         Add  -> (eval tl) + (eval tr)
                         Sub  -> (eval tl) - (eval tr)


-- traverse the Tensor tree and eval the partial derivative of expression's scalar value
diff :: Tensor -> String -> Double
diff t wrt = case (t, wrt) of
  ((Const d), _)             -> 0
  ((Var name t), wrt)        -> if name == wrt then 1 else diff t wrt
  ((UnaryExpr uop t), wrt)   -> case uop of
                                  Negate -> 0 - (diff t wrt)
                                  Log    -> (1 / (eval t)) * (diff t wrt)
                                  Exp    -> (diff t wrt) * exp (eval t)
  ((BinExpr bop tl tr), wrt) -> case bop of
                                  Pow  -> dtr * (diff tl wrt) ** (dtr-1) where dtr = diff tr wrt
                                  Mult -> ((diff tl wrt) * (eval tr)) + ((eval tl) * (diff tr wrt))
                                  Div  -> (((diff tl wrt) * (eval tr)) - ((eval tl) * (diff tr wrt))) / ((eval tr) * (eval tr))
                                  Add  -> (diff tl wrt) + (diff tr wrt)
                                  Sub  -> (diff tl wrt) - (diff tr wrt)

-- test drive it
x = Var "x" (Const 3)
y = Var "y" (Const 5)

numerator   = BinExpr Sub (Const 12) (BinExpr Mult x e2y)
denominator = BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx))
e2y         = UnaryExpr Exp y
e2nx        = UnaryExpr Exp (UnaryExpr Negate x)

example1 = eval z2 where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y

example2 = diff z2 "y" where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y

example3 = diff z "x" where
  z           = BinExpr Div numerator denominator
  numerator   = BinExpr Sub (Const 12) (BinExpr Mult x e2y)
  denominator = BinExpr Add (Const 45) (BinExpr Mult x (BinExpr Mult y e2nx))
  e2y         = UnaryExpr Exp y
  e2nx        = UnaryExpr Exp (UnaryExpr Negate x)

example4 = diff z "y" where
  z           = BinExpr Div numerator denominator

-- execute the tests
main = do
  putStrLn $ "evaluate '(x + y) * y)' at x=3, y=5: " ++ show example1
  putStrLn $ "differentiate '(x + y) * y' with respect to 'y' at x=3, y=5: " ++ show example2
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'x' at x=3, y=5: " ++ show example3
  putStrLn $ "differentiate '(12 - (x * e^y)) / (45 + (x * y * e^(-x)))' with respect to 'y' at x=3, y=5: " ++ show example4
