-- supported expression forms; TODO: parse text inputs into these
data BinOp = Pow | Mult | Div | Add | Sub deriving (Eq, Show)

data UnaryOp = Negate | Log deriving (Eq, Show)

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
                         Negate -> negate (resolve t)
                         Log    -> log (resolve t)
  (BinExpr bop tl tr)  -> case bop of
                         Pow  -> (resolve tl) ** (resolve tr)
                         Mult -> (resolve tl) * (resolve tr)
                         Div  -> (resolve tl) / (resolve tr)
                         Add  -> (resolve tl) + (resolve tr)
                         Sub  -> (resolve tl) - (resolve tr)


-- traverse the Tensor tree and resolve the partial derivative of expression's scalar value
diff :: Tensor -> String -> Double
diff t wrt = case (t, wrt) of
  ((Const d), _)            -> 0
  ((Var name t), wrt)        -> if name == wrt then 1 else diff t wrt
  ((UnaryExpr uop t), wrt)   -> case uop of
                                 Negate -> negate (diff t wrt)
                                 Log    -> (1 / (resolve t)) * (diff t wrt)
  ((BinExpr bop tl tr), wrt) -> case bop of
                                 Pow  -> v * (diff tl wrt) ** (v-1) where v = diff tr wrt
                                 Mult -> ((diff tl wrt) * (resolve tr)) + ((diff tr wrt) * (resolve tl))
                                 Div  -> (((diff tl wrt) * (resolve tr)) + ((diff tr wrt) * (resolve tl))) / ((resolve tr) ** 2)
                                 Add  -> (diff tl wrt) + (diff tr wrt)
                                 Sub  -> (diff tl wrt) - (diff tr wrt)

-- test drive it on "(x + y) * y"
example1 = show $ resolve z where
  z = BinExpr Mult (BinExpr Add x y) y
  x = Var "x" (Const 3)
  y = Var "y" (Const 5)

example2 = show $ diff z2 "y" where
  z2 = BinExpr Mult z1 y
  z1 = BinExpr Add x y
  x = Var "x" (Const 3)
  y = Var "y" (Const 5)

main = do 
  putStrLn "test simple calculation, then differentiation, of the function: (x + y) * y"
  putStrLn $ "calculate at x=3, y=5: " ++ example1
  putStrLn $ "differentiate with respect to 'y' at x=3, y=5: " ++ example2

