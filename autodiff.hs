-- supported expression forms; TODO: parse text inputs into these
data BinOp = Pow | Mult | Div | Add | Sub deriving (Eq, Show)

data UnaryOp = Negate | Log deriving (Eq, Show)

data Tensor = Const Double
            | Var String Double
            | UnaryExpr UnaryOp Tensor
            | BinExpr BinOp Tensor Tensor
            deriving (Show)


-- traverse the Tensor tree and calculate the expression's scalar value
resolve :: Tensor -> Double
resolve t = case t of
  (Const d)         -> d
  (Var _ d)         -> d
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
diff t dv = case (t, dv) of
  ((Const d), _)            -> 0
  ((Var name d), dv)        -> if name == dv then 1 else d
  ((UnaryExpr uop t), dv)   -> case uop of
                                 Negate -> negate (diff t dv)
                                 Log    -> log (diff t dv)
  ((BinExpr bop tl tr), dv) -> case bop of
                                 Pow  -> v * (diff tl dv) ** (v-1) where v = diff tr dv
                                 Mult -> ((diff tl dv) * (resolve tr)) + ((diff tr dv) * (resolve tl))
                                 Div  -> (((diff tl dv) * (resolve tr)) + ((diff tr dv) * (resolve tl))) / ((resolve tr) ** 2)
                                 Add  -> (diff tl dv) + (diff tr dv)
                                 Sub  -> (diff tl dv) - (diff tr dv)

-- test drive it on: dx/dy of "x + (2*y)"
example = show $ diff z "x" where
  z = BinExpr Add x (BinExpr Mult (Const 2) y)
  x = Var "x" 3
  y = Var "y" 5

main = putStrLn example

