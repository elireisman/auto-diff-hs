-- supported expression forms; TODO: parse text inputs into these
data BinOp = Pow | Mult | Div | Add | Sub

data UnaryOp = Negate | Log

data Tensor = Const Double
            | Var String Double
            | UnaryExpr UnaryOp Tensor
            | Expr BinOp Tensor Tensor


-- supported binary operations
(^) :: Tensor -> Tensor -> Tensor
tl `^` tr = BinExpr Pow tl tr

(+) :: Tensor -> Tensor -> Tensor
tl `+` tr = BinExpr Add tl tr

(-) :: Tensor -> Tensor -> Tensor
tl `-` tr = BinExpr Sub tl tr

(*) :: Tensor -> Tensor -> Tensor
tl `*` tr = BinExpr Mult tl tr

(/) :: Tensor -> Tensor -> Tensor
tl `/` tr = BinExpr Div tl tr


-- supported unary operations
neg :: Tensor -> Tensor
neg t = UnaryExpr Negate t

log :: Tensor -> Tensor
log t = UnaryExpr Log t


-- traverse the Tensor tree and calculate the expression's scalar value
resolve :: Tensor -> Double
resolve t = case t of
  (Const d)         -> d
  (Var _ d)         -> d
  (UnaryExpr uop t) -> case uop of
                         Negate -> negate $ resolve t)
                         Log    -> log $ resolve t)
  (Expr bop tl tr)  -> case bop of
                         Pow  -> (resolve tl) ** (resolve tr)
                         Mult -> (resolve tl) * (resolve tr)
                         Div  -> (resolve tl) / (resolve tr)
                         Add  -> (resolve tl) + (resolve tr)
                         Sub  -> (reoslve tl) - (resolve tr)


-- traverse the Tensor tree and resolve the partial derivative of expression's scalar value
diff :: Tensor -> Double
diff t dv = case (t, dv) of
  ((Const d), _)            -> 0
  ((Var name, d), dv)       -> if name == dv then 1 else d
  ((UnaryExpr uop t), dv)   -> case uop of
                                 Negate -> negate $ diff t dv
                                 Log    -> log $ diff t dv
  ((BinExpr bop tl tr), dv) -> case bop of
                                 Pow  -> v * (diff tl dv) ** (v-1) where v = diff tr dv
                                 Mult -> (diff tl dv) * (diff tr dv)
                                 Div  -> (diff tl dv) / (diff tr dv)
                                 Add  -> (diff tl dv) + (diff tr dv)
                                 Sub  -> (diff tl dv) - (diff tr dv)

