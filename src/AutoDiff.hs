module AutoDiff where

-- operation types
data BinOp = Pow | Mult | Div | Add | Sub deriving (Eq, Show)
data UnaryOp = Negate | Log | Exp | Sin | Cos | Tan deriving (Eq, Show)

-- expression types
data Tensor = Const Double
            | Var String Tensor
            | UnaryExpr UnaryOp Tensor
            | BinExpr BinOp Tensor Tensor
            deriving (Eq, Show)


-- traverse the Tensor tree and evaluate the expression with
-- all variables bound to a scalar value, resulting in a scalar
eval :: Tensor -> Double
eval t = case t of
  Const d            -> d
  Var _ t'           -> eval t'
  UnaryExpr uop t'   -> case uop of
                          Negate -> 0 - (eval t')
                          Log    -> log (eval t')
                          Exp    -> exp (eval t')
                          Sin    -> sin (eval t')
                          Cos    -> cos (eval t')
                          Tan    -> tan (eval t')
  BinExpr bop tl tr  -> case bop of
                          Pow  -> (eval tl) ** (eval tr)
                          Mult -> (eval tl) * (eval tr)
                          Div  -> (eval tl) / (eval tr)
                          Add  -> (eval tl) + (eval tr)
                          Sub  -> (eval tl) - (eval tr)

-- traverse the Tensor tree and differentiate the expression with
-- all variables bound to a scalar value, resulting in a scalar
diff :: Tensor -> String -> Double
diff t wrt = case (t, wrt) of
  ((Const _), _)              -> 0
  ((Var name t'), wrt')       -> if name == wrt' then 1 else diff t' wrt'
  ((UnaryExpr uop t'), wrt')  -> case uop of
                                   Negate -> 0 - (diff t' wrt')
                                   Log    -> (diff t' wrt') * (1 / (eval t'))
                                   Exp    -> (diff t' wrt') * exp (eval t')
                                   Sin    -> (diff t' wrt') * cos (eval t')
                                   Cos    -> (diff t' wrt') * (-(sin (eval t')))
                                   Tan    -> (diff t' wrt') * (1 / cos (eval t'))**2
  ((BinExpr bop tl tr), wrt') -> case bop of
                                   Pow  -> dtr * (diff tl wrt') ** (dtr-1) where dtr = diff tr wrt'
                                   Mult -> ((diff tl wrt') * (eval tr)) + ((eval tl) * (diff tr wrt'))
                                   Div  -> (((diff tl wrt') * (eval tr)) - ((eval tl) * (diff tr wrt'))) / (eval tr)**2
                                   Add  -> (diff tl wrt') + (diff tr wrt')
                                   Sub  -> (diff tl wrt') - (diff tr wrt')
