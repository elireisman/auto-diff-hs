module Parser (tensorFromString) where

import Text.Parsec
import AutoDiff

-- TODO: parse
tensorFromString :: String -> Tensor
tensorFromString _ = Const 1

