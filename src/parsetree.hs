module TLParseTree ( parse ) where
import TLTokenize


{- An expression can be a constant or a Binary Expression -}

data Expr a = IConst Integer | FConst Float | BinExpr Token Expr Expr 

tlParse :: TokenStr -> Expr

