{- Emits a list of tokens -}
import Data.Char
module ( tokenize)

data Token =  FVal Float | IVal Integer | ADD | SUB | DIV | MUL deriving (Show)
tokenize :: String -> [Token]
tokenize s = map tokenizeWord $ words s
  where
    tokenizeWord :: String -> Token
    tokenizeWord "+"    = ADD
    tokenizeWord "-"    = SUB
    tokenizeWord "/"    = DIV
    tokenizeWord "*"    = MUL
    tokenizeWord s
      | '.' `elem` s    = FVal $ read s 
      | otherwise       = IVal $ read s
