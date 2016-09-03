{- Emits a list of tokens -}
module TLTokenize ( tokenize ) where

import Data.Char

type TokenStr = [Token]
data Token = FVal Float | IVal Integer 
           | ADD | SUB | DIV | MUL | POW
           | PLEFT | PRIGHT 
           deriving (Show)

tokenize :: String -> TokenStr
tokenize s = map tokenizeWord $ words s
  where
    tokenizeWord :: String -> Token
    tokenizeWord "+"    = ADD
    tokenizeWord "-"    = SUB
    tokenizeWord "/"    = DIV
    tokenizeWord "*"    = MUL
    tokenizeWord "("    = PLEFT
    tokenizeWord ")"    = PRIGHT
    tokenizeWord "^"   = POW
    tokenizeWord s
      | '.' `elem` s    = FVal $ read s 
      | otherwise       = IVal $ read s
