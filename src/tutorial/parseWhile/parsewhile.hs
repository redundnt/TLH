{- A tutorial building a toy language taken from 
 - https://wiki.haskell.org/Parsing_a_simple_imperative_language 
 -}
module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

{- 2: The Language
 - The grammar for expressions is defined as follows:
a   ::= x | n | -a | a opa a 
b   ::= true | false | not b | b opb b | a opr a
opa ::= + | - | * | /
opb ::= and | or
opr ::= > | <

S   ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S
-}

{- 3: Data Structures
 - We need to take care of boolean and arithmetic expressions and the
 - appropriate operators. First let's look at the boolean expressions -}

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

-- Binary Boolean Operators
data BBinOp = And | Or deriving (Show)

-- Relational Operators
data RBinOp = Greater | Less deriving (Show)

-- Now we define the types for arithmetic expressions
data AExpr = Var String
           | Add
           | Subtract
           | Multiply
           | Divide
           deriving (Show)

-- Finally, take care of statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)

{- 4: Lexer
 - Having all the data structures we can go on with writing the code to do
 - actual parsing. First of all, we create the language definition using
 - Haskell's record syntax and the constructor emptyDef (from
 - Text.ParserCombinators.Parsec.Language): -}

languageDef =
    emptyDef { Token.commentStart       = "/*"
             , Token.commentEnd         = "*/"
             , Token.commentLine        = "//"
             , Token.identStart         = letter
             , Token.identLetter        = alphaNum
             , Token.reservedNames      = [ "if"
                                          , "then"
                                          , "else"
                                          , "while"
                                          , "do"
                                          , "skip"
                                          , "true"
                                          , "false"
                                          , "not"
                                          , "and"
                                          , "or"
                                          ]
             , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                       , "<", ">", "and", "or", "not"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier  = Token.identifier  lexer
reserved    = Token.reserved    lexer
reservedOp  = Token.reservedOp  lexer
parens      = Token.parens      lexer

integer     = Token.integer     lexer
semi        = Token.semi        lexer
whiteSpace  = Token.whiteSpace  lexer

{- 5: Main parser -}

statement :: Parser Stmt
statement = parens statement
          <|> sequenceOfStmt
sequenceOfStmt = 
    do list <- (sepBy1 statement' semi)
    -- If there's only one statement ,return it without using Seq
    return $ if length list == 1 then head list else Seq list
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

ifStmt :: Parser Stmt
ifStmt = 
    do  reserved "if"
        cond <- bExpression
        reserved "then"
        stmt1 <- statement
        reserved "else"
        stmt2 <- statement
        return # If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = 
    do reserved "while"
       cond <- bExpression
       reserved "do"
       stmt <- statement
       return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
    do var <- identifier
       reservedOp ":="
       expr <- aExpression
       return $ Assign var expr

skipStmt :: ParserStmt
skipStmt = reserved "skip" >> return Skip

{- 6: Expressions -}
-- What's left is to parse expressions. Parsec provides a very easy way to
-- do that. Let's define the arithmetic and boolean expressions
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

-- Now we have to define the lists with operator precedence, associativity
-- and what constructors to use in each case.
aOperators = [ [Prefix (reservedOp "-" >> return (Neg             ))          ]
             , [Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
             ]
bOperators = [ [Prefix (reservedOp "not" >> return (Not         ))            ]
             , [Infix  (reservedOp "and" >> return (BBinary And ))   AssocLeft,
                Infix  (reservedop "or"  >> return (BBinary Or  ))   AssocLeft]
             ]

-- Finally, the terms
aTerm = parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm = parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
    do a1 <- aExpression
       op <- relation
       a2 <- aExpression
       return $ RBinary op a1 a2

relation =  (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less   )

{- 7: Notes -}
-- These functions are handy for experimenting in ghci

parseString ::String -> Stmt
parseString str = 
    case parse whileParser "" str of
        Left e -> error $ show e
        Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
    do program <- readFile file
        case parse whileParser "" program of
            Left e -> print e >> fail "parse error"
            Right r -> return r
