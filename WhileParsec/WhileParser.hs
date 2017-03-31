module ParseWhile where
import Prelude hiding (Num)
import qualified Prelude (Num)

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Num = Integer
type Var = String

type Z = Integer
type T = Bool
type Pname = String
type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]



data Stm = Seq [Stm]
         | Skip
         | Ass Var Aexp 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
        deriving (Show)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | BBinary BBinOp Bexp Bexp
          | RBinary RBinOp Aexp Aexp
        deriving (Show)

data BBinOp = And
        deriving (Show)

data RBinOp = Eq
            | Le
        deriving (Show)

data Aexp = N Num 
          | V Var 
          | ABinary ABinOp Aexp Aexp
        deriving (Show)

data ABinOp = Mult
            | Add
            | Sub
        deriving (Show)

languageDef =
  emptyDef {   Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            ,Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "!"
                                     , ";"
                                     , "&"
                                     , "call"
                                     , "begin"
                                     , "end"
                                     , "var"
                                     , "proc"
                                     , "is"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", ":="
                                     , "<", "&", "!", "=", ";"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stm
whileParser = whiteSpace >> statement

statement :: Parser Stm
statement =   parens statement
         <|>  compStm
         <|>  statement'

compStm :: Parser Stm
compStm = buildExpressionParser compOp statement'

compOp = [[Infix  (reservedOp ";"   >> return (Comp )) AssocLeft]]

statement' :: Parser Stm
statement' =  ifStm
          <|> whileStm
          <|> skipStm
          <|> assignStm
          <|> blockStm
          <|> callStm

ifStm :: Parser Stm
ifStm =
  do reserved "if"
     cond  <- bexp
     reserved "then"
     stm1 <- statement
     reserved "else"
     stm2 <- statement
     return $ If cond stm1 stm2

whileStm :: Parser Stm
whileStm =
  do reserved "while"
     cond <- bexp
     reserved "do"
     stmt <- statement
     return $ While cond stmt
 
assignStm :: Parser Stm
assignStm =
  do var  <- identifier
     reservedOp ":="
     expr <- aexp
     return $ Ass var expr

skipStm :: Parser Stm
skipStm = reserved "skip" >> return Skip

callStm :: Parser Stm
callStm = 
  do reserved "call"
     call <- pname
     return $ Call call

blockStm :: Parser Stm
blockStm =
  do reserved "begin"
     d <- many (decV)
     p <- many (decP)
     s <- statement
     reserved "end"
     return $ Block d p s

decV :: Parser (Var,Aexp)
decV = 
  do reserved "var"
     var <- identifier
     reservedOp ":="
     expr <- aTerm
     reserved ";"
     return $ (var, expr)

decP :: Parser (Pname, Stm)
decP = 
  do reserved "proc" 
     pro <- pname
     reserved "is"
     s <- statement'
     reserved ";"
     return $ (pro, s)

aexp :: Parser Aexp
aexp = buildExpressionParser aOp aTerm

bexp :: Parser Bexp
bexp = buildExpressionParser bOp bTerm

pname :: Parser Pname
pname = identifier

aOp = [        [Infix  (reservedOp "*"   >> return (ABinary Mult)) AssocLeft ]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Sub)) AssocLeft]
              ]

bOp = [ [Prefix (reservedOp "!" >> return (Neg             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft]
             ]

aTerm =  parens aexp
     <|> liftM V identifier
     <|> liftM N integer

bTerm =  parens bexp
     <|> (reserved "true"  >> return (TRUE ))
     <|> (reserved "false" >> return (FALSE))
     <|> rExpression

rExpression =
  do a1 <- aexp
     op <- relation
     a2 <- aexp
     return $ RBinary op a1 a2

relation = (reservedOp "<" >> return Le)
        <|> (reservedOp "=" >> return Eq)

parseString :: String -> Stm
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stm
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
