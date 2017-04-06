{-# LANGUAGE StandaloneDeriving #-}
module ParseWhile where
import Prelude hiding (Num)
import qualified Prelude (Num)

import Debug.Trace

import Data.List (intercalate)
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

type Num = Integer
type Var = String

type State = Var -> Z

type Z = Integer
type T = Bool
type Pname = String

data Aexp = N Num 
          | V Var 
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
        deriving (Show)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
        deriving (Show)

data Stm = Skip
         | Ass Var Aexp 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm
         | Block DecV DecP Stm
         | Call Pname
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

compStm :: Parser Stm
compStm = buildExpressionParser compOp statement'

compOp = [[Infix  (reservedOp ";"   >> return (Comp )) AssocLeft]]

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

aOp = [        [Infix  (reservedOp "*"   >> return (Mult)) AssocLeft ]
             , [Infix  (reservedOp "+"   >> return (Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Sub)) AssocLeft]
              ]

bOp = [ [Prefix (reservedOp "!" >> return (Neg             ))          ]
             , [Infix  (reservedOp "and" >> return (And     )) AssocLeft]
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
     return $ op a1 a2

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

evalString :: String -> State -> State
evalString str s = s_ds (parseString str) s

n_val :: Num -> Z
n_val x = x 

a_val :: Aexp -> State -> Z
a_val (N n) s = n_val n
a_val (V v) s = s v
a_val (Mult a1 a2) s = (a_val a1 s) * (a_val a2 s)
a_val (Add a1 a2) s = (a_val a1 s) + (a_val a2 s)
a_val (Sub a1 a2) s = (a_val a1 s) - (a_val a2 s)

b_val :: Bexp -> State -> T
b_val TRUE s = True
b_val FALSE s = False
b_val (Neg b) s = case b_val b s of 
                    True -> False
                    False -> True
b_val (And b1 b2) s = case ((b_val b1 s), (b_val b2 s)) of 
                       (True, True) -> True
                       otherwise -> False
b_val (Eq a1 a2) s = if (a_val a1 s) == (a_val a2 s)
                       then True
                       else False
b_val (Le a1 a2) s = if (a_val a1 s) <= (a_val a2 s)
                        then True
                        else False 

fv_aexp :: Aexp -> [Var]
fv_aexp (V v) = [v]
fv_aexp (N n) = []
fv_aexp (Mult a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)
fv_aexp (Add a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)
fv_aexp (Sub a1 a2) = (++) (fv_aexp a1) (fv_aexp a2)

subst_aexp :: Aexp -> Var -> Aexp -> Aexp
subst_aexp (V var) v a2 = if (var == v)
                             then a2
                             else V var
subst_aexp (N n) v a2 = N n
subst_aexp (Mult a11 a12) v a2 =  Mult (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Add a11 a12) v a2  = Add (subst_aexp a11 v a2) (subst_aexp a12 v a2)
subst_aexp (Sub a11 a12) v a2  = Sub (subst_aexp a11 v a2) (subst_aexp a12 v a2) 

update :: State -> Z -> Var -> State
update s i v y = if(v == y) 
                    then i
                    else s y

s' :: State
s' "x" = n_val 5

s :: State
s "x" = n_val 5
s "y" = n_val 2
s "z" = n_val 3
s var = n_val 0

p :: Stm
p = Comp (Ass "y" (N 1)) (While (Neg (Eq (V "x") (N 1))) (Comp (Ass "y" (Mult (V "y") (V "x"))) (Ass "x" (Sub (V "x") (N 1)))))

cond :: (a -> T, a -> a, a -> a) -> (a -> a)
cond (c, a1, a2) s = if (c s)
                        then (a1 s)
                        else (a2 s)

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass var ax) s = update s (a_val ax s) var
s_ds Skip s = s
s_ds (Comp sm1 sm2) s = (s_ds sm2 (s_ds sm1 s))
s_ds (If b sm1 sm2) s = cond (b_val b, s_ds sm1, s_ds sm2) s
s_ds (While b sm) s = fix ff s where
    ff g = cond (b_val b, g . s_ds sm, id)

new :: Loc -> Loc
new l = l + 1

type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type Loc = Num

type EnvV = Var -> Loc
type EnvP = Pname -> Stm

envp :: Stm -> State -> State
envp stm s = s_ds stm s

--upd_p :: Pname -> Stm -> DecP -> EnvP -> EnvP
--upd_p p s d e = upd_p (d, envp p s)