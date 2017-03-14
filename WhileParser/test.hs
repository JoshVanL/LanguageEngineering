module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Data.Char
import Control.Monad
import Control.Applicative
import Par_lib

number :: Parser Num
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

boolean :: Parser Bool
boolean = do
  s <- string "true" <|> string "false"
  return $ read s

data Bexp = T Bool
          | Neg Bexp
          | And Bexp Bexp
          | Imp Bexp Bexp
          | Le Aexp Aexp
          | Eq Aexp Aexp
        deriving Show

data Aexp = N Num
          | V Var
          | Add Aexp Aexp
          | Sub Aexp Aexp
          | Mul Aexp Aexp
          deriving Show

type Num = Integer
type Var = String

eval :: Aexp -> Integer
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  N n   -> n

num :: Parser Aexp
num = do
  n <- number
  return (N n)

bool :: Parser Bexp
bool = do
  b <- boolean
  return (T b)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

bexp :: Parser Bexp
bexp = chain1 bterm bop

bterm :: Parser Bexp
bterm = chain1 bfactor aop

bfactor :: Parser Bexp
bfactor = 
         bool
     <|> parens bexp

neg :: Parser Bexp
neg = do
  n <- string "!"
  b <- bexp
  return (Neg b)

bop :: Parser (Bexp -> Bexp -> Bexp)
bop =  (infixOp "&&" And) 
   <|> (infixOp "->" Imp)

aop :: Parser (Aexp -> Aexp -> Aexp)
aop =  (infixOp "==" Eq) 
   <|> (infixOp "<=" Le) 


aexp :: Parser Aexp
aexp = chain1 aterm addop

aterm :: Parser Aexp
aterm = chain1 afactor mulop

afactor :: Parser Aexp
afactor =
      num
  <|> parens aexp

addop :: Parser (Aexp -> Aexp -> Aexp)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Aexp -> Aexp -> Aexp)
mulop = infixOp "*" Mul

run :: String -> Aexp
run = runParser aexp

