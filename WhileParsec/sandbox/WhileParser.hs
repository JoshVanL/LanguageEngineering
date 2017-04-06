{-# LANGUAGE StandaloneDeriving #-}
module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Data.List (intercalate)

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

statement :: Parser Stm
statement =  try (parens statement)
         <|> try (many (comments) *> compStm)
         <|> try (many (comments) *> stm)

comments :: Parser ()
comments =  (tok "/*" >> manyTill anyChar (tok "*/") >> whitespace >> return ())
        <|> (tok "//" >> manyTill anyChar newline >> whitespace >> return ())

compStm :: Parser Stm
compStm = Comp <$> stm <* tok ";" <* whitespace <*> statement

stm :: Parser Stm
stm =  try (Skip  <$  tok "skip")
   <|> try (Ass   <$> var <* tok ":="   <*> aexp)
   <|> try (If    <$  tok "if" <*> bexp <* tok "then" <*> statement <* tok "else" <*> statement)
   <|> try (While <$  tok "while" <*>  bexp <* tok "do"   <*> statement)
   <|> try (Call  <$  tok "call" <*> pname)
   <|> try (Block <$  tok "begin" <*> (many decv) <*> (many decp) <*> statement <* tok "end")

pname :: Parser Pname
pname = some(oneOf (['A' .. 'Z'] ++ ['a' .. 'z'])) <* whitespace

decv :: Parser (Var,Aexp)
decv = 
  do tok "var"
     var <- var
     tok ":="
     expr <- aterm
     tok ";"
     return $ (var, expr)

decp :: Parser (Pname, Stm)
decp = 
  do tok "proc" 
     pro <- pname
     tok "is"
     s <- stm
     tok ";"
     return $ (pro, s)

aexp :: Parser Aexp
aexp =  try (Mult <$> aterm <* tok "*" <*> aterm)
    <|> try (Add  <$> aterm <* tok "+" <*> aterm)
    <|> try (Sub  <$> aterm <* tok "-" <*> aterm)
    <|> aterm

aterm :: Parser Aexp
aterm =  parens aexp
     <|> V <$> var
     <|> N <$> number

bexp :: Parser Bexp
bexp =  try (And <$> bterm <* tok "&" <*> bterm)
    <|> try (Eq  <$> aterm <* tok "=" <*> aterm)
    <|> try (Le  <$> aterm <* tok "<=" <*> aterm)
    <|> try (Neg <$ tok "!" <*> bexp)
    <|> bterm

bterm :: Parser Bexp
bterm =  parens bexp
     <|> boolean

boolean :: Parser Bexp
boolean = do
  s <- tok "true" <|> tok "false"
  if (s == "true")
    then return TRUE
    else return FALSE


var :: Parser Var
var = some(oneOf (['A' .. 'Z'] ++ ['a' .. 'z'])) <* whitespace

relation = (tok "<" >> return Le)
        <|> (tok "=" >> return Eq)

number :: Parser Num
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

whitespace :: Parser ()
whitespace = many (oneOf " \t\n\r") *> pure ()

cr :: Parser [Char]
cr = many (oneOf "\r\n")

tok :: String -> Parser String
tok t = try (string t <* whitespace)

parseString :: String -> Stm
parseString str =
  case parse statement "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stm
parseFile file =
  do program  <- readFile file
     case parse statement "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

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

envp :: EnvP
envp "s" = Skip
envp var = Skip

updateEnvp :: EnvP -> Stm -> Pname -> EnvP
updateEnvp e s p y = if(p == y)
                    then s
                    else e y

upd_p :: (DecP, EnvP) -> EnvP
upd_p (((p,s):ds), envp) = 
  do envp <- updateEnvp envp s
     upd_p (ds, envp)
upd_p (_, envp) = envp


type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type Loc = Num

type EnvV = Var -> Loc
type EnvP = Pname -> Stm

