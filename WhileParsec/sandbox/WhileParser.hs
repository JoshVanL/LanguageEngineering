{-# LANGUAGE StandaloneDeriving #-}
module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Data.List (intercalate)

type Num = Integer
type Var = String

type State = Var -> Z

type Z = Integer
type T = Bool
type Pname = String


type DecV = [(Var,Aexp)]
type DecP = [(Pname,Stm)]
type Loc = Num

type EnvV = Var -> Loc
type EnvP = Pname -> Stm

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
