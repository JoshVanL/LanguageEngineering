{-# LANGUAGE StandaloneDeriving #-}
module WhileParser where
import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Par_lib

parseFile :: FilePath -> IO ()
parseFile filePath = do
    file <- readFile filePath
    putStrLn (show(runParser prog file))

data Prog = Prog [Stm]

data Stm = Ass Var Aexp 
         | Skip 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm

data Aexp = N Num 
          | V Var 
          | Mult Aexp Aexp 
          | Add Aexp Aexp 
          | Sub Aexp Aexp

data Bexp = TRUE 
          | FALSE 
          | Neg Bexp 
          | And Bexp Bexp 
          | Eq Aexp Aexp 
          | Le Aexp Aexp 
          | Imp Bexp Bexp

type Num = Integer
type Var = String

type Z = Integer
type T = Bool


prog :: Parser Prog
prog = Prog <$> many stm

stm :: Parser Stm
stm =  Ass   <$> var <* tok ":=" <*> aexp
   <|> Skip  <$  tok "Skip"
   <|> Comp  <$> stm <* tok ";" <*> stm
   <|> If    <$  tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
   <|> While <$  tok "while" <*> bexp <* tok "do" <*> stm

aexp :: Parser Aexp
aexp =  Mult <$> aexp <* tok "*" <*> aexp
    <|> Add  <$> aexp <* tok "+" <*> aexp
    <|> Sub  <$> aexp <* tok "-" <*> aexp


bexp :: Parser Bexp
bexp =  TRUE  <$  tok "true" <* whitespace
    <|> FALSE <$  tok "false" <* whitespace
    <|> Neg   <$  tok "!" <*> bexp
    <|> And   <$> bexp <* tok "&&" <*> bexp
    <|> Eq    <$> aexp <* tok "==" <*> aexp
    <|> Le    <$> aexp <* tok "<=" <*> aexp
    <|> Imp   <$> bexp <* tok "->" <*> bexp


number :: Parser Num
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace


var :: Parser Var
var = (some (oneOf ['A' .. 'Z']) >>= return . read) <* whitespace


deriving instance Show Prog
deriving instance Show Stm
deriving instance Show Aexp
deriving instance Show Bexp

