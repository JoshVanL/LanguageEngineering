import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Par_lib

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


--stm :: Parser Stm
--stm  = ifStm
--    <|> whileStm
--    <|> skipStm
--    <|> assStm
--
--ifStm :: Parser Stm
--ifStm = If <$ tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
--
--whileStm :: Parser Stm
--whileStm = While <$ tok "while" <*> bexp <* tok "do" <*> stm
--
--skipStm :: Parser Stm
--skipStm = Skip <$ tok "skip"
--
--assStm :: Parser Stm
--assStm = Ass <$> var <* tok ":=" <*> aexp
--
--aexp :: Parser Aexp
--aexp =  ABinary <$> aBinOp
--    <|> N       <$> number
--    <|> V       <$> var
--
--aBinOp :: Parser Aexp
--aBinOp =  Mult <$> aTerm <* tok "*" <*> aexp
--      <|> Add  <$> aTerm <* tok "+" <*> aexp
--      <|> Sub  <$> aTerm <* tok "-" <*> aexp
--
--aTerm :: Parser Aexp
--aTerm =  N       <$> number
--     <|> V       <$> var
--
--bexp :: Parser Bexp
--bexp =  Neg   <$  tok "!" <*> bexp
--    <|> TRUE  <$  tok "true" <* whitespace
--    <|> FALSE  <$  tok "false" <* whitespace
--    <|> Eq    <$> aexp <* tok "==" <*> aexp
--    <|> Le    <$> aexp <* tok "<=" <*> aexp
--    <|> Imp   <$> bexp <* tok "->" <*> bexp
--
--
--number :: Parser Num
--number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace
--
--
--var :: Parser Var
--var = (some (oneOf ['A' .. 'Z']) )<* whitespace
--


