import Prelude hiding (Num)
import qualified Prelude (Num)
import Control.Applicative
import Par_lib

parseFile :: FilePath -> IO ()
parseFile filePath = do
    file <- readFile filePath
    putStrLn (show(runParser stm file))

data Stm = Seq [Stm]
         | Ass Var Aexp 
         | Skip 
         | Comp Stm Stm 
         | If Bexp Stm Stm
         | While Bexp Stm
        deriving (Show)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | Eq Aexp Aexp
          | And Bexp Bexp
          | Le Aexp Aexp
          | Imp Bexp Bexp
        deriving (Show)

data Aexp = N Num 
          | V Var 
          | ABinary ABinOp Aexp Aexp
        deriving (Show)

data ABinOp = Add
            | Sub
            | Mult
        deriving (Show)


type Num = Integer
type Var = String

type Z = Integer
type T = Bool

stm :: Parser Stm
stm  = ifStm
    <|> whileStm
    <|> skipStm
    <|> assStm

ifStm :: Parser Stm
ifStm = If <$ tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm

whileStm :: Parser Stm
whileStm = While <$ tok "while" <*> bexp <* tok "do" <*> stm

skipStm :: Parser Stm
skipStm = Skip <$ tok "skip"

assStm :: Parser Stm
assStm = Ass <$> var <* tok ":=" <*> aexp

aexp :: Parser Aexp
aexp =  ABinary <$> aBinOp
    <|> N       <$> number
    <|> V       <$> var

aBinOp :: Parser Aexp
aBinOp =  Mult <$> aTerm <* tok "*" <*> aexp
      <|> Add  <$> aTerm <* tok "+" <*> aexp
      <|> Sub  <$> aTerm <* tok "-" <*> aexp

aTerm :: Parser Aexp
aTerm =  N       <$> number
     <|> V       <$> var

bexp :: Parser Bexp
bexp =  Neg   <$  tok "!" <*> bexp
    <|> TRUE  <$  tok "true" <* whitespace
    <|> FALSE  <$  tok "false" <* whitespace
    <|> Eq    <$> aexp <* tok "==" <*> aexp
    <|> Le    <$> aexp <* tok "<=" <*> aexp
    <|> Imp   <$> bexp <* tok "->" <*> bexp


number :: Parser Num
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace


var :: Parser Var
var = (some (oneOf ['A' .. 'Z']) )<* whitespace



