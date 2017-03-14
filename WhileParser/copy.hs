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

data Bexp = Bool T
          | Neg Bexp
          | BBinary BBinOp Bexp Bexp
          | RBinary RBinOp Aexp Aexp
        deriving (Show)

data BBinOp = And
            | Or
        deriving(Show)

data RBinOp = Eq
            | Le
            | Imp
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
stm =  Ass   <$> var <* tok ":=" <*> aexp
   <|> Skip  <$  tok "Skip"
   <|> Comp  <$> stm <* tok ";" <*> stm
   <|> If    <$  tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
   <|> While <$  tok "while" <*> bexp <* tok "do" <*> stm

aexp :: Parser Aexp
aexp =  Mult <$> aexp <* tok "*" <*> aexp
    <|> Add  <$> aexp <* tok "+" <*> aexp
    <|> Sub  <$> aexp <* tok "-" <*> aexp
    <|> N    <$> number
    <|> V    <$> var


bexp :: Parser Bexp
bexp =  Neg   <$  tok "!" <*> bexp
    <|> Bool  <$  tok "true" <* whitespace
    <|> Bool  <$  tok "false" <* whitespace
    <|> And   <$> bexp <* tok "&&" <*> bexp
    <|> Eq    <$> aexp <* tok "==" <*> aexp
    <|> Le    <$> aexp <* tok "<=" <*> aexp
    <|> Imp   <$> bexp <* tok "->" <*> bexp


number :: Parser Num
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace


var :: Parser Var
var = (some (oneOf ['A' .. 'Z']) )<* whitespace



