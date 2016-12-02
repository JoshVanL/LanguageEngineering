{-# LANGUAGE StandaloneDeriving #-}
module TinyBasic where
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List (intercalate)

data Prog  =  Prog [Line]
data Line  =  Cmnd Cmnd | Stmt Stmt | Line Int Stmt
data Cmnd  =  CLEAR | LIST | RUN
data Stmt  =  PRINT Args
           |  IF Expr Rel Expr Stmt
           |  GOTO Expr
           |  INPUT [Ident]
           |  LET Ident Expr
           |  GOSUB Expr
           |  RETURN
           |  END
type Args    =  [Either String Expr]
data Parity  =  POS | NEG
data Expr    =  Expr Parity Term [Exprs]
data Exprs   =  (:+:) Term | (:-:) Term
data Term    =  Term Fact [Terms]
data Terms   =  (:*:) Fact | (:/:) Fact
data Fact    =  Var Ident | Number Int | Parens Expr
data Rel     =  (:<:) | (:<=:) | (:<>:) | (:=:) | (:>:) | (:>=:)
type Ident   =  Char

prog :: Parser Prog
prog = Prog <$> many line

line :: Parser Line
line = Cmnd  <$> cmnd <* cr <|> Stmt  <$> stmt <* cr <|> Line  <$> number <*> stmt <* cr

cmnd :: Parser Cmnd
cmnd = CLEAR  <$ tok "CLEAR" <|> LIST   <$ tok "LIST" <|> RUN    <$ tok "RUN"

stmt :: Parser Stmt
stmt = PRINT   <$ tok "PRINT" <*> args
   <|> IF      <$ tok "IF" <*> expr <*> rel <*> expr <* tok "THEN" <*> stmt
   <|> GOTO    <$ tok "GOTO" <*> expr
   <|> INPUT   <$ tok "INPUT" <*> vars
   <|> LET     <$ tok "LET" <*> var <* tok "=" <*> expr
   <|> GOSUB   <$ tok "GOSUB" <*> expr
   <|> RETURN  <$ tok "RETURN"
   <|> END     <$ tok "END"

args :: Parser Args
args = sepBy1 ((Left <$> str) <|> (Right <$> expr)) (tok ",")

str :: Parser String
str = tok "\"" *> some (noneOf ("\n\r\"")) <* tok "\""

expr :: Parser Expr
expr = Expr <$> ((POS <$ tok "+") <|> (NEG <$ tok "-") <|> pure POS)
   <*> term <*> many exprs

exprs :: Parser Exprs
exprs = (:+:) <$ tok "+" <*> term
     <|> (:-:) <$ tok "|" <*> term

term :: Parser Term
term = Term <$> fact <*> many terms

terms :: Parser Terms
terms = (:*:) <$ tok "*" <*> fact
     <|> (:/:) <$ tok "/" <*> fact

fact :: Parser Fact
fact = Var <$> var
     <|> Number <$> number
     <|> Parens <$ tok "(" <*> expr <* tok ")"

var :: Parser Char
var = oneOf ['A' .. 'Z'] <* whitespace

vars :: Parser [Ident]
vars = sepBy1 var (tok ",")

number :: Parser Int
number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

rel :: Parser Rel
rel = (:<>:)  <$ tok "<>"
  <|> (:<>:)  <$ tok "><"
  <|> (:=:)   <$ tok "="
  <|> (:<=:)  <$ tok "<="
  <|> (:<:)   <$ tok "<"
  <|> (:>=:)  <$ tok ">="
  <|> (:>:)   <$ tok ">"

parseFile :: FilePath -> IO ()
parseFile filePath = do
  file <- readFile filePath
  putStrLn $ case parse prog filePath file of
    Left err   -> parseErrorPretty err
    Right prog -> pretty prog

cr :: Parser [Char]
cr = many (oneOf "\r\n")

tok :: String -> Parser String
tok t = string t <* whitespace

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> pure ()

deriving instance Show Prog
deriving instance Show Line
deriving instance Show Cmnd
deriving instance Show Stmt
deriving instance Show Expr
deriving instance Show Term
deriving instance Show Terms
deriving instance Show Fact
deriving instance Show Parity
deriving instance Show Exprs
deriving instance Show Rel

class Pretty a where
  pretty :: a -> String

instance Pretty Prog where
  pretty (Prog ls) = (unlines . map pretty) ls

instance Pretty Line where
  pretty (Line n stmt) = show n ++ " " ++ pretty stmt
  pretty (Cmnd cmnd) = pretty cmnd
  pretty (Stmt stmt) = pretty stmt

instance Pretty Stmt where
  pretty (PRINT es)                = "PRINT " ++ (intercalate ", " . map (either show pretty)) es
  pretty (IF expr1 rel expr2 stmt) = "IF " ++ pretty expr1 ++ " " ++ pretty rel ++ " " ++ pretty expr2 ++ " " ++ pretty stmt
  pretty (GOTO expr)               = "GOTO " ++ pretty expr
  pretty (INPUT idents )           = "INPUT " ++ (intercalate ", " (map wrap idents))
  pretty (LET ident expr)          = "LET " ++ wrap ident ++ " = " ++ pretty expr
  pretty (GOSUB expr)              = "GOSUB " ++ show expr
  pretty (RETURN)                  = "RETURN"
  pretty (END)                     = "END"

instance Pretty Cmnd where
  pretty (CLEAR) = "CLEAR"
  pretty (LIST)  = "LIST"
  pretty (RUN)   = "RUN"

instance Pretty Expr where
  pretty (Expr NEG term exprs) = "-" ++ pretty term ++ (concat . map pretty) exprs
  pretty (Expr POS term exprs) = pretty term ++ (concat . map pretty) exprs

instance Pretty Exprs where
  pretty ((:+:) term) = " + " ++ pretty term
  pretty ((:-:) term) = " - " ++ pretty term

instance Pretty Rel where
  pretty (:<>:) = "<>"
  pretty (:=:)  = "="
  pretty (:<=:) = "<="
  pretty (:<:)  = "<"
  pretty (:>=:) = ">="
  pretty (:>:)  = ">"

instance Pretty Term where
  pretty (Term fact terms) = pretty fact ++ (concat . map pretty) terms

instance Pretty Terms where
  pretty ((:*:) fact) = " * " ++ pretty fact
  pretty ((:/:) fact) = " / " ++ pretty fact

instance Pretty Fact where
  pretty (Var ident)   = wrap ident
  pretty (Number n)    = show n
  pretty (Parens expr) = "(" ++ pretty expr ++ ")"

wrap :: Char -> String
wrap c = [c]
