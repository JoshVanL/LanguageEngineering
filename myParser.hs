import Data.Char
import Control.Applicative

newtype Parser a = Parser (String -> Maybe [(a, String)])

parse :: Parser a -> (String -> Maybe [(a, String)])
parse (Parser p) = p

produce :: a -> Parser a
produce x = Parser (\ts -> Just [(x, ts)])

failure :: Parser a
failure = Parser (\ts -> Nothing)

item :: Parser Char
item = Parser (\ts -> case ts of
      [] -> Nothing
      (x:xs) -> Just [(x,xs)])

instance Functor Parser where
    fmap f p = Parser (\ts -> case parse p ts of
        Nothing -> Nothing
        Just [(x, ts')] -> Just [((f x), ts')])


instance Applicative Parser where
  pure a = Parser (\ts -> Just [(a, ts)])
  p <*> q = Parser (\ts -> do
          [(f,ts')] <- parse p ts
          [(x,ts'')] <- parse q ts'
          Just [(f x, ts'')])


instance Monad Parser where
  p >>= f = Parser (\ts -> case parse p ts of
        Nothing -> Nothing
        Just [(x,ts')] -> parse (f x) ts')

instance Alternative Parser where
  empty = failure
  (<|>) = option

option :: Parser a -> Parser a -> Parser a
option (Parser px) (Parser py) = Parser (\ts ->
              case px ts of
              Nothing -> py ts
              xs -> xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x ->
            if p x then produce x else failure

char :: Char -> Parser Char
char x = satisfy (x ==)

string :: String -> Parser String
string cs = foldr (\x pxs ->
                  (:) <$> char x <*> pxs)
                  (produce []) cs

oneOf :: [Char] -> Parser Char
oneOf x = satisfy (flip elem x)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

sepBy1 :: Alternative m => m a -> m x -> m [a]
sepBy1 p x = (:) <$> p <*> many (x *> p)


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

noneOf :: [Char] -> Parser Char
noneOf x = satisfy (\ts -> not (elem ts x))

expr :: Parser Expr
expr = Expr <$> ((POS <$ tok "+") <|> (NEG <$ tok "-") <|> pure POS)
   <*> term <*> many exprs

exprs :: Parser Exprs
exprs = (:+:) <$ tok "+" <*> term
     <|> (:-:) <$ tok "-" <*> term

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

cr :: Parser [Char]
cr = some (oneOf "\r\n")

tok :: String -> Parser String
tok t = string t <* whitespace

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> produce ()

wrap :: Char -> String
wrap c = [c]
