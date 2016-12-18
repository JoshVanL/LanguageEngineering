{-# LANGUAGE StandaloneDeriving #-}

parseFile :: FilePath -> IO ()
parseFile filePath = do
  file <- readFile filePath
  putStrLn (show(runParser prog file))


newtype Parser a = Parser (String -> Maybe [(a, String)])

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    Just [(res, [])] -> res
    Just [(_, rs)]   -> error rs
    _           -> error "Parser error."

produce :: a -> Parser a
produce x = Parser (\ts -> Just [(x, ts)])

parse :: Parser a -> (String -> Maybe [(a, String)])
parse (Parser p) = p

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
  p *> q = Parser (\ts -> do
          [(f,ts')] <- parse p ts
          [(x,ts'')] <- parse q ts'
          Just [(x, ts'')])
  p <* q = Parser (\ts -> do
          [(f,ts')] <- parse p ts
          [(x,ts'')] <- parse q ts'
          Just [(f, ts'')])

instance Monad Parser where
  p >>= f = Parser (\ts -> case parse p ts of
        Nothing -> Nothing
        Just [(x,ts')] -> parse (f x) ts')

class Applicative f => Choice f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Choice Parser where
  empty = failure
  px <|> py = Parser (\ts -> case parse px ts of
          Nothing -> parse py ts
          xs -> xs)

many :: Parser a -> Parser [a]
many x = some x <|> produce []

some :: Parser a -> Parser [a]
some x = (:) <$> x <*> many x

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \x ->
        if p x then produce x else failure

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\c -> elem c cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> not(elem c cs))

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p x = (:) <$> p <*> many (x *> p)


char :: Char -> Parser Char
char x = satisfy (x ==)

string :: String -> Parser String
string "" = produce ""
string(x:xs) = char x >>= \x' ->
               string xs >>= \xs' ->
               produce (x:xs)

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> produce ()

tok :: String -> Parser String
tok t = string t <* whitespace

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

achar :: Parser Char
achar = noneOf("\"\n\r.")

str :: Parser String
str = tok "\"" *> many achar <* tok "\""

rel :: Parser Rel
rel = ((:<>:)  <$ tok "<>")
  <|> ((:<>:)  <$ tok "><")
  <|> ((:=:)   <$ tok "=")
  <|> ((:<=:)  <$ tok "<=")
  <|> ((:<:)   <$ tok "<")
  <|> ((:>=:)  <$ tok ">=")
  <|> ((:>:)   <$ tok ">")


digit :: Parser Char
digit = oneOf ['0' .. '9'] <* whitespace

number :: Parser Int
number = ((some digit) >>= return . read) <* whitespace

var :: Parser Ident
var = oneOf ['A' .. 'Z'] <* whitespace

fact :: Parser Fact
fact = (Var <$> var)
  <|> (Number <$> number)
  <|> (Parens <$ tok "(" <*> expr <* tok ")")

expr :: Parser Expr
expr = Expr <$> ((POS <$ tok "+") <|> (NEG <$ tok "-") <|> pure POS)
  <*> term <*> many exprs

exprs :: Parser Exprs
exprs = ((:+:) <$ tok "+" <*> term)
    <|> ((:-:) <$ tok "-" <*> term)

term :: Parser Term
term = Term <$> fact <*> many terms

terms :: Parser Terms
terms = ((:*:) <$ tok "*" <*> fact)
  <|> ((:/:) <$ tok "/" <*> fact)


vars :: Parser [Ident]
vars = sepBy1 var (tok ",")

args :: Parser Args
args = sepBy1 ((Left <$> str) <|> (Right <$> expr)) (tok ",")

stmt :: Parser Stmt
stmt = (PRINT   <$ tok "PRINT" <*> args)
   <|> (IF      <$ tok "IF" <*> expr <*> rel <*> expr <* tok "THEN" <*> stmt)
   <|> (GOTO    <$ tok "GOTO" <*> expr)
   <|> (INPUT   <$ tok "INPUT" <*> vars)
   <|> (LET     <$ tok "LET" <*> var <* tok "=" <*> expr)
   <|> (GOSUB   <$ tok "GOSUB" <*> expr)
   <|> (RETURN  <$ tok "RETURN")
   <|> (END     <$ tok "END")

cr :: Parser [Char]
cr = many (oneOf "\r\n")


cmnd :: Parser Cmnd
cmnd = (CLEAR  <$ tok "CLEAR") <|> (LIST <$ tok "LIST") <|> (RUN <$ tok "RUN")

line :: Parser Line
line = (Cmnd  <$> cmnd <* cr) <|> (Stmt  <$> stmt <* cr) <|> (Line  <$> number <*> stmt <* cr)

prog :: Parser Prog
prog = Prog <$> many line

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
