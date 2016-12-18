newtype Parser a = Parser (String -> Maybe [(a, String)])

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

instance Monad Parser where
  p >>= f = Parser (\ts -> case parse p ts of
        Nothing -> Nothing
        Just [(x,ts')] -> parse (f x) ts')

class Applicative f => Choice f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Choice Parser where
  empty = failure
  (<|>) _ py = parse py
  (<|>) px py = Parser (\ts -> case parse px ts of
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

achar :: Parser Char
achar = noneOf("\"\n\r")

str :: Parser String
str = tok "\"" *> many achar <* tok "\""

data Rel = (:<:) | (:<=:) | (:<>:) | (:=:) | (:>:) | (:>=:)

rel :: Parser Rel
rel = (:<>:)  <$ tok "<>"
  <|> (:<>:)  <$ tok "><"
  <|> (:=:)   <$ tok "="
  <|> (:<=:)  <$ tok "<="
  <|> (:<:)   <$ tok "<"
  <|> (:>=:)  <$ tok ">="
  <|> (:>:)   <$ tok ">"


digit :: Parser Char
digit = oneOf ['0' .. '9']

number :: Parser Int
number = (some digit) >>= return . read

var :: Parser Char
var = oneOf ['A' .. 'Z']
