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
  px <|> py = Parser (\ts -> case parse px ts of
          Nothing -> parse py ts
          xs -> xs)
