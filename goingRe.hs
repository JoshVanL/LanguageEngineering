data Parser a = Parser (String -> Maybe [(a, String)])

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
