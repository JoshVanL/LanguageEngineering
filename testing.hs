> data Parser a = Parser (String -> [(String, a)])

> parse :: Parser a -> (String -> [(String, a)])
> parse (Parser p) = p

> produce :: a -> Parser a
> produce x = Parser (\ ts -> [(ts, x)])

# class Monad m where
#  return :: a -> Parser a
#  (>>=)  :: Parser a -> (a -> Parser b) -> Parser b

> Parser px >>= f = Parser ( \ ts -> concat [parse (f x) ts' | (ts', x) <- px ts])


> item :: Parser Char
> item = Parser ( \ ts -> case ts of
>  [] -> []  (x:xs) -> [(xs, x)] )
