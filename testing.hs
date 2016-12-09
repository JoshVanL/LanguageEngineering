data Parser a = Parser (String -> [(String, a)])

parse :: Parser a -> (String -> [(String, a)])
parse (Parser p) = p

produce :: a -> Parser a
produce x = Parser(\ts -> [(ts, x)])
