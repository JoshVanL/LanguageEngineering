module Par_lib where
import Data.Char
import Control.Applicative

newtype Parser a = Parser (String -> Maybe [(a, String)])

parse :: Parser a -> (String -> Maybe [(a, String)])
parse (Parser p) = p

runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        Just [(res, [])] -> res
        Just [(_, rs)]   -> error rs
        _                -> error "Parser error."

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

cr :: Parser [Char]
cr = some (oneOf "\r\n")

tok :: String -> Parser String
tok t = string t <* whitespace

whitespace :: Parser ()
whitespace = many (oneOf " \t") *> produce ()

wrap :: Char -> String
wrap c = [c]


