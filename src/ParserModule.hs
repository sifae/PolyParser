module ParserModule where

import Control.Monad
import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative

newtype Parser a = Parser {runP :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = Parser $ \str -> do
    (x, str') <- runP p str
    return (f x, str')

instance Applicative Parser where
  pure x = Parser $ \str -> [(x,str)]
  f <*> x = Parser $ \str -> do
    (f', str' ) <- runP f str
    (x', str'') <- runP x str'
    return (f' x', str'')

instance Monad Parser where 
  p >>= k = Parser $ \str -> do 
    (x, str')  <- runP p str
    runP (k x) str'

instance Alternative Parser where
  empty = Parser . const $ []
  p1 <|> p2 = Parser $ \str -> runP p1 str ++ runP p2 str

instance MonadPlus Parser

-- | Apply a parser and return all ambiguous results, 
--   but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = do
    (x, str) <- runP p cs
    guard (null str)
    return x

-- | Apply a parser and only return a result, 
--   if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of 
    [x] -> Just x
    _   -> Nothing 

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP pred = Parser $ \str -> do
    guard (not . null $ str) 
    x <- return . head $ str
    guard (pred x) 
    return (x, tail str)

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Parse a whole string.
stringP :: String -> Parser String
stringP []     = pure []
stringP (x:xs) = (:) <$> charP x <*> stringP xs
