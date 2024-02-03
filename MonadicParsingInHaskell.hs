{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module MonadicParsingInHaskell () where
import Control.Monad (liftM, ap)
import Data.Char 
import GHC.Types
import GHC.Base hiding ((++), many, MonadPlus) 
import Data.List (concat)
import Prelude ((-), (+),(*), div, Monad, Show)
 
newtype Parser a = Parser (String -> [(a,String)]) -- In Haskell, the newtype declaration creates a new type from an existing one.

item :: Parser Char -- item is a function that takes a string and returns a list of pairs, each pair consisting of a character and the remaining string.
item = Parser (\cs -> case cs of
            ""     -> []
            (c:cs) -> [(c,cs)])

instance Monad Parser where
      (>>=) :: Parser a -> (a -> Parser b) -> Parser b
      p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadZero Parser where
      zero :: Parser a
      zero = Parser (\cs -> [])

instance MonadPlus Parser where
  (++) :: Parser a -> Parser a -> Parser a
  p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

class MonadZero m => MonadPlus m where
      (++) :: m a -> m a -> m a

class Monad m => MonadZero m where
    zero :: m a

instance MonadZero [] where
  zero :: [a]
  zero = []

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = liftM

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\cs -> [(a,cs)]) -- same as return
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

instance MonadPlus [] where
  (++) :: [a] -> [a] -> [a]
  [] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)

p :: Parser (Char,Char)
p  = do {c <- item; item; d <- item; return (c,d)}

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

many   :: Parser a -> Parser [a]
many p  = many1 p +++ return []

many'   :: Parser a -> Parser [a]
many' p  = many p +++ return []

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                               []     -> []
                               (x:xs) -> [x])

sat  :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many1  :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep  = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <-p
                    as <- many (do {sep; p})
                    return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                    where
                       rest a = (do f <- op
                                    b <- p
                                    rest (f a b))
                                +++ return a

space :: Parser String
space = many (sat isSpace)

token  :: Parser a ->  Parser a
token p = do
    a <- p; space; return a

symb :: String -> Parser String
symb cs = token (string cs)

apply  :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

expr  :: Parser Int
expr = term `chainl1` addop

addop :: Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return div}

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Int
digit = do {x <- token (sat isDigit);  return (ord x - ord '0')}
