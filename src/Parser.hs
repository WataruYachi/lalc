module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

type Error = String

newtype ParseError = E {errorMsg :: String -> String}

newtype Parser a = P (String -> Either ParseError (a, String))

runParser :: Parser a -> String -> Either ParseError (a,String)
runParser (P p) input = p input

parseTest :: Show a => Parser a -> String -> String
parseTest (P p) input = case p input of
                            Left (E e) -> e "parseError \n"
                            Right (o,s) -> (show o)

item :: Parser Char
item = P (\input -> case input of
                        [] -> Left $ E (\s -> s ++ "[item]: no input \n")
                        (x:xs) -> Right (x,xs))


instance Functor Parser where
    fmap f p = P (\input -> case runParser p input of
                                Left e -> Left e
                                Right (o,s) -> Right (f o, s))

instance Applicative Parser where
    pure v = P (\input -> Right (v, input))
    pg <*> px = P (\input -> case runParser pg input of
                                Left e -> Left e
                                Right (o,s) -> runParser (fmap o px) s)

three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
        where g x y z = (x,z)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\input -> case runParser p input of
                                Left e -> Left e
                                Right (o,s) -> runParser (f o) s)

instance Alternative Parser where
    empty = P (\input -> Left $ E (\s -> s ++ "[empty]: empty"))
    p <|> q = P (\input -> case runParser p input of
                        Left _ -> runParser q input
                        Right (o,s) -> Right (o,s))

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Parser Char
digit = sat isDigit