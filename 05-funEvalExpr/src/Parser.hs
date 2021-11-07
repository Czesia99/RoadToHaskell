module Parser where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f p = Parser parse -- <$>
        where
            parse str = case runParser p str of
                Just (a, str') -> Just (f a, str')
                Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser parse
        where
            parse str = Just (a, str)

    liftA2 f p1 p2 = Parser parse -- <*>
        where
            parse str = case runParser (parseAnd p1 p2) str of
                Just ((a,b), str') -> Just (f a b, str')
                Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser parse
        where
            parse str = runParser (parseOr p1 p2) str

instance Monad Parser where
    p >>= f = Parser parse
        where
            parse str = case runParser p str of
                Nothing -> Nothing
                Just(res, str') -> runParser (f res) str'

parseChar :: Char -> Parser Char
parseChar c = Parser parse
    where
        parse :: String -> Maybe (Char, String)
        parse [] = Nothing
        parse (x:xs)
            | x == c = Just (x, xs)
            | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = empty
parseAnyChar (x:xs) = Parser parse
    where
        parse :: String -> Maybe (Char, String)
        parse [] =  Nothing
        parse str = case runParser (parseChar x) str of
            Just res -> Just res
            Nothing -> runParser (parseAnyChar xs) str

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser parse
    where
        parse str = case runParser p1 str of
            Just res -> Just res
            Nothing -> runParser p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd pa pb = Parser parse
    where
        parse str = case runParser pa str of
            Nothing -> Nothing
            Just (resa, str') -> case runParser pb str' of
                Nothing -> Nothing
                Just (resb, str'') -> Just ((resa, resb), str'')

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f pa pb = Parser parse
    where
        parse str = case runParser (parseAnd pa pb) str of
            Just ((a, b), str) -> Just (f a b, str)
            Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser parse
    where
        parse str = case runParser p str of
            Nothing -> Just([], str)
            Just (a, str') -> case runParser (parseMany p) str' of
                Nothing -> Just ([a], str')
                Just (tab, str'') -> Just (a:tab, str'')

parseSome :: Parser a -> Parser [a]
parseSome p = Parser parse
    where
        parse = runParser $ parseAndWith (:) p (parseMany p)