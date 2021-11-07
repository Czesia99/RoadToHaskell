module Eval
( evalExpr
) where

import Parser
import Data.Fixed
import Control.Applicative

------ in ------
-- expr ::= term + expr
-- term ::= factor * term | factor
-- factor ::= (expr) | int

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Sqr Expression Expression
                | Mod Expression Expression
                | Lit Double

token :: Parser a -> Parser a
token p = do
    b <- parseMany (parseAnyChar " ")
    expr <- p
    a <- parseMany (parseAnyChar " ")
    return expr

parseDigit :: Parser Char
parseDigit = Parser parse
    where
        parse = runParser $ parseAnyChar "1234567890."

parseUDouble :: Parser Double
parseUDouble = read <$> some parseDigit

parseDouble :: Parser Double
parseDouble = do
    arr <- many $ parseAnyChar "+-"
    x <- parseUDouble
    let n = length $ filter ("-" ==) [arr]
    case n `mod` 2 of
        0 -> return x
        _ -> return (negate x)

expr :: Parser Expression
expr = do
    x <- token term
    arr <- many $ do
        o <- parseAnyChar "+-"
        y <- token term
        return (o, y)
    return $ foldl fun x arr
    where
        fun a ('+', b) = Add a b
        fun a ('-', b) = Sub a b
        fun a (_, b) = undefined

term :: Parser Expression
term = do
    x <- power
    arr <- many $ do
        o <- parseAnyChar "*/%"
        y <- token power
        return (o, y)
    return $ foldl fun x arr
    where
        fun a ('*', b) = Mul a b
        fun a ('/', b) = Div a b
        fun a ('%', b) = Mod a b
        fun a (_, b) = undefined

power :: Parser Expression
power = do
    x <- token factor
    arr <- many $ do
        o <- parseAnyChar "^"
        y <- token factor
        return (o, y)
    return $ foldl fun x arr
    where
        fun a ('^', b) = Sqr a b
        fun a (_, b) = undefined

factor :: Parser Expression
factor = do
        parens
        <|> Lit <$> parseDouble

parens :: Parser Expression
parens = do
    parseChar '('
    x <- token expr
    parseChar ')'
    return x

eval :: Expression -> Maybe Double
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = case eval b of
    Just 0 -> Nothing
    _ -> (/) <$> eval a <*> eval b
eval (Sqr a b) = (**) <$> eval a <*> eval b
eval (Mod a b) = mod' <$> eval a <*> eval b
eval (Lit n) = Just n

evalExpr :: String -> Maybe Double
evalExpr str = case runParser expr str of
    Just (e, []) -> eval e
    Just (e, str') -> Nothing
    Nothing -> Nothing