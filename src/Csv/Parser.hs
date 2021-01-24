{-# LANGUAGE FlexibleInstances #-}

-- module Csv.Parser  (
--     CSV(..)
--   , emptyCSV
--   , csvParser
--   , Parser(..)
-- ) where

module Csv.Parser where

import Control.Applicative

type Input = String

newtype Parser a = Parser
    { runParser :: Input -> Either (Maybe String) (Input, a)
    }

data CSV = CSV
    { fields :: [String]
    , records :: [[String]]
    }

emptyCSV :: CSV
emptyCSV = CSV [] []

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (rest, a) <- p input
        pure (rest, f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Right (input, a)
    (Parser pf) <*> (Parser p) = Parser $ \input -> do
        (rest, f) <- pf input
        (rest', v) <- p rest
        pure (rest', f v)

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (rest, v) <- p input
        runParser (f v) rest

instance Alternative (Either (Maybe String)) where
    empty = Left Nothing
    (Left _) <|> rhs = rhs
    lhs <|> _ = lhs

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser l) <|> (Parser r) = Parser $ \input -> 
        l input <|> r input

showLine :: [String] -> String
showLine [] = "\n"
showLine (s:ss) = s ++ ", " ++ showLine ss

instance Show CSV where
    show (CSV fs records) = concat $ showLine fs : (map showLine records)

charParser :: Char -> Parser Char
charParser c = Parser f
    where
        f [] = Left $ Just "Empty parse string"
        f (c':cs)
            | c == c' = Right (cs, c)
            | otherwise = Left $ Just $ "Failed to match character '" ++ [c] ++ "' (found '" ++ [c'] ++ "')."

exceptCharSetParser :: [Char] -> Parser Char
exceptCharSetParser cSet = Parser parse
    where 
        parse [] = Left $ Just "Empty parse string"
        parse (c':cs)
            | contains c' cSet = Left $ Just $ "Failed to match any character except '" ++ cSet ++ "."
            | otherwise = Right (cs, c')
        
        contains _ [] = False
        contains x (x':xs)
            | x == x' = True
            | otherwise = contains x xs

stringParser :: String -> Parser String
stringParser [] = pure []
stringParser (c:cs) = (:) <$> charParser c <*> stringParser cs

charSetParser :: [Char] -> Parser Char
charSetParser cs = foldl (<|>) empty $ map charParser cs

whitespace :: Parser String
whitespace = many $ charSetParser " \t"

newline :: Parser String
newline = some $ charSetParser "\n\r"

parseEntry :: Char -> Parser String
parseEntry delim = many $ exceptCharSetParser special
    where
        special = ['\n', '\r', delim]

parseLine :: Char -> Parser [String]
parseLine delim = do
    ss <- many $ parseEntry delim <* charParser delim
    last <- parseEntry delim <* newline
    pure $ ss ++ [last]

-- CSV Parser - Takes a delimeter character, and a boolean flag to look for a header
csvParser :: Char -> Bool -> Parser CSV
csvParser delim header = do
    hdr <- getHeader
    body <- many $ parseLine delim

    pure $ CSV hdr body
            
    where
        getHeader = if header then parseLine delim else pure []

