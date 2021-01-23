module Csv.Parser  (
    CSV(..)
  , emptyCSV
  , csvParser
) where

import Control.Applicative

type Input = String

data Parser a = Parser
    { runParser :: Input -> Either String (Input, a)
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
        pure $ (rest, f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Right (input, a)
    (Parser pf) <*> (Parser p) = Parser $ \input -> do
        (rest, f) <- pf input
        (rest', v) <- p rest
        pure $ (rest', f v)

instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> do
        (rest, v) <- p input
        runParser (f v) rest

instance Alternative (Either String) where
    empty = Left "Empty"
    Left _ <|> rhs = rhs
    lhs <|> _ = lhs

instance Alternative Parser where
    empty = Parser $ \_ -> empty
    (Parser l) <|> (Parser r) = Parser $ \input -> (l input) <|> (r input)

charParser :: Char -> Parser Char
charParser c = Parser parse
    where
        parse [] = Left "Empty parse string"
        parse (c':cs)
            | c == c' = Right (cs, c)
            | otherwise = Left "Failed to match character '" ++ [c] ++ "' (found '" ++ [c'] ++ "')."

exceptCharParser :: Char -> Parser Char
exceptCharParser c = Parser parse
    where 
        parse [] = Left "Empty parse string"
        parse (c':cs)
            | c == c' = Left "Failed to match any character except '" ++ [c] ++ "."
            | otherwise = Right (cs, c')

stringParser :: String -> Parser String
stringParser [] = pure []
stringParser (c:cs) = (:) <$> charParser c <*> stringParser cs

charSetParser :: [Char] -> Parser Char
charSetParser set = foldl (\cur next -> cur <|> (charParser next)) empty set

exceptCharSetParser :: [Char] -> Parser Char
exceptCharSetParser set = foldl (\cur next -> cur <|> (exceptCharParser next)) empty set

whitespace :: Parser String
whitespace = many $ charSetParser " \t"

newline :: Parser Char
newline = charSetParser "\n\r"

parseEntry :: Char -> Parser String
parseEntry delim = many $ exceptCharSetParser special
    where
        special = ['\n', delim]

parseLine :: Char -> Parser [String]
parseLine delim = do
    
    

-- CSV Parser - Takes a delimeter character, and a boolean flag to look for a header
csvParser :: Char -> Bool -> Parser CSV
csvParser delim header = do
    hdr <- getHeader
    body <- many $ parseLine delim

    pure $ CSV hdr body
            
    where
        getHeader = if header then parseLine delim else pure []

