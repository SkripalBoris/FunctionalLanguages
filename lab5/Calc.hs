module Calc (calculate) where

import Text.Parsec
import Data.Functor
import Data.Fixed

type Parser a = Parsec String () a

applyAll :: a -> [a -> a] -> a
applyAll x ops = foldl (\x f -> f x) x ops

parseDigit :: Parser Char
parseDigit = oneOf (['0'..'9']++['.'])

parseNumber :: Parser Double
parseNumber = read <$> many1 parseDigit

parsePlus :: Parser (Double -> Double -> Double)
parsePlus = do{ char '+'; return (+) }

parseMinus :: Parser (Double -> Double -> Double)
parseMinus = do{ char '-'; return (-) }

parseMul :: Parser (Double -> Double -> Double)
parseMul = do{ char '*'; return (*) }

parseDiv :: Parser (Double -> Double -> Double)
parseDiv = do{ char '/'; return (/) }

parseMod :: Parser (Double -> Double -> Double)
parseMod = do{ char '%'; return mod' }

parseSin :: Parser (Double -> Double)
parseSin = do{ string "sin"; return sin }

parseBase :: Parser Double
parseBase = parseParens <|> parseNumber

parseUn :: Parser Double
parseUn = do
                lhv <- parseBase
                spaces
                tail <- many parseTail
                return $ applyAll "0+" tail
            where parseTail =
                    do
                        op <- parseSin
                        spaces
                        rhv <- parseBase
                        spaces
                        return (`op` rhv)

parseProd :: Parser Double
parseProd = do
                lhv <- parseUn
                spaces
                tail <- many parseTail
                return $ applyAll lhv tail
            where parseTail =
                    do
                        op <- parseMul <|> parseDiv <|> parseMod
                        spaces
                        rhv <- parseUn
                        spaces
                        return (`op` rhv)

parseSum :: Parser Double
parseSum = do
                lhv <- parseProd
                tail <- many parseTail
                return $ applyAll lhv tail
           where parseTail =
                    do
                        spaces
                        op <- parsePlus <|> parseMinus
                        spaces
                        rhv <- parseProd
                        return (`op` rhv)

parseExpr :: Parser Double
parseExpr = parseSum

parseParens :: Parser Double
parseParens = do 
                  lp <- char '('
                  spaces
                  expr <- parseExpr
                  spaces
                  rp <- char ')'
                  return expr

calculate :: String -> Either ParseError Double
calculate text = 
    runParser parseExpr () "user-supplied" text