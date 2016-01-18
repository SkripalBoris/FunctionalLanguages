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

parseCos :: Parser (Double -> Double)
parseCos = do{ string "cos"; return cos }

parseLog :: Parser (Double -> Double)
parseLog = do{ string "log"; return log }

parseSqrt :: Parser (Double -> Double)
parseSqrt = do{ string "sqrt"; return sqrt }

parseNeg :: Parser (Double -> Double)
parseNeg = do{ char '-'; return negate }

parseBase :: Parser Double
parseBase = parseParens <|> parseNumber <|> parseUn

parseUn :: Parser Double
parseUn = do
                 op <- parseSin <|> parseCos <|> parseLog <|> parseSqrt <|> parseNeg
                 spaces
                 rhv <- parseBase
                 spaces
                 return (op rhv)

parseProd :: Parser Double
parseProd = do
                lhv <- parseBase
                spaces
                tail <- many parseTail
                return $ applyAll lhv tail
            where parseTail =
                    do
                        op <- parseMul <|> parseDiv <|> parseMod
                        spaces
                        rhv <- parseBase
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
