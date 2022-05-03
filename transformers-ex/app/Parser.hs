module Parser where

import Types
import Text.Parsec.Char
import Text.Parsec.Prim 
import Text.ParserCombinators.Parsec
import Control.Monad

number :: Parser Term
number = liftM (Con . read) $ many1 digit

divExpr :: Parser Term
divExpr = do
    char '('
    t <- number <|> divExpr
    char '/'
    u <- number <|> divExpr
    char ')'
    return $ Div t u

parseExpr :: Parser Term
parseExpr = number <|> divExpr

readExpr :: String -> Either ParseError Term
readExpr input = parse parseExpr "" input
