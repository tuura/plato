{-# LANGUAGE FlexibleContexts #-}

module Tuura.Parser.Boolean (
    Expr (..),
    listVars, parseExpr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List hiding (or, and)
import Prelude hiding (not, or, and)

data Expr = Not Expr
          | And Expr Expr
          | Or Expr Expr
          | SubExpr Expr
          | Var String
            deriving Eq

not :: Expr -> Expr
not (Not e)     = e
not (And e1 e2) = Or (not e1) (not e2)
not (Or e1 e2)  = And (not e1) (not e2)
not (SubExpr e) = not e
not (Var c)     = Not (Var c)

or :: Expr -> Expr -> Expr
or (And a (Var b)) c = And a (or (Var b) c)
or (And a (Not b)) c = And a (or (Not b) c)
or (And a (Or b c)) d = And a ((or (Or b c) d))
or (Or a b) (Var c) = or a (or b (Var c))
or (Or a b) (Not c) = or a (or b (Not c))
or a b = Or a b

listVars :: Expr -> [String]
listVars (Var a)   = [a]
listVars (Not a)   = [show a]
listVars (And a b) = listVars a ++ listVars b
listVars (Or a b)  = listVars a ++ listVars b
listVars (SubExpr a) = listVars a

instance Show Expr where
  show (Not e)     = "!" ++ show e
  show (And e1 e2) = show e1 ++ " * " ++ show e2
  show (Or e1 e2)  = show e1 ++ " + "  ++ show e2
  show (Var c)     = c
  show (SubExpr e) = "(" ++ show e ++ ")"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""
  where expr      = buildExpressionParser operators term <?> "compound expression"
        term      = parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return not), Prefix (string "!" >> return not), Postfix (string "'" >> return not)]
                    , [binary "AND" And, binary "*" And, binary "&" And,
                       binary "OR" or, binary "|" or, binary "+" or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var     <$> (many1 letter <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces) <?> "parens"

-- main :: IO ()
-- main = case parseExpr "(!x * y') + (a * b | z)" of
--          Left err -> do
--            putStr "parse error at "
--            print err
--          Right x -> do
--           putStrLn $ show $ nub (listVars x)
--           putStrLn $ show x
