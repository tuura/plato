{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tuura.Boolean.Parser (
    Expr (..),
    parseExpr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>))
import Control.Monad
import Prelude hiding (not, or, and)

data Expr a = Var a
          | Not (Expr a)
          | And (Expr a) (Expr a)
          | Or (Expr a) (Expr a)
          | SubExpr (Expr a)
            deriving (Functor, Foldable, Traversable)

instance Show a => Show (Expr a) where
  show (Not e)     = "!" ++ show e
  show (And e1 e2) = show e1 ++ " * " ++ show e2
  show (Or e1 e2)  = show e1 ++ " + "  ++ show e2
  show (Var c)     = show c
  show (SubExpr e) = "(" ++ show e ++ ")"

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expr ession"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not),
                       Prefix (string "!" >> return Not),
                       Postfix (string "'" >> return Not)]
                    , [binary "AND" And, binary "*" And, binary "&" And,
                       binary "OR" Or, binary "|" Or, binary "+" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var <$> (many1 letter <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces)
                           <?> "parens"
