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

not :: (Expr a) -> (Expr a)
not (Not e)     = e
not (And e1 e2) = Or (not e1) (not e2)
not (Or e1 e2)  = And (not e1) (not e2)
not (SubExpr e) = not e
not (Var c)     = Not (Var c)

or :: (Expr a) -> (Expr a) -> (Expr a)
or (And a (Var b)) c = And a (or (Var b) c)
or (And a (Not b)) c = And a (or (Not b) c)
or (And a (Or b c)) d = And a ((or (Or b c) d))
or (Or a b) (Var c) = or a (or b (Var c))
or (Or a b) (Not c) = or a (or b (Not c))
or a b = Or a b

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expr ession"
        operators = [ [Prefix (string "NOT" >> spaces >> return not),
                       Prefix (string "!" >> return not),
                       Postfix (string "'" >> return not)]
                    , [binary "AND" And, binary "*" And, binary "&" And,
                       binary "OR" or, binary "|" or, binary "+" or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var <$> (many1 letter <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces)
                           <?> "parens"
