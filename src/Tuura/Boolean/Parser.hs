{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tuura.Boolean.Parser (
    Expr (..),
    parseExpr) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Prelude hiding (not, or, and)

data Expr a = Var a
          | Not (Expr a)
          | And (Expr a) (Expr a)
          | Or (Expr a) (Expr a)
          | SubExpr (Expr a)
            deriving (Functor, Foldable, Traversable, Show)

parseExpr :: String -> Either ParseError (Expr String)
parseExpr = parse expr ""
  where expr = buildExpressionParser operators term <?> "compound expression"
        term = parens expr <|> variable <?> "full expr ession"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not),
                       Prefix (string "!" >> return Not),
                       Postfix (string "'" >> return Not)]
                    , [binary "AND" And, binary "*" And, binary "&" And],
                      [binary "OR" Or, binary "|" Or, binary "+" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var <$> (varParser <* spaces) <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces)
                           <?> "parens"
        varParser = liftM2 (++) (many1 letter) (many alphaNum)
