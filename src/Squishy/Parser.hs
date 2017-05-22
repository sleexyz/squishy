{-# LANGUAGE OverloadedStrings #-}
module Squishy.Parser where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text
import qualified Data.Char as C

import Squishy.Types

parseAST :: Text -> Either String AST
parseAST = P.parseOnly astParser

astParser :: Parser AST
astParser =
      parenParser
  <|> letParser
  <|> primParser
  <|> varParser

letParser :: Parser AST
letParser = do
  _ <- P.string "let"
  P.skipSpace
  n <- P.takeTill (\c -> C.isSpace c || c == '=')
  P.skipSpace
  _ <- P.char '='
  P.skipSpace
  x <- astParser
  P.skipSpace
  _ <- P.string "in"
  P.skipSpace
  y <- astParser
  P.skipSpace
  return $ Let n x y

parenParser :: Parser AST
parenParser = do
  _ <- P.char '('
  P.skipSpace
  n <- astParser
  P.skipSpace
  _ <- P.char ')'
  return n

primParser :: Parser AST
primParser = (P.string "True" >> return (Prim True))
  <|> (P.string "False" >> return (Prim False))

varParser :: Parser AST
varParser = Var <$> (P.takeWhile C.isAlpha) <*> pure ()
