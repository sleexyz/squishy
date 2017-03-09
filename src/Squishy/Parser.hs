{-# LANGUAGE OverloadedStrings #-}
module Squishy.Parser where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text
import Data.Char (isSpace)

import Squishy.Types

parseAST :: Text -> Either String AST
parseAST = P.parseOnly astParser

astParser :: Parser AST
astParser =
      letParser
  <|> primParser
  <|> varParser
  where
    letParser :: Parser AST
    letParser = do
      P.string "let"
      P.skipSpace
      n <- P.takeTill isSpace
      P.skipSpace
      P.char '='
      P.skipSpace
      x <- astParser
      P.skipSpace
      P.string "in"
      P.skipSpace
      y <- astParser
      P.skipSpace
      return $ Let n x y

    primParser :: Parser AST
    primParser = (P.string "True" >> return (Prim True))
      <|> (P.string "False" >> return (Prim False))

    varParser :: Parser AST
    varParser = Var <$> (P.takeWhile (/= ' ')) <*> pure ()
