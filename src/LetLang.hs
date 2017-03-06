{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TODO: move to Parsec, so we can use
-- a state monad to contain deBrujin information
module LetLang where

import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Text (Parser)
import Data.Function
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Attoparsec.Text as P

-- * Syntax

data ABT where
  Var :: Text -> Int -> ABT
  Let :: Text -> ABT -> ABT -> ABT 
  Prim :: Bool -> ABT
deriving instance (Eq ABT)
deriving instance (Show ABT)

-- FIXME: check for more than just validity of debrujin indicies
isValidExpr :: ABT -> Bool
isValidExpr = isValidExprGivenMax (-1)
  where
    isValidExprGivenMax :: Int -> ABT -> Bool
    isValidExprGivenMax i = \case
      Var _ j | j > i -> False
      Var _ j | otherwise -> True
      Let _ y rest -> isValidExprGivenMax i y && isValidExprGivenMax (i + 1) rest
      Prim x -> True

-- * Semantics

subst :: Int -> ABT -> ABT -> ABT
subst i x = \case
  Var _ j | j == i ->  x
  Var n j | j > i ->  Var n (j - 1)
  Var n j | otherwise -> Var n j
  Let n y rest -> Let n (subst i x y) (subst i x rest)
  Prim x -> Prim x

reduce :: ABT -> ABT
reduce = id
  & handleLet
  where
    handleLet :: (ABT -> ABT) -> (ABT -> ABT)
    handleLet fallback = \case
      Let _ y rest -> reduce $ subst 0 y rest
      x -> fallback x

-- * Parsing

parseABT :: Text -> Either String ABT
parseABT = P.parseOnly astParser

astParser :: Parser ABT
astParser = 
      letParser
  <|> primParser
  <|> varParser 
  where
    letParser :: Parser ABT
    letParser = do
      P.string "let" >> P.skipSpace
      n <- P.takeTill isSpace
      P.skipSpace >> P.char '=' >> P.skipSpace
      P.skipSpace >> P.char '{' >> P.skipSpace
      x <- astParser
      P.skipSpace >> P.char '}' >> P.skipSpace
      P.string "in"
      P.skipSpace >> P.char '{' >> P.skipSpace
      y <- astParser
      P.skipSpace >> P.char '}' >> P.skipSpace
      return $ Let n x y

    primParser :: Parser ABT
    primParser = (P.string "True" >> return (Prim True))
      <|> (P.string "False" >> return (Prim False))

    -- FIXME: convert ast to abt, to get right deBrujin index
    varParser :: Parser ABT
    varParser = Var <$> (P.takeWhile (/= ' ')) <*> pure 0 

-- * Printing

printABT :: ABT -> Text
printABT = \case
  Var n _ -> n
  Let n x y  -> "let " <> n <> " = { " <> printABT x <> " } in { " <> printABT y <> " }"
  Prim True -> "True"
  Prim False -> "False"
