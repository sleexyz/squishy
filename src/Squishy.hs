{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Squishy where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.Function
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens.Plated as Plated

import Squishy.Types
import Squishy.Parser
import Squishy.NameResolution

isValidExpr :: ABT -> Bool
isValidExpr = isValidExprGivenMax (-1)
  where
    isValidExprGivenMax :: Index -> ABT -> Bool
    isValidExprGivenMax i = \case
      Var _ j | j > i -> False
      Var _ j | otherwise -> True
      Let _ y rest -> isValidExprGivenMax i y && isValidExprGivenMax (i + 1) rest
      Prim x -> True

-- * Semantics

subst :: Index -> ABT -> ABT -> ABT
subst i x = Plated.transform $ \case
  Var _ j | j == i -> x
  Var n j | j > i ->  Var n (j - 1)
  y -> y

reduce :: ABT -> ABT
-- reduce = rewrite step' -- TODO: figure out how to use rewrite
reduce x = head (iterateRight step x)

step' :: ABT -> Maybe ABT
step' = \case
  Let _ y rest -> Just (subst 0 y rest)
  x -> Nothing


getReductionSteps :: ABT -> [ABT]
getReductionSteps x = iterateRight step x

iterateRight :: (Alternative f) => (a -> Either a a) -> a -> f a
iterateRight f x = case f x of
  Left y -> pure y
  Right y -> iterateRight f y <|> pure y

-- | Left indicates no recursion
-- Right indicates recursion
step :: ABT -> Either ABT ABT
step = Left
  & handleLet

handleLet :: (ABT -> Either ABT ABT) -> (ABT -> Either ABT ABT)
handleLet fallback = \case
  Let _ y rest -> Right (subst 0 y rest)
  x -> fallback x

-- * Printing

printABT :: ABT -> Text
printABT = \case
  Var n _ -> n
  Let n x y  -> "let " <> n <> " = " <> printABT x <> " in\n" <> printABT y
  Prim True -> "True"
  Prim False -> "False"


integrate :: Text -> Text
integrate x = case (parseAST >=> resolveNames) x of
  Left x -> x
    & (Text.pack                  :: String -> Text)
  Right x -> x
    & (getReductionSteps          :: ABT -> [ABT])
    & (fmap printABT              :: [ABT] -> [Text])
    & (reverse                    :: [Text] -> [Text])
    & (Text.intercalate "\n\n"    :: [Text] -> Text)
