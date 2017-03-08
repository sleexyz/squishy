{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LetLang where

import Control.Applicative
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Char (isSpace)
import Data.Function
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid
import Data.Text (Text)
import Control.Monad.Error.Class

data ASTInternal i where
  Var :: Text -> i -> ASTInternal i
  Let :: Text -> ASTInternal i -> ASTInternal i -> ASTInternal i
  Prim :: Bool -> ASTInternal i
deriving instance (Eq i) => (Eq (ASTInternal i))
deriving instance (Show i) => (Show (ASTInternal i))

type AST = ASTInternal ()

newtype Index = MkIndex Int
  deriving (Num, Eq, Show, Ord)

type ABT = ASTInternal Index

-- FIXME: check for more than just validity of debrujin indicies
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

-- * Printing

printABT :: ABT -> Text
printABT = \case
  Var n _ -> n
  Let n x y  -> "let " <> n <> " = " <> printABT x <> " in\n" <> printABT y
  Prim True -> "True"
  Prim False -> "False"

-- * Name Resolution

resolveNames :: AST -> Either String ABT
resolveNames = resolveNamesWithContext emptyContext

data Context = MkContext {
  indexMap :: HashMap Text Index,
  lastIndex :: Index
}

emptyContext :: Context
emptyContext = MkContext { indexMap, lastIndex }
  where
    indexMap = HashMap.empty
    lastIndex = -1

addBinderToContext :: Text -> Context -> Context
addBinderToContext n ctx@MkContext { indexMap, lastIndex } = ctx { indexMap=indexMap', lastIndex=lastIndex'}
  where
    indexMap' = HashMap.insert n (lastIndex + 1) indexMap
    lastIndex' = lastIndex + 1

resolveNamesWithContext :: Context -> AST -> Either String ABT
resolveNamesWithContext ctx@MkContext{ indexMap, lastIndex } = \case
  Var n () -> case HashMap.lookup n indexMap of
    Just i -> return (Var n i)
    Nothing -> throwError ("Cannot resolve " <> show n)
  Let n x y -> do
    x' <- resolveNamesWithContext ctx x
    y' <- resolveNamesWithContext (addBinderToContext n ctx) y
    return (Let n x' y')
  Prim n -> return (Prim n)
