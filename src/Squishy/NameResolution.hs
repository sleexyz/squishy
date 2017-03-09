{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Squishy.NameResolution where

import Control.Monad.Error.Class
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

import Squishy.Types

resolveNames :: AST -> Either String ABT
resolveNames = resolveNamesWithContext emptyContext

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
