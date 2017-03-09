{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Squishy.Types where

import Data.Text
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Data
import Control.Lens.Plated

data ASTInternal index where
  Var :: Text -> index -> ASTInternal index
  Let :: Text -> ASTInternal index -> ASTInternal index -> ASTInternal index
  Prim :: Bool -> ASTInternal index
deriving instance (Eq index) => (Eq (ASTInternal index))
deriving instance (Show index) => (Show (ASTInternal index))
deriving instance (Data index) => (Data (ASTInternal index))
deriving instance (Typeable (ASTInternal index))

instance Plated ABT

type AST = ASTInternal ()

newtype Index = MkIndex Int
  deriving (Num, Eq, Show, Ord, Data)

type ABT = ASTInternal Index

data Context = MkContext {
  indexMap :: HashMap Text Index,
  lastIndex :: Index
}
