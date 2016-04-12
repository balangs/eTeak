{-# LANGUAGE FlexibleContexts #-}
-- |

module Language.SimpleGo.Balsa.Declarations (
  Binding, Context,
  typeBinding,
  buildBindings, buildContext
  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad (zipWithM)
import qualified Data.Foldable as F

import qualified Context   as C
import qualified ParseTree as PT
import qualified Report    as R

type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

pos :: R.Pos
pos = R.NoPos

typeBinding :: Int -> String -> PT.Type -> C.Binding PT.Decl
typeBinding i name typ' = C.Binding i name C.TypeNamespace R.Incomplete $ PT.TypeDecl pos $ PT.AliasType pos typ'

buildBindings :: (MonadError String m, Foldable f) =>
                (Int -> a -> m Binding) -> f a -> m [Binding]
buildBindings mb as = zipWithM mb [0..] $ F.toList as

buildContext :: (MonadError String m, Foldable f) =>
              (Int -> a -> m Binding) -> f a -> m Context
buildContext mb as = C.bindingsToContext1 <$> buildBindings mb as
