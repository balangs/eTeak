{-# LANGUAGE FlexibleContexts #-}
-- |

module Language.SimpleGo.Balsa.Declarations (
  Binding, Context,
  typeBinding,
  buildBindings, buildContext,
  Decl(..), declContext
  ) where

import           Control.Monad.Except (MonadError)
import           Control.Monad (zipWithM)
import qualified Data.Foldable as F
import qualified Data.Text as T

import qualified Context   as C
import qualified ParseTree as PT
import qualified Report    as R

type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

data Decl = Type PT.Type
          | Const PT.Expr
          | Var PT.Type (Maybe PT.Expr)
          | Chan PT.Type
          | Proc Context PT.Cmd
          -- For parameters
          | In PT.Type
          | Out PT.Type
          | Param PT.Type
          deriving (Show, Eq)

pos :: R.Pos
pos = R.NoPos

typeBinding :: Int -> String -> PT.Type -> Binding
typeBinding i name typ' = C.Binding i name C.TypeNamespace R.Incomplete $ PT.TypeDecl pos $ PT.AliasType pos typ'


buildBindings :: (MonadError String m, Foldable f) =>
                (Int -> a -> m Binding) -> f a -> m [Binding]
buildBindings mb as = zipWithM mb [0..] $ F.toList as

buildContext :: (MonadError String m, Foldable f) =>
              (Int -> a -> m Binding) -> f a -> m Context
buildContext mb as = C.bindingsToContext1 <$> buildBindings mb as

declContext :: [(T.Text, Decl)] -> Context
declContext decls = C.bindingsToContext1 $ zipWith binding [0..] decls
  where
    b (Type t) = (C.TypeNamespace, PT.TypeDecl pos $ PT.AliasType pos t)
    b (Const e) = (C.OtherNamespace, PT.ExprDecl pos e)
    b (Var _ (Just e)) = (C.OtherNamespace, PT.ExprDecl pos e)
    b (Var t Nothing) = (C.OtherNamespace, PT.VarDecl pos t)
    b (Chan t) = (C.OtherNamespace, PT.ChanDecl pos t)
    b (Proc c cmd) = (C.ProcNamespace, PT.ProcDecl pos c [] cmd)
    -- For parameters
    b (In t) = (C.OtherNamespace, PT.PortDecl pos PT.Input t)
    b (Out t) = (C.OtherNamespace, PT.PortDecl pos PT.Output t)
    b (Param t) = (C.OtherNamespace, PT.ParamDecl pos True t)
    binding i (n, a) = C.Binding i (T.unpack n) namespace R.Incomplete decl
      where
        (namespace, decl) = b a
