{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.Transforms (
  exprM, expr, replaceIota
  ) where

import           Control.Monad.Identity (runIdentity)
import           Language.SimpleGo.AST

-- Change expressions
-- TODO: Will not alter function bodies
exprM :: Monad m => (Expr -> m Expr)
        -> Expr -> m Expr
exprM ma e = f e >>= ma
  where
    go = exprM ma
    f Zero = ma Zero
    f (UnOp o ex) = UnOp o <$> go ex
    f (BinOp o ex ex') = BinOp o <$> go ex <*> go ex'
    f (Prim prim) = Prim <$> prim' prim
      where
        prim' l@(LitInt  _) = pure l
        prim' l@(LitReal _) = pure l
        prim' l@(LitImag _) = pure l
        prim' l@(LitChar _) = pure l
        prim' l@(LitStr _) = pure l
        prim' l@(LitFunc _ _) = pure l
        prim' l@(Qual _) = pure l
        prim' l@(Method _ _) = pure l
        prim' (Paren ex) = Paren <$> go ex
        prim' (Cast t ex) = Cast t <$> go ex
        prim' l@(New _) = pure l
        prim' (Make t exs) = Make t <$> traverse go exs
        prim' l@(Select _ _) = pure l
        prim' (Index p ex) = Index <$> prim' p <*> go ex
        prim' (Slice p me me') = Slice <$> prim' p <*> traverse go me <*> traverse go me'
        prim' (TA p t) = TA <$> prim' p <*> pure t
        prim' (Call p exs) = Call <$> prim' p <*> traverse go exs

expr :: (Expr -> Expr) -> Expr -> Expr
expr f ex = runIdentity (exprM (return . f) ex)

replaceIota :: Integer -> Expr -> Expr
replaceIota i = expr f
  where
    f (Prim (Qual (Id "iota"))) = Prim (LitInt i)
    f x = x
