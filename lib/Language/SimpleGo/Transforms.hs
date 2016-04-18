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
    f (UnOp o e) = UnOp o <$> go e
    f (BinOp o e e') = BinOp o <$> go e <*> go e'
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
        prim' (Paren e) = Paren <$> go e
        prim' (Cast t e) = Cast t <$> go e
        prim' l@(New _) = pure l
        prim' (Make t es) = Make t <$> traverse go es
        prim' l@(Select _ _) = pure l
        prim' (Index p e) = Index <$> prim' p <*> go e
        prim' (Slice p me me') = Slice <$> prim' p <*> traverse go me <*> traverse go me'
        prim' (TA p t) = TA <$> prim' p <*> pure t
        prim' (Call p es) = Call <$> prim' p <*> traverse go es

expr :: (Expr -> Expr) -> Expr -> Expr
expr f expr = runIdentity (exprM (return . f) expr)

replaceIota :: Integer -> Expr -> Expr
replaceIota i = expr f
  where
    f (Prim (Qual (Id "iota"))) = Prim (LitInt i)
    f x = x
