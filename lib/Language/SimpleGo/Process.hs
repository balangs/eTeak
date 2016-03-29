{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Probably need to make this an internal module at some point instead

module Language.SimpleGo.Process where

import Control.Monad (join)
import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.List    (runListT, ListT(..))
import qualified Language.Go.Syntax.AST as Go
import qualified Language.SimpleGo.AST  as S
import qualified Data.Vector               as U
import Data.String (fromString)
import Control.Monad.Trans (lift)

compile :: MonadError String m => Go.GoSource -> m S.Program
compile = fmap (S.Program . join) . U.mapM compileDecl . U.fromList . Go.getTopLevelDecl

expandDeclarations :: forall m c. MonadError String m =>
                     (S.Id -> S.Type -> S.Expr -> c) -> [Go.GoCVSpec] -> m [c]
expandDeclarations ctor cs = runListT f
  where
    f :: ListT m c
    f = do
      Go.GoCVSpec ids typ exprs <- ListT $ return cs
      case typ of
        Nothing -> throwError "explicit types are required"
        Just t -> do
          t' <- asType t
          (Go.GoId name, e) <- ListT $ return $ zip ids (map Just exprs ++ repeat Nothing)
          e' <- mayToExpr e
          return $ ctor (S.Id $ fromString name) t' e'

mayToExpr Nothing = return S.Zero
mayToExpr (Just e) = toExpr e

toExpr :: MonadError String m => Go.GoExpr -> m S.Expr
toExpr (Go.GoPrim prim) = S.Prim <$> toPrim prim
toExpr (Go.Go1Op (Go.GoOp op) e) = S.UnOp <$> parseUnOp op <*> toExpr e
toExpr (Go.Go2Op (Go.GoOp op) e e') = S.BinOp <$> parseBinOp op <*> toExpr e <*> toExpr e'

toLit :: MonadError String m => Go.GoLit -> m S.Prim
toLit (Go.GoLitInt  _ i) = return $ S.LitInt i
toLit (Go.GoLitReal _ f) = return $ S.LitReal f
toLit (Go.GoLitImag _ f) = return $ S.LitImag f
toLit (Go.GoLitChar _ c) = return $ S.LitChar c
toLit (Go.GoLitStr  _ s) = return $ S.LitStr s
toLit s = throwError $ "unsupported literal: \"" ++ show s ++ "\""

toPrim (Go.GoLiteral lit) = toLit lit
toPrim s = throwError $ "unsupported literal: \"" ++ show s ++ "\""

asType :: MonadError String m => Go.GoType -> m S.Type
asType (Go.GoTypeName _ (Go.GoId i)) = return $ S.TypeName $ S.Id $ fromString i
asType s = throwError $ "unsupported type: \"" ++ show s ++ "\""

compileDecl :: MonadError String m => Go.GoDecl -> m (U.Vector S.Declaration)
compileDecl (Go.GoConst cs) = U.fromList <$> expandDeclarations S.Const cs

parseUnOp :: MonadError String m => String -> m  S.UnOp
parseUnOp "+" = return S.Plus
parseUnOp "-" = return S.Minus
parseUnOp "^" = return S.Complement
parseUnOp "!" = return S.Not
parseUnOp "*" = return S.Address
parseUnOp "&" = return S.Reference
parseUnOp "<-" = return S.Receive
parseUnOp s = throwError $ "unsupported unary operator \"" ++ s ++ "\""

parseBinOp :: MonadError String m => String -> m S.BinOp
parseBinOp "||" = return S.LogicalOr
parseBinOp "&&" = return S.LogicalAnd
parseBinOp "==" = return S.Equal
parseBinOp "!=" = return S.NotEqual
parseBinOp "<" = return S.LessThan
parseBinOp "<=" = return S.LessThanEqual
parseBinOp ">" = return S.GreaterThan
parseBinOp ">=" = return S.GreaterThanEqual
parseBinOp "^" = return S.BitwiseXor
parseBinOp "|" = return S.BitwiseOr
parseBinOp "+" = return S.Add
parseBinOp "-" = return S.Subtract
parseBinOp "*" = return S.Multiply
parseBinOp "/" = return S.Quotient
parseBinOp "%" = return S.Remainder
parseBinOp "<<" = return S.LeftShift
parseBinOp ">>" = return S.RightShift
parseBinOp "&" = return S.BitwiseAnd
parseBinOp "&^" = return S.AndNot
parseBinOp o = throwError $ "unsupported binary operation \"" ++ o ++ "\""
