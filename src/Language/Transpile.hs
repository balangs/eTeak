{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Language.Transpile  (
  transpile,
  transpileFile
  ) where

import           Control.Monad.Except    (MonadError, throwError, runExcept)
import qualified Context                   as C
import           Language.Go.Parser        (goParse)
import           Language.SimpleGo.AST
import           Language.SimpleGo.Process (compile)
import qualified ParseTree                 as PT
import           Print                     (showTree)
import qualified Report                    as R
import Data.Text (unpack)
import qualified Data.Vector as U

transpileFile f = do
  s <- readFile f
  let
    p = goParse f s
  case p of
    Left s' -> print s'
    Right t -> do
      let p' = runExcept $ do
            simple <- compile t
            transpile simple
      case p' of
        Left s' -> print s'
        Right tree -> putStrLn $ showTree tree

transpile :: MonadError String m => Program -> m (C.Context PT.Decl)
transpile program = C.bindingsToContext1 <$> bindings
  where
    bindings = (toBindings . U.toList . declarations) program

toBindings :: forall m. MonadError String m => [Declaration] -> m [C.Binding PT.Decl]
toBindings = sequence . binds
  where
    binds :: [Declaration] -> [m (C.Binding PT.Decl)]
    binds = zipWith toBinding [(0 :: Int)..]


toExpr :: MonadError String m => Expr -> m PT.Expr
toExpr Zero = throwError "zero values are not supported"
toExpr (Prim prim) = return $ PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 1)
toExpr (UnOp Plus e) = PT.BinExpr R.PosTopLevel PT.NoType PT.BinAdd (PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Minus e) = PT.BinExpr R.PosTopLevel PT.NoType PT.BinSub (PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Not e) = PT.UnExpr R.PosTopLevel PT.NoType PT.UnNot <$> toExpr e
toExpr (UnOp o _) = throwError $ "operator " ++ show o ++ " is not supported"
toExpr (BinOp op e e') = PT.BinExpr R.PosTopLevel PT.NoType <$> binOp op <*> toExpr e <*> toExpr e'

binOp :: MonadError String m => BinOp -> m PT.BinOp
binOp Multiply = return PT.BinMul
binOp Quotient = return PT.BinDiv
binOp Remainder = return PT.BinMod
binOp Add = return PT.BinAdd
binOp Subtract = return PT.BinSub
binOp BitwiseAnd = return PT.BinAnd
binOp BitwiseOr  = return PT.BinOr
binOp BitwiseXor = return PT.BinXor
binOp LessThan = return PT.BinLT
binOp GreaterThan = return PT.BinGT
binOp LessThanEqual = return PT.BinLE
binOp GreaterThanEqual = return PT.BinGE
binOp NotEqual = return PT.BinNE
binOp Equal  = return PT.BinEQ
binOp o = throwError $ "operator " ++ show o ++ " is not supported"

toBinding :: MonadError String m => Int -> Declaration -> m (C.Binding PT.Decl)
toBinding i = bindings
  where
    namespace = C.OtherNamespace
    bindings (Const (Id id') _ expr) = C.Binding i (unpack id') namespace R.Complete . PT.ExprDecl R.PosTopLevel <$> toExpr expr
    bindings (Var (Id id') _ expr) = C.Binding i (unpack id') namespace R.Complete . PT.ExprDecl R.PosTopLevel <$> toExpr expr
    bindings (Type (Id _) _) = throwError "type declarations are not supported"
    bindings (Func (Id _) _ _) = throwError "function declarations are not supported"
