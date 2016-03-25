-- |

module Language.Transpile  (
  transpile,
  transpileFile
  ) where

import qualified Context                as C
import           Language.Go.Parser     (goParse)
import           Language.Go.Syntax.AST
import qualified ParseTree              as PT
import           Print                  (showTree)
import qualified Report                 as R

transpileFile f = do
  s <- readFile f
  let
    p = goParse f s
  print p
  case p of
    Left s -> print s
    Right t -> putStrLn $ showTree $ transpile t

transpile :: GoSource -> C.Context PT.Decl
transpile = C.bindingsToContext1 . toBindings . getTopLevelDecl

toBindings :: [GoDecl] -> [C.Binding PT.Decl]
toBindings = zipWith (flip ($)) [(0 :: Int)..] . concatMap toBinding

toExpr :: GoExpr -> PT.Expr
toExpr (GoPrim prim) = PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 1)
toExpr (Go1Op (GoOp op) e) = PT.UnExpr R.PosTopLevel PT.NoType (unOp op) (toExpr e)
toExpr (Go2Op (GoOp op) e e') = PT.BinExpr R.PosTopLevel PT.NoType (binOp op) (toExpr e) (toExpr e')

binOp :: String -> PT.BinOp
binOp "*" = PT.BinMul
binOp "/"  = PT.BinDiv
binOp "?"  = PT.BinMod
binOp "**"  = PT.BinPow
binOp "+"  = PT.BinAdd
binOp "-"  = PT.BinSub
binOp "&&"  = PT.BinAnd
binOp "||"  = PT.BinOr
binOp "^"  = PT.BinXor
binOp "<"  = PT.BinLT
binOp ">"  = PT.BinGT
binOp "<="  = PT.BinLE
binOp ">="  = PT.BinGE
binOp "!="  = PT.BinNE
binOp "=="  = PT.BinEQ
binOp _ = error "welp"

unOp "+" = error "unsupported operator"
unOp "-" = PT.UnNeg
unOp "!" = PT.UnNot
unOp "^" = error "unsupported operator"
unOp "*" = error "unsupported operator"
unOp "&" = error "unsupported operator"
unOp "<-"= error "unsupported operator"
unOp _ = error "unrecognized operator"

mayToExpr :: Maybe GoExpr -> PT.Expr
mayToExpr Nothing = undefined
mayToExpr (Just e) = toExpr e

toBinding :: GoDecl -> [Int -> C.Binding PT.Decl]
toBinding = bindings
  where
    namespace = C.OtherNamespace
    bindings (GoConst cs) = cvSpecsToBindings toConst cs
    bindings (GoVar cs) = cvSpecsToBindings id cs
    bindings (GoType _) = [] -- cvTypeSpecToBindings
    bindings (GoFunc (GoFuncDecl i s b)) = []
    bindings (GoMeth _ ) = []
    toConst :: PT.Expr -> PT.Expr
    toConst = PT.ConstCheckExpr R.PosTopLevel
    cvSpecsToBindings f cs = do
      GoCVSpec ids typ exprs <- cs
      let
        exprs' = map Just exprs ++ repeat Nothing
      (GoId name, e) <- zip ids exprs'
      return $ \i -> C.Binding i name namespace R.Complete $ PT.ExprDecl R.PosTopLevel $ f $ mayToExpr e
