{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Language.Transpile  (
  transpile,
  transpileFile
  ) where

import           Control.Monad.Except    (MonadError, throwError, runExcept)
import Control.Monad (zipWithM)
import qualified Context                   as C
import           Language.Go.Parser        (goParse)
import           Language.SimpleGo.AST
import           Language.SimpleGo.Process (compile)
import qualified ParseTree                 as PT
import           Print                     (showTree)
import qualified Report                    as R
import Data.Text (unpack)
import qualified Data.Vector as U
import qualified Data.Foldable as F
import qualified Bits as B


type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

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
        Left s' -> putStrLn s'
        Right tree -> writeFile (f ++ "-tree") $ showTree tree

transpile :: MonadError String m => Program -> m Context
transpile = buildContext toBinding . declarations

buildContext :: forall m f a. (MonadError String m, Foldable f) =>
              (Int -> a -> m Binding) -> f a -> m Context
buildContext mb as = C.bindingsToContext1 <$> bindings
  where
    bindings :: m [Binding]
    bindings = zipWithM mb [0..] $ F.toList as

toExpr :: MonadError String m => Expr -> m PT.Expr
toExpr Zero = throwError "zero values are not supported"
toExpr (Prim prim) = fromPrim prim
toExpr (UnOp Plus e) = PT.BinExpr R.PosTopLevel PT.NoType PT.BinAdd (PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Minus e) = PT.BinExpr R.PosTopLevel PT.NoType PT.BinSub (PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Not e) = PT.UnExpr R.PosTopLevel PT.NoType PT.UnNot <$> toExpr e
toExpr (UnOp o _) = throwError $ "operator " ++ show o ++ " is not supported"
toExpr (BinOp op e e') = PT.BinExpr R.PosTopLevel PT.NoType <$> binOp op <*> toExpr e <*> toExpr e'

fromPrim :: MonadError String m => Prim -> m PT.Expr
fromPrim (LitInt i) = return $ PT.ValueExpr R.PosTopLevel PT.NoType (PT.IntValue i)
-- TODO maybe fix this parser? Should it be qual here?
fromPrim (Qual id') = return $ PT.NameExpr R.PosTopLevel (unId id')
fromPrim s = throwError $ "unsupported primitive" ++ show s

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

unId :: Id -> String
unId (Id id') = unpack id'

toBinding :: MonadError String m => Int -> Declaration -> m Binding
toBinding i = bindings
  where
    namespace = C.OtherNamespace
    bindings (Const id' _ expr) = C.Binding i (unId id') namespace R.Incomplete . PT.ExprDecl R.PosTopLevel <$> toExpr expr
    bindings (Var id' _ expr) = C.Binding i (unId id') namespace R.Incomplete . PT.ExprDecl R.PosTopLevel <$> toExpr expr
    bindings (Type _ _) = throwError "type declarations are not supported"
    bindings (Func id' sig block) = C.Binding i (unId id') C.ProcNamespace R.Incomplete <$> decl
      where
        decl = if isProc sig
               then procedure
               else throwError "non procedure functions are not supported"
        isProc = const True
        procedure = PT.ProcDecl R.PosTopLevel
          <$> sigContext sig
          <*> pure []  -- not sure what annotations are
          <*> blockCmd block

asType ::  MonadError String m => Type -> m PT.Type
asType (TypeName id') = return $ PT.NameType R.PosTopLevel (unId id')
asType t = throwError $ "usupported type " ++ show t

sigTypeDecl :: MonadError String m => Type -> m PT.Decl
sigTypeDecl (Channel Bidirectional typ) = PT.ChanDecl R.PosTopLevel <$> asType typ
sigTypeDecl (Channel Input typ) = PT.PortDecl R.PosTopLevel PT.Input <$> asType typ
sigTypeDecl (Channel Output typ) = PT.PortDecl R.PosTopLevel PT.Output <$> asType typ
sigTypeDecl t = throwError $ "unsupported signature type " ++ show t

paramBinding :: MonadError String m => Int -> Param -> m Binding
paramBinding i (Param id' t) = C.Binding i (unId id') C.OtherNamespace R.Incomplete <$> sigTypeDecl t

sigContext :: MonadError String m => Signature -> m Context
sigContext (Signature inputs _) = buildContext paramBinding inputs

blockCmd :: MonadError String m => Block -> m PT.Cmd
blockCmd (Block statements) = seqCmd $ U.toList statements

seqCmd :: MonadError String m => [Statement] -> m PT.Cmd
seqCmd ss = collapsePars <$> mapM parCmd ss

caseCmds :: MonadError String m => [Case Expr] -> m ([PT.CaseCmdGuard], PT.Cmd)
caseCmds cs = (,) <$> explicits <*> def
  where
    isDefault (Default _) = True
    isDefault _ = False
    def = case (filter isDefault cs) of
      [] -> return PT.NoCmd
      (Default ss : _ ) -> seqCmd ss
    explicits = mapM guard $ filter (not . isDefault) cs
    guard (Case es ss) = PT.CaseCmdGuard R.PosTopLevel <$> mapM match es <*> seqCmd ss
    match e = PT.ExprCaseMatch R.PosTopLevel <$> toExpr e


data Par a = Par a | Seq a

parCmd :: MonadError String m => Statement -> m (Par PT.Cmd)
parCmd c@(Go _) = Par <$> cmd c
parCmd c = Seq <$> cmd c

collapsePars :: [Par PT.Cmd] -> PT.Cmd
collapsePars = go
  where
    isPar :: Par a -> Bool
    isPar (Par _) = True
    isPar _ = False

    seqs :: [Par a] -> ([Par a], [Par a])
    seqs = break isPar

    pars :: [Par a] -> ([Par a], [Par a])
    pars = span isPar

    undo :: Par a -> a
    undo (Par a) = a
    undo (Seq a) = a

    go  [] = PT.NoCmd
    go [Par c] = c
    go [Seq c] = c
    go (Par c : next) = p
      where
        (ps, ss) = pars next
        p = PT.ParCmd R.PosTopLevel (c:(map undo ps ++ [s]))
        s = collapsePars ss
    go (Seq c : next) = s
      where
        (ss, ps) = seqs next
        s = PT.SeqCmd R.PosTopLevel (c:(map undo ss ++ [p]))
        p = collapsePars ps

cmd :: forall m . MonadError String m => Statement -> m PT.Cmd
-- for { } construct is identical to loop ... end
cmd (For (ForWhile Nothing) block) = PT.LoopCmd R.PosTopLevel <$> blockCmd block
-- <var> <- <chan>
cmd (For (ForThree (SimpVar id (Prim (LitInt start)))
                   (Just (BinOp LessThan (Prim (Qual id')) (Prim (LitInt end))))
                   (Inc (Prim (Qual id'')))) block)
  | id' == id && id'' == id = PT.ForCmd R.PosTopLevel PT.Seq interval <$> c <*> blockCmd block
  where
    interval = (PT.Interval (start, end) PT.NoType)
    c :: m Context
    c = buildContext b [id]
    b :: Int -> Id -> m Binding
    b i id = return $ C.Binding 1 (unId id) C.OtherNamespace R.Incomplete (PT.ParamDecl R.PosTopLevel False PT.NoType)
--cmd (For (ForThree s (Just e) s')) =
cmd (Simple (SimpVar id' (UnOp Receive (Prim (Qual chan))))) = PT.InputCmd R.PosTopLevel
  <$> pure (PT.NameChan R.PosTopLevel (unId chan))
  <*> pure (PT.NameLvalue R.PosTopLevel (unId id'))
-- <chan> <- <expr>
cmd (Simple (Send (Prim (Qual chan)) prim)) = PT.OutputCmd R.PosTopLevel
     <$> pure (PT.NameChan R.PosTopLevel (unId chan))
     <*> toExpr prim
cmd (Switch (Cond Nothing (Just expr)) cases) = do
  (cs, def) <- caseCmds cases
  PT.CaseCmdE R.PosTopLevel <$> toExpr expr <*> pure cs <*> pure def
cmd (Go p) = exprCmd p
cmd (Simple (SimpleExpr (Prim (Call (Qual (Id "print")) es)))) = PT.PrintCmd R.PosTopLevel <$> mapM toExpr es
cmd (Simple (SimpleExpr (Prim (Call (Qual id') es)))) = (c . map PT.ExprProcActual)  <$> mapM toExpr es
  where
    c = PT.CallCmd R.PosTopLevel (PT.NameCallable R.PosTopLevel (unId id')) C.EmptyContext
cmd s = throwError $ "unsupported statement " ++ show s

exprCmd :: MonadError String m => Expr -> m PT.Cmd
exprCmd (Prim (Call (LitFunc sig block) es)) = PT.BlockCmd R.PosTopLevel
  <$> buildContext toBinding [Func (Id "$") sig block]
  <*> call
  where
    call = PT.CallCmd R.PosTopLevel (PT.NameCallable R.PosTopLevel "$") C.EmptyContext . map PT.ExprProcActual <$> mapM toExpr es
exprCmd e = throwError $ "unsupported expression " ++ show e
