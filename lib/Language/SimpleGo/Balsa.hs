{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Language.SimpleGo.Balsa  (
  compile,
  synthesizeFile
  ) where

import           Control.Monad.Except (MonadError, throwError, ExceptT(..), runExceptT)
import           Control.Monad (zipWithM)
import           Control.Monad.State (modify', MonadState, runStateT)
import           Control.Monad.Trans (liftIO)
import           Data.Text (unpack)
import qualified Data.Foldable as F
import qualified Data.Vector as U

import qualified Context as C
import           Language.Helpers (bind, eval, finish, teak, writeTeak, writeGates)
import           Language.SimpleGo.AST
import           Language.SimpleGo.Process (compileFile)
import qualified ParseTree as PT
import           Print (showTree)
import qualified Report as R

type Binding = C.Binding PT.Decl
type Context = C.Context PT.Decl

pos = R.PosTopLevel

synthesizeFile :: FilePath -> IO ()
synthesizeFile f = do
  e <- runExceptT go
  case e of
    Left s -> putStrLn s
    Right a -> return a
  where
    write s t = liftIO $ do
      putStrLn $ "running " ++ s
      writeFile (f ++ ".tree-" ++ s) $ showTree t
    go = do
      goSource <- ExceptT $ compileFile f
      balsa <- compile goSource
      write "parse" balsa
      bound <- bind balsa
      write "bind" bound
      evaled <- eval bound
      write "eval" evaled
      finished <- finish evaled
      write "finish" finished
      teak' <- teak [] finished
      writeTeak (f ++ ".teak") teak'
      writeGates f teak'


compile :: MonadError String m => Program -> m Context
compile program = C.bindingsToContext1 <$> all
  where
    all = do
      d <- declared
      return $ builtins ++ d
    declared = (buildBindings toBinding . declarations) program
    builtins :: [C.Binding PT.Decl]
    builtins = [
      (C.Binding 4 "token" C.TypeNamespace R.Incomplete (PT.TypeDecl pos (PT.AliasType pos (PT.NumType pos (PT.ValueExpr pos (PT.Bits 1) (PT.IntValue 0)) False)))),
      (C.Binding 6 "bit" C.TypeNamespace R.Incomplete (PT.TypeDecl pos (PT.AliasType pos (PT.NumType pos (PT.ValueExpr pos (PT.Bits 1) (PT.IntValue 1)) False)))),
      (C.Binding 7 "String" C.TypeNamespace R.Incomplete (PT.TypeDecl pos (PT.AliasType pos (PT.BuiltinType "String")))),
      (C.Binding 8 "byte" C.TypeNamespace R.Incomplete (PT.TypeDecl pos (PT.AliasType pos byte))),
      (C.Binding 8 "bool" C.TypeNamespace R.Incomplete (PT.TypeDecl pos (PT.AliasType pos bool)))
      ]


buildBindings :: (MonadError String m, Foldable f) =>
                (Int -> a -> m Binding) -> f a -> m [Binding]
buildBindings mb as = zipWithM mb [0..] $ F.toList as

buildContext :: (MonadError String m, Foldable f) =>
              (Int -> a -> m Binding) -> f a -> m Context
buildContext mb as = C.bindingsToContext1 <$> buildBindings mb as

byte :: PT.Type
byte = PT.NumType pos (PT.ValueExpr pos (PT.Bits 4) (PT.IntValue 8)) False

bool :: PT.Type
bool = PT.Bits 1

true :: PT.Value
true = PT.IntValue 1

false :: PT.Value
false = PT.IntValue 0

string :: PT.Type
string = PT.NameType pos "String"


toExpr :: MonadError String m => Expr -> m PT.Expr
toExpr Zero = throwError "zero values are not supported"
toExpr (Prim prim) = fromPrim prim
toExpr (UnOp Plus e) = PT.BinExpr pos PT.NoType PT.BinAdd (PT.ValueExpr pos byte (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Minus e) = PT.BinExpr pos PT.NoType PT.BinSub (PT.ValueExpr pos byte (PT.IntValue 0)) <$> toExpr e
toExpr (UnOp Not e) = PT.UnExpr pos PT.NoType PT.UnNot <$> toExpr e
toExpr (UnOp o _) = throwError $ "operator " ++ show o ++ " is not supported"
toExpr (BinOp op e e') = PT.BinExpr pos PT.NoType <$> binOp op <*> toExpr e <*> toExpr e'

fromPrim :: MonadError String m => Prim -> m PT.Expr
fromPrim (LitInt i) = return $ PT.ValueExpr pos byte (PT.IntValue i)
fromPrim (LitStr s) = return $ PT.ValueExpr pos string (PT.StringValue s)
-- TODO maybe fix this parser? Should it be qual here?
fromPrim (Qual id') = return $ PT.NameExpr pos (unId id')
fromPrim s = throwError $ "unsupported primitive" ++ show s

binOp :: MonadError String m => Binary -> m PT.BinOp
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
    bindings (Const id' _ expr) = C.Binding i (unId id') namespace R.Incomplete . PT.ExprDecl pos <$> toExpr expr
    bindings (Var id' typ Zero) = C.Binding i (unId id') namespace R.Incomplete . PT.VarDecl pos <$> asType typ
    bindings (Var id' _ (Prim (Make (Channel Bidirectional typ) []))) = C.Binding i (unId id') namespace R.Incomplete . PT.ChanDecl pos <$> asType typ
    bindings (Var id' _ expr) = C.Binding i (unId id') namespace R.Incomplete . PT.ExprDecl pos <$> toExpr expr
    bindings (Type _ _) = throwError "type declarations are not supported"
    bindings (Func id' sig block) = C.Binding i (unId id') C.ProcNamespace R.Incomplete <$> decl
      where
        decl = if isProc sig
               then procedure
               else throwError "non procedure functions are not supported"
        isProc = const True
        procedure = PT.ProcDecl pos
          <$> sigContext sig
          <*> pure []  -- not sure what annotations are
          <*> blockCmd block

asType ::  MonadError String m => Type -> m PT.Type
asType (TypeName id') = return $ PT.NameType pos (unId id')
asType t = throwError $ "usupported type " ++ show t

sigTypeDecl :: MonadError String m => Type -> m PT.Decl
sigTypeDecl (Channel Input typ) = PT.PortDecl pos PT.Input <$> asType typ
sigTypeDecl (Channel Output typ) = PT.PortDecl pos PT.Output <$> asType typ
sigTypeDecl (Channel Bidirectional typ) = PT.ChanDecl pos <$> asType typ
sigTypeDecl t@(TypeName _) = PT.VarDecl pos <$> asType t
sigTypeDecl t = throwError $ "unsupported signature type " ++ show t

paramBinding :: MonadError String m => Int -> Param -> m Binding
paramBinding i (Param id' t) = C.Binding i (unId id') C.OtherNamespace R.Incomplete <$> sigTypeDecl t

sigContext :: MonadError String m => Signature -> m Context
sigContext (Signature inputs _) = buildContext paramBinding inputs

blockCmd :: MonadError String m => Block -> m PT.Cmd
blockCmd (Block statements) = seqCmd $ U.toList statements

seqCmd :: MonadError String m => [Statement] -> m PT.Cmd
seqCmd ss = do
  (cmd', bindings) <- runStateT (collapsePars <$> mapM parCmd ss) []
  case bindings of
    [] -> return cmd'
    bs -> PT.BlockCmd pos <$> c <*> pure cmd'
      where
        c = buildContext toBinding bs


caseCmds :: MonadError String m => [Case Expr] -> m ([PT.CaseCmdGuard], PT.Cmd)
caseCmds cs = (,) <$> explicits <*> def
  where
    isDefault (Default _) = True
    isDefault _ = False
    def = case filter isDefault cs of
      [] -> return PT.NoCmd
      (Default ss : _ ) -> seqCmd ss
      _ -> error "refactor `caseCmds` to be total"
    explicits = mapM cmdGuard $ filter (not . isDefault) cs
    cmdGuard (Case es ss) = PT.CaseCmdGuard pos <$> mapM match es <*> seqCmd ss
    cmdGuard _ = error "refactor `caseCmds` to be total"
    match e = PT.ExprCaseMatch pos <$> toExpr e


data Par a = Par a | Seq a

parCmd :: (MonadState [Declaration] m, MonadError String m) => Statement -> m (Par PT.Cmd)
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


    justCmds = filter (not . isNoCmd)
      where
        isNoCmd PT.NoCmd = True
        isNoCmd _ = False

    go  [] = PT.NoCmd
    go [Par c] = c
    go [Seq c] = c
    go (Par c : next) = p
      where
        (ps, ss) = pars next
        p = PT.ParCmd pos $ justCmds (c:(map undo ps ++ [s]))
        s = collapsePars ss
    go (Seq c : next) = s
      where
        (ss, ps) = seqs next
        s = PT.SeqCmd pos $ justCmds (c:(map undo ss ++ [p]))
        p = collapsePars ps

paramNameBinding :: MonadError String m => Int -> Id -> m Binding
paramNameBinding i name = return $ C.Binding i (unId name) C.OtherNamespace R.Incomplete (PT.ParamDecl pos False byte)

declare :: (MonadState [a] m) => a -> m ()
declare d = modify' (`mappend` [d])

cmd :: forall m . (MonadState [Declaration] m, MonadError String m) => Statement -> m PT.Cmd
-- for { } construct is identical to loop ... end
cmd (ForWhile Nothing block) = PT.LoopCmd pos <$> blockCmd block
-- for i := <start>; i < <end>; i++ { } is equivalent to a range
cmd (ForThree
     (SimpVar id (Prim (LitInt start)))
     (Just (BinOp LessThan (Prim (Qual id')) (Prim (LitInt end))))
     (Inc (Prim (Qual id'')))
     block)
  | id' == id && id'' == id = PT.ForCmd pos PT.Seq interval <$> c <*> blockCmd block
  where
    interval = PT.Interval (start, pred end) byte
    c :: m Context
    c = buildContext paramNameBinding [id]
cmd (If (Cond Nothing (Just expr)) block s) = PT.CaseCmdE pos <$> toExpr expr <*> fmap return trueBlock <*> s'
  where
    s' = maybe (return PT.NoCmd) cmd s
    trueBlock = PT.CaseCmdGuard pos [PT.ExprCaseMatch pos (PT.ValueExpr pos bool true)] <$> blockCmd block

-- <var> := <- <chan>
cmd (Simple (SimpVar id' (UnOp Receive (Prim (Qual chan))))) = do
  -- need actual type checking here :/
  declare $ Var id' (TypeName (Id "byte")) Zero
  return $ PT.InputCmd pos (PT.NameChan pos (unId chan)) (PT.NameLvalue pos (unId id'))
-- <chan> <- <expr>
cmd (Simple (Send (Prim (Qual chan)) prim)) = PT.OutputCmd pos
     <$> pure (PT.NameChan pos (unId chan))
     <*> toExpr prim
cmd (Switch (Cond Nothing (Just expr)) cases) = do
  (cs, def) <- caseCmds cases
  PT.CaseCmdE pos <$> toExpr expr <*> pure cs <*> pure def
cmd (Go p) = exprCmd p
cmd (Simple (SimpleExpr (Prim (Call (Qual (Id "print")) es)))) = PT.PrintCmd pos <$> mapM toExpr es
cmd (Simple (SimpleExpr (Prim (Call (Qual id') es)))) = (c . map PT.ExprProcActual)  <$> mapM toExpr es
  where
    c = PT.CallCmd pos (PT.NameCallable pos (unId id')) C.EmptyContext
cmd (StmtDecl decl) = do
  declare decl
  return PT.NoCmd
cmd (StmtBlock block) = blockCmd block
cmd (StmtSelect cases) = PT.SelectCmd pos False <$> traverse chanCase cases
  where
    chanCase (Case as stmnts) = PT.ChanGuard pos <$> traverse chanGuard as <*> pure C.EmptyContext <*> seqCmd stmnts
      where
        chanGuard (ChanRecv Nothing (UnOp Receive (Prim (Qual id')))) = return $ PT.NameChan pos (unId id')
    chanCase (Default _) = throwError "default select not supported"
cmd s = throwError $ "unsupported statement " ++ show s

exprCmd :: MonadError String m => Expr -> m PT.Cmd
exprCmd (Prim (Call (LitFunc sig block) es)) = PT.BlockCmd pos
  <$> buildContext toBinding [Func (Id "$") sig block]
  <*> call
  where
    call = PT.CallCmd pos (PT.NameCallable pos "$") C.EmptyContext . map PT.ExprProcActual <$> mapM toExpr es
exprCmd (Prim (Call (Qual id') es)) = PT.CallCmd pos (PT.NameCallable pos (unId id')) C.EmptyContext . map PT.ExprProcActual <$> mapM toExpr es
exprCmd e = throwError $ "unsupported expression " ++ show e
