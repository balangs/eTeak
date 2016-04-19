{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Language.SimpleGo.Balsa  (
  synthesizeFile
  ) where

import           Control.Monad                        (forM_)
import           Control.Monad.Except                 (ExceptT (..), runExceptT,
                                                       withExceptT)
import           Control.Monad.Trans                  (liftIO)
import           Data.Text                            (pack, unpack)
import qualified Data.Vector                          as U

import qualified Context                              as C
import           Language.Helpers                     (bind, eval, finish, teak,
                                                       writeGates, writeTeak)
import           Language.SimpleGo.AST
import qualified Language.SimpleGo.AST.Operators      as Operators
import           Language.SimpleGo.Balsa.Builtins     (bool, byte, string)
import qualified Language.SimpleGo.Balsa.Builtins     as Builtins
import           Language.SimpleGo.Balsa.Declarations (Context, Decl)
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified Language.SimpleGo.Eval               as Eval
import           Language.SimpleGo.Monad              (declare, unsupported)
import qualified Language.SimpleGo.Monad              as M
import           Language.SimpleGo.Process            (compileFile)
import qualified ParseTree                            as PT
import           Print                                (showTree)
import qualified Report                               as R

type TranslateM = M.TranslateT IO Decl

pos :: R.Pos
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
      balsa <- withExceptT show $ ExceptT $ M.runTranslateT $ asBalsa goSource
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

asBalsa :: Program -> TranslateM Context
asBalsa program = do
  -- Implementation bug: "String" must be defined
  declare "String" $ D.Type string
  root' program
  D.declContext <$> M.popContext

root' :: Program -> TranslateM ()
root' program = do
  forM_ Builtins.types $ \(n,t) -> declare (pack n) $ D.Type t
  forM_ (declarations program) declareTopLevel


balsaType :: Type -> TranslateM PT.Type
balsaType (TypeName id') = return $ PT.NameType pos (unId id')
balsaType t = M.unsupported "type" t

typedExpr :: Type -> Expr -> TranslateM PT.Expr
typedExpr t e = do
  t' <- balsaType t
  typedExpr' t' e

-- deprecated
toExpr :: Expr -> TranslateM PT.Expr
toExpr = typedExpr' PT.NoType

typedExpr' :: PT.Type -> Expr -> TranslateM PT.Expr
typedExpr' _ (Prim prim) = fromPrim prim
typedExpr' t (UnOp Plus e) = PT.BinExpr pos t PT.BinAdd (PT.ValueExpr pos t (PT.IntValue 0)) <$> typedExpr' t e
typedExpr' t (UnOp Minus e) = PT.BinExpr pos t PT.BinSub (PT.ValueExpr pos t (PT.IntValue 0)) <$> toExpr e
typedExpr' t (UnOp Not e) = PT.UnExpr pos t PT.UnNot <$> typedExpr' t e
typedExpr' t (BinOp op e e') = PT.BinExpr pos t <$> binOp op <*> typedExpr' t e <*> typedExpr' t e'
typedExpr' _ e = M.unsupported "expr" e

declareTopLevel :: Declaration -> TranslateM ()
declareTopLevel (Const (Id id') typ e) = do
  e' <- typedExpr typ e
  declare id' $ D.Const e'
declareTopLevel (Var (Id id') _ (Prim (Make (Channel Bidirectional typ') []))) = do
  t' <- balsaType typ'
  declare id' $ D.Chan t'
declareTopLevel (Var (Id id') typ e) = case e of
  Zero -> do
    t <- balsaType typ
    declare id' $ D.Var t Nothing
  _  -> do
    t <- balsaType typ
    e' <- typedExpr' t e
    declare id' $ D.Var t (Just e')
declareTopLevel (Type (Id id') typ) = do
  t <- balsaType typ
  declare id' $ D.Type t
--declareTopLevel f = M.unsupported "top level binding" f
declareTopLevel f@(Func (Id id') sig block) = declare id' =<< decl
  where
    decl = if isProc sig
           then procedure
           else M.unsupported "function" f
    isProc = const True
    procedure = do
      M.newContext
      declareSig sig
      b <- blockCmd block
      sigDecl' <- M.popContext
      return $ D.Proc (D.declContext sigDecl') b

true :: PT.Value
true = PT.IntValue 1

fromPrim :: Prim -> TranslateM PT.Expr
fromPrim (LitInt i) = return $ PT.ValueExpr pos byte (PT.IntValue i)
fromPrim (LitStr s) = return $ PT.ValueExpr pos string (PT.StringValue (unpack s))
-- TODO maybe fix this parser? Should it be qual here?
fromPrim (Qual id') = return $ PT.NameExpr pos (unId id')
-- special case type coercions
fromPrim c@(Call (Qual id') [arg]) = case lookup (unId id') Builtins.types of
  Just typ -> PT.CastExpr pos typ <$> toExpr arg
  Nothing -> M.unsupported "call expression" c
fromPrim s = M.unsupported  "primitive" s

binOp :: Binary -> TranslateM PT.BinOp
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
binOp o = M.unsupported "operator" o

unId :: Id -> String
unId (Id id') = unpack id'

sigDecl :: Type -> TranslateM D.Decl
sigDecl (Channel Input typ) = D.In <$> balsaType typ
sigDecl (Channel Output typ) = D.Out <$> balsaType typ
--sigDecl (Channel Bidirectional typ) = PT.ChanDecl pos <$> balsaType typ
sigDecl t@(TypeName _) = D.Param <$> balsaType t
sigDecl t = unsupported "signature type" t

declareParam :: Param -> TranslateM ()
declareParam (Param (Just (Id id')) t) = do
  t' <- sigDecl t
  declare id' t'
declareParam (Param Nothing t) = do
  t' <- sigDecl t
  declare "_" t'

declareSig :: Signature -> TranslateM ()
declareSig (Signature inputs _) = U.forM_ inputs declareParam

blockCmd :: Block -> TranslateM PT.Cmd
blockCmd (Block statements) = do
  M.newContext
  cmd' <- seqCmd $ U.toList statements
  c <- D.declContext <$> M.popContext
  return $ PT.BlockCmd pos c cmd'

seqCmd :: [Statement] -> TranslateM PT.Cmd
seqCmd ss = collapsePars <$> traverse parCmd ss


caseCmds :: [Case Expr] -> TranslateM ([PT.CaseCmdGuard], PT.Cmd)
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

parCmd :: Statement -> TranslateM (Par PT.Cmd)
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

cmd :: Statement -> TranslateM PT.Cmd
cmd (Go p) = exprCmd p
cmd (Simple s) = simpleCmd s
cmd (StmtBlock block) = blockCmd block
-- for { } construct is identical to loop ... end
cmd (ForWhile Nothing block) = PT.LoopCmd pos <$> blockCmd block
cmd (ForWhile (Just e) block) = PT.WhileCmd pos PT.NoCmd <$> toExpr e <*> blockCmd block
-- for i := <start>; i < <end>; i++ { } is equivalent to a range
cmd f@(ForThree
       (SimpVar id' start)
       (Just (BinOp LessThan (Prim (Qual id'')) end))
       (Inc (Prim (Qual id''')))
       block)
  | id' == id'' && id' == id''' = b interval
  where
    b (Just i@(PT.Interval _ typ')) = do
       M.newContext
       declare (idText id') (D.Param typ')
       c <- D.declContext <$> M.popContext
       PT.ForCmd pos PT.Seq i c <$> blockCmd block
    b _ = unsupported "ForLoop" f
    interval = do
      (start', end') <- (,) <$> Eval.eval start <*> Eval.eval end
      case (start', end') of
        (Eval.IntegralR t s, Eval.IntegralR _ e) ->
          return $ PT.Interval (s, pred e) $ lookupType t
        _ -> Nothing
    lookupType Eval.Int8 = Builtins.int8
    lookupType Eval.Int16 = Builtins.int16
    lookupType Eval.Int32 = Builtins.int32
    lookupType Eval.Int64 = Builtins.int64
    lookupType Eval.Uint8 = Builtins.uint8
    lookupType Eval.Uint16 = Builtins.uint16
    lookupType Eval.Uint32 = Builtins.uint32
    lookupType Eval.Uint64 = Builtins.uint64
    lookupType Eval.GoInt = Builtins.int
    lookupType Eval.GoUint = Builtins.uint
cmd (If (Cond Nothing (Just expr)) block s) = PT.CaseCmdE pos <$> toExpr expr <*> fmap return trueBlock <*> s'
  where
    s' = maybe (return PT.NoCmd) cmd s
    trueBlock = PT.CaseCmdGuard pos [PT.ExprCaseMatch pos (PT.ValueExpr pos bool true)] <$> blockCmd block
cmd (Switch (Cond Nothing (Just expr)) cases) = do
  (cs, def) <- caseCmds cases
  PT.CaseCmdE pos <$> toExpr expr <*> pure cs <*> pure def
cmd (StmtDecl decl) = do
  declareTopLevel decl
  return PT.NoCmd
cmd (StmtSelect cases) = PT.SelectCmd pos False <$> traverse chanCase cases
  where
    chanCase (Case as stmnts) = PT.ChanGuard pos <$> traverse chanGuard as <*> pure C.EmptyContext <*> seqCmd stmnts
      where
        chanGuard (ChanRecv Nothing (UnOp Receive (Prim (Qual id')))) = return $ PT.NameChan pos (unId id')
        chanGuard c = unsupported "guard" c
    chanCase d@(Default _) = unsupported "select" d
cmd s = unsupported "statment" s

simpleCmd :: Simp -> TranslateM PT.Cmd
simpleCmd Empty = return PT.NoCmd
simpleCmd (Inc e@(Prim (Qual id'))) = simpleCmd $ SimpVar id' $ BinOp Operators.Add e (Prim (LitInt 1))
simpleCmd (Dec e@(Prim (Qual id'))) = simpleCmd $ SimpVar id' $ BinOp Operators.Subtract e (Prim (LitInt 1))
simpleCmd (Send (Prim (Qual chan)) prim) = PT.OutputCmd pos
  <$> pure (PT.NameChan pos (unId chan))
  <*> toExpr prim
simpleCmd (SimpVar (Id id') (UnOp Receive (Prim (Qual (Id chan))))) = do
  chan' <- M.lookup chan
  case chan' of
    Nothing -> M.notDefined (unpack chan)
    Just t -> do
      r <- receiveType t
      declare id' (D.Var r Nothing)
      return $ PT.InputCmd pos (PT.NameChan pos (unpack chan)) (PT.NameLvalue pos (unpack id'))
    where
      receiveType :: D.Decl -> TranslateM PT.Type
      receiveType (D.In t) = return t
      receiveType (D.Chan t) = return t
      receiveType s = M.typeError "chan or <-chan" (show s)

simpleCmd (SimpleExpr e) = exprCmd e
simpleCmd s = unsupported "simple expression " s

exprCmd :: Expr -> TranslateM PT.Cmd
exprCmd (Prim (Call (LitFunc sig block) es)) = do
      M.newContext
      declareTopLevel $ Func (Id "$") sig block
      c <- D.declContext <$> M.popContext
      PT.BlockCmd pos c <$> call
  where
    call = PT.CallCmd pos (PT.NameCallable pos "$") C.EmptyContext . map PT.ExprProcActual <$> mapM toExpr es
exprCmd (Prim (Call (Qual (Id "print")) es)) = PT.PrintCmd pos <$> mapM toExpr es
exprCmd (Prim (Call (Qual (Id "println")) es)) = PT.PrintCmd pos <$> mapM toExpr es
exprCmd (Prim (Call (Qual id') es)) = PT.CallCmd pos (PT.NameCallable pos (unId id')) C.EmptyContext . map PT.ExprProcActual <$> mapM toExpr es
exprCmd e = unsupported "expression" e
