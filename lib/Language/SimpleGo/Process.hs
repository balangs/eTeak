{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | Probably need to make this an internal module at some point instead

module Language.SimpleGo.Process where

import           Control.Monad                (join)
import           Control.Monad.Except         (MonadError, catchError,
                                               runExcept, throwError)
import           Control.Monad.List           (ListT (..), runListT)
import           Control.Monad.State.Strict   (StateT, evalStateT, get, gets,
                                               put)
import           Data.String                  (fromString)
import qualified Data.Vector                  as U
import           Language.Go.Parser           (goParse)
import qualified Language.Go.Syntax.AST       as Go
import qualified Language.SimpleGo.AST        as S
import qualified Language.SimpleGo.Transforms as Transforms

compileFile :: String -> IO (Either String S.Program)
compileFile f = do
  s <- readFile f
  let
    p = goParse f s
  case p of
    Left s' -> return $ Left $ show s'
    Right t -> return $ runExcept $ compile t

compile :: MonadError String m => Go.GoSource -> m S.Program
compile = fmap (S.Program . join) . U.mapM compileDecl . U.fromList . Go.getTopLevelDecl

flattenCVSpec :: MonadError String m => Go.GoCVSpec -> ListT m (S.Id, S.Type, S.Expr)
flattenCVSpec (Go.GoCVSpec ids (Just typ) exprs) = do
      typ' <- asType typ
      (id', mayExpr) <- ListT $ return $ zip ids (map Just exprs ++ repeat Nothing)
      expr' <- mayToExpr mayExpr
      return (asId id', typ', expr')
flattenCVSpec c@(Go.GoCVSpec _ Nothing _) = throwError $ "explicit types are required in declaration: " ++ show c


-- Desugar a parenthesized const declaration, which is a list of const specs.
-- See the spec at https://golang.org/ref/spec#Constant_declarations
-- For each spec, the keyword iota is replaced with an incrementing integer
-- An expressionless const spec substitutes the previous const spec's expression.
constDeclarations :: forall m. MonadError String m => [Go.GoCVSpec] -> m [S.Declaration]
constDeclarations cs = evalStateT (runListT f) (Nothing, 0)
  where
    mkDecl c@(Go.GoCVSpec _ Nothing _) =
      throwError $ "explicit types are required " ++ show c
    mkDecl c = do
      iota <- gets snd
      (id', typ, expr) <- flattenCVSpec c
      put (Just c, succ iota)
      return $ S.Const id' typ (Transforms.replaceIota iota expr)

    f :: ListT (StateT (Maybe Go.GoCVSpec, Integer) m) S.Declaration
    f = do
      c <- ListT $ return cs
      prev <- gets fst
      case (prev, c) of
        --
        (Just (Go.GoCVSpec ids' typ' exprs'), Go.GoCVSpec ids Nothing []) ->
          if length ids == length ids'
          then mkDecl (Go.GoCVSpec ids typ' exprs')
          else throwError "need the same number of ids if using blank expressions in a constant declaration"
        _ -> mkDecl c

varDeclarations :: forall m. MonadError String m => [Go.GoCVSpec] -> m [S.Declaration]
varDeclarations cs = runListT $ do
  c <- ListT $ return cs
  (id', typ, expr) <- flattenCVSpec c
  return $ S.Var id' typ expr


mayToExpr :: MonadError String m => Maybe Go.GoExpr -> m S.Expr
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
toLit (Go.GoLitStr  _ s) = return $ S.LitStr (fromString s)
toLit (Go.GoLitFunc (Go.GoFuncExpr sig block)) = S.LitFunc <$> asSig sig <*> asBlock block
toLit s = throwError $ "unsupported literal: \"" ++ show s ++ "\""

toPrim :: MonadError String m => Go.GoPrim -> m S.Prim
toPrim (Go.GoLiteral lit) = toLit lit
toPrim (Go.GoQual Nothing i) = return $ S.Qual $ asId i
toPrim (Go.GoCall _ _ True) = throwError "variadic calls are not supported"
toPrim (Go.GoCall prim exprs _) = S.Call <$> toPrim prim <*> traverse toExpr exprs
toPrim (Go.GoMake typ es) = S.Make <$> asType typ <*> traverse toExpr es
toPrim (Go.GoParen e) = S.Paren <$> toExpr e
toPrim s = throwError $ "unsupported primitive: \"" ++ show s ++ "\""

asType :: MonadError String m => Go.GoType -> m S.Type
asType (Go.GoTypeName _ i) = return $ S.TypeName $ asId i
asType (Go.GoChannelType kind typ) = S.Channel (asKind kind) <$> asType typ
asType (Go.GoSliceType typ) = S.SliceType <$> asType typ
asType s = throwError $ "unsupported type: \"" ++ show s ++ "\""

asKind :: Go.GoChanKind -> S.ChanKind
asKind Go.GoIChan = S.Input
asKind Go.GoOChan = S.Output
asKind Go.GoIOChan = S.Bidirectional

compileDecl :: MonadError String m => Go.GoDecl -> m (U.Vector S.Declaration)
compileDecl (Go.GoConst cs) = U.fromList <$> constDeclarations cs
compileDecl (Go.GoVar cs) = U.fromList <$> varDeclarations cs
compileDecl (Go.GoFunc (Go.GoFuncDecl i s block)) = U.singleton <$> g
  where
    f = S.Func (asId i) <$> asSig s <*> asBlock block
    g = f `catchError` \e -> throwError $ "error compiling declaration " ++ show i ++ " :" ++ e
compileDecl (Go.GoType cs) = U.fromList <$> traverse typeDeclaration cs
  where
    typeDeclaration (Go.GoTypeSpec id' typ) = S.Type (asId id') <$> asType typ
compileDecl d = throwError $ "unsupported declaration: " ++ show d

asId :: Go.GoId -> S.Id
asId (Go.GoId i) = S.Id (fromString i)

asSig :: MonadError String m => Go.GoSig -> m S.Signature
asSig (Go.GoSig input output) = S.Signature <$> toParams input <*> toParams output

toParams :: forall m. MonadError String m => [Go.GoParam] -> m (U.Vector S.Param)
toParams ps = U.fromList <$> mapM toParam ps
  where
    toParam :: Go.GoParam -> m S.Param
    toParam (Go.GoParam [] typ) = S.Param Nothing <$> asType typ
    toParam (Go.GoParam [i] typ) = S.Param (Just $ asId i) <$> asType typ
    toParam (Go.GoParam _ _) = throwError "only parameters of exactly one identifier are supported"

asBlock :: forall m. MonadError String m => Go.GoBlock -> m S.Block
asBlock Go.GoNoBlock = return $ S.Block U.empty
asBlock (Go.GoBlock s) = S.Block . U.fromList <$> m
  where
    m :: m [S.Statement]
    m = concat <$> mapM asStatement s


-- ugh gross
asStatement :: forall m . MonadError String m => Go.GoStmt -> m [S.Statement]
asStatement (Go.GoStmtDecl d) = (U.toList . fmap S.StmtDecl) <$> compileDecl d
asStatement (Go.GoStmtLabeled _ _) = throwError "labels are not supported"
asStatement (Go.GoStmtSimple s) = return . S.Simple <$> asSimple s
asStatement (Go.GoStmtGo e) = return . S.Go <$> toExpr e
asStatement (Go.GoStmtReturn es) = return . S.Return <$> mapM toExpr es
asStatement (Go.GoStmtBreak i) = return $ return $ S.Break $ asId <$> i
asStatement (Go.GoStmtContinue i) = return $ return $ S.Continue $ asId <$> i
asStatement Go.GoStmtFallthrough = return $ return S.Fallthrough
asStatement (Go.GoStmtBlock b) = return . S.StmtBlock <$> asBlock b
asStatement (Go.GoStmtFor clause block) = do
  b <- asBlock block
  return <$> forClause clause b
asStatement (Go.GoStmtSwitch c cases) = return <$> (S.Switch <$> cond c <*> p)
  where
    p :: m [S.Case S.Expr]
    p = traverse (asCase toExpr) cases
asStatement (Go.GoStmtIf c block stmt') = return <$> (S.If <$> cond c <*> asBlock block <*> traverse stmt stmt')
asStatement (Go.GoStmtSelect cases) =return <$> S.StmtSelect <$> traverse (asCase asChan) cases
--asStatement (Go.GoStmtTypeSwitch GoCond [GoCase GoType] (Maybe GoId)) =
--asStatement (Go.GoStmtDefer GoExpr) =
asStatement s = throwError $ "unsupported statement: " ++ show s

asChan :: forall m . MonadError String m => Go.GoChan -> m S.Chan
asChan (Go.GoChanRecv m e') = S.ChanRecv <$> traverse f m  <*> toExpr e'
  where
    f (e, may, Go.GoOp op) = (,,) <$> toExpr e <*> traverse toExpr may <*> parseUnOp op
asChan (Go.GoChanSend e e') = S.ChanSend <$> toExpr e <*> toExpr e'

-- Wrap a statement in a block if there are multiple of them
stmt :: forall m . MonadError String m => Go.GoStmt -> m S.Statement
stmt s = do
  statements <- asStatement s
  case statements of
    [a] -> return a
    as -> return $ S.StmtBlock $ S.Block $ U.fromList as


cond :: MonadError String m => Go.GoCond -> m S.Cond
cond (Go.GoCond maySimp mayExpr) = S.Cond <$> traverse asSimple maySimp
                                          <*> traverse toExpr mayExpr

asCase :: MonadError String m => (a -> m b) -> Go.GoCase a -> m (S.Case b)
asCase ma (Go.GoCase as statements) = S.Case <$> traverse ma as <*> (concat <$> traverse asStatement statements)
asCase _ (Go.GoDefault statements) = S.Default <$> (concat <$> traverse asStatement statements)

asSimple :: MonadError String m => Go.GoSimp -> m S.Simp
asSimple Go.GoSimpEmpty = return S.Empty
asSimple (Go.GoSimpSend e e') = S.Send <$> toExpr e <*> toExpr e'
asSimple (Go.GoSimpExpr e) = S.SimpleExpr <$> toExpr e
asSimple (Go.GoSimpInc e) = S.Inc <$> toExpr e
asSimple (Go.GoSimpDec e) = S.Dec <$> toExpr e
--asSimple Go.GoSimpAsn [GoExpr] GoOp [GoExpr] =
asSimple (Go.GoSimpVar [i] [e]) = S.SimpVar (asId i) <$> toExpr e
asSimple (Go.GoSimpVar _ _) = throwError "Simple vars only accept a single id and expr"
asSimple s = throwError $ "unsupported simple expr" ++ show s

forClause :: MonadError String m => Go.GoForClause -> S.Block -> m S.Statement
forClause (Go.GoForWhile mayExpr) b = S.ForWhile <$> traverse toExpr mayExpr <*> pure b
forClause (Go.GoForThree simp mayExpr simp') b = S.ForThree <$> asSimple simp <*> traverse toExpr mayExpr <*> asSimple simp' <*> pure b
forClause f _ = throwError $ "unsupported for clause" ++ show f

parseUnOp :: MonadError String m => String -> m S.Unary
parseUnOp "+" = return S.Plus
parseUnOp "-" = return S.Minus
parseUnOp "^" = return S.Complement
parseUnOp "!" = return S.Not
parseUnOp "*" = return S.Address
parseUnOp "&" = return S.Reference
parseUnOp "<-" = return S.Receive
parseUnOp s = throwError $ "unsupported unary operator \"" ++ s ++ "\""

parseBinOp :: MonadError String m => String -> m S.Binary
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
