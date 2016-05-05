{-
    Teak synthesiser for the Balsa language
    Copyright (C) 2007-2010 The University of Manchester

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
    School of Computer Science, The University of Manchester
    Oxford Road, MANCHESTER, M13 9PL, UK
-}

module ParseTree (
    Attr (..),
    BinOp (..),
    Interval (..),
    Callable (..),
    CaseCmdGuard (..),
    CaseDeclGuard (..),
    CaseMatch (..),
    Chan (..),
    ChanGuard (..),
    CheckMode (..),
    Cmd (..),
    Decl (..),
    Direction (..),
    Expr (..),
    FuncActual (..),
    Lvalue (..),
    ParSeq (..),
    ProcActual (..),
    RecordElem (..),
    Sense (..),
    SimpleBinding (..),
    Type (..),
    TypeBody (..),
    UnOp (..),
    Value (..),
    chanPos,
    cmdPos,
    exprPos,
    exprRef,
    invertSense,
    lvaluePos,
    caseDeclSampleDecl,
    indexInInterval,
    isBuiltinFuncDecl,
    isFuncDecl,
    isNoDecl,
    isNoDeclRef,
    isProcDecl,
    isSharedDecl,
    unaliasDecl,
    unaliasRef,
    validDeclContextSize,
    findArrayedDeclElem,
    findAttr
    ) where

    import Report
    import Show
    import Context
    import Bits

    import Data.Maybe
    import Data.Array
    import Data.List

    data ParSeq = Par | Seq
        deriving (Show, Read, Eq, Ord)

    data Direction = Input | Output
        deriving (Show, Read, Eq, Ord)

    data CheckMode = Explicit | Inserted | Transient
        deriving (Show, Read, Eq, Ord)

    data SimpleBinding = SimpleBinding { simpleBindingName :: String, simpleBindingValue :: Integer }
        deriving (Show, Read, Eq, Ord)

    data Attr = ExprAttr String Expr
        deriving (Show, Read, Eq, Ord)

    data Interval =
          IntervalE Pos Expr Expr
        | IntervalOver Pos Type
        | Interval { intervalRange :: (Integer, Integer), intervalType :: Type }
        deriving (Show, Read)

    instance Eq Interval where
        (Interval range1 _) == (Interval range2 _) = range1 == range2
        _ == _ = False

    instance Ord Interval where
        (Interval range1 _) `compare` (Interval range2 _) = range1 `compare` range2
        _ `compare` _ = LT

    data Sense = Passive | Active
        deriving (Show, Read, Eq, Ord)

    data Decl =
          NoDecl { declPos :: Pos } -- Only use NoDecl to remove declaration bindings
        | PosDecl { declPos :: Pos }
        | ExprDecl { declPos :: Pos, declExpr :: Expr }
        | ChanExprDecl { declPos :: Pos, declChan :: Chan }
        | ProcDecl { declPos :: Pos, declFormals :: Context Decl, declAttrs :: [Attr], declCmd :: Cmd }
        | ProcAliasDecl { declPos :: Pos, declCallable :: Callable, declFormals :: Context Decl,
            declActuals :: [ProcActual] }
        | FuncDecl { declPos :: Pos, declFormals :: Context Decl, declExpr :: Expr }
        | BuiltinFuncDecl { declPos :: Pos, declFormals :: Context Decl, declType :: Type }
        | VarDecl { declPos :: Pos, declType :: Type }
        | ParamDecl { declPos :: Pos, declIsParam :: Bool, declType :: Type }
        | TypeParamDecl { declPos :: Pos }
        | ChanDecl { declPos :: Pos, declType :: Type }
        | PortDecl { declPos :: Pos, declDirection :: Direction, declType :: Type }
        | OpenChanDecl { declPos :: Pos, declRef :: Ref, declType :: Type }
        | OpenChanDeclE { declPos :: Pos, declRef :: Ref, declExprs :: [[(Expr, Expr)]] }
        | FlatArrayedDecl { declPos :: Pos, declInterval :: Interval, declArray :: Array Integer Decl }
        | AliasDecl { declPos :: Pos, declRef :: Ref, declType :: Type }
        | SharedDecl { declPos :: Pos, declFormals :: Context Decl, declCmd :: Cmd }
        | TypeDecl { declPos :: Pos, declTypeBody :: TypeBody }
        | ArrayedDecl { declPos :: Pos, declInterval :: Interval, declSubDecl :: Decl }
        | CaseDecl { declPos :: Pos, declCaseExpr :: Expr, declCaseGuards :: [CaseDeclGuard], declCaseElse :: Decl }
        | DeferDecl { declPos :: Pos, declCompleteness :: Completeness, declDecl :: Decl }
        deriving (Show, Eq, Ord)

    data CaseDeclGuard =
          CaseDeclGuard Pos [CaseMatch] Decl
        | ListCaseDeclGuard [CaseDeclGuard]
        deriving (Show, Read, Eq, Ord)

    data RecordElem = RecordElem Pos String Type
        deriving (Show, Read, Eq, Ord)

    data TypeBody =
          AliasType Pos Type
        | RecordType Pos [RecordElem] Type
        | EnumType Pos (Context Decl) Type -- Context is of Binding name OtherNamespace (ExprDecl value)
        deriving (Show, Read, Eq, Ord)

    data Type =
          NoType
        | NameType Pos String
        | NumType Pos Expr Bool
        | ArrayType Interval Type
        | BuiltinType String
        -- Final types
        | Type Ref
        | Bits Int
        | SignedBits Int
        | StructRecordType String [RecordElem] Type
        | StructEnumType String [SimpleBinding] Type
        | StructArrayType Interval Type
        deriving (Show, Read, Eq, Ord)

    data Lvalue =
          CastLvalue Pos Type Lvalue
        | TypeCheckLvalue Pos CheckMode Type Lvalue
        | NameLvalue Pos String
        | RecElemLvalue Pos Lvalue String
        | SmashLvalue Pos Lvalue
        | IndexLvalue Pos Lvalue Expr
        | SliceLvalue Pos Lvalue Expr Expr
        --
        | BitfieldLvalue Pos Type (Slice Int) Lvalue
        | CaseLvalue Pos Expr [[Implicant]] [Lvalue]
        --
        | VarWrite Pos Type (Slice Int) Ref
        deriving (Show, Read, Eq, Ord)

    data Chan =
          NameChan Pos String
        | IndexChan Pos Chan Expr
        | SliceChan Pos Chan Expr Expr
        | PartialArrayedChan Pos Ref
        | FlatArrayedChan Pos Interval [Chan]
        | CheckChan Pos Decl Chan
        | Chan Pos Ref
        deriving (Show, Read, Eq, Ord)

    data Callable =
          Callable Ref
        | NameCallable Pos String
        deriving (Show, Read, Eq, Ord)

    data Value =
          IntValue Integer
        | ImpValue Implicant
        | StringValue String
        | DontCareValue
        deriving (Show, Read, Eq, Ord)

    data BinOp = BinMul
               | BinDiv
               | BinMod
               | BinPow
               | BinAdd
               | BinSub
               | BinAnd
               | BinOr
               | BinXor
               | BinLT
               | BinGT
               | BinLE
               | BinGE
               | BinNE
               | BinEQ
         deriving (Show, Read, Eq, Ord)

    data UnOp = UnNot | UnNeg | UnLog | UnSmash
        deriving (Show, Read, Eq, Ord)

    data Expr =
          NameExpr Pos String
        | BinExpr Pos Type BinOp Expr Expr
        | IndexExpr Pos Expr Expr -- 0[1]
        | SliceExpr Pos Expr Expr Expr -- 0[1..2]
        | RecElemExpr Pos Expr String
        | UnExpr Pos Type UnOp Expr -- sizeof not log - + #
        | CastExpr Pos Type Expr
        | TypeCheckExpr Pos CheckMode Type Expr
        | ConstCheckExpr Pos Expr
        | ArrayElemTypeCheckExpr Pos Type Expr
        | CallExpr Pos Callable (Context Decl) [FuncActual]
        | BuiltinCallExpr Pos String [FuncActual] [Expr] Type -- name parameters args returnType
        -- ConsExpr { consExprPos :: Pos, exprType :: Type, exprConsType :: Type, exprConsElems :: [Expr] }
        | ConsExpr Pos Type Type [Expr]
        | AppendExpr Pos Type Expr Expr
        | EnumElemExpr Pos Type String
        | SizeofExpr Pos Type
        --
        | BitfieldExpr Pos Type (Slice Int) Expr
        | ExtendExpr Pos Type Int Bool Expr
        | CaseExpr Pos Expr [[Implicant]] [Expr]
        --
        | ValueExpr Pos Type Value
        | VarRead Pos Type (Slice Int) Ref
        | OpenChanRead Pos Type (Slice Int) Ref
        | PartialArrayedRead Pos Ref
        | MaybeTypeExpr Pos Ref
        | MaybeOtherExpr Pos Ref
        | Expr Pos Ref
        deriving (Show, Read, Eq, Ord)

    data CaseMatch =
          ExprCaseMatch Pos Expr
        | RangeCaseMatch Pos Expr Expr
        | ImpCaseMatches Pos [Implicant]
        deriving (Show, Read, Eq, Ord)

    data CaseCmdGuard =
          CaseCmdGuard Pos [CaseMatch] Cmd
        | ForCaseCmdGuard Pos String [CaseMatch] (Context Decl) Cmd
        | ListCaseCmdGuard [CaseCmdGuard]
        deriving (Show, Read, Eq, Ord)

    data ChanGuard = ChanGuard Pos [Chan] (Context Decl) Cmd
        deriving (Show, Read, Eq, Ord)

    data Cmd =
          NoCmd
        | LabelCmd Pos String Cmd
        | SeqCmd Pos [Cmd]
        | ParCmd Pos [Cmd]
        | BlockCmd Pos (Context Decl) Cmd
        | EncInputCmd Pos ChanGuard
        | InputCmd Pos Chan Lvalue
        | OutputCmd Pos Chan Expr
        | SelectCmd Pos Bool [ChanGuard]
        | AssignCmd Pos Lvalue Expr
        | SinkCmd Pos Expr
        | CaseCmdE Pos Expr [CaseCmdGuard] Cmd
        | CaseCmd Pos Expr [[Implicant]] [Cmd]
        | CallCmd Pos Callable (Context Decl) [ProcActual]
        | InstanceCallCmd Pos Callable [Chan]
        | SharedCallCmd Pos Callable
        | PrintCmd Pos [Expr]
        | ForCmd Pos ParSeq Interval (Context Decl) Cmd
        | LoopCmd Pos Cmd
        | WhileCmd Pos Cmd Expr Cmd
        | DeferCmd Completeness Cmd
        deriving (Show, Read, Eq, Ord)

    data FuncActual =
          TypeFuncActual { funcActualType :: Type }
        | ExprFuncActual { funcActualIsParameter :: Bool, funcActualExpr :: Expr }
        deriving (Show, Eq, Ord)

    data ProcActual =
          TypeProcActual { procActualType :: Type }
        | ExprProcActual { procActualExpr :: Expr }
        | ChanProcActual { procActualChan :: Chan }
        deriving (Show, Eq, Ord)

    -- functions below

    invertSense :: Sense -> Sense
    invertSense Passive = Active
    invertSense Active = Passive

    lvaluePos :: Lvalue -> Pos
    lvaluePos (CastLvalue pos _ _) = pos
    lvaluePos (TypeCheckLvalue pos _ _ _) = pos
    lvaluePos (NameLvalue pos _) = pos
    lvaluePos (RecElemLvalue pos _ _) = pos
    lvaluePos (SmashLvalue pos _) = pos
    lvaluePos (IndexLvalue pos _ _) = pos
    lvaluePos (SliceLvalue pos _ _ _) = pos
    lvaluePos (BitfieldLvalue pos _ _ _) = pos
    lvaluePos (CaseLvalue pos _ _ _) = pos
    lvaluePos (VarWrite pos _ _ _) = pos

    chanPos :: Chan -> Pos
    chanPos (NameChan pos _) = pos
    chanPos (IndexChan pos _ _) = pos
    chanPos (SliceChan pos _ _ _) = pos
    chanPos (PartialArrayedChan pos _) = pos
    chanPos (FlatArrayedChan pos _ _) = pos
    chanPos (CheckChan pos _ _) = pos
    chanPos (Chan pos _) = pos

    exprPos :: Expr -> Pos
    exprPos (NameExpr pos _) = pos
    exprPos (BinExpr pos _ _ _ _) = pos
    exprPos (IndexExpr pos _ _) = pos
    exprPos (SliceExpr pos _ _ _) = pos
    exprPos (RecElemExpr pos _ _) = pos
    exprPos (UnExpr pos _ _ _) = pos
    exprPos (CastExpr pos _ _) = pos
    exprPos (TypeCheckExpr pos _ _ _) = pos
    exprPos (ConstCheckExpr pos _) = pos
    exprPos (ArrayElemTypeCheckExpr pos _ _) = pos
    exprPos (CallExpr pos _ _ _) = pos
    exprPos (BuiltinCallExpr pos _ _ _ _) = pos
    exprPos (ConsExpr pos _ _ _) = pos
    exprPos (AppendExpr pos _ _ _) = pos
    exprPos (EnumElemExpr pos _ _) = pos
    exprPos (SizeofExpr pos _) = pos
    exprPos (BitfieldExpr pos _ _ _) = pos
    exprPos (ExtendExpr pos _ _ _ _) = pos
    exprPos (CaseExpr pos _ _ _) = pos
    exprPos (ValueExpr pos _ _) = pos
    exprPos (VarRead pos _ _ _) = pos
    exprPos (OpenChanRead pos _ _ _) = pos
    exprPos (PartialArrayedRead pos _) = pos
    exprPos (MaybeTypeExpr pos _) = pos
    exprPos (MaybeOtherExpr pos _) = pos
    exprPos (Expr pos _) = pos

    exprRef :: Expr -> Maybe Ref
    exprRef (VarRead _ _ _ ref) = Just ref
    exprRef (OpenChanRead _ _ _ ref) = Just ref
    exprRef (PartialArrayedRead _ ref) = Just ref
    exprRef (MaybeTypeExpr _ ref) = Just ref
    exprRef (MaybeOtherExpr _ ref) = Just ref
    exprRef (Expr _ ref) = Just ref
    exprRef _ = Nothing

    cmdPos :: Cmd -> Pos
    cmdPos NoCmd = NoPos
    cmdPos (LabelCmd pos _ _) = pos
    cmdPos (SeqCmd pos _) = pos
    cmdPos (ParCmd pos _) = pos
    cmdPos (BlockCmd pos _ _) = pos
    cmdPos (EncInputCmd pos _) = pos
    cmdPos (InputCmd pos _ _) = pos
    cmdPos (OutputCmd pos _ _) = pos
    cmdPos (SelectCmd pos _ _) = pos
    cmdPos (AssignCmd pos _ _) = pos
    cmdPos (SinkCmd pos _) = pos
    cmdPos (CaseCmdE pos _ _ _) = pos
    cmdPos (CaseCmd pos _ _ _) = pos
    cmdPos (CallCmd pos _ _ _) = pos
    cmdPos (InstanceCallCmd pos _ _) = pos
    cmdPos (SharedCallCmd pos _) = pos
    cmdPos (PrintCmd pos _) = pos
    cmdPos (ForCmd pos _ _ _ _) = pos
    cmdPos (LoopCmd pos _) = pos
    cmdPos (WhileCmd pos _ _ _) = pos
    cmdPos (DeferCmd _ cmd) = cmdPos cmd

    instance Read Decl where
        readsPrec _ = parenC $ \str -> maybeToList $ do
            (headToken, rest) <- maybeLex str
            case headToken of
                "NoDecl" -> readFields fields (NoDecl NoPos) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }) ]
                "PosDecl" -> readFields fields (PosDecl NoPos) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }) ]
                "ExprDecl" -> readFields fields (ExprDecl NoPos undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declExpr" readsC (\o f -> o { declExpr = f }) ]
                "ChanExprDecl" -> readFields fields (ChanExprDecl NoPos undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declChan" readsC (\o f -> o { declChan = f }) ]
                "ProcDecl" -> readFields fields (ProcDecl NoPos undefined undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declFormals" readsC (\o f -> o { declFormals = f }),
                            ReadField "declAttrs" readListC (\o f -> o { declAttrs = f }),
                            ReadField "declCmd" readsC (\o f -> o { declCmd = f }) ]
                "ProcAliasDecl" -> readFields fields (ProcAliasDecl NoPos undefined undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declCallable" readsC (\o f -> o { declCallable = f }),
                            ReadField "declFormals" readsC (\o f -> o { declFormals = f }),
                            ReadField "declActuals" readListC (\o f -> o { declActuals = f }) ]
                "FuncDecl" -> readFields fields (FuncDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declFormals" readsC (\o f -> o { declFormals = f }),
                            ReadField "declExpr" readsC (\o f -> o { declExpr = f }) ]
                "BuiltinFuncDecl" -> readFields fields (BuiltinFuncDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declFormals" readsC (\o f -> o { declFormals = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "VarDecl" -> readFields fields (VarDecl NoPos undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "ParamDecl" -> readFields fields (ParamDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declIsParam" readsC (\o f -> o { declIsParam = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "TypeParamDecl" -> readFields fields (TypeParamDecl NoPos) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }) ]
                "ChanDecl" -> readFields fields (ChanDecl NoPos undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "PortDecl" -> readFields fields (PortDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declDirection" readsC (\o f -> o { declDirection = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "OpenChanDecl" -> readFields fields (OpenChanDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declRef" readsC (\o f -> o { declRef = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "OpenChanDeclE" -> readFields fields (OpenChanDeclE NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declRef" readsC (\o f -> o { declRef = f }),
                            ReadField "declExprs" (readListCWith (readListCWith (readPairC readsC readsC)))
                            (\o f -> o { declExprs = f }) ]
                "FlatArrayedDecl" -> readFields fields (FlatArrayedDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declInterval" readsC (\o f -> o { declInterval = f }),
                            ReadField "declArray" readArrayC (\o f -> o { declArray = f }) ]
                "AliasDecl" -> readFields fields (AliasDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declRef" readsC (\o f -> o { declRef = f }),
                            ReadField "declType" readsC (\o f -> o { declType = f }) ]
                "SharedDecl" -> readFields fields (SharedDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declFormals" readsC (\o f -> o { declFormals = f }),
                            ReadField "declCmd" readsC (\o f -> o { declCmd = f }) ]
                "TypeDecl" -> readFields fields (TypeDecl NoPos undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declTypeBody" readsC (\o f -> o { declTypeBody = f }) ]
                "ArrayedDecl" -> readFields fields (ArrayedDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declInterval" readsC (\o f -> o { declInterval = f }),
                            ReadField "declSubDecl" readsC (\o f -> o { declSubDecl = f }) ]
                "CaseDecl" -> readFields fields (CaseDecl NoPos undefined undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declCaseExpr" readsC (\o f -> o { declCaseExpr = f }),
                            ReadField "declCaseGuards" readListC (\o f -> o { declCaseGuards = f }),
                            ReadField "declCaseElse" readsC (\o f -> o { declCaseElse = f }) ]
                "DeferDecl" -> readFields fields (DeferDecl NoPos undefined undefined) rest
                    where fields = [
                            ReadField "declPos" readsC (\o f -> o { declPos = f }),
                            ReadField "declCompleteness" readsC (\o f -> o { declCompleteness = f }),
                            ReadField "declDecl" readsC (\o f -> o { declDecl = f }) ]
                _ -> Nothing

    instance Read FuncActual where
        readsPrec _ = parenC $ \str -> maybeToList $ do
            (headToken, rest) <- maybeLex str
            case headToken of
                "TypeFuncActual" -> readFields fields (TypeFuncActual undefined) rest
                    where fields = [
                            ReadField "funcActualType" readsC (\o f -> o { funcActualType = f }) ]
                "ExprFuncActual" -> readFields fields (ExprFuncActual False undefined) rest
                    where fields = [
                            ReadField "funcActualIsParameter" readsC (\o f -> o { funcActualIsParameter = f }),
                            ReadField "funcActualExpr" readsC (\o f -> o { funcActualExpr = f }) ]
                _ -> Nothing

    instance Read ProcActual where
        readsPrec _ = parenC $ \str -> maybeToList $ do
            (headToken, rest) <- maybeLex str
            case headToken of
                "TypeProcActual" -> readFields fields (TypeProcActual undefined) rest
                    where fields = [
                            ReadField "procActualType" readsC (\o f -> o { procActualType = f }) ]
                "ExprProcActual" -> readFields fields (TypeProcActual undefined) rest
                    where fields = [
                            ReadField "procActualExpr" readsC (\o f -> o { procActualExpr = f }) ]
                "ChanProcActual" -> readFields fields (ChanProcActual undefined) rest
                    where fields = [
                            ReadField "procActualChan" readsC (\o f -> o { procActualChan = f }) ]
                _ -> Nothing

    instance BindingValue Decl where
        findArrayedElem = findArrayedDeclElem
        valuePos = declPos
        unaliasValue = unaliasDecl
        unaliasValueRef = unaliasRef

    indexInInterval :: Interval -> Integer -> Bool
    indexInInterval (Interval interval _) = inRange interval
    indexInInterval _ = error "indexInInterval: bad Interval"

    findArrayedDeclElem :: Decl -> Integer -> Maybe Decl
    findArrayedDeclElem (ArrayedDecl _ interval decl) i
        | indexInInterval interval i = Just $ decl
    findArrayedDeclElem (FlatArrayedDecl _ interval decls) i
        | indexInInterval interval i = Just (decls ! i)
    findArrayedDeclElem _ _ = Nothing

    unaliasDecl :: [Context Decl] -> Decl -> Decl
    unaliasDecl cs (AliasDecl _ ref _) = unaliasDecl cs $ bindingValue $ findBindingByRef cs ref
    unaliasDecl _ decl = decl

    -- FIXME, clean up
    unaliasRef :: [Context Decl] -> Ref -> Ref
    unaliasRef cs (Ref refNo) = body [] refNo where
        body visited int
            | length visited > 100 = error $ "Looking for too long for " ++ show visited ++ " " ++ show cs
            | otherwise = case bindingValue $ findBindingByIndex cs int of
                AliasDecl _ (Ref ref) _ -> body (ref : visited) ref
                _ -> Ref int
    unaliasRef _ ref = error $ "unaliasRef: can't handle ref `" ++ show ref ++ "'"

    isNoDecl :: Decl -> Bool
    isNoDecl (NoDecl {}) = True
    isNoDecl _ = False

    isProcDecl :: Decl -> Bool
    isProcDecl (ProcDecl {}) = True
    isProcDecl _ = False

    isFuncDecl :: Decl -> Bool
    isFuncDecl (FuncDecl {}) = True
    isFuncDecl _ = False

    isBuiltinFuncDecl :: Decl -> Bool
    isBuiltinFuncDecl (BuiltinFuncDecl {}) = True
    isBuiltinFuncDecl _ = False

    isSharedDecl :: Decl -> Bool
    isSharedDecl (SharedDecl {}) = True
    isSharedDecl _ = False

    flattenCaseDeclGuard :: CaseDeclGuard -> [Decl]
    flattenCaseDeclGuard (CaseDeclGuard _ _ decl) = [decl]
    flattenCaseDeclGuard (ListCaseDeclGuard guards) = concatMap flattenCaseDeclGuard guards

    caseDeclDecls :: Decl -> [Decl]
    caseDeclDecls (CaseDecl _ _ guards elseDecl) = concatMap flattenCaseDeclGuard guards ++ [elseDecl]
    caseDeclDecls decl = error $ "caseDeclDecls: unhandled decl `" ++ show decl ++ "'"

    caseDeclSampleDecl :: Decl -> Decl
    caseDeclSampleDecl caseDecl  = fromJust $ find notNoDecl $ caseDeclDecls caseDecl
        where
            notNoDecl (NoDecl {}) = False
            notNoDecl _ = True

    isNoDeclRef :: [Context Decl] -> Ref -> Bool
    isNoDeclRef cs ref = isNoDecl $ bindingValue $ findBindingByRef cs ref

    validDeclContextSize :: Context Decl -> Int
    validDeclContextSize context = length $ filter (not . isNoDecl) $ map bindingValue $ contextBindingsList context

    findAttr :: String -> [Attr] -> Maybe Expr
    findAttr _ [] = Nothing
    findAttr name (ExprAttr attrName value : attrs)
        | name == attrName = Just value
        | otherwise = findAttr name attrs
