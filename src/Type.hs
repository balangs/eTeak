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

module Type (
    TypeClass (..),
    arrayInterval,
    arrayElemType,
    bitArrayType,
    bitType,
    intervalFromRange,
    intervalIndex,
    intervalIndices,
    intervalSize,
    builtinTypeWidth,
    classOfType,
    consTypeElemTypes,
    constExpr,
    constImpExpr,
    constImpOrIntExpr,
    constIndexExpr,
    exprValue,
    findRecElemByName,
    intCoercable,
    isStructType,
    isTokenValueExpr,
    makeConsValue,
    numTypeUnsignedWidth,
    rangeOfEnumType,
    rangeOfIndexType,
    rangeOfNumType,
    recoverValue,
    showTypeName,
    smallestNumTypeEnclosing,
    typeBuiltinOffsets,
    typeCoercable,
    typeEquiv,
    typeExpr,
    typeForInt,
    typeGetUnaliasedBody,
    typeIsSigned,
    typeLvalue,
    typeOfChan,
    typeOfDecl,
    typeOfExpr,
    typeOfLvalue,
    typeOfRange,
    typeOfRef,
    typeToStructType,
    unaliasType,
    widthOfType
    ) where

    import ParseTree
    import Context
    import Report
    import Bits

    import qualified Data.Ix as Ix
    import Data.List
    import Data.Maybe
    import Data.Bits

    bitType :: Type
    bitType = Bits 1

    builtinTypeWidth :: Int
    builtinTypeWidth = 64

    bitArrayType :: Int -> Type
    bitArrayType n = ArrayType (Interval (0, n' - 1) (typeForInt (n' - 1))) bitType
        where n' = toInteger n

    isTokenValueExpr :: [Context Decl] -> Expr -> Bool
    isTokenValueExpr cs (ValueExpr _ typ (IntValue 0)) = widthOfType cs typ == 0
    isTokenValueExpr _ _ = False

    typeForInt :: Integer -> Type
    typeForInt int
        | int == -1 = SignedBits 1
        | int < -1 = SignedBits $ 1 + intWidth (-int - 1)
        | otherwise = Bits $ intWidth int

    notNumeric :: [Type] -> a
    notNumeric [t] = error $ "`" ++ show t ++ "' is not a numeric type"
    notNumeric ts = error $ "one of" ++ types ++ " is not a numeric type"
        where
            types = concatMap showType ts
            showType t = " `" ++ show t ++ "'"

    rangeOfNumType :: Type -> (Integer, Integer)
    rangeOfNumType (Bits width) = (0, bit width - 1)
    rangeOfNumType (SignedBits width) = (- (bit (width - 1)), bit (width - 1) - 1)
    rangeOfNumType t = notNumeric [t]

    typeOfRange :: (Integer, Integer) -> Type
    typeOfRange (low, high) = smallestNumTypeEnclosing (typeForInt high) (typeForInt low)

    rangeOfEnumType :: TypeBody -> (Integer, Integer)
    rangeOfEnumType (EnumType _ es _) = range
        where
            range = (minimum values, maximum values)
            values = map enumElemValue $ contextBindingsList es
            enumElemValue binding = fromJust $ constExpr $ declExpr $ bindingValue binding
    rangeOfEnumType typ = error $ "rangeOfEnumType: not an enum type `" ++ show typ ++ "'"

    rangeOfIndexType :: [Context Decl] -> Type -> (Integer, Integer)
    rangeOfIndexType cs typ = body $ typeGetUnaliasedBody cs typ
        where
            body typ@(EnumType {}) = rangeOfEnumType typ
            body (AliasType _ typ@(Bits {})) = rangeOfNumType typ
            body (AliasType _ typ@(SignedBits {})) = rangeOfNumType typ
            body typ = error $ "rangeOfIndexType: not an index type `" ++ show typ ++ "'"

    smallestNumTypeEnclosing :: Type -> Type -> Type
    smallestNumTypeEnclosing (Bits uw1) (Bits uw2) = Bits $ max uw1 uw2
    smallestNumTypeEnclosing (SignedBits w1) (Bits uw2) = SignedBits $ 1 + max (w1 - 1) uw2
    smallestNumTypeEnclosing (Bits uw1) (SignedBits w2) = SignedBits $ 1 + max uw1 (w2 - 1)
    smallestNumTypeEnclosing (SignedBits w1) (SignedBits w2) = SignedBits $ max w1 w2
    smallestNumTypeEnclosing t1 t2 = notNumeric [t1, t2]

    widthOfType :: [Context Decl] -> Type -> Int
    widthOfType _ (Bits width) = width
    widthOfType _ (SignedBits width) = width
    widthOfType cs (ArrayType interval elemType) = (widthOfType cs elemType) * (intervalSize interval)
    widthOfType cs (StructArrayType interval elemType) = (widthOfType cs elemType) * (intervalSize interval)
    widthOfType _ (BuiltinType _) = builtinTypeWidth
    widthOfType cs (StructRecordType _ _ overType) = widthOfType cs overType
    widthOfType cs (StructEnumType _ _ overType) = widthOfType cs overType
    widthOfType _ NoType = 0
    widthOfType cs typ = widthOfTypeBody cs $ typeGetUnaliasedBody cs typ

    widthOfTypeBody :: [Context Decl] -> TypeBody -> Int
    widthOfTypeBody cs (AliasType _ typ) = widthOfType cs typ
    widthOfTypeBody cs (RecordType _ _ typ) = widthOfType cs typ
    widthOfTypeBody cs (EnumType _ _ typ) = widthOfType cs typ

    typeIsSigned :: [Context Decl] -> Type -> Bool
    typeIsSigned _ (SignedBits {}) = True
    typeIsSigned _ (Bits {}) = False
    typeIsSigned _ (ArrayType {}) = False
    typeIsSigned _ (StructArrayType {}) = False
    typeIsSigned _ (StructRecordType {}) = False
    typeIsSigned cs (StructEnumType _ _ overType) = typeIsSigned cs overType
    typeIsSigned _ NoType = False
    typeIsSigned cs typ = typeBodyIsSigned cs $ typeGetUnaliasedBody cs typ

    typeBodyIsSigned :: [Context Decl] -> TypeBody -> Bool
    typeBodyIsSigned cs (AliasType _ typ) = typeIsSigned cs typ
    typeBodyIsSigned cs (EnumType _ _ typ) = typeIsSigned cs typ
    typeBodyIsSigned _ _ = False

    data TypeClass = NumClass | EnumClass | ArrayClass | RecordClass | BuiltinClass | NoTypeClass
        deriving (Eq, Show, Read)
    
    classOfType :: [Context Decl] -> Type -> TypeClass
    classOfType _ NoType = NoTypeClass
    classOfType _ (Bits {}) = NumClass
    classOfType _ (SignedBits {}) = NumClass
    classOfType _ (ArrayType {}) = ArrayClass
    classOfType _ (BuiltinType {}) = BuiltinClass
    classOfType cs typ = classOfTypeBody cs $ typeGetUnaliasedBody cs typ

    classOfTypeBody :: [Context Decl] -> TypeBody -> TypeClass
    classOfTypeBody cs (AliasType _ typ) = classOfType cs typ
    classOfTypeBody _ (EnumType {}) = EnumClass
    classOfTypeBody _ (RecordType {}) = RecordClass
    
    intInWidth :: Integer -> Int -> Integer
    intInWidth i w = if i < 0 then bit w + i else i

    makeConsValue :: [Context Decl] -> Type -> [Expr] -> Value
    makeConsValue cs typ es
        | dcs == 0 = IntValue value
        | otherwise = ImpValue $ Imp value dcs
        where
            valueDcPairs = map getImpPairs es
            valueDcPairs' = map insertValue $ zip3 offsets valueDcPairs widths
            (value, dcs) = foldl' addImp (0, 0) valueDcPairs'

            addImp (v1, dc1) (v2, dc2) = (v1 + v2, dc1 + dc2)

            widths = map (widthOfType cs) $ fromJust $ consTypeElemTypes cs typ

            getImpPairs expr
                | isJust int = (fromJust int, 0)
                | otherwise = (value, dcs)
                where
                    int = constExpr expr
                    Just (Imp value dcs) = constImpExpr expr

            offsets = scanl (+) 0 widths
            insertValue (offset, (value, dcs), width) = (insert value, insert dcs)
                where insert value = (intInWidth value width) `shiftL` offset

    consTypeElemTypes :: [Context Decl] -> Type -> Maybe [Type]
    consTypeElemTypes cs typ = elemTypes $ typeGetUnaliasedBody cs typ
        where
            elemTypes (RecordType _ formals _) = Just $ map recordElemType formals
                where recordElemType (RecordElem _ _ elemType) = elemType
            elemTypes (AliasType _ (ArrayType interval elemType)) = Just $
                map (const elemType) $ intervalIndices interval
            elemTypes _ = Nothing

    arrayElemType :: Type -> Type
    arrayElemType (ArrayType _ elemType) = elemType
    arrayElemType typ = error $ "arrayElemType: not an array type `" ++ show typ ++ "'"

    arrayInterval :: Type -> Interval
    arrayInterval (ArrayType interval _) = interval
    arrayInterval typ = error $ "arrayInterval: not an array type `" ++ show typ ++ "'"

    typeCoercable :: [Context Decl] -> Type -> Type -> Bool
    typeCoercable cs fromType toType = coercable (unaliasType cs fromType) (unaliasType cs toType)
        where
            coercable (SignedBits _) (Bits _) = False
            coercable (Bits wFrom) (SignedBits wTo) = wTo > wFrom
            coercable (Bits wFrom) (Bits wTo) = wTo >= wFrom
            coercable (SignedBits wFrom) (SignedBits wTo) = wTo >= wFrom
            -- coercable fromType toType = typeEquiv cs fromType toType
            coercable _ _ = False

    intCoercable :: [Context Decl] -> Integer -> Type -> Bool
    intCoercable cs int toType = coercable (unaliasType cs toType)
        where
            coercable (Bits wTo) = int >= 0 && int <= maxInt wTo
            coercable (SignedBits wTo)
                | int >= 0 = int <= maxInt (wTo - 1)
                | int < 0 = int >= (-1 - maxInt (wTo - 1))
            coercable _ = False

            maxInt width = bit width - 1

    typeEquiv :: [Context Decl] -> Type -> Type -> Bool
    typeEquiv cs typ1 typ2 = equiv (unaliasType cs typ1) (unaliasType cs typ2)
        where
            equiv (Bits w1) (Bits w2) = w1 == w2
            equiv (SignedBits w1) (SignedBits w2) = w1 == w2
            equiv (Type ref1) (Type ref2) = ref1 == ref2 -- for the same context path
            equiv (BuiltinType name1) (BuiltinType name2) = name1 == name2
            equiv (ArrayType interval1 elemType1) (ArrayType interval2 elemType2) = interval1 == interval2 &&
                elemType1 `equiv` elemType2
            equiv _ _ = False

    unaliasType :: [Context Decl] -> Type -> Type
    unaliasType cs (ArrayType interval elemType) = ArrayType interval $ unaliasType cs elemType
    unaliasType cs (Type ref) = case decl of
        TypeDecl {} -> unaliasedType
        TypeParamDecl {} -> Type ref
        _ -> error $ "unaliasType: not a type decl: " ++ show decl ++ " " ++ show ref
        where
            decl = bindingValue $ findBindingByRef cs ref
            unaliasedType = case declTypeBody decl of
                AliasType _ typ -> unaliasType cs typ
                _ -> Type ref
    unaliasType _ typ = typ

    typeExpr :: Type -> Expr -> Expr
    typeExpr typ (BinExpr pos _ op left right) = BinExpr pos typ op left right
    typeExpr typ (AppendExpr pos _ left right) = AppendExpr pos typ left right
    typeExpr typ (UnExpr pos _ op expr) = UnExpr pos typ op expr
    typeExpr typ (ConsExpr pos _ consType es) = ConsExpr pos typ consType es
    typeExpr typ (BuiltinCallExpr pos callable ctx actuals _) = BuiltinCallExpr pos callable ctx actuals typ
    typeExpr typ (BitfieldExpr pos _ range expr) = BitfieldExpr pos typ range expr
    typeExpr typ (ExtendExpr pos _ width signedness expr) = ExtendExpr pos typ width signedness expr
    typeExpr typ (ValueExpr pos _ value) = ValueExpr pos typ value
    typeExpr typ (CaseExpr pos expr impss exprs) = CaseExpr pos expr impss (map (typeExpr typ) exprs)
    typeExpr typ (VarRead pos _ range ref) = VarRead pos typ range ref
    typeExpr typ (OpenChanRead pos _ range ref) = OpenChanRead pos typ range ref
    typeExpr _ expr = expr

    typeOfRef :: [Context Decl] -> Ref -> Type
    typeOfRef cs ref = typeOfDecl $ bindingValue $ findBindingByRef cs ref

    typeOfExpr :: [Context Decl] -> Expr -> Type
    typeOfExpr _ (BinExpr _ typ _ _ _) = typ
    typeOfExpr _ (AppendExpr _ typ _ _) = typ
    typeOfExpr _ (UnExpr _ typ _ _) = typ
    typeOfExpr _ (CastExpr _ typ _) = typ
    typeOfExpr _ (TypeCheckExpr _ _ typ _) = typ
    typeOfExpr cs (ConstCheckExpr _ expr) = typeOfExpr cs expr
    typeOfExpr cs (ArrayElemTypeCheckExpr _ _ expr) = typeOfExpr cs expr
    typeOfExpr _ (BuiltinCallExpr _ _ _ _ typ) = typ
    typeOfExpr _ (ConsExpr _ typ _ _) = typ
    typeOfExpr _ (BitfieldExpr _ typ _ _) = typ
    typeOfExpr _ (ExtendExpr _ typ _ _ _) = typ
    typeOfExpr _ (ValueExpr _ typ _) = typ
    typeOfExpr _ (VarRead _ typ _ _) = typ
    typeOfExpr _ (OpenChanRead _ typ _ _) = typ
    typeOfExpr cs (PartialArrayedRead _ ref) = typeOfRef cs ref
    typeOfExpr cs (MaybeTypeExpr _ ref) = typeOfRef cs ref
    typeOfExpr cs (MaybeOtherExpr _ ref) = typeOfRef cs ref
    typeOfExpr cs (Expr _ ref) = typeOfRef cs ref
    typeOfExpr cs (CaseExpr _ _ _ (expr:_)) = typeOfExpr cs expr
    typeOfExpr _ expr = error $ "can't type " ++ show expr

    typeOfChan :: [Context Decl] -> Chan -> Type
    typeOfChan cs (Chan _ ref) = typeOfRef cs ref
    typeOfChan cs (CheckChan _ _ chan) = typeOfChan cs chan
    typeOfChan _ _ = NoType

    typeOfDecl :: Decl -> Type
    typeOfDecl decl = declType decl

    typeLvalue :: Type -> Lvalue -> Lvalue
    typeLvalue typ (BitfieldLvalue pos _ range lvalue) = BitfieldLvalue pos typ range lvalue
    typeLvalue typ (VarWrite pos _ range ref) = VarWrite pos typ range ref
    typeLvalue typ (CaseLvalue pos expr impss lvalues) = CaseLvalue pos expr impss (map (typeLvalue typ) lvalues)
    typeLvalue _ lvalue = lvalue

    typeOfLvalue :: [Context Decl] -> Lvalue -> Type
    typeOfLvalue _ (BitfieldLvalue _ typ _ _) = typ
    typeOfLvalue _ (VarWrite _ typ _ _) = typ
    typeOfLvalue cs (CaseLvalue _ _ _ (lvalue:_)) = typeOfLvalue cs lvalue
    typeOfLvalue _ lvalue = error $ "can't type " ++ show lvalue

    exprValue :: Expr -> Maybe Value
    exprValue (ValueExpr _ _ value) = Just value
    exprValue _ = Nothing

    constExpr :: Expr -> Maybe Integer
    constExpr (ValueExpr _ _ (IntValue int)) = Just int
    constExpr _ = Nothing

    constImpExpr :: Expr -> Maybe Implicant
    constImpExpr (ValueExpr _ _ (ImpValue imp)) = Just imp
    constImpExpr _ = Nothing

    constImpOrIntExpr :: Expr -> Maybe Implicant
    constImpOrIntExpr (ValueExpr _ _ (IntValue int)) = Just $ Imp int 0
    constImpOrIntExpr (ValueExpr _ _ (ImpValue imp)) = Just imp
    constImpOrIntExpr _ = Nothing

    constIndexExpr :: [Context Decl] -> Expr -> Maybe Integer
    constIndexExpr cs (ValueExpr _ typ (IntValue int)) = case classOfType cs typ of
        EnumClass -> Just int
        NumClass -> Just int
        _ -> Nothing
    constIndexExpr _ _ = Nothing

    typeGetUnaliasedBody :: [Context Decl] -> Type -> TypeBody
    typeGetUnaliasedBody cs typ = typeGetBody cs $ unaliasType cs typ

    typeGetBody :: [Context Decl] -> Type -> TypeBody
    typeGetBody cs (Type ref) = body
        where
            body = declBody $ bindingValue $ findBindingByRef cs ref

            declBody (TypeDecl _ typeBody) = typeBody
            declBody decl = error $ "Huh2? " ++ show decl
    typeGetBody _ typ = AliasType NoPos typ

    intervalIndices :: Interval -> [Integer]
    intervalIndices = Ix.range . intervalRange

    intervalSize :: Interval -> Int
    intervalSize = Ix.rangeSize . intervalRange

    intervalIndex :: Interval -> Integer -> Int
    intervalIndex = Ix.index . intervalRange

    -- intervalFromRange : make an Interval over the given range.  This involves synthesising the interval range type.
    intervalFromRange :: (Integer, Integer) -> Interval
    intervalFromRange range = Interval range (typeOfRange range)

    showTypeName :: [Context Decl] -> Type -> String
    showTypeName cs (Type ref) = string
        where
            binding = findBindingByRef cs ref
            name = bindingName $ binding
            decl = bindingValue $ binding
            string = name ++ case decl of
                TypeDecl _ (AliasType _ typ) -> " (" ++ showTypeName cs typ ++ ")"
                _ -> ""
    showTypeName _ (Bits width) = show width ++ " bits"
    showTypeName _ (SignedBits width) = show width ++ " signed bits"
    showTypeName cs (ArrayType (Interval (low, high) ivType) elemType) = "array " ++ intervalStr
        ++ " of " ++ elemTypeStr
        where
            asStr val typ = "(" ++ show val ++ " as " ++ showTypeName cs typ ++ ")"
            intervalStr = if classOfType cs ivType == EnumClass
                then asStr low ivType ++ " .. " ++ asStr high ivType
                else show low ++ " .. " ++ show high
            elemTypeStr = showTypeName cs elemType
    showTypeName _ (StructRecordType name _ _) = name
    showTypeName _ (StructEnumType name _ _) = name
    showTypeName cs (StructArrayType interval elemType) = showTypeName cs (ArrayType interval elemType)
    showTypeName _ other = show other

    numTypeUnsignedWidth :: Type -> Int
    numTypeUnsignedWidth (Bits width) = width
    numTypeUnsignedWidth (SignedBits width) = width - 1
    numTypeUnsignedWidth _ = error "numTypeUnsignedWidth: not a numeric type"

    findRecElemByName :: [Context Decl] -> TypeBody -> String -> Maybe (Int, Type)
    findRecElemByName cs (RecordType _ es _) name = findRecElem es 0
        where
            findRecElem [] _ = Nothing
            findRecElem ((RecordElem _ name' typ):recElems) offset
                | name' == name = Just (offset, typ)
                | otherwise = findRecElem recElems (offset + (widthOfType cs typ))
    findRecElemByName _ _ _ = error "findRecElemByName: not a RecordType"

    typeToStructType :: [Context Decl] -> Type -> Type
    typeToStructType cs typ = body $ unaliasType cs typ
        where
            self = typeToStructType cs

            body (Type ref) = typeBodyToStructType cs name $ declTypeBody decl
                where
                    decl = bindingValue binding
                    name = bindingName binding
                    binding = findBindingByRef cs ref
            body (ArrayType interval subType) = StructArrayType interval $ self subType
            body otherType = otherType

    isStructType :: Type -> Bool
    isStructType (StructRecordType {}) = True
    isStructType (StructArrayType {}) = True
    isStructType (StructEnumType {}) = True
    isStructType _ = False

    typeBodyToStructType :: [Context Decl] -> String -> TypeBody -> Type
    typeBodyToStructType _ _ (AliasType _ typ) = typ
    typeBodyToStructType cs typeName (RecordType _ es overType) = StructRecordType typeName
        (map (recElemToST cs) es) (typeToStructType cs overType)
    typeBodyToStructType cs typeName (EnumType _ context overType) = StructEnumType typeName
        (mapMaybe bindingToEnumPair bindings) (typeToStructType cs overType)
        where
            bindings = contextBindingsList context
            bindingToEnumPair (Binding _ name _ _ (ExprDecl _ (ValueExpr _ _ (IntValue int)))) =
                Just (SimpleBinding name int)
            bindingToEnumPair _ = Nothing

    -- typeBuiltinOffsets : produces a list of bit offsets for the given type which identify
    --    each builtin typed element of a value of that type. `offset' is added to each offset in the returned list
    typeBuiltinOffsets :: [Context Decl] -> Int -> Type -> [Int]
    typeBuiltinOffsets cs offset typ = body $ unaliasType cs typ
        where
            body (Type ref) = typeBodyBuiltinOffsets cs offset $ declTypeBody decl
                where
                    decl = bindingValue binding
                    binding = findBindingByRef cs ref
            body (ArrayType interval subType) = array interval subType
            body (StructArrayType interval subType) = array interval subType
            body (StructRecordType _ es overType) =
                typeBodyBuiltinOffsets cs offset (RecordType undefined es overType)
            body (BuiltinType {}) = [offset]
            body _ = []

            array interval subType = concatMap offsetElemRanges elemOffsets
                where
                    offsetElemRanges elemOffset = map (+ elemOffset) elemRanges
                    elemOffsets = map (* elemWidth) [0..intervalSize interval - 1]
                    elemWidth = widthOfType cs subType
                    elemRanges = typeBuiltinOffsets cs offset subType

    typeBodyBuiltinOffsets :: [Context Decl] -> Int -> TypeBody -> [Int]
    typeBodyBuiltinOffsets cs offset (AliasType _ typ) = typeBuiltinOffsets cs offset typ
    typeBodyBuiltinOffsets cs offset0 (RecordType _ es _) = concat elemRangess
        where
            (_, elemRangess) = mapAccumL elemRanges offset0 es

            elemRanges offset (RecordElem _ _ elemType) = (offset + width, typeBuiltinOffsets cs offset elemType)
                where width = widthOfType cs elemType
    typeBodyBuiltinOffsets _ _ (EnumType {}) = []

    recElemToST :: [Context Decl] -> RecordElem -> RecordElem
    recElemToST cs (RecordElem pos name typ) = RecordElem pos name (typeToStructType cs typ)

    recoverValue :: [Context Decl] -> Type -> Integer -> Integer
    recoverValue cs typ val
        | typeIsSigned cs typ = recoverSign (widthOfType cs typ) val
        | otherwise = val

