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

module Expr (
    binExprEval,
    unExprEval,
    indexLvalueEval,
    indexExprEval,
    sliceEval,
    castExprEval,
    makeInterval
    ) where

    import ParseTree
    import Misc
    import Type
    import Context
    import Bits
    import Print
    import Report

    import Data.Maybe
    import Data.Ix
    import Data.Bits

    ok :: node -> (Completeness, node)
    ok node = (Complete, node)

    numType :: Int -> Bool -> Type
    numType unsignedWidth True = SignedBits $ unsignedWidth + 1
    numType unsignedWidth False = Bits unsignedWidth

    exprIsConst :: Expr -> Bool
    exprIsConst = isJust . constExpr

    exprIntValue :: Expr -> Integer
    exprIntValue = fromJust . constExpr

    constResult :: Pos -> Integer -> Expr
    constResult pos int = ValueExpr pos (typeForInt int) $ IntValue int

    exprIsNaturalConst :: Expr -> Bool
    exprIsNaturalConst expr
        | exprIsConst expr = (exprIntValue expr) >= 0
        | otherwise = False

    -- ^ * / % + - @ < > <= >= = /= and xor or
    binExprEval :: [Context Decl] -> Expr -> (Completeness, Expr)
    binExprEval cs expr@(BinExpr pos origType op l r)
        | classL == classR = body classL sameType
        | otherwise = localError "binary operator arguments must be of the same type class"
        where
            symbol = binOpSymbol op
            typeL = unaliasType cs $ typeOfExpr cs l
            typeR = unaliasType cs $ typeOfExpr cs r
            classL = classOfType cs typeL
            classR = classOfType cs typeR
            sameType = typeEquiv cs typeL typeR

            localError message = (parseNodeError cs pos expr message, expr)

            body NumClass _ = num op
            body EnumClass True = enum op
            body ArrayClass _ = array op
            body RecordClass True = record op
            body _ False = localError ("cannot perform binary operation `" ++ symbol ++
                "' on arguments of different types")
            body BuiltinClass True
                | typeEquiv cs typeL stringType = string op
                | otherwise = localError ("cannot perform binary operation `" ++ symbol ++
                    "' on builtin typed expressions")
            body klass _ = error $ "binExprEval: bad class `" ++ show klass ++ "'"

            stringType = BuiltinType "String"

            w = if unsignedWidthL > unsignedWidthR then l else r

            typeW = unaliasType cs $ typeOfExpr cs w

            isConstR = exprIsConst r
            isConstL = exprIsConst l
            valueR = exprIntValue r
            valueL = exprIntValue l
            unsignedWidthR = numTypeUnsignedWidth typeR
            unsignedWidthL = numTypeUnsignedWidth typeL
            unsignedWidthW = numTypeUnsignedWidth typeW

            numTypeResult unsignedWidth signed
                | origType == NoType = typeExpr (numType unsignedWidth signed) expr
                | otherwise = expr

            num BinPow
                | exprIsNaturalConst r = ok $ tryConst isConstL
                | otherwise =
                    localError "for operator `^', right hand argument must be a non-negative numeric expression"
                    where
                        tryConst True = constResult pos $ valueL ^ valueR
                        tryConst False = numTypeResult (valueR' * unsignedWidthL) signedOp
                        valueR' = fromEnum valueR
            num BinMul = ok $ tryConst isConstL isConstR
                where
                    tryConst True True = constResult pos $ valueL * valueR
                    tryConst _ _ = numTypeResult (unsignedWidthR + unsignedWidthL) signedOp
            num BinDiv = divisive div
            num BinMod = divisive rem
            num BinAdd = additive (+) signedOp
            num BinSub = additive (-) True
            num BinAnd = logical (.&.)
            num BinXor = logical xor
            num BinOr = logical (.|.)
            num BinLT = relation (<)
            num BinGT = relation (>)
            num BinLE = relation (<=)
            num BinGE = relation (>=)
            num BinEQ = relation (==)
            num BinNE = relation (/=)
            -- num _ = localError ("unsupported operator `" ++ symbol ++ "' for numeric typed arguments")

            enum BinLT = relation (<)
            enum BinGT = relation (>)
            enum BinLE = relation (<=)
            enum BinGE = relation (>=)
            enum BinEQ = relation (==)
            enum BinNE = relation (/=)
            enum _ = localError ("unsupported operator `" ++ symbol ++ "' for enumeration typed arguments")

            array BinEQ = relation (==)
            array BinNE = relation (/=)
            array _ = localError ("unsupported operator `" ++ symbol ++ "' for array typed arguments")

            record BinEQ = relation (==)
            record BinNE = relation (/=)
            record _ = localError ("unsupported operator `" ++ symbol ++ "' for record typed arguments")

            -- NameCallable?
            string BinAdd = ok $ BuiltinCallExpr pos "StringAppend" [] [l, r] stringType
            string _ = localError ("unsupported operator `" ++ symbol ++ "' for string typed arguments")

            signedOp = typeIsSigned cs typeL || typeIsSigned cs typeR

            additive f signed = ok $ tryConst isConstL isConstR
                where
                    tryConst True True = constResult pos $ valueL `f` valueR
                    tryConst _ _ = numTypeResult (unsignedWidthW + 1) signed

            divisive f
                | not isConstR || valueR /= 0 = ok $ tryConst isConstL isConstR
                | otherwise = localError ("for operator `" ++ symbol ++
                    "', right hand expression must be non-zero")
                    where
                        tryConst True True = constResult pos $ valueL `f` valueR
                        tryConst _ _ = numTypeResult unsignedWidthL signedOp

            logical f
                | sameType && not signedOp = ok $ tryConst isConstL isConstR
                | otherwise = localError ("logical operator `" ++ symbol ++
                    "' must have unsigned arguments of the same width")
                    where
                        tryConst True True = constResult pos $ f valueL valueR
                        tryConst _ _ = numTypeResult unsignedWidthL False

            relation f = ok $ tryConst isConstL isConstR
                where
                    tryConst True True = constResult pos $ boolToBit $ valueL `f` valueR
                    tryConst _ _ = typeExpr (Bits 1) expr
    binExprEval _ _ = error "binExprEval: not a BinExpr"

    -- # not log -
    unExprEval :: [Context Decl] -> Expr -> (Completeness, Expr)
    unExprEval cs expr@(UnExpr pos origType op r) = body classR
        where
            symbol = unOpSymbol op
            body NumClass = num op
            body BuiltinClass = localError ("cannot perform unary operation `" ++ symbol ++ "' on given expression")
            body _ = other op

            localError message = (parseNodeError cs pos expr message, expr)
            typeR = unaliasType cs $ typeOfExpr cs r
            classR = classOfType cs typeR
            signedOp = (typeIsSigned cs typeR)

            numTypeResult unsignedWidth signed
                | origType == NoType = typeExpr (numType unsignedWidth signed) expr
                | otherwise = expr

            valueR = exprIntValue r
            unsignedWidthR = numTypeUnsignedWidth typeR

            num UnNot
                | not signedOp = ok $ if exprIsConst r    
                    then constResult pos $ bitNot unsignedWidthR valueR
                    else numTypeResult unsignedWidthR False
                | otherwise = localError ("logical operator `" ++ symbol ++ "' must have an unsigned argument")
            num UnLog
                | exprIsNaturalConst r = ok $ constResult pos $ fromIntegral $ log2 valueR
                | otherwise = localError "for operator `log', argument must be a non-negative numeric expression"
            num UnNeg = ok $ if exprIsConst r
                then constResult pos $ (- valueR)
                else numTypeResult unsignedWidthR True
            num UnSmash = other UnSmash
            -- num _ = localError ("unsupported operator `" ++ symbol ++ "' for numeric typed arguments")

            other UnSmash
                | origType == NoType = ok $ typeExpr (bitArrayType (widthOfType cs typeR)) r
                | otherwise = ok r
            other _ = localError ("cannot perform unary operation `" ++ symbol ++ "' on given expression")
    unExprEval _ _ = error "unExprEval: not an UnExpr"

    -- makeInterval : make an Interval out of a pair of constant expressions.  Does all the error handling for
    --    for the expressions being non-const and returns the usual (Completeness, node) pair that eval does.
    makeInterval :: ShowParseNode node => Pos -> [Context Decl] -> node -> Expr -> Expr -> (Completeness, Interval)
    makeInterval pos cs errorNode left right
        | isJust leftIndex && isJust rightIndex = ok $ Interval range rangeType
        | otherwise = (parseNodeError cs pos errorNode "bad range expressions", IntervalE pos left right)
        where
            leftIndex = constIndexExpr cs left
            rightIndex = constIndexExpr cs right
            leftType = typeOfExpr cs left
            leftValue = fromJust leftIndex
            rightValue = fromJust rightIndex
            isEnumLeft = classOfType cs leftType == EnumClass

            range = (min leftValue rightValue, max leftValue rightValue)
            rangeType = if isEnumLeft
                then leftType
                else typeOfRange range

    makeBitfield :: Pos -> [Context Decl] -> (Pos -> Type -> Slice Int -> arr -> node) ->
        Type -> arr -> Integer -> node
    makeBitfield pos cs bitfieldCons (ArrayType interval elemType) array i = bitfield
        where
            bitfield = bitfieldCons pos elemType (sliceFromLH lowBitIndex highBitIndex) array

            elemWidth = widthOfType cs elemType
            lowBitIndex = (intervalIndex interval i) * elemWidth
            highBitIndex = lowBitIndex + elemWidth - 1
    makeBitfield _ _ _ typ _ _ = error $ "makeBitfield: not an array type `" ++ show typ ++ "'"

    indexExprEval :: Pos -> [Context Decl] -> Expr -> Expr -> Expr -> (Completeness, Expr)
    indexExprEval pos cs node array index =
        indexEval pos cs BitfieldExpr CaseExpr typeOfExpr node array index

    indexLvalueEval :: Pos -> [Context Decl] -> Lvalue -> Lvalue -> Expr -> (Completeness, Lvalue)
    indexLvalueEval pos cs node array index =
        indexEval pos cs BitfieldLvalue CaseLvalue typeOfLvalue node array index

    -- indexEval : evaluate an expression or lvalue indexing.
    --    Returns (Complete, bitfieldCons (lowIndex, highIndex) array) on success or
    --    (/= Complete, node) on failure.
    --    For non-const indices, will build a case expression
    indexEval :: ShowParseNode node => Pos -> [Context Decl] ->
        (Pos -> Type -> Slice Int -> arr -> node) ->
        (Pos -> Expr -> [[Implicant]] -> [node] -> node) ->
        ([Context Decl] -> arr -> Type) ->
        node -> arr -> Expr -> (Completeness, node)
    indexEval pos cs bitfieldCons caseCons typeOfFunc node array index
        | indexOutOfRange = (parseNodeError cs pos node "constant index is not in range", node)
        | indexIsConst = ok $ makeBitfield pos cs bitfieldCons arrayType array index'
        | otherwise = ok $ caseCons pos index impss guardeds
        where
            constIndex = constExpr index
            indexIsConst = isJust constIndex
            indexOutOfRange = if indexIsConst then not $ indexInInterval interval index' else False
            arrayType@(ArrayType interval _) = unaliasType cs $ typeOfFunc cs array
            index' = fromJust $ constIndex

            (impss, guardeds) = unzip $ map makeCase $ range $ intervalRange interval
            makeCase i = ([Imp i 0], makeBitfield pos cs bitfieldCons arrayType array i)

    -- sliceEval : like indexEval but for slices
    sliceEval :: ShowParseNode node => Pos -> [Context Decl] -> (Pos -> Type -> Slice Int -> arr -> node) ->
        node -> arr -> Type -> Expr -> Expr -> (Completeness, node)
    sliceEval pos cs bitfieldCons node array arrayType li ri
        | indicesOutOfRange = (parseNodeError cs pos node "constant indices are out of range", node)
        | indicesAreConst = ok $ bitfieldCons pos resultType (sliceFromLH lowBitIndex highBitIndex) array
        | otherwise = (parseNodeError cs pos node "non-const slicing not supported", node)
        where
            indicesAreConst = comp == Complete
            indicesOutOfRange = if indicesAreConst
                then not $ indexInInterval interval low && indexInInterval interval high else False
            ArrayType interval elemType = unaliasType cs arrayType
            (comp, range@(Interval (low, high) _)) = makeInterval pos cs node li ri
            elemCount = intervalSize range
            elemWidth = widthOfType cs elemType
            lowBitIndex = (intervalIndex interval low) * elemWidth
            highBitIndex = lowBitIndex + (elemCount * elemWidth) - 1
            resultHigh = toInteger (elemCount - 1)
            resultType = ArrayType (intervalFromRange (0, resultHigh)) elemType

    castExprEval :: [Context Decl] -> Expr -> (Completeness, Expr)
    castExprEval cs castExpr@(CastExpr pos typ expr)
        | eitherBuiltin = (parseNodeError cs pos castExpr "cannot cast builtin typed values", castExpr)
        | toWidth == 0 = result $ ValueExpr pos typ $ IntValue 0
        -- Same width but may involve a sign change
        | fromWidth == toWidth = result $ BitfieldExpr pos typ (sliceFromOW 0 toWidth) expr
        -- Truncate
        | fromWidth > toWidth = result $ BitfieldExpr pos typ (sliceFromOW 0 toWidth) expr
        -- Extend
        | fromWidth < toWidth = result $ ExtendExpr pos typ toWidth (bothNumeric && fromSigned) expr
        where
            toType = unaliasType cs typ
            fromType = unaliasType cs $ typeOfExpr cs expr

            fromClass = classOfType cs fromType
            toClass = classOfType cs toType

            bothNumeric = fromClass == NumClass && toClass == NumClass
            fromSigned = typeIsSigned cs fromType

            eitherBuiltin = fromClass == BuiltinClass || toClass == BuiltinClass

            fromWidth = widthOfType cs fromType
            toWidth = widthOfType cs toType

            result expr = ok $ typeExpr typ expr
    castExprEval _ _ = error "castExprEval: not a CastExpr"
