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

module BalsaParser (
    balsaParser
    ) where

    import Parser
    import Report
    import ParseTree
    import BalsaLexer
    import Misc
    import Type
    import Context
    import Print
    import Bits

    import Data.List
    import Data.Maybe
    import Control.Monad
    import Data.Bits

    type BalsaParser = Parser (LA1Tokens BalsaToken) Report

    -- Terminals

    wrong :: String -> Report
    wrong = Report NoPos

    expectingError :: LA1Tokens BalsaToken -> String -> BalsaParser syn
    expectingError (LA1Tokens [BalsaErrorToken _ _ (Just report)]) _ = parserFail report
    expectingError (LA1Tokens [BalsaErrorToken pos _ Nothing]) _ = parserFail (Report pos "lexer error")
    expectingError _ str = parserFail $ expecting str

    identifier :: BalsaParser (Pos, String)
    identifier = matchToken eofReport match where
        match (LA1Tokens [BalsaName pos str]) = return (pos, str)
        match token = expectingError token "an identifier"

    keyword :: String -> BalsaParser Pos
    keyword word = matchToken eofReport match where
        match (LA1Tokens [BalsaKeyword pos word2]) | word == word2 = return pos
        match token = expectingError token ("keyword `" ++ word ++ "'")

    label :: BalsaParser (Pos, String)
    label = matchToken eofReport match where
        match (LA1Tokens [BalsaLabel pos word _]) = return (pos, word)
        match token = expectingError token "a label"

    pathElement :: BalsaParser String
    pathElement = matchToken eofReport match where
        match (LA1Tokens [BalsaName _ str]) = return str
        match (LA1Tokens [BalsaKeyword _ str]) = return str
        match token = expectingError token "a dotted path element"

    integer :: BalsaParser (Pos, Integer)
    integer = matchToken eofReport match where
        match (LA1Tokens [BalsaInt pos _ int]) = return (pos, int)
        match token = expectingError token "an integer"

    implicant :: BalsaParser (Pos, Integer, Integer)
    implicant = matchToken eofReport match where
        match (LA1Tokens [BalsaImplicant pos _ v dc]) = return (pos, v, dc)
        match token = expectingError token "an implicant"

    string :: BalsaParser (Pos, String)
    string = matchToken eofReport match where
        match (LA1Tokens [BalsaString pos _ str]) = return (pos, str)
        match token = expectingError token "a string"

    identifierList :: BalsaParser [String]
    identifierList = sepList (keyword ",") $ do
        (_, str) <- identifier
        return str

    optionalKeyword :: String -> BalsaParser Pos
    optionalKeyword word = optional NoPos $ keyword word

    -- Binding helpers

    otherBinding :: String -> Decl -> Binding Decl
    otherBinding name decl = Binding 0 name OtherNamespace Incomplete decl

    typeBinding :: String -> Pos -> TypeBody -> Binding Decl
    typeBinding name pos decl = Binding 0 name TypeNamespace Incomplete $ TypeDecl pos decl

    procBinding :: String -> Decl -> Binding Decl
    procBinding name decl = Binding 0 name ProcNamespace Incomplete decl

    makeBindings :: [String] -> Decl -> [Binding Decl]
    makeBindings names decl = map binding names
        where binding name = otherBinding name decl

    makeChans :: [String] -> Type -> Pos -> (Decl -> Decl) -> [Binding Decl]
    makeChans names typ pos declWrapper = makeBindings names $ declWrapper $ ChanDecl pos typ

    makePorts :: [String] -> Direction -> Type -> Pos -> (Decl -> Decl) -> [Binding Decl]
    makePorts names direction typ pos declWrapper =
        makeBindings names $ declWrapper $ PortDecl pos direction typ

    makeTypeParams :: [String] -> Pos -> [Binding Decl]
    makeTypeParams names pos = map binding names
        where binding name = Binding 0 name TypeNamespace Incomplete (TypeParamDecl pos)

    makeParams :: Bool -> [String] -> Pos -> Type -> [Binding Decl]
    makeParams isParam names pos typ = makeBindings names $ ParamDecl pos isParam typ

    makeVars :: [String] -> Pos -> Type -> [Binding Decl]
    makeVars names pos typ = makeBindings names $ VarDecl pos typ

    constDecl :: Pos -> Expr -> Decl
    constDecl pos = ExprDecl pos . ConstCheckExpr pos

    -- Actual

    data Actual = ExprActual Expr | TypeActual Type
        deriving (Show)

    actualToFuncActual :: Actual -> FuncActual
    actualToFuncActual (ExprActual expr) = ExprFuncActual False expr
    actualToFuncActual (TypeActual typ) = TypeFuncActual typ

    actualToProcActual :: Actual -> ProcActual
    actualToProcActual (ExprActual expr) = ExprProcActual expr
    actualToProcActual (TypeActual typ) = TypeProcActual typ

    arrayIndexInterval :: BalsaParser (Pos, Interval)
    arrayIndexInterval = do
        pos <- keyword "array"
        b <- arrayIndexIntervalExpr
        keyword "of"
        return (pos, b)

    arrayIndexIntervalExpr :: BalsaParser Interval
    arrayIndexIntervalExpr = anyOneOfStr "array index bounds" [
        do
            left <- expr
            let
                leftPos = exprPos left
                zero = intExpr leftPos 0
                one = intExpr leftPos 1
                singleValInterval = IntervalE leftPos zero (BinExpr leftPos NoType BinSub left one)
            optional singleValInterval $ do
                pos <- keyword ".."
                right <- expr
                return $ IntervalE pos left right,
        do
            pos <- keyword "over"
            t <- typeRef
            return $ IntervalOver pos t ]

    actual :: BalsaParser Actual
    actual = anyOneOfStr "an actual argument" [
        do
            keyword "type"
            typ <- typeRef
            return $ TypeActual typ,
        do
            left <- expr
            anyOneOfStr "a numeric type" [
                do
                    typ <- numericType left
                    return $ TypeActual typ,
                return $ ExprActual left ],
        do
            (_, b) <- arrayIndexInterval
            t <- typeRef
            return $ TypeActual $ ArrayType b t ]

    -- Expr

    un :: UnOp -> BalsaParser Expr
    un op = do
        pos <- keyword $ unOpSymbol op
        right <- exprUn
        return $ UnExpr pos NoType op right

    binRhs :: BalsaParser Expr -> BinOp -> BalsaParser (Expr -> Expr)
    binRhs rhs op = do
        pos <- keyword $ binOpSymbol op
        right <- rhs
        let cons left = BinExpr pos NoType op left right
        return cons

    exprParen :: BalsaParser Expr
    exprParen = do
        keyword "("
        e <- expr
        keyword ")"
        return e

    intExpr :: Pos -> Integer -> Expr
    intExpr pos int = ValueExpr pos (typeForInt int) $ IntValue int

    strExpr :: Pos -> String -> Expr
    strExpr pos str = ValueExpr pos (NameType pos "String") $ StringValue str

    addExpr :: Pos -> Expr -> Integer -> Expr
    addExpr pos expr int = BinExpr pos NoType BinAdd expr $ intExpr pos int

    exprLeaf :: BalsaParser Expr
    exprLeaf = anyOneOfStr "a leaf expression" [
        identifier >>= return . (uncurry NameExpr),
        integer >>= return . (uncurry intExpr),
        string >>= return . (uncurry strExpr),
        do
            (pos, v, dc) <- implicant
            let
                valueType = typeForInt v
                dcsType = typeForInt dc
                typ = smallestNumTypeEnclosing valueType dcsType
            return $ ValueExpr pos typ $ ImpValue $ Imp v dc,
        do
            pos <- keyword "?"
            return $ ValueExpr pos NoType DontCareValue,
        do
            pos <- keyword "sizeof"
            -- e <- expr
            -- t <- exprToType e
            (namePos, n) <- identifier
            return $ SizeofExpr pos $ NameType namePos n,
        exprParen ]

    exprElem :: BalsaParser Expr
    exprElem = exprLeaf

    exprEnumElem :: BalsaParser Expr
    exprEnumElem = do
        left <- exprElem
        optional left $ do
            pos <- keyword "'"
            (_, n) <- identifier
            t <- exprToType left
            return $ EnumElemExpr pos t n

    exprConsBody :: BalsaParser (Pos, [Expr])
    exprConsBody = do
        pos <- keyword "{"
        es <- exprList
        keyword "}"
        return (pos, es)

    exprCons :: BalsaParser Expr
    exprCons = anyOneOfStr "a valid expression" [
        do
            (pos, es) <- exprConsBody
            return $ ConsExpr pos NoType NoType es,
        do
            left <- exprEnumElem
            optional left $ do
                (pos, es) <- exprConsBody
                typ <- exprToType left
                return $ ConsExpr pos typ typ es ]

    exprCall :: BalsaParser Expr
    exprCall = do
        left <- exprCons
        optional left $ do
            pos <- keyword "("
            as <- optional [] $ sepList (keyword ",") $ do
                a <- actual
                return $ actualToFuncActual a
            keyword ")"
            c <- exprToCallable left
            return $ CallExpr pos c emptyContext as

    exprSmash :: BalsaParser Expr
    exprSmash = anyOneOfStr "a valid expression" [
        do
            pos <- keyword "#"
            right <- exprSmash
            return $ UnExpr pos NoType UnSmash right,
        exprCall ]

    exprArray :: BalsaParser Expr
    exprArray = leftAssocInfix exprSmash [
        do
            pos <- keyword "["
            leftIndex <- expr
            let indexCons left = IndexExpr pos left leftIndex
            ret <- optional indexCons $ do
                keyword ".."
                rightIndex <- expr
                let sliceCons left = SliceExpr pos left leftIndex rightIndex
                return sliceCons
            keyword "]"
            return ret,
        do
            pos <- keyword "."
            (_, n) <- identifier
            let recElem left = RecElemExpr pos left n
            return recElem ]

    exprUn :: BalsaParser Expr
    exprUn = anyOneOfStr "a valid expression" $ exprArray : map un [UnNot, UnLog, UnNeg]

    exprPow :: BalsaParser Expr
    exprPow = do
        left <- exprUn
        optional left $ do
            pos <- keyword "^"
            right <- exprPow
            return $ BinExpr pos NoType BinPow left right

    appendRhs :: BalsaParser Expr -> BalsaParser (Expr -> Expr)
    appendRhs rhs = do
        pos <- keyword "@"
        right <- rhs
        let cons left = AppendExpr pos NoType left right
        return cons

    exprMul :: BalsaParser Expr
    exprMul = leftAssocInfix exprPow $ map (binRhs exprPow) [BinMul, BinDiv, BinMod]

    exprAdd :: BalsaParser Expr
    exprAdd = leftAssocInfix exprMul $ [binRhs exprMul BinAdd, binRhs exprMul BinSub, appendRhs exprMul]

    exprIneq :: BalsaParser Expr
    exprIneq = leftAssocInfix exprAdd $ map (binRhs exprAdd) [BinLT, BinGT, BinLE, BinGE]

    exprEq :: BalsaParser Expr
    exprEq = leftAssocInfix exprIneq $ map (binRhs exprIneq) [BinEQ, BinNE]

    exprAnd :: BalsaParser Expr
    exprAnd = leftAssocInfix exprEq [binRhs exprEq BinAnd]

    exprOr :: BalsaParser Expr
    exprOr = leftAssocInfix exprAnd $ map (binRhs exprAnd) [BinXor, BinOr]

    exprTyping :: BalsaParser Expr
    exprTyping = do
        left <- exprOr
        exprTypingRhs left

    exprTypingRhs :: Expr -> BalsaParser Expr
    exprTypingRhs left = optional left $ do
        cons <- anyOneOfStr "a valid expression" [
            do
                pos <- keyword "as"
                return $ CastExpr pos,
            do
                pos <- keyword ":"
                return $ TypeCheckExpr pos Explicit ]
        right <- typeRef
        exprTypingRhs $ cons right left

    expr :: BalsaParser Expr
    expr = exprTyping

    -- Lvalue

    lvalueParen :: BalsaParser Lvalue
    lvalueParen = do
        keyword "("
        l <- lvalue
        keyword ")"
        return l

    lvalueLeaf :: BalsaParser Lvalue
    lvalueLeaf = anyOneOfStr "a valid variable reference" [
        do
            (pos, n) <- identifier
            return $ NameLvalue pos n,
        lvalueParen ]

    lvalueElem :: BalsaParser Lvalue
    lvalueElem = lvalueLeaf

    lvalueSmash :: BalsaParser Lvalue
    lvalueSmash = anyOneOfStr "a valid variable reference" [
        do
            pos <- keyword "#"
            right <- lvalueElem
            return $ SmashLvalue pos right,
        lvalueElem ]

    lvalueArray :: BalsaParser Lvalue
    lvalueArray = leftAssocInfix lvalueSmash [
        do
            pos <- keyword "["
            leftIndex <- expr
            let indexCons left = IndexLvalue pos left leftIndex
            ret <- optional indexCons $ do
                keyword ".."
                rightIndex <- expr
                let sliceCons left = SliceLvalue pos left leftIndex rightIndex
                return sliceCons
            keyword "]"
            return ret,
        do
            pos <- keyword "."
            (_, n) <- identifier
            let recElem left = RecElemLvalue pos left n
            return recElem ]

    lvalueTyping :: BalsaParser Lvalue
    lvalueTyping = do
        left <- lvalueArray
        lvalueTypingRhs left

    lvalueTypingRhs :: Lvalue -> BalsaParser Lvalue
    lvalueTypingRhs left = optional left $ do
        cons <- anyOneOfStr "a valid variable reference" [
            do
                pos <- keyword "as"
                return $ CastLvalue pos,
            do
                pos <- keyword ":"
                return $ TypeCheckLvalue pos Explicit ]
        right <- typeRef
        lvalueTypingRhs $ cons right left

    lvalue :: BalsaParser Lvalue
    lvalue = lvalueTyping

    -- Types

    paramNamesAndType :: Bool -> Pos -> BalsaParser [Binding Decl]
    paramNamesAndType isParam pos = do
        ns <- identifierList
        pos2 <- keyword ":"
        let usedPos = if pos == NoPos then pos2 else pos
        anyOneOfStr "a parameter type argument" [
            do
                keyword "type"
                return $ makeTypeParams ns usedPos,
            do
                t <- typeRef
                return $ makeParams isParam ns usedPos t ]

    exprToType :: Expr -> BalsaParser Type
    exprToType (NameExpr pos name) = return $ NameType pos name
    exprToType _ = parserFail $ expecting "invalid type expression"

    numericType :: Expr -> BalsaParser Type
    numericType expr = anyOneOfStr "`bits' or `signed bits'" [
        do
            pos <- keyword "bits"
            return $ NumType pos expr False,
        do
            pos <- keyword "signed"
            keyword "bits"
            return $ NumType pos expr True ]

    typeRef :: BalsaParser Type
    typeRef = anyOneOfStr "a type" [
        do
            left <- expr
            anyOneOfStr "a numeric type" [
                numericType left,
                exprToType left ],
        do
            (_, b) <- arrayIndexInterval
            t <- typeRef
            return $ ArrayType b t ]

    recordElems :: BalsaParser [RecordElem]
    recordElems = do
        ns <- identifierList
        pos <- keyword ":"
        t <- typeRef
        return $ map (\name -> RecordElem pos name t) ns

    overType :: BalsaParser Type
    overType = anyOneOfStr "`over' type or `end'" [
        do
            keyword "over"
            typeRef,
        do
            keyword "end"
            return NoType ]

    recordDecl :: BalsaParser TypeBody
    recordDecl = do
        pos <- keyword "record"
        es <- liftM concat $ sepList (keyword ";") recordElems
        t <- overType
        return $ RecordType pos es t

    enumElems :: Expr -> Integer -> BalsaParser [Binding Decl]
    enumElems valExpr offset = do
        (pos, n) <- identifier
        (e, valExpr', offset') <- optional (addExpr pos valExpr offset, valExpr, offset + 1) $ do
            keyword "="
            e <- expr
            return (e, e, 1)
        let elem = otherBinding n $ constDecl pos e
        optional [elem] $ do
            keyword ","
            elems <- enumElems valExpr' offset'
            return (elem:elems)

    enumDecl :: BalsaParser TypeBody
    enumDecl = do
        pos <- keyword "enumeration"
        es <- enumElems (intExpr pos 0) 0
        t <- overType
        return $ EnumType pos (bindingsToContext1 es) t

    typeDecl :: BalsaParser [Binding Decl]
    typeDecl = do
        pos <- keyword "type"
        (_, n) <- identifier
        keyword "is"
        t <- anyOneOfStr "a type declaration body" [
            recordDecl,
            enumDecl,
            do
                t <- typeRef
                return $ AliasType pos t,
            do
                keyword "builtin"
                return $ AliasType pos $ BuiltinType n ]
        return [typeBinding n pos t]

    -- Cmd

    blockAfterBegin :: String -> BalsaParser Cmd
    blockAfterBegin endSymbol = do
        command <- optional NoCmd command
        keyword endSymbol
        return $ command

    blockAfterLocal :: BalsaParser Cmd
    blockAfterLocal = do
        ds <- innerDecls
        let makeBlock posKw c
              | isEmptyContext ds = c
              | otherwise = BlockCmd posKw ds c
        anyOneOfStr  "block body" [
            do
                posBegin <- keyword "begin"
                c <- blockAfterBegin "end"
                return $ makeBlock posBegin c,
            do
                (posLoop, c) <- whileCmd
                return $ makeBlock posLoop c ]

    lvalueToChan :: Lvalue -> BalsaParser Chan
    lvalueToChan (NameLvalue pos string) = return $ NameChan pos string
    lvalueToChan (IndexLvalue pos lvalue index) = do
        chan <- lvalueToChan lvalue
        return $ IndexChan pos chan index
    lvalueToChan (SliceLvalue pos lvalue li ri) = do
        chan <- lvalueToChan lvalue
        return $ SliceChan pos chan li ri
    lvalueToChan lvalue = parserFail $ Report (lvaluePos lvalue) "bad channel expression"

    encInputCmd :: [Lvalue] -> Pos -> BalsaParser Cmd
    encInputCmd channels pos = do
        keyword "then"
        enclosedCmd <- command
        keyword "end"
        cs <- mapM lvalueToChan channels
        return $ EncInputCmd pos $ ChanGuard pos cs emptyContext enclosedCmd

    exprList :: BalsaParser [Expr]
    exprList = sepList (keyword ",") expr

    lvalueList :: BalsaParser [Lvalue]
    lvalueList = sepList (keyword ",") lvalue

    procActual :: BalsaParser ProcActual
    procActual = anyOneOfStr "a procedure actual argument" [
        do
            a <- actual
            return $ actualToProcActual a,
        do
            keyword "type"
            t <- typeRef
            return $ TypeProcActual t ]

    commandStartsWithLvalue :: Lvalue -> BalsaParser Cmd
    commandStartsWithLvalue lhs = anyOneOfStr "a command" [
        do
            pos <- keyword ":="
            rhs <- expr
            return $ AssignCmd pos lhs rhs,
        do
            keyword ","
            channels <- lvalueList
            pos <- keyword "->"
            optionalKeyword "!"
            encInputCmd (lhs:channels) pos,
        do
            pos <- keyword "->"
            optionalKeyword "!"
            anyOneOfStr "a variable or channel reference" [
                encInputCmd [lhs] pos,
                do
                    rhs <- lvalue
                    lhs' <- lvalueToChan lhs
                    return $ InputCmd pos lhs' rhs ],
        do
            pos <- keyword "<-"
            rhs <- expr
            lhs' <- lvalueToChan lhs
            return $ OutputCmd pos lhs' rhs,
        do
            pos <- keyword "("
            as <- optional [] $ sepList (keyword ",") procActual
            keyword ")"
            c <- lvalueToCallable lhs
            return $ CallCmd pos c emptyContext as ]

    selectGuard :: BalsaParser ChanGuard
    selectGuard = do
        channels <- lvalueList
        pos <- keyword "then"
        action <- command
        cs <- mapM lvalueToChan channels
        return $ ChanGuard pos cs emptyContext action

    block :: BalsaParser Cmd
    block = anyOneOfStr "a block" [
        do
            keyword "begin"
            blockAfterBegin "end",
        do
            keyword "["
            blockAfterBegin "]" ]

    parSeq :: BalsaParser ParSeq
    parSeq = anyOneOfStr "`||' or `;'" [
        do
            keyword "||"
            return Par,
        do
            keyword ";"
            return Seq ]

    forCmd :: BalsaParser Cmd
    forCmd = do
        pos <- keyword "for"
        ps <- optional Seq parSeq
        (_, n) <- identifier
        keyword "in"
        (li, ri) <- range
        keyword "then"
        c <- command
        keyword "end"
        -- The 0 just stands in place for the constant in the repeated cmd
        return $ ForCmd pos ps (IntervalE pos li ri)
            (bindingsToContext1 [otherBinding n $ ParamDecl pos False NoType]) c

    -- loop [c1] [while e [then c2]] end
    whileCmd :: BalsaParser (Pos, Cmd)
    whileCmd = do
        pos <- keyword "loop"
        c1 <- optional NoCmd command
        ret <- optional (LoopCmd pos c1) $ do
            pos <- keyword "while"
            e <- expr
            optional (WhileCmd pos c1 e NoCmd) $ do
                keyword "then"
                c2 <- command
                return $ WhileCmd pos c1 e c2
        keyword "end"
        return (pos, ret)

    selectBody :: BalsaParser [ChanGuard]
    selectBody = do
        gs <- sepList (keyword "|") selectGuard
        keyword "end"
        return gs

    lvalueToCallable :: Lvalue -> BalsaParser Callable
    lvalueToCallable (NameLvalue pos name) = return $ NameCallable pos name
    lvalueToCallable _ = parserFail $ expecting "invalid call"

    exprToCallable :: Expr -> BalsaParser Callable
    exprToCallable (NameExpr pos name) = return $ NameCallable pos name
    exprToCallable _ = parserFail $ expecting "invalid call"

    commandOther :: BalsaParser Cmd
    commandOther = anyOneOfStr "command" [
        block,
        do
            (labelPos, lab) <- label
            cmd <- command
            return $ LabelCmd labelPos lab cmd,
        do
            keyword "local"
            blockAfterLocal,
        do
            keyword "continue"
            return NoCmd,
        do
            pos <- keyword "select"
            optionalKeyword "!"
            optionalKeyword "|"
            gs <- selectBody
            return $ SelectCmd pos False gs,
        do
            pos <- keyword "arbitrate"
            optionalKeyword "!"
            optionalKeyword "|"
            gs <- selectBody
            return $ SelectCmd pos True gs,
        do
            pos <- keyword "print"
            vs <- exprList
            return $ PrintCmd pos vs,
        do
            pos <- keyword "sink"
            e <- expr
            return $ SinkCmd pos e,
        do
            lhs <- lvalue
            commandStartsWithLvalue lhs,
        do
            (pos, c) <- condParser True NoCmd command
            let caseCmd = caseToCaseCmd pos c
            return caseCmd,
        do
            (_, c) <- whileCmd
            return c,
        forCmd ]

    par :: BalsaParser Pos
    par = do
        pos <- keyword "||"
        optionalKeyword "!"
        return pos

    commandPar :: BalsaParser Cmd
    commandPar = do
        l <- commandOther
        optional l $ do
            pos <- par
            r <- sepList par commandOther
            return $ ParCmd pos (l:r)

    commandSeq :: BalsaParser Cmd
    commandSeq = do
        l <- commandPar
        optional l $ do
            pos <- keyword ";"
            r <- sepList (keyword ";") commandPar
            return $ SeqCmd pos (l:r)

    command :: BalsaParser Cmd
    command = commandSeq

    -- ProcFormals

    direction :: BalsaParser (Pos, Direction)
    direction = anyOneOfStr "port direction" [
        do
            pos <- keyword "input"
            return (pos, Input),
        do
            pos <- keyword "output"
            return (pos, Output) ]

    channelProcFormal :: (Decl -> Decl) -> BalsaParser [Binding Decl]
    channelProcFormal declWrapper = anyOneOfStr "procedure formal argument" [
        do
            (pos, d) <- direction
            ns <- identifierList
            keyword ":"
            t <- typeRef
            return $ makePorts ns d t pos declWrapper ]

    procFormal :: BalsaParser [Binding Decl]
    procFormal = anyOneOfStr "procedure formal argument" [
        do
            wrapper <- arrayedDecl
            channelProcFormal wrapper,
        channelProcFormal id,
        do
            pos <- keyword "parameter"
            paramNamesAndType True pos,
        do
            (pos, d) <- condParser False [] procFormalsBody
            caseDecl <- caseToCaseDecl pos d
            return caseDecl ]

    range :: BalsaParser (Expr, Expr)
    range = do
        left <- expr
        optional (left, left) $ do
            keyword ".."
            right <- expr
            return (left, right)

    procFormalsBody :: BalsaParser [Binding Decl]
    procFormalsBody = optional [] $ liftM concat $ sepList (keyword ";") procFormal

    procFormals :: BalsaParser (Context Decl)
    procFormals = do
        keyword "("
        ps <- procFormalsBody
        keyword ")"
        return $ bindingsToContext1 ps

    attribute :: BalsaParser Attr
    attribute = do
        (pos, n) <- identifier
        v <- optional (ValueExpr pos (Bits 1) (IntValue 1)) $ do
            keyword "="
            e <- expr
            return $ ConstCheckExpr pos e
        return $ ExprAttr n v

    attributes :: BalsaParser [Attr]
    attributes = optional [] $ do
        keyword "(*"
        attrs <- sepList (keyword ",") attribute
        keyword "*)"
        return attrs

    -- FuncFormals

    funcFormal :: BalsaParser [Binding Decl]
    funcFormal = anyOneOfStr "a function formal argument" [
        do
            pos <- keyword "parameter"
            paramNamesAndType True pos,
        paramNamesAndType False NoPos,
        do
            (pos, d) <- condParser False [] funcFormalsBody
            caseDecl <- caseToCaseDecl pos d
            return caseDecl ]

    funcFormalsBody :: BalsaParser [Binding Decl]
    funcFormalsBody = optional [] $ liftM concat $ sepList (keyword ";") funcFormal

    funcFormals :: BalsaParser (Context Decl)
    funcFormals = do
        keyword "("
        fs <- funcFormalsBody
        keyword ")"
        return $ bindingsToContext1 fs

    -- Imports

    import_ :: BalsaParser (Pos, ImportPath)
    import_ = do
        pos <- keyword "import"
        keyword "["
        path <- sepList (keyword ".") pathElement
        keyword "]"
        return (pos, ImportPath path)

    imports :: BalsaParser [(Pos, ImportPath)]
    imports = rep import_

    -- Conditionals

    ifGuard :: BalsaParser a -> BalsaParser (Expr, a)
    ifGuard bodyParser = do
        e <- expr
        keyword "then"
        b <- bodyParser
        return (e, b)

    caseMatch :: BalsaParser CaseMatch
    caseMatch = do
        left <- expr
        optional (ExprCaseMatch (exprPos left) left) $ do
            pos <- keyword ".."
            right <- expr
            return $ RangeCaseMatch pos left right

    caseGuard :: Bool -> BalsaParser node -> BalsaParser (CaseGuard node)
    caseGuard allowFor bodyParser
        | allowFor = anyOneOfStr "case match" [forParse, guardParser]
        | otherwise = anyOneOfStr "case match" [guardParser]
        where
            guardParser = do
                ms <- sepList (keyword ",") caseMatch
                pos <- keyword "then"
                b <- bodyParser
                return $ CaseGuard pos ms b
            forParse = do
                pos <- keyword "for"
                (_, n) <- identifier
                keyword "in"
                ms <- sepList (keyword ",") caseMatch
                keyword "then"
                b <- bodyParser
                return $ ForCaseGuard pos n ms
                    (bindingsToContext1 [Binding 0 n OtherNamespace Complete $ ParamDecl pos False NoType]) b

    ifElse :: node -> BalsaParser node -> BalsaParser (Pos, Cond node)
    ifElse noElse bodyParser = do
        pos <- keyword "if"
        optionalKeyword "|"
        gs <- sepList (keyword "|") $ ifGuard bodyParser
        e <- optional noElse $ do
            keyword "else"
            bodyParser
        keyword "end"
        return (pos, If gs e)

    caseElse :: Bool -> node -> BalsaParser node -> BalsaParser (Pos, Cond node)
    caseElse allowFor noElse bodyParser = do
        pos <- keyword "case"
        m <- expr
        keyword "of"
        optionalKeyword "|"
        gs <- sepList (keyword "|") $ caseGuard allowFor bodyParser
        e <- optional noElse $ do
            keyword "else"
            bodyParser
        keyword "end"
        return (pos, Case m gs e)

    condParser :: Bool -> b -> BalsaParser b -> BalsaParser (Pos, Cond b)
    condParser allowFor noElse bodyParser = do
        (pos, c) <- anyOneOfStr "conditional statement" [
            ifElse noElse bodyParser,
            caseElse allowFor noElse bodyParser ]
        return (pos, c)

    -- Decls

    arrayedDecl :: BalsaParser (Decl -> Decl)
    arrayedDecl = body id
        where
            body arraying = do
                (pos, b) <- arrayIndexInterval
                let arraying' decl = arraying $ ArrayedDecl pos b decl
                optional arraying' $ body arraying'

    channel :: (Decl -> Decl) -> BalsaParser [Binding Decl]
    channel declWrapper = do
        pos <- keyword "channel"
        ns <- identifierList
        keyword ":"
        t <- typeRef
        return $ makeChans ns t pos declWrapper

    innerDecl :: BalsaParser [Binding Decl]
    innerDecl = anyOneOfStr "declaration inside a procedure" [
        do
            pos <- keyword "variable"
            vars <- identifierList
            keyword ":"
            t <- typeRef
            return $ makeVars vars pos t,
        channel id,
        do
            wrapper <- arrayedDecl
            channel wrapper,
        do
            pos <- keyword "shared"
            (_, n) <- identifier
            keyword "is"
            optionalKeyword "local"
            b <- blockAfterLocal
            return [procBinding n $ SharedDecl pos emptyContext b],
        do
            (pos, d) <- condParser False [] (anyOneOfStr "declarations" [liftM concat $ rep innerDecl,
                liftM concat $ rep outerDecl])
            caseDecl <- caseToCaseDecl pos d
            return caseDecl,
        outerDecl ]

    constantDecl :: BalsaParser [Binding Decl]
    constantDecl = do
        pos <- keyword "constant"
        (_, n) <- identifier
        keyword "="
        v <- expr
        return [otherBinding n $ constDecl pos v]

    procDecl :: BalsaParser [Binding Decl]
    procDecl = do
        pos <- keyword "procedure"
        (_, n) <- identifier
        b <- anyOneOfStr "procedure ports or procedure body (keywords `is' or `(')" [
            do
                ps <- procFormals
                attrs <- attributes
                keyword "is"
                optionalKeyword "local"
                c <- blockAfterLocal
                return $ ProcDecl pos ps attrs c,
            do
                attrs <- attributes
                keyword "is"
                anyOneOfStr "procedure or procedure alias body" [
                    do
                        l <- lvalue
                        c <- lvalueToCallable l
                        as <- optional [] $ do
                            keyword "("
                            as <- optional [] $ sepList (keyword ",") procActual
                            keyword ")"
                            return as
                        return $ ProcAliasDecl pos c emptyContext as,
                    do
                        optionalKeyword "local"
                        b <- blockAfterLocal
                        return $ ProcDecl pos emptyContext attrs b ] ]
        return [procBinding n b]

    funcDecl :: BalsaParser [Binding Decl]
    funcDecl = do
        pos <- keyword "function"
        (_, n) <- identifier
        fs <- optional emptyContext funcFormals
        b <- anyOneOfStr "function body" [
            do
                keyword "="
                v <- expr
                return $ FuncDecl pos fs v,
            do
                keyword "is"
                keyword "builtin"
                keyword ":"
                t <- typeRef
                return $ BuiltinFuncDecl pos fs t ]
        return [procBinding n b]

    outerDecl :: BalsaParser [Binding Decl]
    outerDecl = anyOneOfStr "top level declaration" [
        procDecl,
        funcDecl,
        typeDecl,
        constantDecl,
        do
            (pos, d) <- condParser False [] $ liftM concat $ rep outerDecl
            caseDecl <- caseToCaseDecl pos d
            return caseDecl ]

    decls :: BalsaParser [Binding Decl] -> BalsaParser (Context Decl)
    decls parser = do
        bindings <- liftM concat $ rep parser
        return $ bindingsToContext1 bindings

    innerDecls :: BalsaParser (Context Decl)
    innerDecls = decls innerDecl

    outerDecls :: BalsaParser (Context Decl)
    outerDecls = decls outerDecl

    balsaDescription :: BalsaParser ([(Pos, ImportPath)], Context Decl)
    balsaDescription = do
        is <- imports
        ds <- outerDecls
        return (is, ds)

    checkedGuard :: Expr -> Expr
    checkedGuard guard = TypeCheckExpr (exprPos guard) Inserted bitType guard

    guardVector :: Pos -> [Expr] -> Expr
    guardVector pos guards = CastExpr pos (bitArrayType guardCount) cons
        where
            guardCount = length guards
            guardCons = ConsExpr pos NoType NoType $ map checkedGuard guards
            cons = TypeCheckExpr pos Inserted (bitArrayType guardCount) guardCons

    data CaseGuard node =
          CaseGuard Pos [CaseMatch] node
        | ForCaseGuard Pos String [CaseMatch] (Context Decl) node
        deriving (Show, Read)

    caseGuardNode :: CaseGuard node -> node
    caseGuardNode (CaseGuard _ _ node) = node
    caseGuardNode (ForCaseGuard _ _ _ _ node) = node

    data Cond node =
          If [(Expr, node)] node
        | Case Expr [(CaseGuard node)] node
        deriving (Show, Read)

    caseToCaseCmd :: Pos -> Cond Cmd -> Cmd
    caseToCaseCmd condPos cond = CaseCmdE condPos m (map changeGuard gs) e
        where
            Case m gs e = makeCaseFromCond condPos cond

            changeGuard (CaseGuard pos matches cmd) = CaseCmdGuard pos matches cmd
            changeGuard (ForCaseGuard pos name ctx matches cmd) =
                ForCaseCmdGuard pos name ctx matches cmd

    data OtherNamespaceNature = VariableNature | ConstantNature | ArrayedNature | ChanNature | UnmatchedNature
        deriving (Eq, Ord)

    instance Show OtherNamespaceNature where
        showsPrec _ VariableNature = showString "variable"
        showsPrec _ ConstantNature = showString "constant"
        showsPrec _ ArrayedNature = showString "arrayed channel"
        showsPrec _ ChanNature = showString "channel"
        showsPrec _ UnmatchedNature = showString "other"

    otherDeclNature :: Decl -> OtherNamespaceNature
    otherDeclNature (ExprDecl {}) = ConstantNature
    otherDeclNature (VarDecl {}) = VariableNature
    otherDeclNature (ParamDecl {}) = ConstantNature
    -- otherDeclNature (TypeParamDecl {}) = ConstantNature ?
    otherDeclNature (ChanDecl {}) = ChanNature
    otherDeclNature (PortDecl {}) = ChanNature
    otherDeclNature caseDecl@(CaseDecl {}) = otherDeclNature (caseDeclSampleDecl caseDecl)
    otherDeclNature (ArrayedDecl {}) = ArrayedNature
    otherDeclNature _ = UnmatchedNature

    caseToCaseDecl :: Pos -> Cond [Binding Decl] -> BalsaParser [Binding Decl]
    caseToCaseDecl condPos cond = liftM catMaybes $ mapM processName qualifiedNames
        where
            Case m gs e = makeCaseFromCond condPos cond

            getQualifiedName binding = (bindingName binding, bindingNamespace binding)
            matchQualifiedName (name, namespace) binding =
                bindingName binding == name && bindingNamespace binding == namespace

            elseQualifiedNames = map getQualifiedName e

            qualifiedNames = nub $ elseQualifiedNames ++ concatMap ((map getQualifiedName) . caseGuardNode) gs

            guardConss = map changeGuard gs
            guardBindingss = map caseGuardNode gs

            applyToValidDecl _ (NoDecl {}) = Nothing
            applyToValidDecl f decl = Just $ f decl

            processName qName@(name, namespace) = do
                guardDecls <- mapM filterBindings guardBindingss
                let gs' = mapMaybe (uncurry applyToValidDecl) $ zip guardConss guardDecls
                e' <- checkForRepeats (filter (matchQualifiedName qName) e)

                let
                    allDecls = filter (not . isNoDecl) $ e':guardDecls
                    natures = map otherDeclNature allDecls
                    natureFrequencies = sortBy compareFst $ frequencyBy (==) natures

                    showNatureFrequency (0, _) = Nothing
                    showNatureFrequency (1, declNature) = Just $ "1 " ++ show declNature
                    showNatureFrequency (n, declNature) = Just $ show n ++ " " ++ show declNature ++ "s"

                    naturesAllSame = all (== (head natures)) natures

                case (allDecls, naturesAllSame) of
                    ([], _) -> return Nothing
                    (_, False) -> parserFail $ wrong $
                        "not all declarations for `" ++ name ++ "' are of the same nature ("
                            ++ joinWith ", " (mapMaybe showNatureFrequency natureFrequencies) ++ ")"
                    (_, True) -> return $ Just $ Binding 0 name namespace Incomplete $ CaseDecl condPos m gs' e'
                where
                    filterBindings bindings = checkForRepeats $ filter (matchQualifiedName qName) bindings

                    checkForRepeats [] = return $ NoDecl condPos
                    checkForRepeats [binding] = return $ bindingValue binding
                    checkForRepeats _ = parserFail $ wrong $ "more than one binding for name `" ++ name ++ "'"

            changeGuard (CaseGuard pos matches _) = CaseDeclGuard pos matches
            changeGuard _ = error "changeGuard: not a CaseGuard"

    -- makeCaseFromCond: if a then b | c then d else e end -> case {a,c} of 0bX1 then c | 0b10 then d else e end
    makeCaseFromCond :: Pos -> Cond node -> Cond node
    makeCaseFromCond _ (Case m gs e) = Case m gs e
    makeCaseFromCond pos (If guards elseThing) = Case guardExpr guards' elseThing
        where
            (guardExprs, guardedThings) = unzip guards
            guardExpr = guardVector pos guardExprs
            guardCount = length guards
            guards' = map makeGuard $ zip [0..guardCount-1] guardedThings
                where makeGuard (index, guarded) = CaseGuard pos (matchValue pos guardCount index) guarded

    matchValue :: Pos -> Int -> Int -> [CaseMatch]
    matchValue pos width n = [ExprCaseMatch pos $ ValueExpr pos (bitArrayType width) matchImp]
        where matchImp = ImpValue $ Imp (bit n) (bit n - 1)

    balsaParser :: [BalsaToken] -> (ParseStatus Report ([(Pos, ImportPath)], Context Decl), [BalsaToken])
    balsaParser = parseLA1 balsaDescription
