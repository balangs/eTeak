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

module Bind (
    bind,
    gatherPositions
    ) where

    import Prelude hiding (traverse)
    import ParseTree
    import Context
    import Traverse
    import Report
    import Chan
    import Bits

    import Data.Maybe
    import Data.List
    import Data.Array

    -- gatherPositions : gather procedure/function/label positions from a parse tree, annotating
    --    positions in the tree and returning: (list of new positions to bind at the top, modified tree)
    gatherPositions :: Int -> [Binding Decl] -> ([Binding Decl], [Binding Decl])
    gatherPositions firstPosNumber bindings = (reverse revPoss, bindings')
        where
            (_, revPoss, bindings') = foldl' handleBinding (firstPosNumber, [], []) bindings
            handleBinding i binding = body i binding

            body (i, poss, r) binding = (i', poss', r ++ [binding'])
                where
                    Pos refPos _ _ = declPos $ bindingValue binding
                    ((i', poss'), _, binding') = traverse gatherPosReplaceApply refPos (i, poss) binding

    gatherPosReplaceApply :: Apply Int (Int, [Binding Decl]) Completeness
    gatherPosReplaceApply = emptyApply {
        preBinding = gatherPosPreReplaceBinding,
        preCmd = gatherPosPreReplaceCmd,
        postPos = gatherPosPostReplacePos
        }

    type GatherPosPre node = ApplyPreFunc Int (Int, [Binding Decl]) node
    type GatherPosPost node = ApplyPostFunc Int Completeness node

    gatherPosPreReplaceBinding :: GatherPosPre (Binding Decl)
    gatherPosPreReplaceBinding refPos accum@(i, poss) binding = (refPos', accum', binding)
        where
            name = bindingName binding
            decl = bindingValue binding

            (refPos', accum') = pre decl

            newDeclPos pos = PosLabel (posInsertRef pos refPos) name

            pre (ProcDecl pos _ _ _) = (i, (i + 1, (posBinding i (newDeclPos pos)):poss))
            pre _ = (refPos, accum)

    gatherPosPreReplaceCmd :: GatherPosPre Cmd
    gatherPosPreReplaceCmd refPos (i, poss) cmd@(LabelCmd _ label _) = (i, accum', cmd)
        where
            newPos = PosLabel (PosRef refPos) label
            binding = posBinding i newPos
            accum' = (i + 1, binding:poss)
    gatherPosPreReplaceCmd cs accum node = (cs, accum, node)

    posInsertRef :: Pos -> Int -> Pos
    posInsertRef (Pos _ line column) newFrom = Pos newFrom line column
    posInsertRef NoPos _ = NoPos
    posInsertRef pos _ = error $ "posInsertRef: can't insert ref into `" ++ show pos ++ "'"

    posBinding :: Int -> Pos -> Binding Decl
    posBinding i pos = Binding i ("@" ++ show i) PosNamespace Complete (PosDecl pos)

    gatherPosPostReplacePos :: GatherPosPost Pos
    gatherPosPostReplacePos refPos (r, pos) = (r, posInsertRef pos refPos)

    -- bind : convert names to references by threading contexts
    bind :: Context Decl -> Why (Context Decl)
    bind context = Why r context'
        where (_, r, context') = traverse bindApply [] () context

    chainContexts :: Context Decl -> [Context Decl] -> Context Decl
    chainContexts context cs = rebaseContextIndices adjustment context
        where
            firstNewIndex = contextsNextIndex cs
            (firstOldIndex, _) = bounds $ contextBindings context
            adjustment = firstNewIndex - firstOldIndex

    bindApply :: Apply [Context Decl] () Completeness
    bindApply = emptyApply {
        applyContextCons = (:),
        nullResult = Complete,
        combineResult = andCompleteness,
        postBinding = bindPostBinding,
        preBinding = preNoAccum bindPreBinding,
        midChanGuard = bindMidChanGuard,
        preCmd = preNoAccum bindPreCmd,
        preCaseCmdGuard = preNoAccum bindPreCaseCmdGuard,
        preDecl = preNoAccum bindPreDecl,
        postExpr = bindPostExpr, postLvalue = bindPostLvalue,
        postType = bindPostType, postChan = bindPostChan,
        postCallable = bindPostCallable }

    type BindPre node = [Context Decl] -> node -> ([Context Decl], node)
    type BindPost node = ApplyPostFunc [Context Decl] Completeness node

    -- Context chaining
    bindPreDecl :: BindPre Decl
    bindPreDecl cs (ProcDecl pos ctx attrs cmd) = (cs, ProcDecl pos (chainContexts ctx cs) attrs cmd)
    bindPreDecl cs (FuncDecl pos ctx expr) = (cs, FuncDecl pos (chainContexts ctx cs) expr)
    bindPreDecl cs (BuiltinFuncDecl pos ctx typ) = (cs, BuiltinFuncDecl pos (chainContexts ctx cs) typ)
    bindPreDecl cs (TypeDecl typePos (EnumType pos ctx typ)) =
        (cs, TypeDecl typePos $ EnumType pos (chainContexts ctx cs) typ)
    bindPreDecl cs other = (cs, other)

    -- Check for colliding names
    bindPostBinding :: BindPost (Binding Decl)
    bindPostBinding (context:cs) (Complete, binding@(Binding _ name namespace _ decl)) = binding'
        where
            (lowVisible, _) = contextIndexRange context

            searchContext = case decl of
                ProcDecl {}
                    | bindingIndex binding == lowVisible -> emptyContext
                    | otherwise -> context { contextVisibleNameRange = (lowVisible, bindingIndex binding - 1) }
                _ -> context

            existingRef = findRefByName [searchContext] namespace name
            existingPos = bindingPos $ findBindingByRef [searchContext] $ fromJust existingRef
            pos = declPos decl

            binding'
                | existingRef /= Nothing = (Wrong [alreadyBound], binding)
                | otherwise = (Complete, binding)

            alreadyBound = Report pos $ "name `" ++ name ++ "' may already bound (at "
                ++ showPos (Just (last (context:cs))) existingPos ++ ")"
    bindPostBinding _ other = other

    -- Context chaining
    bindPreCmd :: BindPre Cmd
    bindPreCmd cs (BlockCmd pos ctx cmd) = (cs, BlockCmd pos (chainContexts ctx cs) cmd)
    bindPreCmd cs (ForCmd pos parSeq bound ctx cmd) = (cs, ForCmd pos parSeq bound (chainContexts ctx cs) cmd)
    bindPreCmd cs (InputCmd pos chan lvalue)
        | isChan && isJust cmd = (cs, fromJust cmd)
        where
            isChan = lvalueIsChan cs lvalue
            cmd = do
                toChan <- simpleLvalueToChan lvalue
                expr <- simpleChanToExpr chan
                return $ EncInputCmd pos $ ChanGuard pos [chan] emptyContext $ OutputCmd pos toChan expr
    bindPreCmd cs other = (cs, other)

    bindPreCaseCmdGuard :: BindPre CaseCmdGuard
    bindPreCaseCmdGuard cs (ForCaseCmdGuard pos name matches ctx cmd) = (cs, ForCaseCmdGuard pos name
        matches (chainContexts ctx cs) cmd)
    bindPreCaseCmdGuard cs other = (cs, other)

    -- Enclosing channels
    bindMidChanGuard :: BindPost ChanGuard
    bindMidChanGuard cs (r, ChanGuard pos chans _ cmd) = (r, ChanGuard pos [] ctx' cmd)
        where ctx' = chainContexts (makeOpenChanContext pos cs chans) cs

    bindPreBinding :: BindPre (Binding Decl)
    bindPreBinding [] binding = ([], binding)
    bindPreBinding (c:cs) binding = (cs', binding)
        where
            (lowVisible, _) = contextIndexRange c

            cs' = case decl of
                ProcDecl {} -> c { contextVisibleNameRange = (lowVisible, bindingIndex binding) } : cs
                _ -> c:cs

            decl = bindingValue binding

    badBindingMatchError :: Decl -> Pos -> String -> node -> (Completeness, node)
    badBindingMatchError _ pos name node = (Wrong [Report pos message], node)
        where message = "name `" ++ name ++ "' is bound to something which cannot be used here"

    unboundNameError :: Pos -> String -> node -> (Completeness, node)
    unboundNameError pos name node = (Wrong [Report pos message], node)
        where message = "name `" ++ name ++ "' is unbound"

    -- Actual name to reference binding
    bindPostExpr :: BindPost Expr
    bindPostExpr cs (_, expr@(NameExpr pos name)) = findBinding pos cs OtherNamespace name match tryType
        where
            match (ExprDecl {}) ref = (Complete, Expr pos ref)
            match (VarDecl _ typ) ref = (Complete, VarRead pos typ emptySlice ref)
            match (ParamDecl {}) ref = (Complete, Expr pos ref)
            match (OpenChanDecl _ _ typ) ref = (Complete, OpenChanRead pos typ emptySlice ref)
            match (OpenChanDeclE {}) ref = (Complete, PartialArrayedRead pos ref)
            match (ArrayedDecl {}) ref = (Complete, MaybeOtherExpr pos ref)
            match (ChanDecl {}) ref = (Complete, MaybeOtherExpr pos ref)
            match (PortDecl {}) ref = (Complete, MaybeOtherExpr pos ref)
            match caseDecl@(CaseDecl {}) ref = match (caseDeclSampleDecl caseDecl) ref
            match decl _ = badBindingMatchError decl pos name expr

            tryType = findBinding pos cs TypeNamespace name matchType (Complete, expr)
                where
                    matchType (TypeDecl {}) ref = (Complete, MaybeTypeExpr pos ref)
                    matchType (TypeParamDecl {}) ref = (Complete, MaybeTypeExpr pos ref)
                    matchType caseDecl@(CaseDecl {}) ref = matchType (caseDeclSampleDecl caseDecl) ref
                    matchType decl _ = error $ "matchType: not a type decl `" ++ show decl ++ "'"
    bindPostExpr _ other = other

    lvalueIsChan :: [Context Decl] -> Lvalue -> Bool
    lvalueIsChan cs lvalue = r
        where
            (_, r, _) = traverse lvalueIsChanApply cs () (lvalue :: Lvalue)
            lvalueIsChanApply = emptyApply {
                nullResult = True, combineResult = (&&),
                {- gatherResult = and, -} postLvalue = testLvalue }
            testLvalue cs (_, lvalue@(NameLvalue pos name)) =
                (findBinding pos cs OtherNamespace name match False, lvalue)
            testLvalue _ node = node
            match decl _ = isChan cs decl

    simpleLvalueToChan :: Lvalue -> Maybe Chan
    simpleLvalueToChan (NameLvalue pos name) = return $ NameChan pos name
    simpleLvalueToChan (IndexLvalue pos lvalue index) = do
        chan <- simpleLvalueToChan lvalue
        return $ IndexChan pos chan index
    simpleLvalueToChan _ = Nothing

    simpleChanToExpr :: Chan -> Maybe Expr
    simpleChanToExpr (NameChan pos name) = return $ NameExpr pos name
    simpleChanToExpr (IndexChan pos lvalue index) = do
        expr <- simpleChanToExpr lvalue
        return $ IndexExpr pos expr index
    simpleChanToExpr _ = Nothing

    bindPostLvalue :: BindPost Lvalue
    bindPostLvalue cs (_, lvalue@(NameLvalue pos name)) = findBinding pos cs OtherNamespace name match unbound
        where
            match (VarDecl _ typ) ref = (Complete, VarWrite pos typ emptySlice ref)
            match caseDecl@(CaseDecl {}) ref = match (caseDeclSampleDecl caseDecl) ref
            match decl _ = badBindingMatchError decl pos name lvalue
            unbound = unboundNameError pos name lvalue
    bindPostLvalue _ other = other

    bindPostChan :: BindPost Chan
    bindPostChan cs (_, chanR@(NameChan pos name)) = findBinding pos cs OtherNamespace name match unbound
        where
            match (ChanDecl {}) ref = (Complete, Chan pos ref)
            match (PortDecl {}) ref = (Complete, Chan pos ref)
            match (ArrayedDecl {}) ref = (Complete, PartialArrayedChan pos ref)
            match (OpenChanDeclE {}) ref = (Complete, PartialArrayedChan pos ref)
            match caseDecl@(CaseDecl {}) ref = match (caseDeclSampleDecl caseDecl) ref
            match decl _ = badBindingMatchError decl pos name chanR
            unbound = unboundNameError pos name chanR
    bindPostChan _ other = other

    bindPostType :: BindPost Type
    bindPostType cs (_, typ@(NameType pos name)) = findBinding pos cs TypeNamespace name match unbound
        where
            match (TypeDecl {}) ref = (Complete, Type ref)
            match (TypeParamDecl {}) ref = (Complete, Type ref)
            match caseDecl@(CaseDecl {}) ref = match (caseDeclSampleDecl caseDecl) ref
            match decl _ = badBindingMatchError decl pos name typ
            unbound = unboundNameError pos name typ
    bindPostType _ other = other

    bindPostCallable :: BindPost Callable
    bindPostCallable cs (_, typ@(NameCallable pos name)) = findBinding pos cs ProcNamespace name match unbound
        where
            match _ ref = (Complete, Callable ref)
            unbound = unboundNameError pos name typ
    bindPostCallable _ other = other

    findBinding :: Pos -> [Context Decl] -> Namespace -> String -> (Decl -> Ref -> ret) -> ret -> ret
    findBinding _ cs namespace name matchFunc failValue
        | isJust ref = matchFunc decl $ fromJust ref
        | otherwise = failValue
        where
            ref = findRefByName cs namespace name
            decl = bindingValue $ findBindingByRef cs $ fromJust ref
