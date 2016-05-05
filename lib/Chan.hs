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

module Chan (
    makeOpenChanContext,
    flattenArrayedDecl,
    flattenPartialArrayedChan,
    insertArrayedIndices,
    arrayedChanIndexEval,
    arrayedChanSliceEval,
    arrayedRefSliceEval,
    arrayedRefIndexEval,
    arrayedIndexType,
    arrayedChanIndexType,
    isArrayedChan,
    isChan,
    arrayedDeclInterval,
    chanDeclsEquiv,
    makeDeclForChan
    ) where

    import ParseTree
    import Context
    import Type
    import Print
    import Report

    import Data.List
    import Data.Maybe
    import Data.Array

    evalError :: ShowParseNode node => [Context Decl] -> Pos -> node -> String -> (Completeness, node)
    evalError cs pos node message = (parseNodeError cs pos node message, node)

    isChan :: [Context Decl] -> Decl -> Bool
    isChan _ (ChanDecl {}) = True
    isChan _ (PortDecl {}) = True
    isChan cs (FlatArrayedDecl _ _ declArray) = isChan cs (declArray ! (fst $ bounds declArray))
    isChan cs (ArrayedDecl _ _ decl) = isChan cs decl
    isChan _ _ = False
    -- FIXME, aliases

    -- flattenPartialArrayedChan : flatten a PartialArrayedChan to just a construct of just
    --    Chan and FlatArrayedChan elements reflecting a flat view of its declared elements
    flattenPartialArrayedChan :: [Context Decl] -> Chan -> Chan
    flattenPartialArrayedChan cs (PartialArrayedChan pos ref) = flatten chanDecl ref
        where
            chanDecl = unaliasDecl cs $ bindingValue $ findBindingByRef cs ref

            flatten decl ref
                | isSingleChan decl = Chan pos ref
            flatten (FlatArrayedDecl pos interval decls) ref = FlatArrayedChan pos interval decls'
                where
                    indicesXelems = zip (range $ intervalRange interval) $ elems decls
                    decls' = map flattenElem indicesXelems
                    flattenElem (index, decl) = flatten decl (IndexRef ref index)
            flatten (ArrayedDecl pos interval decl) ref = FlatArrayedChan pos interval decls'
                where
                    decls' = map flattenAtIndex $ range $ intervalRange interval
                    flattenAtIndex index = flatten decl (IndexRef ref index)
            flatten decl@(AliasDecl {}) ref = flatten (unaliasDecl cs decl) ref
            flatten node _ = error $ show node
    flattenPartialArrayedChan _ _ = error "flattenPartialArrayedChan: not a PartialArrayedChan"

    flattenChanIndices :: [Chan] -> [(Ref, [[(Expr, Expr)]])]
    flattenChanIndices chans = grouped
        where
            flattenedChans = mapMaybe (flatten []) chans
            refs = nub (map fst flattenedChans)

            grouped = map groupRef refs

            groupRef ref = (ref, indicess)
                where indicess = map snd $ filter (\(ref2, _) -> ref2 == ref) flattenedChans

            -- ... (IndexChan (Chan ref) e1) e2 ... -> Just (ref, [e1, e2])
            flatten es (SliceChan _ chan li ri) = flatten ((li, ri):es) chan
            flatten es (IndexChan _ chan expr) = flatten ((expr, expr):es) chan
            flatten es (PartialArrayedChan _ ref) = Just (ref, es)
            flatten es (Chan _ ref) = Just (ref, es)
            flatten _ _ = Nothing

    -- FIXME, must check that indices are correct length against each other and the definitions
    makeOpenChanContext :: Pos -> [Context Decl] -> [Chan] -> Context Decl
    makeOpenChanContext pos cs chans = context
        where
            chanRefs = flattenChanIndices chans
            context = bindingsToContext1 $ map makeOpenChanFromRef chanRefs

            makeOpenChanFromRef (ref, indicess) = case decl of
                ArrayedDecl {} -> bindIncomplete openChanArray
                FlatArrayedDecl {} -> bindIncomplete openChanArray
                OpenChanDeclE {} -> bindIncomplete openChanArray
                ChanDecl {} -> bindIncomplete $ OpenChanDecl pos ref $ declType decl
                PortDecl {} -> bindIncomplete $ OpenChanDecl pos ref $ declType decl
                _ -> error $ "makeOpenChanContext: can't make open chan from `" ++ show decl ++ "'"
                where
                    binding = findBindingByRef cs ref
                    name = bindingName binding
                    decl = bindingValue binding
                    bindIncomplete = Binding 0 name OtherNamespace Incomplete
                    openChanArray = OpenChanDeclE pos ref indicess

    -- FIXME indices must be different for different chans
    flattenArrayedDecl :: [Context Decl] -> Decl -> Ref -> Decl
    flattenArrayedDecl cs (ArrayedDecl pos interval@(Interval {}) decl) ref
        = FlatArrayedDecl pos interval $ listArray range $ map flatten indices
            where
                flatten index = flattenArrayedDecl cs decl (IndexRef ref index)
                indices = intervalIndices interval
                range = intervalRange interval
    flattenArrayedDecl cs (FlatArrayedDecl pos interval decls) ref
        = FlatArrayedDecl pos interval $ listArray range $ map flatten $ zip (elems decls) indices
            where
                flatten (decl, index) = flattenArrayedDecl cs decl (IndexRef ref index)
                indices = intervalIndices interval
                range = intervalRange interval
    flattenArrayedDecl cs decl ref = AliasDecl (declPos decl) ref $ typeOfDecl $ unaliasDecl cs decl

    insertArrayedIndices :: Ref -> [Integer] -> Decl -> [(Integer, Integer)] -> (Decl, [[Integer]])
    insertArrayedIndices _ _ (AliasDecl pos ref typ) [] = (OpenChanDecl pos ref typ, [])
    insertArrayedIndices ref badPrefix array@(FlatArrayedDecl pos interval decls) indices =
        (FlatArrayedDecl pos interval decls', concat badIndicess)
        where
            (pairs, badIndicess) = unzip $ sliceDeclPair indices'
            decls' = decls // (catMaybes pairs)
            indices' = if null indices then [intervalRange interval] else indices

            sliceDeclPair ((li, ri):is) = map (\i -> indexDeclPair i is) [min li ri .. max li ri]
            sliceDeclPair _ = error "sliceDeclPair: can't happen"

            indexDeclPair index is
                | indexInInterval interval index = (Just (index, subDecl), subBadIndices)
                | isNothing decl = (Nothing, [badPrefix'])
                | otherwise = (Nothing, subBadIndices ++ [badPrefix'])
                where
                    badPrefix' = badPrefix ++ [index]
                    (subDecl, subBadIndices) = insertArrayedIndices indexRef badPrefix' (fromJust decl) is
                    decl = findArrayedElem array index
                    indexRef = IndexRef ref index
    insertArrayedIndices _ badPrefix decl _ = (decl, [badPrefix]) -- ++ indices])

    isArrayedChan :: Chan -> Bool
    isArrayedChan (PartialArrayedChan {}) = True
    isArrayedChan (FlatArrayedChan {}) = True
    isArrayedChan _ = False

    arrayedRefIndexEval :: [Context Decl] -> Ref -> Integer -> (Ref -> (Completeness, arr)) ->
        (Decl -> Ref -> (Completeness, arr)) -> (String -> (Completeness, arr)) -> (Completeness, arr)
    arrayedRefIndexEval cs ref index partialCons otherCons errorCons
        | isJust maybeElemDecl = case elemDecl of
            FlatArrayedDecl {} -> partialCons indexRef
            ArrayedDecl {} -> partialCons indexRef
            _ -> otherCons elemDecl indexRef
        | otherwise = errorCons "arrayed index out of range"
        where
            decl = bindingValue $ findBindingByRef cs ref
            indexRef = IndexRef ref index
            maybeElemDecl = findArrayedElem decl index
            elemDecl = unaliasDecl cs $ fromJust maybeElemDecl

    arrayedRefSliceEval :: [Context Decl] -> Ref -> (Integer, Integer) -> (Interval -> [arr] -> arr) ->
        (Ref -> (Completeness, arr)) -> (Decl -> Ref -> (Completeness, arr)) -> (String -> (Completeness, arr)) ->
        (Completeness, arr)
    arrayedRefSliceEval cs ref interval flatDecl partialCons otherCons errorCons
        --  | otherwise = error $ show interval
        | result == Complete = (Complete, flatDecl interval' elems)
        | otherwise = (result, errorNode)
        where
            (_, errorNode) = errorCons "" -- Just to get the errorNode
            (results, elems) = unzip $ map makeElem $ range interval
            result = gatherCompleteness results
            makeElem index = arrayedRefIndexEval cs ref index partialCons otherCons errorCons
            interval' = intervalFromRange (0, toInteger (rangeSize interval) - 1)

    makeChanForIndexOfDecl :: [Context Decl] -> Chan -> Decl -> Ref -> (Completeness, Chan)
    makeChanForIndexOfDecl _ node (ChanDecl {}) indexRef = (Complete, Chan (chanPos node) $ indexRef)
    makeChanForIndexOfDecl _ node (PortDecl {}) indexRef = (Complete, Chan (chanPos node) $ indexRef)
    makeChanForIndexOfDecl cs node (OpenChanDecl {}) (IndexRef _ index) = evalError cs (chanPos node) node $
        "index `" ++ show index ++ "' cannot be used as a channel when enclosing this command"
    makeChanForIndexOfDecl cs node _ (IndexRef _ index) = evalError cs (chanPos node) node $
        "index `" ++ show index ++ "' is not a channel"
    makeChanForIndexOfDecl _ _ decl _ = error $ "makeChanForIndexOfDecl: bad decl `" ++ show decl ++ "'"

    arrayedFlatIndexEval :: Interval -> [Chan] -> Integer -> (String -> (Completeness, Chan)) -> (Completeness, Chan)
    arrayedFlatIndexEval interval chans i errorCons
        | inRange range i = (Complete, chans !! index range i)
        | otherwise = errorCons "arrayed index out of range"
        where range = intervalRange interval

    arrayedChanIndexEval :: [Context Decl] -> Chan -> (Completeness, Chan)
    arrayedChanIndexEval cs node@(IndexChan pos array i)
        | not $ isArrayedChan array = evalError cs pos node "invalid channel indexing"
        | isNothing constI = evalError cs pos node "non-constant arrayed indices not supported"
        | otherwise = body array
        where
            constI = constExpr i
            i' = fromJust constI
            errorCons = evalError cs pos node

            body (FlatArrayedChan _ interval chans) = arrayedFlatIndexEval interval chans i' errorCons
            body (PartialArrayedChan _ ref) =
                arrayedRefIndexEval cs ref i' partial (makeChanForIndexOfDecl cs node) errorCons
            body _ = error "arrayedChanIndexEval: not an arrayed chan"

            partial ref = (Complete, PartialArrayedChan pos ref)
    arrayedChanIndexEval _ _ = error "arrayedChanIndexEval: not an IndexChan"

    arrayedChanSliceEval :: [Context Decl] -> Chan -> (Completeness, Chan)
    arrayedChanSliceEval cs node@(SliceChan pos array li ri)
        | not $ isArrayedChan array = evalError cs pos node "invalid channel indexing"
        | isNothing constLi || isNothing constRi = evalError cs pos node "non-constant arrayed indices not supported"
        | otherwise = body array
        where
            constLi = constExpr li
            constRi = constExpr ri
            li' = fromJust constLi
            ri' = fromJust constRi
            indexRange = (min li' ri', max li' ri')
            errorCons = evalError cs pos node

            body (FlatArrayedChan pos interval chans)
                | result == Complete = (Complete, FlatArrayedChan pos interval' chans')
                | otherwise = (result, node)
                where
                    (results, chans') = unzip $ map makeElem $ range indexRange
                    result = gatherCompleteness results
                    makeElem index = arrayedFlatIndexEval interval chans index errorCons
                    interval' = intervalFromRange (0, toInteger (rangeSize indexRange) - 1)
            body (PartialArrayedChan pos ref) = arrayedRefSliceEval cs ref indexRange (FlatArrayedChan pos)
                partial (makeChanForIndexOfDecl cs node) errorCons
            body _ = error "arrayedChanSliceEval: not an arrayed type"

            partial ref = (Complete, PartialArrayedChan pos ref)
    arrayedChanSliceEval _ _ = error "arrayedChanSliceEval: not a SliceChan"

    arrayedChanIndexType :: [Context Decl] -> Chan -> Maybe Type
    arrayedChanIndexType _ (FlatArrayedChan _ interval _) = Just $ intervalType interval
    arrayedChanIndexType cs (PartialArrayedChan _ ref) = arrayedIndexType cs ref
    arrayedChanIndexType _ _ = error "arrayedChanIndexType: not an arrayed type"

    arrayedIndexType :: [Context Decl] -> Ref -> Maybe Type
    arrayedIndexType cs ref = if completeness == Complete then Just indexType else Nothing
        where
            binding = findBindingByRef cs ref
            completeness = bindingCompleteness binding
            indexType = intervalType $ arrayedDeclInterval cs $ bindingValue binding

    arrayedDeclInterval :: [Context Decl] -> Decl -> Interval
    arrayedDeclInterval _ (ArrayedDecl _ interval _) = interval
    arrayedDeclInterval _ (FlatArrayedDecl _ interval _) = interval
    arrayedDeclInterval cs (OpenChanDeclE _ ref _) = arrayedDeclInterval cs $ bindingValue $ findBindingByRef cs ref
    arrayedDeclInterval _ decl = error $ "arrayedDeclInterval: not an arrayed decl: " ++ show decl

    -- FIXME, check that all elements of a FlatArrayedChan have the same type in { } chan expressions

    makeDeclForChan :: [Context Decl] -> Chan -> (Completeness, Decl)
    makeDeclForChan cs (Chan _ ref) = (Complete, bindingValue $ findBindingByRef cs ref)
    makeDeclForChan cs (PartialArrayedChan _ ref) = (Complete, bindingValue $ findBindingByRef cs ref)
    makeDeclForChan cs chan@(FlatArrayedChan pos interval chans)
        | chansResult == Complete && equiv == Complete = (Complete, ArrayedDecl pos interval declsHd)
        | chansResult == Complete = (equiv, undefined)
        | otherwise = (chansResult, undefined)
        where
            (chansResults, (declsHd:declsTl)) = unzip $ map (makeDeclForChan cs) chans
            chansResult = gatherCompleteness chansResults

            equiv = gatherCompleteness $ map declEquiv declsTl
            declEquiv decl = chanDeclsEquiv pos cs declsHd decl chan
    makeDeclForChan cs chan = (parseNodeError cs (chanPos chan) chan
        "can't determine type/structure of channel expression", undefined)

    isSingleChan :: Decl -> Bool
    isSingleChan (ChanDecl {}) = True
    isSingleChan (PortDecl {}) = True
    isSingleChan _ = False

    chanDeclsEquiv :: ShowParseNode node => Pos -> [Context Decl] -> Decl -> Decl -> node -> Completeness
    chanDeclsEquiv pos cs decl1 decl2 errorNode = body decl1 decl2
        where
            -- FIXME, more stuff
            -- Decls only take the form 'PortDecl', 'ChanDecl', 'ArrayedDecl', 'FlatArrayedDecl'

            -- Errors
            badType cs type1 type2 = parseNodeError cs pos errorNode $
                "bad type match between `" ++ showTypeName cs type1 ++ "' and `" ++ showTypeName cs type2 ++ "'"
            badSize = parseNodeError cs pos errorNode $ "bad arrayed channel size match between `"
                ++ showParseNode cs (unaliasDecl cs decl1) ++ "' and `"
                ++ showParseNode cs (unaliasDecl cs decl2) ++ "'" -- ++ " " ++ showTree cs
            badKindMatch = parseNodeError cs pos errorNode $ "arrayed and single channel incorrectly mixed between `"
                ++ showParseNode cs (unaliasDecl cs decl1) ++ "' and `"
                ++ showParseNode cs (unaliasDecl cs decl2) ++ "'" -- ++ " " ++ showTree cs

            body :: Decl -> Decl -> Completeness

            -- base combinations
            body decl1 decl2
                | isSingleChan decl1 && isSingleChan decl2 = if typeEquiv cs type1 type2
                    then Complete
                    else badType cs type1 type2
                    where
                        type1 = declType decl1
                        type2 = declType decl2
            body array1@(ArrayedDecl {}) array2@(ArrayedDecl {}) = gatherCompleteness [subEquiv, intervalsEquiv]
                where
                    subEquiv = chanDeclsEquiv pos cs (declSubDecl array1) (declSubDecl array2) errorNode
                    intervalsEquiv = if intervalSize (declInterval array1) == intervalSize (declInterval array2)
                        then Complete
                        else badSize
            -- error combinations
            body array1 array2
                | isSingleChan array1 || isSingleChan array2 = badKindMatch
            -- transformable combinations
            body array1@(FlatArrayedDecl {}) array2 = body (flatArrayedDeclToArrayedDecl array1) array2
            body array1 array2@(FlatArrayedDecl {}) = body (flatArrayedDeclToArrayedDecl array1) array2
            body array1 array2 = error $ "problem with chanDeclsEquiv of `" ++ show array1 ++ "' and `"
                ++ show array2 ++ "'"

            flatArrayedDeclToArrayedDecl (FlatArrayedDecl pos interval decls) = (ArrayedDecl pos interval firstDecl)
                where
                    firstDecl = decls ! firstIndex
                    -- FIXME, good grief
                    firstIndex = head $ range $ intervalRange interval
            flatArrayedDeclToArrayedDecl _ = error "flatArrayedDeclToArrayedDecl: can't happen"
