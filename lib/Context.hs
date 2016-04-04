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

module Context (
    bindingIsTopLevel,
    bindingsToContext,
    bindingsToContext1,
    bindingPos,
    contextBindingsList,
    contextCompleteness,
    contextIndexRange,
    contextMap,
    contextMapAccumL,
    contextProgressiveFold,
    contextSize,
    contextToPosArray,
    contextsLastIndex,
    contextsNextIndex,
    emptyContext,
    isEmptyContext,
    findBindingByRef,
    findBindingByIndex,
    findBindingContextByRef,
    findRefByName,
    rebaseContextIndices,
    refBinding,
    Namespace (..),
    Context (..),
    Ref (..),
    Binding (..),
    BindingValue (..)
    ) where

    import Data.List
    import Data.Maybe
    import Data.Array

    import Misc
    import Report
    import Show

    data Namespace = OtherNamespace | TypeNamespace | ProcNamespace | PosNamespace
        deriving (Show, Read, Eq, Ord)

    class (Show value, Read value) => BindingValue value where
        findArrayedElem :: value -> Integer -> Maybe value
        findArrayedElem _ _ = Nothing

        valuePos :: value -> Pos
        valuePos _ = NoPos

        unaliasValue :: [Context value] -> value -> value
        unaliasValue _ v = v

        unaliasValueRef :: [Context value] -> Ref -> Ref
        unaliasValueRef _ ref = ref

    data {- BindingValue value => -} Binding value = Binding {
        bindingIndex :: Int,
        bindingName :: String,
        bindingNamespace :: Namespace,
        bindingCompleteness :: Completeness,
        bindingValue :: value }
        deriving (Show, Eq, Ord)

    data {- BindingValue value => -} Context value =
          EmptyContext
        | Context {
            contextVisibleNameRange :: (Int, Int),
            contextBindings :: Array Int (Binding value) }
        deriving (Show, Eq, Ord)

    data Ref =
          Ref Int
        | IndexRef Ref Integer
        deriving (Show, Read, Eq, Ord)

    emptyContext :: BindingValue value => Context value
    emptyContext = EmptyContext

    isEmptyContext :: BindingValue value => Context value -> Bool
    isEmptyContext EmptyContext = True
    isEmptyContext _ = False

    refBinding :: BindingValue value => Binding value -> Ref
    refBinding = Ref . bindingIndex

    contextCompleteness :: BindingValue value => Context value -> Completeness
    contextCompleteness EmptyContext = Complete
    contextCompleteness context@(Context {}) = gatherCompleteness completenesss
        where
            bindings = indices $ contextBindings context
            completenesss = map (bindingCompleteness . (findBindingByIndex [context])) bindings

    contextIndexRange :: Context value -> (Int, Int)
    contextIndexRange EmptyContext = error "contextIndexRange: can't take range of an empty context"
    contextIndexRange context@(Context {}) = bounds $ contextBindings context

    contextsLastIndex :: BindingValue value => [Context value] -> Int
    contextsLastIndex [] = 0
    contextsLastIndex (EmptyContext:contexts) = contextsLastIndex contexts
    contextsLastIndex (context@(Context {}):_) = lastIndex
        where (_, lastIndex) = bounds $ contextBindings context

    contextsNextIndex :: BindingValue value => [Context value] -> Int
    contextsNextIndex cs = contextsLastIndex cs + 1

    numberBindings :: BindingValue value => (Int, Int) -> [Binding value] -> Array Int (Binding value)
    numberBindings indexRange bindings = array indexRange $ map numberBinding $ zip (range indexRange) bindings
        where numberBinding (i, binding) = (i, binding { bindingIndex = i })

    bindingsToContext :: BindingValue value => (Int, Int) -> [Binding value] -> Context value
    bindingsToContext _ [] = EmptyContext
    bindingsToContext indexRange bindings = Context indexRange $ numberBindings indexRange bindings

    bindingsToContext1 :: BindingValue value => [Binding value] -> Context value
    bindingsToContext1 bindings = bindingsToContext indexRange bindings
        where indexRange = (1, length bindings)

    contextBindingsList :: BindingValue value => Context value -> [Binding value]
    contextBindingsList EmptyContext = []
    contextBindingsList context = elems $ contextBindings context

    contextSize :: BindingValue value => Context value -> Int
    contextSize EmptyContext = 0
    contextSize context = rangeSize $ contextIndexRange context

    findRefByName :: BindingValue value => [Context value] -> Namespace -> String -> Maybe Ref
    findRefByName [] _ _ = Nothing
    findRefByName (EmptyContext:contexts) namespace name = findRefByName contexts namespace name
    findRefByName (context:contexts) namespace name
        | isJust foundIndex = Just $ Ref $ fromJust foundIndex
        | otherwise = findRefByName contexts namespace name
        where
            bindings = contextBindings context

            foundIndex = findIndexArray $ range $ contextVisibleNameRange context

            findIndexArray [] = Nothing
            findIndexArray (i:is)
                | match (bindings ! i) = Just i
                | otherwise = findIndexArray is

            match (Binding _ name' namespace' _ _) = name' == name && namespace' == namespace

    contextIsTopLevel :: BindingValue value => Context value -> Bool
    contextIsTopLevel context@(Context {}) = firstIndex == 1
        where (firstIndex, _) = contextIndexRange context
    contextIsTopLevel _ = False

    bindingIsTopLevel :: BindingValue value => [Context value] -> Ref -> Bool
    bindingIsTopLevel cs = contextIsTopLevel . findBindingContextByRef cs

    findBindingContextByRef :: BindingValue value => [Context value] -> Ref -> Context value
    findBindingContextByRef cs (Ref int) = findBindingContextByIndex cs int
    findBindingContextByRef _ ref = error $ "findBindingContextByRef: can't handle ref `" ++ show ref ++ "'"

    findBindingContextByIndex :: BindingValue value => [Context value] -> Int -> Context value
    findBindingContextByIndex _ 0 = error "body, index == 0"
    findBindingContextByIndex contexts i = body contexts
        where
            body [] = error $ "findBindingContextByIndex: index not found: "
                ++ show i ++ " " ++ show (map contextIndexRange contexts) ++ "\n" ++ show contexts
            body (EmptyContext:cs) = body cs
            body (context@(Context {}):cs)
                | inRange ctxRange i = context
                | otherwise = body cs
                where ctxRange = bounds $ contextBindings context

    findBindingByRef :: BindingValue value => [Context value] -> Ref -> Binding value
    findBindingByRef cs ref@(Ref {}) = findBindingByIndex cs int'
        where Ref int' = unaliasValueRef cs ref
    findBindingByRef cs (IndexRef subRef i)
        | isJust elemValue = unaliasedBinding
        | otherwise = error "findBindingByRef: IndexRef with incorrect index"
        where
            arrayedBinding = findBindingByRef cs subRef
            elemValue = findArrayedElem (bindingValue arrayedBinding) i
            unaliasedValue = unaliasValue cs $ fromJust elemValue
            unaliasedBinding = arrayedBinding { bindingName = bindingName arrayedBinding ++ "[" ++ show i ++ "]",
                bindingValue = unaliasedValue }

    findBindingByIndex :: BindingValue value => [Context value] -> Int -> Binding value
    findBindingByIndex cs i = (contextBindings foundContext) ! i
        where foundContext = findBindingContextByIndex cs i

    contextMap :: BindingValue value => (Binding value -> Binding value) -> Context value -> Context value
    contextMap f context = snd $ contextMapAccumL f' () context
        where f' _ binding = ((), f binding)

    contextMapAccumL :: BindingValue value => (a -> Binding value -> (a, Binding value)) -> a -> Context value -> (a, Context value)
    contextMapAccumL _ a EmptyContext = (a, EmptyContext)
    contextMapAccumL f a context@(Context {}) = (a', context { contextBindings = bindings' })
        where (a', bindings') = arrayMapAccumL f' a $ contextBindings context
                where f' acc (_, binding) = f acc binding

    contextProgressiveFold :: BindingValue value => (a -> Context value -> Binding value -> (a, Binding value)) -> a -> Context value -> (a, Context value)
    contextProgressiveFold _ a EmptyContext = (a, EmptyContext)
    contextProgressiveFold f a context@(Context {}) = (a', Context indexRange bindings')
        where
            indexRange@(low, _) = contextIndexRange context

            (a', bindings') = foldl' f' (a, contextBindings context) $ range indexRange

            f' (acc, bindings) i
                | bindingIndex binding' /= i || bindingIndex binding /= i = error $
                    "contexProgressiveError: binding index error: should be: " ++ show i
                        ++ " binding: " ++ show (bindingIndex binding)
                        ++ " binding': " ++ show (bindingIndex binding')
                | otherwise = (acc', bindings // [(bindingIndex binding', binding')])
                where
                    binding = bindings ! i
                    pCtx = Context (low, i - 1) bindings
                    (acc', binding') = f acc pCtx binding

    instance BindingValue value => PosContext (Context value) where
        posIndex c i
            | indexOK = Just pos
            | otherwise = Nothing
            where
                indexOK = case c of
                    EmptyContext -> False
                    Context {} -> inRange (contextIndexRange c) i
                binding = findBindingByRef [c] (Ref i)
                pos = bindingPos binding

    bindingPos :: BindingValue value => Binding value -> Pos
    bindingPos = valuePos . bindingValue

    contextToPosArray :: BindingValue value => Context value -> Maybe PosArray
    contextToPosArray context
        | null poss = Nothing
        | otherwise = Just $ PosArray $ array (fst (head poss), fst (last poss)) poss
        where
            poss = sortBy compareFst $ mapMaybe findPos (range (contextIndexRange context))

            findPos i
                | bindingNamespace binding == PosNamespace = Just (i, valuePos $ bindingValue binding)
                | otherwise = Nothing
                where binding = (contextBindings context) ! i

    -- rebaseContextIndices: renumber the elements of the given context by adding `adjustment'
    --    Does no recursive descend into values
    rebaseContextIndices :: BindingValue value => Int -> Context value -> Context value
    rebaseContextIndices _ EmptyContext = EmptyContext
    rebaseContextIndices adjustment (Context accessRange bindings) = Context accessRange' bindings'
        where
            (oldContentsLow, oldContentsHigh) = bounds bindings
            (oldAccessLow, oldAccessHigh) = accessRange
            accessRange' = (oldAccessLow + adjustment, oldAccessHigh + adjustment)
            contentsRange' = (oldContentsLow + adjustment, oldContentsHigh + adjustment)
            bindings' = numberBindings contentsRange' $ elems bindings -- listArray contentsRange' $ elems bindings
    -- rebaseContextIndices adjustment (Context _ _ _) = error "cannot rebase a Context carrying substs"

    instance BindingValue value => Read (Context value) where
        readsPrec _ = parenC $ \str -> maybeToList $ do
            (headToken, rest) <- maybeLex str
            case headToken of
                "Context" -> readFields fields (Context (1,1) (array (1,1) [])) rest
                    where fields = [
                            ReadField "contextVisibleNameRange" (readPairC readsC readsC)
                            (\o f -> o { contextVisibleNameRange = f }),
                            ReadField "contextBindings" readArrayC (\o f -> o { contextBindings = f }) ]
                "EmptyContext" -> Just (EmptyContext, rest)
                _ -> Nothing

    instance BindingValue value => Read (Binding value) where
        readsPrec _ = parenC $ \str -> maybeToList $ do
            (headToken, rest) <- maybeLex str
            case headToken of
                -- FIXME, changed from (NoDecl ...).  Check this is correct
                "Binding" -> readFields fields (Binding 0 "" OtherNamespace Complete undefined) rest
                    where fields = [
                            ReadField "bindingIndex" readsC (\o f -> o { bindingIndex = f }),
                            ReadField "bindingName" readsC (\o f -> o { bindingName = f }),
                            ReadField "bindingNamespace" readsC (\o f -> o { bindingNamespace = f }),
                            ReadField "bindingCompleteness" readsC (\o f -> o { bindingCompleteness = f }),
                            ReadField "bindingValue" readsC (\o f -> o { bindingValue = f }) ]
                _ -> Nothing
