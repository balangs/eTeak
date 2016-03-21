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

module Misc (
    applyPrecedence,
    arrayMapAccumL,
    bangDebug,
    canReadFile,
    canReadDirectory,
    columnFormat,
    compareFst,
    crossLists,
    deleteAtIndices,
    dropSuffix,
    eqFst,
    errorFixme,
    escapeString,
    filledArray,
    flattenSome,
    frequencyBy,
    fromJustDebug,
    fromMany,
    fromOne,
    fromSome,
    indent,
    isMany,
    isOne,
    isSome,
    joinWith,
    listAtLeastLength,
    log10,
    log2,
    mapAccumM,
    mapN,
    mapOverSome,
    mapSnd,
    maybeMergeByWith,
    mean,
    mergeByWith,
    mergeWith,
    midPoints,
    mySystem,
    partitions,
    permute,
    plural,
    capitalise,
    prePadZip,
    replaceAt,
    replaceElemInSome,
    scatter,
    showSomeHaskell,
    Some (..),
    someAt,
    sortAndGroupByElem,
    splitWith,
    uncurry3,
    uncurry4,
    mapM'
    ) where

    import Data.List
    import Data.Array
    import Data.Maybe
    import Numeric (showHex)
    import System.Process
    import System.Directory
    import System.Exit
    import Data.Char
    import Control.Monad

    log2 :: Integral a => a -> Int
    log2 0 = error "log2 0 is not computable"
    log2 1 = 0
    log2 n = log2 (n `div` 2) + 1

    log10 :: Integral a => a -> Int
    log10 0 = error "log10 0 is not computable"
    log10 a
        | a < 10 = 1
        | otherwise = log10 (a `div` 10) + 1

    -- joinWith : join a list using e as a separator
    joinWith :: [a] -> [[a]] -> [a]
    joinWith _ [] = []
    joinWith _ [a] = a
    joinWith e (a:rest) = a ++ e ++ joinWith e rest

    -- splitWith : split list into sublists removing elements from seps, using them as separators
    splitWith :: Eq a => [a] -> [a] -> [[a]]
    splitWith seps list
        | null rest = [retElem]
        | otherwise = retElem : splitWith seps (tail rest)
        where (retElem, rest) = span (`notElem` seps) list

    -- partitions : all partitions of a given list from ([], list) to (list, [])
    partitions :: [a] -> [([a], [a])]
    partitions list = body [] list
        where
            body pref [] = [(pref, [])]
            body pref (r:rs) = (pref, r:rs) : body (pref ++ [r]) rs

    -- replace : replace index index of list with elem
    replaceAt :: [a] -> Int -> a -> [a]
    replaceAt list i newElem = take i list ++ [newElem] ++ drop (i + 1) list

    -- mapN : apply f to chunks of n elements of the given list.
    --    The last f call may be with a list of fewer than n elements
    mapN :: Int -> ([a] -> b) -> [a] -> [b]
    mapN _ _ [] = []
    mapN n f l = f (take n l) : mapN n f (drop n l)

    -- maybeMergeByWith : merge two sorted lists using `by' to compare elements,
    --    `with' to combine equal elements and fail (return Nothing) if any equal elements compared
    --    fail the test `test'
    maybeMergeByWith :: (a -> a -> Ordering) -> (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> Maybe [a]
    maybeMergeByWith by test with lefts rights = body lefts rights []
        where
            body [] rs ret = Just $ (reverse ret) ++ rs
            body ls [] ret = Just $ (reverse ret) ++ ls
            body (l:ls) (r:rs) ret = case by l r of
                LT -> body ls (r:rs) (l:ret)
                GT -> body (l:ls) rs (r:ret)
                EQ | test l r -> body ls rs (with l r : ret)
                _ -> Nothing

    -- mergeByWith : as maybeMergeByWith but always merge (no test)
    mergeByWith :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a] -> [a]
    mergeByWith by with ls rs = fromJust $ maybeMergeByWith by (\_ _ -> True) with ls rs

    -- mergeWith : merge two lists using f on equal left and right values
    mergeWith :: Ord a => (a -> a -> a) -> [a] -> [a] -> [a]
    mergeWith = mergeByWith compare

    -- mapAccumM : mapAccumL for Monads
    mapAccumM :: Monad m => (a -> b -> m (a, d)) -> a -> [b] -> m (a, [d])
    mapAccumM _ s [] = return (s, [])
    mapAccumM f s (x:xs) = do
        (s', y) <- f s x
        (s'', ys) <- mapAccumM f s' xs
        return (s'', y:ys)

    -- deleteAtIndices : returns elements of `list' with indices not found in `indices'
    --    the returned list will be in the same order as `list'.  `indices' must be in sorted into ascending order
    deleteAtIndices :: [Int] -> [a] -> [a]
    deleteAtIndices is list = body 0 (sort is) list
        where
            body _ [] l = l
            body i (j:js) l = (take skip l) ++ body (j + 1) js (drop (skip + 1) l)
                where skip = j - i

    data Some a = One a | Many [a] | Some [Some a]
        deriving (Show, Read, Eq, Ord)

    fromOne :: Some a -> a
    fromOne (One x) = x
    fromOne _ = error "fromOne: not One"

    fromMany :: Some a -> [a]
    fromMany (Many x) = x
    fromMany _ = error "fromMany: not Many"

    fromSome :: Some a -> [Some a]
    fromSome (Some x) = x
    fromSome _ = error "fromSome: not Some"

    isOne :: Some a -> Bool
    isOne (One {}) = True
    isOne _ = False

    isMany :: Some a -> Bool
    isMany (Many {}) = True
    isMany _ = False

    isSome :: Some a -> Bool
    isSome (Some {}) = True
    isSome _ = False

    flattenSome :: Some a -> [a]
    flattenSome (One x) = [x]
    flattenSome (Many xs) = xs
    flattenSome (Some xs) = concatMap flattenSome xs

    instance Functor Some where
        fmap f (One x) = One (f x)
        fmap f (Many x) = Many (map f x)
        fmap f (Some x) = Some (map (fmap f) x)

    mapOverSome :: (a -> b -> c) -> Some a -> [b] -> (Some c, [b])
    mapOverSome f (Many xs) ys = (Many $ map (uncurry f) $ zip xs ys, drop count ys)
        where count = length xs
    mapOverSome _ (Some []) ys = (Some [], ys)
    mapOverSome f (Some (x:xs)) ys = (Some (rx:rrest), ys'')
        where
            (rx, ys') = mapOverSome f x ys
            (Some rrest, ys'') = mapOverSome f (Some xs) ys'
    -- mapOverSome f (One x) [] = (One x, [])
    mapOverSome f (One x) (y:ys) = (One (f x y), ys)
    mapOverSome _ _ _ = error "mapOverSome: bad map"

    replaceElemInSome :: [Int] -> a -> Some a -> Some a
    replaceElemInSome toAddr newLeaf some = body [] some
        where
            body addr leaf@(One _)
                | addr == toAddr = One newLeaf
                | otherwise = leaf
            body addr (Many es) = Many $ map (bodyElem addr) (zip [0..] es)
            body addr (Some es) = Some $ map (bodySome addr) (zip [0..] es)

            bodySome addr (i, someElem) = body (addr ++ [i]) someElem

            bodyElem addr (i, someElem)
                | (addr ++ [i]) == toAddr = newLeaf
                | otherwise = someElem

    someAt :: Some a -> [Int] -> a
    someAt (Some xs) (addr:addrs) = someAt (xs !! addr) addrs
    someAt (Many xs) [addr] = xs !! addr
    someAt (One x) [] = x
    someAt _ addr = error $ "someAt: bad Some indexing" ++ show addr

    showSomeHaskell :: (a -> String) -> Some a -> String
    showSomeHaskell showLocal some = showSomeList showLocal showHaskellList some

    showHaskellList :: [String] -> String
    showHaskellList es = "[" ++ joinWith "," es ++ "]"

    showSomeList :: (a -> String) -> ([String] -> String) -> Some a -> String
    showSomeList showLocal _ (One link) = showLocal link
    showSomeList showLocal showListLocal (Many links) = showListLocal (map showLocal links)
    showSomeList showLocal showListLocal (Some links) = showListLocal
        (map (showSomeList showLocal showListLocal) links)

    -- listAtLeastLength :: Integral a => a -> [b] -> Bool
    listAtLeastLength :: Int -> [b] -> Bool
    listAtLeastLength 0 _ = True
    listAtLeastLength _ [] = False
    listAtLeastLength n (_:l) = listAtLeastLength (n - 1) l

    eqFst :: Eq a => (a, b) -> (a, c) -> Bool
    eqFst l r = fst l == fst r

    compareFst :: Ord a => (a, b) -> (a, c) -> Ordering
    compareFst l r = fst l `compare` fst r

    sortAndGroupByElem :: (Ord a, Eq a) => (b -> a) -> [b] -> [(a, [b])]
    sortAndGroupByElem elem l = map rearrange $ groupBy eqElem $ sortBy compareElem l
        where
            eqElem l r = elem l == elem r
            compareElem l r = elem l `compare` elem r

            rearrange (e:es) = (elem e, e:es)
            rearrange [] = error "rearrange: can't happen"

    -- columnFormat : format the given columns of strings into a list of row strings
    --    with columns separated by `firstLineSeps' for the first row from each column
    --    position or `otherLineSeps' for rows formed from \n split continuations of
    --    those rows.  If any rows first line has cells with '\n' as the first character,
    --  drop that '\n' and treat the line as an `other' line.
    --  The first element of the separator lists is a line prefix.
    --  For example:
    --    columnFormat [["A\nl","B","C"],["D","E\nG","F"],["H","I","J"]] ["1)"," - ", "@"] ["2)","   ", ":"] gives:
    --  1)A - D@H
    --  2)l    :
    --  1)B - E@I
    --  2)    G:
    --  1)C - F@J
    columnFormat :: [[String]] -> [String] -> [String] -> [String]
    columnFormat columns firstLineSeps otherLineSeps = concatMap joinRows brokenRowss
        where
            specialCellPrefix = "\n"

            brokenRowss = map lineBreak $ transpose columns

            columnWidths = map (maximum . map length) $ transpose $ concat brokenRowss
            leftAlign str width = str ++ replicate (width - length str) ' '

            lineBreak strs = transpose $ map padColumn splitStrs
                where
                    padColumn c = c ++ replicate (rowCount - length c) ""
                    splitStrs = map splitLine strs
                    rowCount = maximum $ map length splitStrs
                    -- split a line but leave prefixing specialCellPrefix
                    splitLine line
                        | isPrefixOf specialCellPrefix line = let
                            s:ss = splitWith "\n" (dropPrefix specialCellPrefix line)
                            in (specialCellPrefix ++ s):ss
                        | otherwise = splitWith "\n" line

            -- join line elements padding all but the last one and alternating with `seps'
            joinLine seps row = concat $ alternate seps $ zipWith leftAlign (init row) columnWidths ++ [last row]

            joinRows rows = joinLine firstSeps trimmedFirstLine : map (joinLine otherLineSeps) otherLines
                where
                    firstLine:otherLines = rows
                    trimmedFirstLine = map (dropPrefix specialCellPrefix) firstLine
                    firstSeps
                        | any (isPrefixOf specialCellPrefix) firstLine = otherLineSeps
                        | otherwise = firstLineSeps

    alternate :: [a] -> [a] -> [a]
    alternate a b = concat $ transpose [a, b]

    dropSuffix :: Eq a => [a] -> [a] -> [a]
    dropSuffix suffix list
        | isSuffixOf suffix list = take (length list - length suffix) list
        | otherwise = list

    dropPrefix :: Eq a => [a] -> [a] -> [a]
    dropPrefix prefix list
        | isPrefixOf prefix list = drop (length prefix) list
        | otherwise = list

    prePadZip :: a -> [a] -> [a] -> [(a, a)]
    prePadZip pad l1 l2
        | len1 < len2 = zip (replicate diff pad ++ l1) l2
        | otherwise = zip l1 (replicate diff pad ++ l2)
        where
            len1 = length l1
            len2 = length l2
            diff = abs (len1 - len2)

    mean :: Fractional a => [a] -> a
    mean l = sum l / fromIntegral (length l)

    midPoints :: Fractional a => [a] -> [a]
    midPoints (l1:l2:ls) = mean [l1, l2] : midPoints (l2:ls)
    midPoints _ = []

    fromJustDebug :: String -> Maybe a -> a
    fromJustDebug message Nothing = error $ "fromJustDebug: " ++ message
    fromJustDebug _ (Just ret) = ret

    bangDebug :: (Show i, Ix i) => String -> Array i e -> i -> e
    bangDebug message arr i
        | inRange (bounds arr) i = arr ! i
        | otherwise = error $ "bangDebug: " ++ message ++ " " ++ show (bounds arr) ++ " " ++ show i

    frequencyBy :: (a -> a -> Bool) -> [a] -> [(Int, a)]
    frequencyBy _ [] = []
    frequencyBy eq (l:ls) = (1 + length eqLs, l) : frequencyBy eq notEqLs
        where
            (eqLs, notEqLs) = partition (eq l) ls

    filledArray :: (Enum a, Ix a) => (a,a) -> b -> [(a,b)] -> Array a b
    filledArray arrBounds defaultVal arrAssocs = array arrBounds $
        mergeByWith compareFst (\_ r -> r) defaults $ sortBy compareFst arrAssocs
        where defaults = zip (range arrBounds) (repeat defaultVal)

    arrayMapAccumL :: Ix i => (a -> (i, e) -> (a, f)) -> a -> Array i e -> (a, Array i f)
    arrayMapAccumL f acc arr = (acc', listArray (bounds arr) arr')
        where (acc', arr') = mapAccumL f acc $ assocs arr

    crossLists :: [[a]] -> [[a]]
    crossLists [] = []
    crossLists [ls] = [[l] | l <- ls]
    crossLists (ls:lss) = [l:rest | l <- ls, rest <- crossLists lss]

    permute :: [a] -> [[a]]
    permute [] = [[]]
    permute (a:as) = concatMap (scatter a) (permute as)

    scatter :: a -> [a] -> [[a]]
    scatter e [] = [[e]]
    scatter e (f:fs) = (e:f:fs) : map (f:) (scatter e fs)

    mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
    mapSnd f = map (\(l, r) -> (l, f r))

    -- errorFixme : to mark errors which need to be replaced with `proper' error handling
    errorFixme :: String -> a
    errorFixme = error

    escapeString :: String -> String -> String
    escapeString extraEscapeChars str = concatMap escapeChar str
        where
            escapeChar '\\' = "\\\\"
            escapeChar '"' = "\\\""
            escapeChar '\n' = "\\n"
            escapeChar chr
                | fromEnum chr < 32 = "\\x" ++ showHex (fromEnum chr) ""
                | chr `elem` extraEscapeChars = ['\\', chr]
            escapeChar chr = [chr]

    mySystem :: String -> [String] -> IO ExitCode
    mySystem prog args = system $ prog ++ " " ++ joinWith " " args

    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (a, b, c) = f a b c

    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f (a, b, c, d) = f a b c d

    plural :: String -> [a] -> String
    plural word [_] = word
    plural word _ = word ++ "s"

    capitalise :: String -> String
    capitalise (c:cs)
        | isAlpha c = toUpper c : cs
    capitalise cs = cs

    -- indent : indent the given string with string `tab'.  `tab' will be inserted at the start of the string
    --    and after every `\n'
    indent :: String -> String -> String
    indent tab str = tab ++ concatMap replaceNl str
        where
            replaceNl '\n' = "\n" ++ tab
            replaceNl chr = [chr]

    applyPrecedence :: Int
    applyPrecedence = 10

    canReadFile :: FilePath -> IO Bool
    canReadFile filename = do
        fileExists <- doesFileExist filename
        if fileExists
            then do
                filePerms <- getPermissions filename
                return $ readable filePerms
            else return False

    canReadDirectory :: FilePath -> IO Bool
    canReadDirectory filename = do
        fileExists <- doesDirectoryExist filename
        if fileExists
            then do
                filePerms <- getPermissions filename
                return $ readable filePerms
            else return False

    mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
    mapM' f = liftM reverse . foldM (\ret arg -> liftM (:ret) $ f arg) []
