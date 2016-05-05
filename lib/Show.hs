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

{-# LANGUAGE ExistentialQuantification #-}

module Show (
    ShowTab (..),
    tabbedNL,
    showDropArray,
    showDropArrayFilter,
    showDropList,
    showDropListWith,
    showBlockList,
    showBlockListWith,
    showListWithComment,
    showLineList,
    showsTab,
    showListWithSep,
    showListWith,
    showTab,
    skipComments,
    maybeLex,
    maybeRead,
    maybeCurl,
    maybeKeyword,
    readFields,
    ReadField (..),
    readsC,
    readArrayC,
    parenC,
    readPairC,
    readListC,
    readListCWith,
    readBareListC
    ) where

    import Data.List
    import Data.Maybe
    import Control.Monad
    import Data.Char
    import Data.Array.IArray

    tabDepth :: Int
    tabDepth = 2

    -- compose sep = foldr (.) id . intersperse sep

    tabbedNL :: Int -> ShowS
    tabbedNL tabs = showString $ '\n' : replicate (tabs * tabDepth) ' '

    class Show a => ShowTab a where
        showsPrecTab :: Int -> Int -> a -> ShowS
        showsPrecTab prec _tabs value after = showsPrec prec value after
        showListTab :: Int -> [a] -> ShowS
        showListTab = showDropList
        showArrayTab :: (Ix b, ShowTab b, IArray c a) => Int -> c b a -> ShowS
        showArrayTab = showDropArray

    showsTab :: ShowTab a => Int -> a -> ShowS
    showsTab = showsPrecTab 0

    showTab :: ShowTab a => a -> String
    showTab value = showsTab 0 value ""

    showListWithSep :: ShowS -> (a -> ShowS) -> [a] -> ShowS
    showListWithSep _ _ [] = id
    showListWithSep _ localShows [e] = localShows e
    showListWithSep sep localShows (e:es) = localShows e . sep . showListWithSep sep localShows es

    showListWith :: (a -> ShowS) -> [a] -> ShowS
    showListWith _ [] = id
    showListWith localShows (e:es) = localShows e . showListWith localShows es

    showDropArrayFilter :: (Ix b, ShowTab a, ShowTab b) => ((b, a) -> Bool) -> Int -> Array b a -> ShowS
    -- showDropArrayFilter : only show assoc. elements which pass p
    showDropArrayFilter p tabs arr = showString "array " . shows (bounds arr) . showChar ' ' .
        showDropList tabs (filter p $ assocs arr)

    showDropArray :: (Ix b, ShowTab a, ShowTab b, IArray c a) => Int -> c b a -> ShowS
    showDropArray tabs arr = showString "array " . shows (bounds arr) . showChar ' ' .
        showDropList tabs (assocs arr)

    showDropList :: ShowTab a => Int -> [a] -> ShowS
    showDropList = showDropListWith showsTab

    showDropListWith :: (Int -> a -> ShowS) -> Int -> [a] -> String -> String
    showDropListWith _ _ [] = showString "[]"
    showDropListWith showsTabL tabs es = showChar '[' .
        tabbedNL tabs' . showListWithSep (showChar ',' . tabbedNL tabs') (showsTabL tabs') es .
        showChar ']'
        where tabs' = tabs + 1

    showBlockListWith :: (Int -> a -> ShowS) -> Int -> [a] -> ShowS
    showBlockListWith _ _ [] = showString "[]"
    showBlockListWith showsTabL tabs es = tabbedNL tabs . showChar '[' .
        tabbedNL tabs' . showListWithSep (showChar ',' . tabbedNL tabs') (showsTabL tabs') es .
        tabbedNL tabs . showChar ']'
        where tabs' = tabs + 1

    showBlockList :: ShowTab a => Int -> [a] -> ShowS
    showBlockList = showBlockListWith showsTab

    showListWithComment :: ShowTab a => ((Int -> (Int, a) -> ShowS) -> Int -> [(Int, a)] -> ShowS) ->
        Bool -> (Int -> ShowS) -> Int -> [a] -> ShowS
    showListWithComment showListLocal _ _ tabs [] = showListLocal undefined tabs []
    showListWithComment showListLocal after commentF tabs es = showListLocal showElem tabs $ zip [0..] es
        where
            nullShow _ = id

            (beforeComment, afterComment)
                | after = (nullShow, blockComment)
                | otherwise = (blockComment, nullShow)

            blockComment i = showString "{- " . commentF i . showString " -} "
            showElem elemTabs (i, e) = beforeComment i . showsTab elemTabs e . afterComment i

    showLineList :: ShowTab a => Int -> [a] -> ShowS
    showLineList _ [] = showString "[]"
    showLineList tabs es = showChar '[' . showListWithSep (showChar ',') (showsTab tabs') es . showChar ']'
        where tabs' = tabs + 1

    instance ShowTab a => ShowTab [a] where
        showsPrecTab _ = showListTab

    instance ShowTab Int
    instance ShowTab Integer
    {-
    instance ShowTab Char where
        showListTab _ = showString
        -}

    instance ShowTab ()

    instance ShowTab a => ShowTab (Maybe a) where
        showsPrecTab _ _ Nothing = showString "Nothing"
        showsPrecTab _ tabs (Just a) = (showString "(Just " . showsPrecTab 11 tabs a . showString ")")

    instance (ShowTab a, ShowTab b) => ShowTab (a,b) where
        showsPrecTab _ tabs (va,vb) = showChar '(' . showsTab tabs va . showChar ','
            . showsTab tabs vb . showChar ')'

    maybeLex :: String -> Maybe (String, String)
    maybeLex str = do
        (token, rest) <- listToMaybe $ lex $ skipComments str
        if token == ""
            then Nothing
            else return (token, rest)

    maybeRead :: Read a => String -> Maybe a
    maybeRead str = do
        (ret, rest) <- listToMaybe $ reads str
        case rest of
            [] -> return ret
            _ -> fail ""

    maybeCurl :: String -> Maybe (Bool, String)
    maybeCurl str = do
        (token, rest) <- maybeLex str
        if token == "{"
            then return (True, rest)
            else return (False, str)

    maybeKeyword :: String -> String -> Maybe String
    maybeKeyword keyword str = do
        (token, rest) <- maybeLex str
        if token == keyword
            then return rest
            else Nothing

    data ReadField dataType = forall fieldType .
        ReadField String (String -> [(fieldType, String)]) (dataType -> fieldType -> dataType)

    readAndSet :: ReadField a -> a -> String -> Maybe (a, String)
    readAndSet (ReadField _ reader setter) object str = do
        (value, rest) <- listToMaybe $ reader str
        return (setter object value, rest)

    readNamedFields :: [ReadField a] -> a -> String -> Maybe (a, String)
    readNamedFields fields object str = do
        (fieldName, rest) <- maybeLex str
        let
            parseReadField object2 str2
                | isJust field = readAndSet (fromJust field) object2 str2
                | otherwise = error $ "cannot parse field `" ++ fieldName ++ "'"
                where field = find findReadField fields
            findReadField (ReadField name _ _) = name == fieldName
        case fieldName of
            "}" -> return (object, rest)
            "," -> readNamedFields fields object rest
            _ -> do
                rest2 <- maybeKeyword "=" rest
                (object', rest3) <- parseReadField object rest2
                readNamedFields fields object' rest3

    readUnnamedFields :: [ReadField a] -> a -> String -> Maybe (a, String)
    readUnnamedFields [] object str = return (object, str)
    readUnnamedFields (field:fields) object str = do
        (object', rest) <- readAndSet field object str
        readUnnamedFields fields object' rest

    readFields :: [ReadField a] -> a -> String -> Maybe (a, String)
    readFields fields nullObject str = do
        (isCurl, rest) <- maybeCurl str
        if isCurl
            then readNamedFields fields nullObject rest
            else readUnnamedFields fields nullObject rest

    skipWS :: String -> String
    skipWS = dropWhile isSpace

    skipEOL :: String -> String
    skipEOL ('\n':rest) = rest
    skipEOL l = l

    skipToEOL :: String -> String
    skipToEOL = skipEOL . dropWhile (/= '\n')

    skipComments :: String -> String
    skipComments str = body (skipWS str)
        where
            body ('{':'-':rest) = skipComments (skipBlockComment 1 rest)
            body ('-':'-':rest) = skipComments (skipToEOL rest)
            body rest = rest

    skipBlockComment :: Int -> String -> String
    skipBlockComment depth str = body (dropWhile (/= '-') str)
        where
            body ('-':'}':rest)
                | depth == 1 = rest
                | otherwise = skipBlockComment (depth - 1) rest
            body rest = skipBlockComment depth rest

    readsC :: Read a => String -> [(a, String)]
    readsC = reads . skipComments

    readBareListC :: Read a => String -> ([a], String)
    readBareListC str0 = body str0 []
        where
            body str ret = case readRet of
                [(e, rest)] -> body rest (e:ret)
                _ -> (reverse ret, str)
                where readRet = readsC str

    readListC :: Read a => ReadS [a]
    readListC = readListCWith readsC

    readListCWith :: Read a => ReadS a -> ReadS [a]
    readListCWith reader str = maybeToList $ do
        rest <- liftM skipComments $ maybeKeyword "[" str
        if isPrefixOf "]" rest
            then return ([], tail rest)
            else body [] $ skipComments rest
        where
            body ret rest = do
                (value, rest2) <- listToMaybe $ reader rest
                (sep, rest3) <- maybeLex rest2
                let ret' = value:ret
                case sep of
                    "]" -> return (reverse ret', rest3)
                    "," -> body ret' rest3
                    _ -> Nothing

    readPairC :: (Read a, Read b) =>
        (String -> [(a, String)]) -> (String -> [(b, String)]) -> String -> [((a,b), String)]
    readPairC subReaderL subReaderR str = maybeToList $ do
        rest <- maybeKeyword "(" str
        (low,rest2) <- listToMaybe $ subReaderL rest
        rest3 <- maybeKeyword "," rest2
        (high,rest4) <- listToMaybe $ subReaderR rest3
        rest5 <- maybeKeyword ")" rest4
        return ((low,high), rest5)

    parenC :: ReadS a -> ReadS a
    parenC reads' str = fromMaybe (reads' str) $ do
        rest <- liftM skipComments $ maybeKeyword "(" str
        (ret, rest2) <- listToMaybe $ parenC reads' rest
        rest3 <- liftM skipComments $ maybeKeyword ")" rest2
        return [(ret, rest3)]

    readArrayC :: (Read a, Read b, Ix b, IArray c a) => String -> [(c b a, String)]
    readArrayC = parenC $ \str -> maybeToList $ do
        (headToken, rest) <- maybeLex str
        let rest' = skipComments rest
        case headToken of
            "listArray" -> do
                ((low,high), rest2) <- listToMaybe $ readPairC readsC readsC rest'
                (listElems,rest3) <- listToMaybe $ readListC rest2
                return (listArray (low, high) listElems, rest3)
            "array" -> do
                ((low,high), rest2) <- listToMaybe $ readPairC readsC readsC rest'
                (listElems,rest3) <- listToMaybe $ readListC rest2
                return (array (low, high) listElems, rest3)
            _ -> Nothing
