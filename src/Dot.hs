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

module Dot (
    DotGraph (..),
    DotEdge (..),
    DotNode (..),
    DotNVPair,
    BoundingBox (..),
    DPoint,
    diffDPoint,
    distDPoint,
    (+~),
    (-~),
    (/~),
    (*~),
    (.~),
    meanDPoints,
    boundingBoxHeight,
    boundingBoxWidth,
    boundingBoxScale,
    boundingBoxCentre,
    cornersToBoundingBox,
    showDotGraph,
    dotGraphBoundingBox,
    parseDotFile,
    flattenDotGraph,
    graphPassThroughDot,
    pointInBoundingBox,
    pointWHToBoundingBox,
    pointInBoundingBoxs,
    writeGraphsToFile
    ) where

    import Control.Monad
    import Data.Char (isAlphaNum)
    import Data.List
    import System.IO
    import Control.Exception (finally)
    import qualified Data.IntMap as IM

    import Parser
    import Report
    import Misc
    import Lexer
    import Show

    data DotToken =
          DotString Pos String
        | DotKeyword Pos String
        | DotError Pos String (Maybe Report)
            deriving Show

    tokenPos :: DotToken -> Pos
    tokenPos (DotString pos _) = pos
    tokenPos (DotKeyword pos _) = pos
    tokenPos (DotError pos _ _) = pos

    showToken :: DotToken -> String
    showToken (DotString _ str) = "\"" ++ escapeString "" str ++ "\""
    showToken (DotKeyword _ str) = str
    showToken (DotError _ str _) = "?" ++ str

    dotSymbols :: [String]
    dotSymbols = ["{", "}", "[", "]", "=", ",", ";", "->"]
    dotKeywords :: [String]
    dotKeywords = ["digraph", "subgraph"]

    keywordMatches :: MatchTree String
    keywordMatches = makeMatchTree dotKeywords
    symbolMatches :: MatchTree String
    symbolMatches = makeMatchTree dotSymbols

    tokenStringTail :: String -> Lexer String
    tokenStringTail ret = do
        let again c = tokenStringTail (c ++ ret)
        (_, c) <- anyChar
        case c of
            '\\' -> do
                (_, c2) <- anyChar
                case c2 of
                    -- '"' -> again ['"']
                    _ -> again [c2, '\\']
            '"' -> return $ reverse ret
            _ -> tokenStringTail (c:ret)

    tokenString :: Lexer (Maybe DotToken)
    tokenString = do
        (pos, _) <- char '"'
        s <- tokenStringTail ""
        return $ Just $ DotString pos s

    dotIdChar :: Lexer (Pos, Char)
    dotIdChar = makeCharParser "dot unquoted name character" match where
        match chr = isAlphaNum chr || chr `elem` "._:"

    tokenName :: Lexer (Maybe DotToken)
    tokenName = do
        (pos, c) <- dotIdChar
        (_, s) <- repChar dotIdChar
        let str = c:s
        if matchInMatchTree keywordMatches str
            then return $ Just $ DotKeyword pos str
            else return $ Just $ DotString pos str

    tokenSymbol :: Lexer (Maybe DotToken)
    tokenSymbol = do
        (pos:_, s) <- liftM unzip $ parseMatchTree (expecting "symbol") (mapM char) symbolMatches
        return $ Just $ DotKeyword pos s

    getToken :: Lexer (Maybe DotToken)
    getToken = anyOneOfStr "a valid token" [
        do
            ws
            return Nothing,
        tokenString,
        tokenName,
        tokenSymbol,
        do
            (pos, char) <- anyChar
            return $ Just $ DotError pos [char] $ Just $ Report pos $ "bad character `" ++ [char] ++ "'"]

    dotLexer :: Pos -> String -> [DotToken]
    dotLexer = lexer (DotError NoPos "") getToken

    type DotParser = Parser (LA1Tokens DotToken) Report

    -- Terminals

    wrong :: String -> Report
    wrong = Report NoPos

    eofReason :: Report
    eofReason = wrong "unexpected end of file"

    expectingError :: LA1Tokens DotToken -> String -> Parser tokens Report syn
    expectingError (LA1Tokens [DotError _ _ (Just report)]) _ = parserFail report
    expectingError (LA1Tokens [DotError pos _ Nothing]) _ = parserFail (Report pos "lexer error")
    expectingError _ str = parserFail $ expecting str

    anyString :: DotParser (Pos, String)
    anyString = matchToken eofReason match where
        match (LA1Tokens [DotString pos str]) = return (pos, str)
        match token = expectingError token "expecting a string"

    keyword :: String -> DotParser Pos
    keyword word = matchToken eofReason match where
        match (LA1Tokens [DotKeyword pos word2]) | word == word2 = return pos
        match token = expectingError token ("expecting keyword `" ++ word ++ "'")

    optionalKeyword :: String -> DotParser Pos
    optionalKeyword word = optional NoPos $ keyword word

    -- Dot

    data DotGraph = DotGraph {
        dotGraphName :: String, dotGraphProps :: [DotNVPair],
        dotGraphNodes :: [DotNode], dotGraphEdges :: [DotEdge],
        dotGraphSubGraphs :: [DotGraph]
        }

    type DotNVPair = (String, String)

    data DotNode = DotNode { dotNodeName :: String, dotNodeNVs :: [DotNVPair] }
        deriving (Show)

    data DotEdge = DotEdge { dotEdgeFrom :: String, dotEdgeto :: String, dotEdgeNVs :: [DotNVPair] }
        deriving (Show)

    sp :: ShowS
    sp = showChar ' '

    esc :: String -> String
    -- esc = escapeString ""
    esc = id

    showNVS :: [DotNVPair] -> ShowS
    showNVS pairs = showChar '[' . showListWithSep (showString ", ") showPair pairs . showChar ']'

    showPair :: DotNVPair -> ShowS
    showPair (name, value) = showString name . showChar '=' . showChar '"' . showString (esc value) . showChar '"'

    showNode :: DotNode -> ShowS
    showNode (DotNode name nvs) = showString name . sp . showNVS nvs

    showEdge :: DotEdge -> ShowS
    showEdge (DotEdge from to nvs) = showString from . showString " -> " . showString to . sp . showNVS nvs

    showDotGraph :: String -> Int -> DotGraph -> ShowS
    showDotGraph prefix tabs (DotGraph name props nodes edges subGraphs) =
        showString prefix . sp . showString name . sp .
        showChar '{' .
        showIfNotNull showPair props .
        showIfNotNull showNode nodes .
        showIfNotNull showEdge edges .
        showIfNotNull (showDotGraph "subgraph" (tabs + 1)) subGraphs .
        tabbedNL tabs .
        showChar '}'
        where
            sep = tabbedNL (tabs + 1)
            showIfNotNull shows list
                | not (null list) = sep . showListWithSep (showChar ';' . sep) shows list
                | otherwise = id

    instance ShowTab DotGraph where
        showsPrecTab prec tabs (DotGraph name props nodes edges subGraphs) = showParen (prec > 11) $
            showString "DotGraph " . shows name . sp
                . showListWith showNVPair props . sp
                . showsPrecTab 11 (tabs + 1) nodes . sp
                . showsPrecTab 11 (tabs + 1) edges . sp
                . showsPrecTab 11 (tabs + 1) subGraphs
            where showNVPair (name, value) = showChar '(' . shows name . showString ", " . shows value . showChar ')'

    instance Show DotGraph where
        showsPrec prec graph = showsPrecTab prec 0 graph

    instance ShowTab DotNode
    instance ShowTab DotEdge

    nameValuePair :: DotParser DotNVPair
    nameValuePair = do
        (_, name) <- anyString
        keyword "="
        (_, value) <- anyString
        return (name, value)

    nameValuePairs :: DotParser [DotNVPair]
    nameValuePairs = do
        keyword "["
        nameValuePairsTail []

    nameValuePairsTail :: [DotNVPair] -> DotParser [DotNVPair]
    nameValuePairsTail ret = do
        optionalKeyword ","
        anyOneOfStr "" [
            do
                keyword "]"
                return $ reverse ret,
            do
                pair <- nameValuePair
                nameValuePairsTail (pair:ret) ]

    edge :: String -> DotParser ([DotNVPair], [DotNode], [DotEdge], [DotGraph])
    edge from = do
        keyword "->"
        (_, to) <- anyString
        nvs <- optional [] nameValuePairs
        return $ ([], [], [DotEdge from to nvs], [])

    node :: String -> DotParser ([DotNVPair], [DotNode], [DotEdge], [DotGraph])
    node name = do
        nvs <- optional [] nameValuePairs
        return $ ([], [DotNode name nvs], [], [])

    prop :: String -> DotParser ([DotNVPair], [DotNode], [DotEdge], [DotGraph])
    prop name = do
        keyword "="
        (_, value) <- anyString
        return ([(name, value)], [], [], [])

    nodeOrEdge :: DotParser ([DotNVPair], [DotNode], [DotEdge], [DotGraph])
    nodeOrEdge = do
        (_, name1) <- anyString
        anyOneOfStr "node or edge" [ prop name1, edge name1, node name1 ]

    nodeEdgeSubGraph :: DotParser ([DotNVPair], [DotNode], [DotEdge], [DotGraph])
    nodeEdgeSubGraph = do
        elem <- anyOneOfStr "node, edge or subgraph" [
            nodeOrEdge,
            do
                keyword "subgraph"
                g <- graph
                return ([], [], [], [g]) ]
        optionalKeyword ";"
        return elem

    digraph :: DotParser DotGraph
    digraph = do
        keyword "digraph"
        graph

    graph :: DotParser DotGraph
    graph = do
        (_, name) <- anyString
        keyword "{"
        (propss, nodess, edgess, subGraphss) <- liftM unzip4 $ rep nodeEdgeSubGraph
        let
            props = concat propss
            nodes = concat nodess
            edges = concat edgess
            subGraphs = concat subGraphss
        keyword "}"
        return $ DotGraph name props nodes edges subGraphs

    dotDescription :: DotParser [DotGraph]
    dotDescription = rep digraph

    dotParser :: [DotToken] -> ParseResult [DotToken] [DotGraph] Report
    dotParser = parseLA1 dotDescription

    joinBrokenLines :: String -> String
    joinBrokenLines [] = []
    joinBrokenLines ('\\':'\n':rest) = joinBrokenLines rest
    joinBrokenLines (char:rest) = char : joinBrokenLines rest

    parseDotFile :: FilePath -> IO [DotGraph]
    parseDotFile file = do
        contents <- liftM joinBrokenLines $ readFile file
        let
            pos = PosLC (PosFile file (ImportFile file)) 1 1
            Why comp ret = lexAndParse dotLexer dotParser tokenPos showToken pos [] contents
        printCompleteness noPosContext comp
        return $ if comp == Complete
            then ret
            else []

    flattenDotGraph :: DotGraph -> DotGraph
    flattenDotGraph graph@(DotGraph name _ _ _ _) = DotGraph name [] nodes' edges' []
        where
            (nodes', edges') = body graph

            body (DotGraph _ _ nodes edges subGraphs) = (nodes ++ concat subNodess, edges ++ concat subEdgess)
                where (subNodess, subEdgess) = unzip $ map body subGraphs

    writeGraphsToFile :: String -> [DotGraph] -> IO ()
    writeGraphsToFile filename graphs = do
        handle <- openFile filename WriteMode
        finally (forM_ graphs $ \graph -> hPutStrLn handle $ showDotGraph "digraph" 0 graph "") (hClose handle)
        return ()

    graphPassThroughDot :: (String -> IO ()) -> String -> DotGraph -> IO DotGraph
    graphPassThroughDot rm baseName graph = do
        writeGraphsToFile dotDot [graph]
        hPutStr stderr "running dot ... "
        mySystem "dot" ["-Tdot", "-o", dotDot1, dotDot]
        hPutStrLn stderr "finished"
        graph' <- parseDotFile dotDot1
        rm dotDot
        rm dotDot1
        return $ head graph'
        where
            dotDot = baseName ++ ".dot"
            dotDot1 = baseName ++ ".dot1"

    data BoundingBox = BoundingBox {
        boundingBoxTop :: Double,
        boundingBoxLeft :: Double,
        boundingBoxBottom :: Double,
        boundingBoxRight :: Double }
        deriving (Show)

    type DPoint = (Double, Double)

    dotGraphBoundingBox :: DotGraph -> BoundingBox
    dotGraphBoundingBox graph = BoundingBox top left bottom right
        where
            matchNode name2 (DotNode name _) = name == name2
            nodeNVs (DotNode _ nvs) = nvs

            graphNodes = filter (matchNode "graph") $ dotGraphNodes graph
            graphNVs = concatMap nodeNVs graphNodes
            Just bbStr = lookup "bb" graphNVs
            [left, top, right, bottom] = map read $ splitWith "," bbStr

    boundingBoxWidth :: BoundingBox -> Double
    boundingBoxWidth bb = boundingBoxRight bb - boundingBoxLeft bb

    boundingBoxHeight :: BoundingBox -> Double
    boundingBoxHeight bb = boundingBoxBottom bb - boundingBoxTop bb

    -- boundingBoxScale :: scale a bounding box around its centre
    boundingBoxScale :: Double -> BoundingBox -> BoundingBox
    boundingBoxScale scaleFactor bb = BoundingBox {
        boundingBoxTop = centreY - scaledHeight / 2,
        boundingBoxBottom = centreY + scaledHeight / 2,
        boundingBoxLeft = centreX - scaledWidth / 2,
        boundingBoxRight = centreX + scaledWidth / 2 }
        where
            width = boundingBoxWidth bb
            height = boundingBoxHeight bb
            (centreX, centreY) = boundingBoxCentre bb
            scaledWidth = scaleFactor * width
            scaledHeight = scaleFactor * height

    boundingBoxCentre :: BoundingBox -> DPoint
    boundingBoxCentre bb = meanDPoints [(boundingBoxLeft bb, boundingBoxTop bb),
        (boundingBoxRight bb, boundingBoxBottom bb)]

    pointInBoundingBox :: BoundingBox -> DPoint -> Bool
    pointInBoundingBox bb (x, y) =
        boundingBoxLeft bb <= x && boundingBoxRight bb >= x &&
        boundingBoxTop bb <= y && boundingBoxBottom bb >= y

    pointWHToBoundingBox :: DPoint -> Double -> Double -> BoundingBox
    pointWHToBoundingBox (x, y) width height = BoundingBox {
        boundingBoxTop = y - halfHeight,
        boundingBoxBottom = y + halfHeight,
        boundingBoxLeft = x - halfWidth,
        boundingBoxRight = x + halfWidth }
        where
            halfHeight = height / 2
            halfWidth = width / 2

    pointInBoundingBoxs :: (a -> [BoundingBox]) -> IM.IntMap a -> DPoint -> [Int]
    pointInBoundingBoxs getBoundingBox arr point = IM.keys $ IM.filter inIndex arr
        where
            inIndex arrElem
                | any (\bb -> pointInBoundingBox bb point) bbs = True
                | otherwise = False
                where bbs = getBoundingBox arrElem

    infixl 6 +~
    infixl 6 -~
    infixl 7 *~
    infixl 7 /~
    infixl 7 .~

    (+~) :: DPoint -> DPoint -> DPoint
    (x1, y1) +~ (x2, y2) = (x1 + x2, y1 + y2)

    (-~) :: DPoint -> DPoint -> DPoint
    (x1, y1) -~ (x2, y2) = (x1 - x2, y1 - y2)

    (/~) :: DPoint -> Double -> DPoint
    (x, y) /~ m = (x / m, y / m)

    (*~) :: DPoint -> Double -> DPoint
    (x, y) *~ m = (x * m, y * m)

    (.~) :: DPoint -> DPoint -> DPoint
    (x1, y1) .~ (x2, y2) = (x1 * x2, y1 * y2)

    diffDPoint :: DPoint -> DPoint -> DPoint
    diffDPoint (x1, y1) (x2, y2) = (abs (x1 - x2), abs (y1 - y2))

    distDPoint :: DPoint -> DPoint -> Double
    distDPoint p1 p2 = sqrt (xDist * xDist + yDist * yDist)
        where (xDist, yDist) = diffDPoint p1 p2

    meanDPoints :: [DPoint] -> DPoint
    meanDPoints pointsL = (foldl' (+~) (0.0, 0.0) pointsL) /~ pointsLCount
        where pointsLCount = fromIntegral $ length pointsL

    cornersToBoundingBox :: DPoint -> DPoint -> BoundingBox
    cornersToBoundingBox (x1, y1) (x2, y2) = BoundingBox {
        boundingBoxLeft = min x1 x2,
        boundingBoxRight = max x1 x2,
        boundingBoxTop = min y1 y2,
        boundingBoxBottom = max y1 y2 }

