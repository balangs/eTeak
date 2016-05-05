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

module Gates (
    GateNetlist (..),
    GatePort (..),
    GateElem (..),
    GateConn (..),
    GateParam (..),
    isGateNet,
    isGateInstance,
    parseVerilogFile,
    showVerilog,
    ) where

    import Parser
    import Report
    import ParseTree
    import Lexer
    import Show
    import Bits

    import Control.Monad
    import Data.List
    import Data.Maybe

    type GateProperty = (String, String)

    data GateNetlist = GateNetlist String [GatePort] [GateProperty] [GateElem]
        deriving (Show, Read)

    data GatePort = GatePort String Direction Int
        deriving (Show, Read)

    data GateElem = GateNet String Int
        | GateInstance String [[GateConn]]
        | GateInstanceParam String [GateParam] [[GateConn]]
        --  | GateInstanceNamedConns String [(String, [GateConn])]
        --  | GateBehav [[GateConn]] [GateBehavStatement]
        | GateDRTest String [GateConn] [GateConn]
        deriving (Show, Read)

    data GateParam = GateParamInt Int
        | GateParamString String
        deriving (Show, Read)

    {-
    data GateBehavStatement = GateBehavAssign [GateConn] GateBehavExpr
        deriving (Show, Read)

    data GateBehavExpr = GateBehavConst Int
        deriving (Show, Read) -}

    data GateConn = GateConn String (Slice Int)
        deriving (Show, Read)

    data VerilogToken =
          VerilogName { tokenPos :: Pos, tokenRaw :: String }
        | VerilogKeyword { tokenPos :: Pos, tokenRaw :: String }
        | VerilogPreproc { tokenPos :: Pos, tokenRaw :: String }
        | VerilogInteger { tokenPos :: Pos, tokenRaw :: String, valueInteger :: Integer }
        | VerilogFloat { tokenPos :: Pos, tokenRaw :: String, valueFloat :: Double }
        | VerilogString { tokenPos :: Pos, tokenRaw :: String }
        --  | VerilogInt { tokenPos :: Pos, tokenRaw :: String, tokenValInt :: Integer }
        --  | VerilogImplicant { tokenPos :: Pos, tokenRaw :: String,
        --    tokenValInt :: Integer, tokenDCInt :: Integer }
        --  | VerilogString { tokenPos :: Pos, tokenRaw :: String, tokenValStr :: String }
        | VerilogError { tokenPos :: Pos, tokenRaw :: String, tokenReport :: Maybe Report }
            deriving (Show)

    verilogSymbols :: [String]
    verilogSymbols = ["(", ")", ",", ";", ".", "/", "[", ":", "]", "(*", "*)", "="]
    verilogKeywords :: [String]
    verilogKeywords = ["module", "macromodule", "endmodule", "input", "output", "wire",
        "`timescale"]

    keywordMatches :: MatchTree String
    keywordMatches = makeMatchTree verilogKeywords
    symbolMatches :: MatchTree String
    symbolMatches = makeMatchTree verilogSymbols

    tokenPreprocessor :: Lexer (Maybe VerilogToken)
    tokenPreprocessor = do
        (pos, _) <- makeCharParser "expecting `" (== '`')
        (_, c) <- firstId
        (_, s) <- repChar nameChar
        let str = c:s
        return $ Just $ VerilogPreproc pos str

    tokenString :: Lexer (Maybe VerilogToken)
    tokenString = do
        (pos, _) <- makeCharParser "expecting \"" (== '"')
        (_, str) <- liftM unzip $ rep $ makeCharParser "" (/= '"')
        makeCharParser "expecting \"" (== '"')
        return $ Just $ VerilogString pos str

    tokenName :: Lexer (Maybe VerilogToken)
    tokenName = do
        (pos, c) <- firstId
        (_, s) <- repChar nameChar
        let str = c:s
        return $ Just $ if matchInMatchTree keywordMatches str
            then VerilogKeyword pos str
            else VerilogName pos str

    tokenSymbol :: Lexer (Maybe VerilogToken)
    tokenSymbol = do
        (pos:_, s) <- liftM unzip $ parseMatchTree (expecting "symbol") (mapM char) symbolMatches
        return $ Just $ VerilogKeyword pos s

    tokenNumber :: Lexer (Maybe VerilogToken)
    tokenNumber = do
        (pos, c) <- decDigitChar
        (_, s) <- repChar decDigitChar
        let int = read (c:s)
        liftM Just $ optional (VerilogInteger pos (c:s) int) $ do
            makeCharParser "expecting ." (== '.')
            (_, fc) <- decDigitChar
            (_, fs) <- repChar decDigitChar
            let raw = (c:s) ++ "." ++ (fc:fs)
            return $ VerilogFloat pos raw (read raw)

    getToken :: Lexer (Maybe VerilogToken)
    getToken = anyOneOfStr "a valid token" [
        do
            ws
            return Nothing,
        tokenPreprocessor,
        tokenName,
        do
            comment (string "//") (string "/*") (string "*/")
            return Nothing,
        tokenSymbol,
        tokenString,
        tokenNumber,
        do
            (pos, char) <- anyChar
            return $ Just $ VerilogError pos [char] $ Just $ Report pos $ "bad character `" ++ [char] ++ "'"]

    verilogLexer :: Pos -> String -> [VerilogToken]
    verilogLexer = lexer (VerilogError NoPos "") getToken

    type VerilogParser = Parser (LA1Tokens VerilogToken) Report

    -- Terminals

    wrong :: String -> Report
    wrong = Report NoPos

    eofReason :: Report
    eofReason = wrong "unexpected end of file"

    expectingError :: LA1Tokens VerilogToken -> String -> Parser tokens Report syn
    expectingError (LA1Tokens [token@(VerilogError _ _ (Just _))]) _ = parserFail $ fromJust $ tokenReport token
    expectingError (LA1Tokens [VerilogError pos _ Nothing]) _ = parserFail (Report pos "lexer error")
    expectingError _ str = parserFail $ expecting str

    name :: VerilogParser String
    name = matchToken eofReason match where
        match (LA1Tokens [VerilogName pos str]) = return str
        match token = expectingError token "an identifier"

    matchString :: VerilogParser String
    matchString = matchToken eofReason match where
        match (LA1Tokens [VerilogString pos str]) = return str
        match token = expectingError token "a string"

    timeName :: VerilogParser Integer
    timeName = matchToken eofReason match where
        match (LA1Tokens [VerilogName pos str]) = case str of
            "s" -> return 0
            "ms" -> return (-3)
            "ns" -> return (-6)
            "ps" -> return (-9)
            "fs" -> return (-12)
            _ -> parserFail (Report pos "expecting a timescale multiplier")
        match token = expectingError token "an identifier"

    keyword :: String -> VerilogParser Pos
    keyword word = matchToken eofReason match where
        match (LA1Tokens [VerilogKeyword pos word2]) | word == word2 = return pos
        match token = expectingError token ("keyword `" ++ word ++ "'")

    preproc :: String -> VerilogParser Pos
    preproc word = matchToken eofReason match where
        match (LA1Tokens [VerilogPreproc pos word2]) | word == word2 = return pos
        match token = expectingError token ("preproc `" ++ word ++ "'")

    float :: VerilogParser Double
    float = matchToken eofReason match where
        match (LA1Tokens [VerilogFloat _ _ val]) = return val
        match (LA1Tokens [VerilogInteger _ _ val]) = return (fromInteger val)
        match token = expectingError token "a floating point number"

    number :: VerilogParser Integer
    number = matchToken eofReason match where
        match (LA1Tokens [VerilogInteger _ _ val]) = return val
        match token = expectingError token "a floating point number"

    nameList :: VerilogParser [String]
    nameList = sepList (keyword ",") name

    -- Actual

    direction :: VerilogParser Direction
    direction = anyOneOfStr "port direction" [
        do
            keyword "input"
            return Input,
        do
            keyword "output"
            return Output ]

    verilogNet :: VerilogParser [GateElem]
    verilogNet = do
        keyword "wire"
        width <- portOrWireWidth
        portNames <- nameList
        let makeNet name = GateNet name width
        keyword ";"
        return $ map makeNet portNames

    slice :: VerilogParser (Pos, Integer, Integer)
    slice = do
        pos <- keyword "["
        lhs <- number
        keyword ":"
        rhs <- number
        keyword "]"
        return (pos, lhs, rhs)

    portOrWireWidth :: VerilogParser Int
    portOrWireWidth = optional 1 $ do
        (pos, left, right) <- slice
        if right == 0
            then return (fromInteger left + 1)
            else parserFail (Report pos "port and wire defns. must have low index of 0")

    verilogPort :: VerilogParser [GatePort]
    verilogPort = do
        portDirection <- direction
        width <- portOrWireWidth
        portNames <- nameList
        let makePort name = GatePort name portDirection width 
        keyword ";"
        return $ map makePort portNames

    {-
    namedConn :: VerilogParser [GateConn]
    namedConn = do
        keyword "."
        portName <- name
        keyword "("
        netName <- name
        keyword ")"
        return (portName, [GateConn netName 0 1])
        -}

    unnamedConn :: VerilogParser [GateConn]
    unnamedConn = do
        netName <- name
        sl <- optional (0 +: 1) $ do
            (pos, left, right) <- slice
            when (left < right) $ parserFail (Report pos "left index must be >= right index")
            return (fromInteger left >: fromInteger right)
        return [GateConn netName sl]

    verilogInstance :: VerilogParser GateElem
    verilogInstance = do
        moduleName <- name
        {- instanceName <- -}
        name
        keyword "("
        inst <- anyOneOfStr "connections" [
            -- do
            --    conns <- sepList (keyword ",") namedConn
            --    return $ GateInstanceNamedConns moduleName conns,
            do
                conns <- sepList (keyword ",") $ optional [] unnamedConn
                return $ GateInstance moduleName conns ]
        keyword ")"
        keyword ";"
        return inst

    property :: VerilogParser GateProperty
    property = do
        n <- name
        keyword "="
        v <- matchString
        return (n, v)

    macromodule :: VerilogParser GateNetlist
    macromodule = do
        let
            applyDirections startDirection = snd . mapAccumL body startDirection
                where
                    body _ (Just newDirection, name) = (newDirection, GatePort name newDirection 1)
                    body direction (Nothing, name) = (direction, GatePort name direction 1)

        anyOneOfStr "module keyword" [ keyword "module", keyword "macromodule" ]
        moduleName <- name
        {- portNames <- -}
        portListPorts <- optional [] $ do
            keyword "("
            ports <- anyOneOfStr "ports" [
                do
                    nameList
                    return [],
                do
                    firstPortDirection <- direction
                    firstPortName <- name
                    let firstPort = GatePort firstPortName firstPortDirection 1
                    otherPorts <- optional [] $ do
                        liftM (applyDirections firstPortDirection) $ rep $ do
                            keyword ","
                            portDirection <- optional Nothing (direction >>= return . Just)
                            portName <- name
                            return (portDirection, portName)
                    return $ firstPort:otherPorts ]
            keyword ")"
            return ports
        properties <- optional [] $ do
            keyword "(*"
            props <- rep property
            keyword "*)"
            return props
        -- Check portNames match
        keyword ";"
        (internalPortss, elemss) <- liftM unzip $ rep verilogElement
        let
            internalPorts = concat internalPortss
            elems = concat elemss
        keyword "endmodule"
        return $ GateNetlist moduleName (portListPorts ++ internalPorts) properties elems

    verilogElement :: VerilogParser ([GatePort], [GateElem])
    verilogElement = anyOneOfStr "netlist element" [
        do
            inst <- verilogInstance
            return ([], [inst]),
        do
            nets <- verilogNet
            return ([], nets),
        do
            ports <- verilogPort
            return (ports, []) ]

    timescale :: VerilogParser ()
    timescale = do
        preproc "timescale"
        float
        timeName
        keyword "/"
        float
        timeName
        return ()

    verilogDescription :: VerilogParser [GateNetlist]
    verilogDescription = liftM catMaybes $ rep $ anyOneOfStr "verilog top level" [
        liftM Just macromodule,
        liftM (\_ -> Nothing) timescale
        ]

    verilogParser :: [VerilogToken] -> ParseResult [VerilogToken] [GateNetlist] Report
    verilogParser = parseLA1 verilogDescription

    parseVerilogFile :: FilePath -> WhyT IO [GateNetlist]
    parseVerilogFile filename = do
        contents <- whyTReadFile NoPos filename 
        let pos = PosLC (PosFile filename (ImportFile filename)) 1 1
        WhyT $ return $ lexAndParse verilogLexer verilogParser tokenPos tokenRaw pos [] contents

    space :: ShowS
    space = showChar ' '

    showBitRange :: Int -> Int -> ShowS
    showBitRange offset width =
        showChar '[' . shows (offset + width - 1) . showChar ':' . shows offset . showChar ']'

    showWidthRange :: Int -> ShowS
    showWidthRange = showBitRange 0

    isGateNet :: GateElem -> Bool
    isGateNet (GateNet {}) = True
    isGateNet _ = False

    isGateInstance :: GateElem -> Bool
    isGateInstance (GateInstance {}) = True
    isGateInstance _ = False

    showVerilog :: Bool -> GateNetlist -> ShowS
    showVerilog clipNames (GateNetlist name ports props elems) =
        showString "module " . showStringClip name . showString " (" .
        showCommaList showString (map portName ports) . showString ");\n" .
        -- FIXME, show properties
        showLines showPort ports .
        showLines showNet nets .
        showLines showInstance (zip [0..] instances) .
        showString "endmodule\n"
        where
            portName (GatePort name _ _) = name
            (nets, instances) = partition isGateNet elems
            onebPorts = concatMap oneb ports
                where
                    oneb (GatePort name _ width)
                        | width == 1 = [name]
                        | otherwise = []
            onebNets = onebPorts ++ (concatMap oneb nets)
                where
                    oneb (GateNet name width)
                        | width == 1 = [name]
                        | otherwise = []
                    oneb _ = error "oneb: not a net"

            -- CLips module names with length > 240 to make them compatible with nanosim
            -- FIXME: it might still produce aliased names for long names with differences only in the clipped part
            showStringClip s
                | clipNames = showString sClipped
                | otherwise = showString s
                where
                    sClipped
                        | 240 < length s = take 230 s ++ "_c_" ++ reverse (take 8 (reverse s))
                        | otherwise = s

            showPort (GatePort name dir width)
                | singleNA = showString portStr . space . showString name
                | otherwise = showString portStr . space . showWidthRange width . space . showString name
                where
                    portStr = case dir of
                        Input -> "input"
                        Output -> "output"
                    singleNA = width == 1

            showNet (GateNet name width)
                | singleNA = showString "wire " . showString name
                | otherwise = showString "wire " . showWidthRange width . space . showString name
                where
                    singleNA = width == 1
            showNet _ = id

            showParams [] = id
            showParams params = showString "#(" . showCommaList showParam params . showString ") "

            showParam (GateParamInt int) = shows int
            showParam (GateParamString string) = shows string

            -- showNamed name showsLocal = showChar '.' .
            --    showString name . showChar '(' . showsLocal . showChar ')'

            showInstance :: (Int, GateElem) -> ShowS
            showInstance (i, (GateInstance moduleName conns)) = showStringClip moduleName . space .
                showString ("I" ++ show i) . showString " (" . showCommaList showSomeGateConn conns . showString ")"
            -- showInstance (i, (GateInstanceNamedConns moduleName conns)) = showStringClip moduleName . space .
            --    showString ("I" ++ show i) . showString " (" . showCommaList showSomeGateConn conns . showString ")"
            showInstance (i, (GateInstanceParam moduleName params conns)) = showStringClip moduleName . space .
                showParams params .
                showString ("I" ++ show i) .
                showString " (" . showCommaList showSomeGateConn conns . showString ")"
            showInstance (_, (GateDRTest name f t)) =
                showString "wire " . output . showString "; assign " . output . showString " = |("
                    . showSomeGateConn f . showString " & " . showSomeGateConn t . showString "); always @("
                    . output . showString ") if (" . output . showString ") $display (\"Ouch!\")"
                where
                    output = showString ("T" ++ name)
            showInstance _ = id

            showSomeGateConn [] = id -- Nothing for unconnected.  This may not always be the correct behaviour
            showSomeGateConn [conn] = showGateConn conn
            showSomeGateConn conns = showChar '{' . showCommaList showGateConn conns . showChar '}'
            showGateConn (GateConn name slice)
                | singleNA = showString name
                | otherwise = showString name . verilogShowSlice slice
                where
                    singleNA = name `elem` onebNets
            -- showGateConn (NamedGateConn portName name offset width) = showNamed portName
            --    (showGateConn (GateConn name offset width))
            showLines shows es = showListWith showLine es
                where showLine e = showString "  " . shows e . showString ";\n"

            showCommaList = showListWithSep (showString ", ")
