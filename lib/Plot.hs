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

module Plot (
    plotPart,
    plotOptionUsage,
    PlotOption (..),
    PlotLanguage (..),
    PlotPartialElem,
    defaultPlotOptions
    ) where

    import Misc
    import NetParts
    import ParseTree
    import Report
    import Bits
    import Dot
    import Show
    import Options

    import Data.Array
    import Data.List
    import Data.Maybe
    import Control.Monad

    data PlotOption =
          PlotBreakVariables
        | PlotShowPoss
        | PlotTokenColour String
        | PlotCompColour String
        | PlotHorizParts
        | PlotPartSubGraph
        | PlotShowTitle
        | PlotOnlyParts [String]
        | PlotOLength Int
        | PlotRatio { plotOptionRatio :: Double }
        | PlotPaperSize (Double, Double)
        | PlotSize (Double, Double)
        | PlotLanguage PlotLanguage
        | PlotUniformLatch
        | PlotPartial [PlotPartialElem]
        | PlotShowUnconnected

    type PlotPartialElem = ([String], Int)

    data PlotLanguage = PlotPS | PlotPDF | PlotSVG
        deriving Eq

    instance SubOption PlotOption where
        matchSubOption (PlotBreakVariables) (PlotBreakVariables) = True
        matchSubOption (PlotShowPoss) (PlotShowPoss) = True
        matchSubOption (PlotTokenColour {}) (PlotTokenColour {}) = True
        matchSubOption (PlotCompColour {}) (PlotCompColour {}) = True
        matchSubOption (PlotHorizParts) (PlotHorizParts) = True
        matchSubOption (PlotPartSubGraph) (PlotPartSubGraph) = True
        matchSubOption (PlotShowTitle) (PlotShowTitle) = True
        matchSubOption (PlotOnlyParts {}) (PlotOnlyParts {}) = True
        matchSubOption (PlotOLength {}) (PlotOLength {}) = True
        matchSubOption (PlotRatio {}) (PlotRatio {}) = True
        matchSubOption (PlotSize {}) (PlotSize {}) = True
        matchSubOption (PlotPaperSize {}) (PlotPaperSize {}) = True
        matchSubOption (PlotLanguage {}) (PlotLanguage {}) = True
        matchSubOption (PlotUniformLatch) (PlotUniformLatch) = True
        matchSubOption (PlotPartial {}) (PlotPartial {}) = True
        matchSubOption (PlotShowUnconnected {}) (PlotShowUnconnected {}) = True
        matchSubOption _ _ = False

    showPlotOptValue :: PlotOption -> String
    showPlotOptValue (PlotTokenColour colour) = colour
    showPlotOptValue (PlotCompColour colour) = colour
    showPlotOptValue (PlotOnlyParts parts) = joinWith "," parts
    showPlotOptValue (PlotOLength len) = show len
    showPlotOptValue (PlotRatio ratio) = show ratio
    showPlotOptValue (PlotSize (width, height)) = show (truncate width :: Integer) ++ "x"
        ++ show (truncate height :: Integer)
    showPlotOptValue (PlotPaperSize (width, height)) = show (truncate width :: Integer) ++ "x"
        ++ show (truncate height :: Integer)
    showPlotOptValue (PlotLanguage lang) = show lang
    showPlotOptValue (PlotPartial parts) = joinWith "," $ map (flip showPartial "") parts
    showPlotOptValue _ = ""

    showPartial :: PlotPartialElem -> ShowS
    showPartial (label, depth) = showString (joinWith "." label) .
        (if depth > 1 then showChar '+' . shows depth else id)

    parsePartials :: String -> Maybe [PlotPartialElem]
    parsePartials partStr = mapM parsePartial $ splitWith "," partStr
        where
            parsePartial str = case splitWith "+" str of
                [label] -> return (splitWith "." label, 1)
                [label, depthStr] -> do
                    depth <- parseInt depthStr
                    return (splitWith "." label, depth)
                _ -> fail ""

    instance Show PlotLanguage where
        showsPrec _ PlotPS = showString "ps"
        showsPrec _ PlotSVG = showString "svg"
        showsPrec _ PlotPDF = showString "pdf"

    instance Read PlotLanguage where
        readsPrec _ str = maybeToList $ do
            (token, rest) <- maybeLex str
            case token of
                "ps" -> return (PlotPS, rest)
                "svg" -> return (PlotSVG, rest)
                "pdf" -> return (PlotPDF, rest)
                _ -> fail ""

    splitWxH :: String -> Maybe DPoint
    splitWxH arg = case map (reads :: String -> [(Double, String)]) (splitWith "x" arg) of
        [[(width, "")], [(height, "")]] -> Just (width, height)
        _ -> Nothing

    parseDouble :: String -> Maybe Double
    parseDouble arg = case (reads :: String -> [(Double, String)]) arg of
        [(double, "")] -> Just double
        _ -> Nothing

    parseInt :: String -> Maybe Int
    parseInt arg = case (reads :: String -> [(Int, String)]) arg of
        [(int, "")] -> Just int
        _ -> Nothing

    plotOptionUsage :: SubOptionUsages PlotOption
    plotOptionUsage = SubOptionUsages "plot" showPlotOptValue Nothing Nothing [
        ("break-v", boolSubOption "break Vs into read/write portions" PlotBreakVariables),
        ("show-pos", boolSubOption "show component positions" PlotShowPoss),
        ("title", boolSubOption "show parts titles" PlotShowTitle),
        ("box", boolSubOption "show parts with outline boxes" PlotPartSubGraph),
        ("uniform-l", boolSubOption "show Ls all the same size" PlotUniformLatch),
        ("show-uc", boolSubOption "show unconnected end points on links" PlotShowUnconnected),
        ("token-colour", SubOptionUsage False "colour" "zero-width link colour" "black"
            (const True) (\arg -> ([PlotTokenColour arg], []))),
        ("comp-colour", SubOptionUsage False "colour" "component background colour" "black"
            (const True) (\arg -> ([PlotCompColour arg], []))),
        ("horiz-parts", boolSubOption "connect parts horizontally" PlotHorizParts),
        -- part sub graph
        -- show title
        ("parts", SubOptionUsage False "parts" "plot only named parts (no- prints all parts)" "" (const True)
            (\parts -> ([PlotOnlyParts (splitWith "," parts)], []))),
        ("paper", SubOptionUsage False "WxH" "plot paper size (in mm)" "1x1" (isJust . splitWxH) (\arg -> let
            Just (width, height) = splitWxH arg in ([PlotPaperSize (width, height)], []))),
        ("size", SubOptionUsage False "WxH" "plot size (in mm)" "1x1" (isJust . splitWxH) (\arg -> let
            Just (width, height) = splitWxH arg in ([PlotSize (width, height)], []))),
        ("ratio", SubOptionUsage False "ratio" "plot aspect ratio (>1 means portrait)" "1"
            (isJust . parseDouble) (\arg -> let Just ratio = parseDouble arg in ([PlotRatio ratio], []))),
        ("o-len", SubOptionUsage False "o-len" "longest O description string length" "100"
            (isJust . parseInt) (\arg -> let Just len = parseInt arg in ([PlotOLength len], []))),
        ("lang", SubOptionUsage False "lang" "output language {ps,pdf,svg}" "ps"
            (isJust . listToMaybe . readsLang) (\arg -> let [(lang, _)] = readsLang arg in ([PlotLanguage lang], []))),
        ("labels", SubOptionUsage False "parts" "plot labelled components (comma sep. list): <label>('+'<depth>)*"
            "" (isJust . parsePartials) (\arg -> let Just parts = parsePartials arg
                in ([PlotPartial parts], [])))
        ]
        where
            readsLang :: String -> [(PlotLanguage, String)]
            readsLang = reads

    defaultPlotOptions :: [PlotOption]
    defaultPlotOptions = [PlotPartSubGraph, PlotShowTitle, PlotSize (180, 260), PlotRatio 2,
        PlotCompColour "white", PlotTokenColour "black", PlotLanguage PlotPS, PlotShowUnconnected]

    showCompRef :: NetworkCompRef -> String
    showCompRef (Comp compNo) = "comp" ++ show compNo

    plotPortNames :: NetworkComp -> [String]
    plotPortNames (TeakComp { nwTeakType = TeakV {} }) = ["w", "w", "r", "r"]
    plotPortNames comp = nwCompPortNames comp

    compUsePortNames :: NetworkComp -> Int -> Bool
    compUsePortNames (TeakComp { nwTeakType = TeakO {} }) _ = False
    compUsePortNames (TeakComp { nwTeakType = TeakI {} }) _ = False
    compUsePortNames (TeakComp { nwTeakType = TeakX {} }) 1 = False
    compUsePortNames _ _ = True

    -- portEsc : escape a port name to be a valid dot record elem name
    --    '_' -> "__", '[' -> "_o", ']' -> "_c"
    portEsc :: String -> String
    portEsc port = concatMap escPortChar port
        where
            escPortChar '_' = "__"
            escPortChar '[' = "_o"
            escPortChar ']' = "_c"
            escPortChar c = [c]

    escapeLabel :: String -> String
    escapeLabel = escapeString "|{}<>"

    linkConnEndpoint :: NetworkIF network => [PlotOption] -> [NetworkPort] -> Sense -> NetworkLinkRef ->
        NetworkMonad network ([DotNode], String)
    linkConnEndpoint opts ports sense ref = do
        maybeLinkName <- nwGetLinkName ref
        conn <- nwGetLinkUsage sense ref
        let
            unconnected = return ([DotNode name $ [
                ("ucref", linkNo),
                ("label", label),
                ("shape", "triangle"),
                ("fontsize", "8"), ("fontname", "Helvetica"),
                ("height", "0.5"),
                ("width", "0.2"),
                -- ("labeljust", "c"), ("labelloc", "c"),
                ("style", "bold")
                ] ++ compNodeProps opts], name {- ++ nsPort sense -})
                where
                    label = escapeLabel $ fromMaybe linkNo maybeLinkName
                    linkNo = show (nwLink ref)
                    name = "uc" ++ linkNo

            body Nothing = unconnected
            body (Just (LinkAccess accessRef _)) = do
                let portIndex = nwFindPortIndexByRef ports accessRef
                if isJust portIndex
                    then return ([], showPortNo (fromJust portIndex))
                    else unconnected
                -- where matchPort (i, port) = nwPortRef port == Just access
            body (Just (LinkComp compNo (portNo:sectNo))) = do
                Just comp <- nwGetComp compNo
                let
                    portNames = plotPortNames comp
                    portSenses = nwCompPortSenses comp
                    subPort = if null sectNo then "" else show (head sectNo)
                    portDirection
                        | isMux comp && portNo == 1 = ":w"
                        | findBoolSubOption opts PlotHorizParts && isInstanceComp comp = wePort $ portSenses !! portNo
                        | otherwise = nsPort $ portSenses !! portNo
                    portName = portEsc (portNames !! portNo ++ subPort)
                    usedPortName = if compUsePortNames comp portNo
                        then ":" ++ portName
                        else ""
                    subComp = if isTeakV comp && findBoolSubOption opts PlotBreakVariables
                        then portName
                        else ""
                    connStr = showCompRef compNo ++ subComp ++ usedPortName ++ portDirection

                return ([], connStr)
            body _ = error "linkConnEndpoint: can't happen"

        (newNodes, nodeName) <- body conn
        return (newNodes, nodeName)
        where
            nsPort Active = ":s"
            nsPort Passive = ":n"

            wePort Active = ":e"
            wePort Passive = ":w"

    showPortNo :: Int -> String
    showPortNo portNo = "port" ++ show portNo

    compLine :: [PlotOption] -> (Pos -> String) -> NetworkComp -> ([DotNode], [DotGraph])
    compLine opts showPos comp
        | optBreakVars && isTeakV comp = let
            TeakV name _ _ ws rs = nwTeakType comp
            writeCount = length ws
            readCount = length rs
            makeSuffix str i = str ++ show i
            makeVarPart side i = showCompLine (brokenVar name side i) (makeSuffix side i)

            (rNodess, rSubGraphss) = unzip $ map (makeVarPart "r") [0..readCount-1]
            (wNodess, wSubGraphss) = unzip $ map (makeVarPart "w") [0..writeCount-1]
            in
                (concat rNodess ++ concat wNodess, concat rSubGraphss ++ concat wSubGraphss)
        | otherwise = showCompLine otherComps ""
        where
            optBreakVars = findBoolSubOption opts PlotBreakVariables
            optShowPos = findBoolSubOption opts PlotShowPoss
            maxOLength = do
                found <- findSubOption opts $ PlotOLength undefined
                let PlotOLength len = found
                return len

            showCompLine body suffix = if optShowPos
                then if pos == NoPos
                    then ([], [DotGraph ("cluster_pos_" ++ name) [("style", "invis")] [node] [] []])
                    else ([], [DotGraph ("cluster_pos_" ++ name) [
                        ("label", showPos pos ++ "\\n#" ++ show (nwCompIndex comp)),
                        ("fontsize", "8"), ("fontname", "Helvetica"),
                        ("labeljust", "r"), ("labelloc", "t")]
                        [node]
                        [] []])
                else ([node], [])
                where
                    pos = case comp of
                        TeakComp {} -> nwCompPos comp
                        _ -> NoPos
                    name = showCompRef (refComp comp) ++ suffix
                    node = DotNode name $ [("compref", show (nwCompIndex comp))] ++ body comp

            brokenVar name side i _ = [("label", label)] ++ params ++ [("shape", shape)]
                where
                    label = escapeLabel name ++ "\\n(" ++ show (nwCompIndex comp) ++ ")|"
                        ++ "<" ++ portEsc (side ++ show i) ++ ">" ++ side ++ show i
                    shape = "record"
                    params = [("compportion", side ++ show i)]

            otherComps comp@(TeakComp {}) = [("label", label)] ++ params ++
                [("shape", "record")]
                where
                    (label, params) = teakProps maxOLength comp opts
                    -- params = paramsF opts
            otherComps comp@(InstanceComp _ name _ _ _) = [("label", label)] ++
                compNodeProps opts ++ [("shape", "Mrecord")]
                where
                    label
                        | isHoriz = "{" ++ escapeLabel name ++ "|" ++ ports ++ "}"
                        | otherwise = escapeLabel name ++ "|" ++ ports
                    ports = compPortRecordElems True comp
                    isHoriz = findBoolSubOption opts PlotHorizParts

    compNodeProps :: [PlotOption] -> [DotNVPair]
    compNodeProps _ = [("nojustify", "true")]

    teakNodeProps :: [PlotOption] -> [DotNVPair]
    teakNodeProps opts = [("height", "0.4"), ("fontsize", "2")] ++ compNodeProps opts

    teakLProps :: [PlotOption] -> [DotNVPair]
    teakLProps _ = [("height", "0.02")]

    teakOProps :: [PlotOption] -> [DotNVPair]
    teakOProps opts = compNodeProps opts ++ maybe [] (\length -> [("maxlabelwidth", show length)]) maxOLength
        where
            maxOLength = do
                found <- findSubOption opts $ PlotOLength undefined
                let PlotOLength len = found
                return len

    compPortRecordElems :: Bool -> NetworkComp -> String
    compPortRecordElems showNames comp = "{" ++ joinWith "|" (pas ++ act) ++ "}"
        where
            links = nwCompLinks comp
            pas = portsWithSense Passive
            act = portsWithSense Active

            portsWithSense sense
                | null nameXindicess = []
                | otherwise = [ "{" ++ joinWith "|" (concatMap makeName nameXindicess) ++ "}" ]
                where
                    makeName (name, indices) = map perIndex indices
                        where perIndex i = "<" ++ portEsc (name ++ i) ++ ">" ++ if showNames
                                                                                then name ++ i
                                                                                else ""
                    (_, nameXindicess) = unzip $ filter ((== sense) . fst) nameSenseIndices

            nameSenseIndices
                -- remove the "s" port for X components.  FIXME, should add an attribute to nwCompPortNames
                | isMux comp = take 1 ret ++ drop 2 ret
                | otherwise = ret
                where ret = zip portSenses $ zip portNames linkIndicess

            portNames = plotPortNames comp
            portSenses = nwCompPortSenses comp
            linkIndicess = map linkIndices links

            linkIndices (One _) = [""]
            linkIndices (Many links) = map show [0..length links - 1]
            linkIndices _ = error "linkIndices: can't happen"

    teakProps :: Maybe Int -> NetworkComp -> [PlotOption] -> (String, [DotNVPair])
    teakProps maxOLength comp opts = (name, props)
        where
            (name, props) = body $ nwTeakType comp

            portRecordElems = compPortRecordElems False comp

            body TeakJ = (portRecordElems, teakNodeProps opts)
            body TeakM = (portRecordElems, teakNodeProps opts)
            body (TeakF {}) = (portRecordElems, teakNodeProps opts)
            body (TeakS {}) = (portRecordElems, teakNodeProps opts)
            body (TeakX {}) = (portRecordElems, teakNodeProps opts)
            body (TeakO terms) = (escapeLabel $ joinWith "\n" $ shortPrettyTerms, teakOProps opts)
                where
                    shorten len term
                        | listAtLeastLength len term = take (len - 4) term ++ " ..."
                        | otherwise = term

                    prettyTerms = prettyPrintOTerms 0 (\_ -> showString "in") terms
                    shortPrettyTerms
                        | isJust maxOLength = map (shorten (fromJust maxOLength)) prettyTerms
                        | otherwise = prettyTerms
            body (TeakV name _ _ ws rs) = (label, [])
                where
                    label = escapeLabel name ++ "|" ++
                        joinWith "|" (map (\i -> "<" ++ portEsc ("w" ++ show i) ++ ">w" ++ show i)
                            [0..length ws - 1]) ++ "|" ++
                        joinWith "|" (map (\i -> "<" ++ portEsc ("r" ++ show i) ++ ">r" ++ show i)
                            [0..length rs - 1])
            body TeakA = (portRecordElems, teakNodeProps opts)
            body TeakI = (portRecordElems, teakNodeProps opts ++ [("width", "0.4")])
            body TeakR = (portRecordElems, teakNodeProps opts ++ [("width", "0.4")])
            -- body comp = (Nothing, show comp ++ portRecordElems, Just "box", \opts -> teakNodeProps opts)

    latch :: [PlotOption] -> String -> Int -> DotNode
    latch opts name depth = DotNode name ([("label", label)] ++ props ++ [("shape", "record")])
        where
            (label, props)
                | findBoolSubOption opts PlotUniformLatch = ("{" ++ makeChev (1::Int) ++ "}", fontSize0)
                | depth == 0 = ("", teakLProps opts)
                | depth > 4 = (show depth, teakLProps opts ++ [("fontsize", "8")])
                | otherwise = ("{" ++ joinWith "|" (map makeChev [1..depth]) ++ "}", fontSize0)
                where
                    makeChev i = "<p" ++ show i ++ ">"
                    fontSize0 = teakLProps opts ++ [("fontsize", "0")]

    makeLinkWidthProps :: [PlotOption] -> Int -> [DotNVPair]
    makeLinkWidthProps _ width = [("penwidth",  show (intWidth width)), ("weight", show (intWidth width))]

    concatUnzip :: [([a], [b])] -> ([a], [b])
    concatUnzip abs = (concat as, concat bs)
        where (as, bs) = unzip abs

    plotPart :: NetworkIF network => Part network -> [PlotOption] -> IO DotGraph
    plotPart (Part name ports body) opts = do
        positionKeyNodes <- tryNm $ if findBoolSubOption opts PlotShowPoss
            then do
                maybePosArray <- nwGetPosArray
                if isJust maybePosArray
                    then makePositionKey $ fromJust maybePosArray
                    else return []
            else return []

        let portNodes = concatMap portLine $ zip ([0..] :: [Int]) ports

        (compNodes, compSubGraphs) <- tryNm $ liftM concatUnzip $
            nwMapComps (return . compLine opts (showPos noPosContext))

        (linkNodes, linkEdges) <- tryNm $ liftM concatUnzip $ nwMapLinks linkLine

        let
            nodes = portNodes ++ compNodes ++ linkNodes
            edges = linkEdges
            subGraphs = compSubGraphs

            titleNode = DotNode "title" [("label", name), ("fontsize", "20")]

            headNodes = generalNodes ++ positionKeyNodes ++ if findBoolSubOption opts PlotShowTitle
                then [titleNode]
                else []

            graph = if findBoolSubOption opts PlotPartSubGraph
                then DotGraph name props [] []
                    [DotGraph ("cluster_" ++ name) [] (headNodes ++ nodes) edges subGraphs]
                else DotGraph name props (headNodes ++ nodes) edges subGraphs
        -- hPutStrLn handle $ showDotGraph "digraph" 0 graph ""
        return graph
        where
            PlotSize (width, height) = getSubOption opts $ PlotSize (180, 260)
            maybeRatio = findSubOption opts $ PlotRatio 0

            props = [
                ("outputorder", "edgesfirst"),
                ("size", show (width / 25.4) ++ "," ++ show (height / 25.4)),
                ("ratio", maybe "2" (show . plotOptionRatio) maybeRatio) ]
            generalNodes = [
                DotNode "edge" [("fontsize", "8"), ("fontname", "Helvetica"),
                    ("labelfontname", "Helvetica"), ("labelfontsize", "8"),
                    ("arrowhead", "normal")],
                DotNode "node" [("fontsize", "12"), ("fontname", "Helvetica-Bold"), ("shape", "box")] ]

            tryNm mod = return $ tryNetwork body mod

            makePositionKey (PosArray posArray) = do
                poss <- nwMapComps (return . nwCompPos)
                maybePosArray <- nwGetPosArray
                let
                    allRefs = nub $ concatMap posGetRefs poss
                    usedPoss = map getAssoc allRefs

                    showPosPair :: (Int, Pos) -> String
                    showPosPair (i, pos) = "#" ++ show i ++ ": " ++ showPosForNetwork maybePosArray pos
                if null usedPoss
                    then return []
                    else return [DotNode "key" [("label", "Position prefices:\\n" ++
                        escapeLabel (joinWith "\\n" (map showPosPair usedPoss)))]]
                where getAssoc i = (i, posArray ! i)

            linkLine link = do
                let
                    ref = refLink link
                    width = nwLinkWidth link
                    depth = latchingDepth $ nwLinkLatching link
                do
                    (actUC, actPort) <- linkConnEndpoint opts ports Active ref
                    (pasUC, pasPort) <- linkConnEndpoint opts ports Passive ref
                    let
                        props = [("linkref", show (nwLink ref)), ("label", show (nwLink ref))] ++
                            makeLinkWidthProps opts width

                        latchName = "latch" ++ show (nwLink ref)
                        isUnconnected = not (null actUC) || not (null pasUC)

                    if isUnconnected && not (findBoolSubOption opts PlotShowUnconnected)
                        then return ([], [])
                        else if depth /= 0
                            then return ([latch opts latchName depth] ++ actUC ++ pasUC,
                                [DotEdge actPort (latchName ++ ":n") ([("linkend", "before")] ++ props),
                                 DotEdge (latchName ++ ":s") pasPort ([("linkend", "after")] ++ props)
                                ])
                            else return (actUC ++ pasUC, [DotEdge actPort pasPort props])

            portLine (i, NetworkPort name _ _ (Just _)) = [DotNode (showPortNo i) [
                ("portref", show i), ("label", escapeLabel name), ("style", "bold"), ("shape", "none")]]
            portLine _ = []
