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

module Layout (
    PortNode (..),
    CompNode (..),
    LinkEnd (..),
    LinkEdge (..),
    UCNode (..),
    PartGraphical (..),
    OGraphical (..),
    GuiGraphical,
    LayoutZoom (..),
    LayoutZoomMove (..),
    GuiMiscRender,
    compNodeBoundingBox,
    diffParts,
    partLayoutSetHighlightComps,
    pixelsPerPoint,
    GuiLayout (..),
    plotParts,
    initLayout,
    setLayoutPart,
    setLayoutMoving,
    setLayoutGraphical,
    setLayoutButtonsSensitive,
    setLayoutBoundingBox,
    setLayoutSizeRequest,
    setLayoutViewScale,
    setLayoutViewOrigin,
    getLayoutProportionalOrigin,
    setLayoutProportionalOrigin,
    setLayoutPreRender,
    setLayoutPostRender,
    clearLayout,
    fitLayout,
    exposeLayout,
    a4Paper,
    renderToPS,
    renderToPDF,
    renderToSVG,
    renderPartForPaper,
    partGraphicalInfo,
    partGraphicalSpringInfo,
    partDrawLink,
    oGraphicalInfo,
    makeLayoutTable,
    trimPartForPartialPlot,
    noGuiMiscRender
    )
    where

    import GuiSupport
    import Data.Maybe

    import Control.Monad
    import Data.List
    import qualified Data.IntMap as IM
    import Data.IORef
    import Data.Char
    import Data.Array

    import Misc
    import Dot
    import NetParts
    import Plot
    import ParseTree
    import Bits
    import Options
    import State
    import Report

    -- import Graphics.UI.Gtk hiding ({- fill, lineWidth, -} layoutHeight, layoutWidth)
    import qualified Graphics.UI.Gtk as Gtk
    import Graphics.Rendering.Cairo
    import Graphics.UI.Gtk.Gdk.Events

    data LinkEnd = LinkDirect | LinkBeforeLatch | LinkAfterLatch
        deriving (Show, Ord, Eq)

    doDebug :: Bool
    doDebug = False

    debugPrint :: String -> IO ()
    debugPrint = when (doDebug) . putStrLn

    strToDouble :: String -> Double
    strToDouble = read

    strToInt :: String -> Int
    strToInt = read

    data CompNode = CompNode {
        compNodePortion :: Maybe (Char, Int),
        compNodeOrigin :: DPoint,
        compNodeWidth :: Double,
        compNodeHeight :: Double,
        compNodeLabelLength :: Int,
        compNodeVelocity :: DPoint }
        deriving Show

    defaultCompNode :: CompNode
    defaultCompNode = CompNode {
        compNodePortion = Nothing,
        compNodeOrigin = (0, 0),
        compNodeWidth = 0,
        compNodeHeight = 0,
        compNodeLabelLength = 0,
        compNodeVelocity = atRest }

    atRest :: DPoint
    atRest = (0, 0)

    data PortNode = PortNode { portNodeOrigin :: DPoint }
        deriving Show

    data UCNode = UCNode { ucNodeLink :: NetworkLinkRef, ucNodeOrigin :: DPoint }
        deriving Show

    data LinkEdge = LinkEdge { linkEdgePoints :: [(LinkEnd, [DPoint])], linkEdgeLatchPoints :: Maybe (DPoint, DPoint) }
        deriving Show

    translateDotCoord :: Double -> DPoint -> DPoint
    translateDotCoord graphBottom (x, y) = (x, graphBottom - y)

    isCompNode :: (DPoint -> DPoint) -> DotNode -> Maybe (Int, CompNode)
    isCompNode translateCoord (DotNode _ nvs)
        | isJust compRef = Just (strToInt $ fromJust compRef, defaultCompNode {
            compNodePortion = compPortion,
            compNodeOrigin = origin,
            compNodeWidth = width,
            compNodeHeight = height,
            compNodeLabelLength = maybe 0 strToInt labelLength })
        | otherwise = Nothing
        where
            compRef = lookup "compref" nvs
            compPortion = do
                portion <- lookup "compportion" nvs
                case portion of
                    port:index | port `elem` "rw" -> do
                        (i, _) <- listToMaybe $ reads index
                        return (port, i)
                    _ -> fail ""
            Just pos = lookup "pos" nvs
            labelLength = lookup "maxlabelwidth" nvs
            origin = parsePos translateCoord pos
            width = 72 * (strToDouble $ fromJust $ lookup "width" nvs)
            height = 72 * (strToDouble $ fromJust $ lookup "height" nvs)

    parsePos :: (DPoint -> DPoint) -> String -> DPoint
    parsePos scalePointL pos = scalePointL $ listToPair $ splitWith "," pos
        where
            listToPair [_, x, y] = listToPair [x, y]
            listToPair [x, y] = (strToDouble x, strToDouble y)
            listToPair l = error $ "listToPair: ? " ++ show l

    isPortNode :: (DPoint -> DPoint) -> DotNode -> Maybe (Int, PortNode)
    isPortNode translateCoord (DotNode _ nvs)
        | isJust portRef = Just (strToInt $ fromJust portRef, PortNode origin)
        | otherwise = Nothing
        where
            portRef = lookup "portref" nvs
            Just pos = lookup "pos" nvs
            origin = parsePos translateCoord pos

    isUCNode :: (DPoint -> DPoint) -> DotNode -> Maybe UCNode
    isUCNode translateCoord (DotNode _ nvs)
        | isJust ucRef = Just $ UCNode (Link $ strToInt $ fromJust ucRef) origin
        | otherwise = Nothing
        where
            ucRef = lookup "ucref" nvs
            Just pos = lookup "pos" nvs
            origin = parsePos translateCoord pos

    isLinkEdge :: (DPoint -> DPoint) -> DotEdge -> Maybe (Int, (LinkEnd, [DPoint]))
    isLinkEdge translateCoord (DotEdge _ _ nvs)
        | isJust linkRef = Just (strToInt $ fromJust linkRef, (linkEnd, points))
        | otherwise = Nothing
        where
            linkEnd = fromMaybe LinkDirect $ do
                end <- lookup "linkend" nvs
                case end of
                    "direct" -> return LinkDirect
                    "before" -> return LinkBeforeLatch
                    "after" -> return LinkAfterLatch
                    _ -> fail ""

            linkRef = lookup "linkref" nvs
            Just pos = lookup "pos" nvs
            points = edgeParsePos translateCoord pos

    edgeParsePos :: (DPoint -> DPoint) -> String -> [DPoint]
    edgeParsePos scalePointL pos = points
        where
            pos' = splitWith " " pos

            pos'Points = map (parsePos scalePointL) pos'
            points
                | isPrefixOf "e" firstPos = init (tail pos'Points) ++ [head pos'Points]
                --  | isPrefixOf "s" firstPos = makePoint firstPos
                | otherwise = pos'Points
                where firstPos = head pos'

    data PartGraphical = PartGraphical {
        partGraphBB :: BoundingBox,
        partPortNodes :: IM.IntMap PortNode,
        partCompNodes :: IM.IntMap [CompNode],
        partLinkEdges :: IM.IntMap LinkEdge,
        partUcNodes :: [UCNode],
        partHighlightComps :: [NetworkCompRef],
        partShowLinkColours :: Bool
        }
        deriving Show

    partLayoutSetHighlightComps :: NetworkIF network => IORef (GuiLayout network PartGraphical) ->
        [NetworkCompRef] -> IO ()
    partLayoutSetHighlightComps layoutRef comps = do
        modifyIORef layoutRef $ \layout -> fromMaybe layout $ do
            graphical <- layoutGraphical layout
            return $ layout { layoutGraphical = Just (graphical { partHighlightComps = comps }) }

    data OGraphical = OGraphical {
        oGraphBB :: BoundingBox,
        oInput :: Maybe DPoint,
        oOutput :: Maybe DPoint,
        oTermNodes :: [(String, CompNode, SimpleShapeRender)],
        oResultEdges :: [([DPoint], Int, String)] }

    class GuiGraphical graphical where
        graphicalBB :: graphical -> BoundingBox
        graphicalStep :: NetworkIF network => Part network -> graphical -> graphical
        graphicalStep = const id
        graphicalDraw :: NetworkIF network => graphical -> Part network -> Double -> Render ()

    instance GuiGraphical PartGraphical where
        graphicalBB = partGraphBB
        graphicalStep = springStep
        graphicalDraw = drawPart

    instance GuiGraphical OGraphical where
        graphicalBB = oGraphBB
        graphicalDraw graphical _ = drawO graphical

    rm :: String -> IO ()
    rm file = do
        mySystem "rm" ["-f", file]
        return ()

    oGraphicalInfo :: Int -> [(Int, TeakOTerm)] -> IO OGraphical
    oGraphicalInfo inputWidth terms = do
        graph <- graphPassThroughDot rm
            "op" $ DotGraph "op" [("size", "10,10") {- ("ratio", "0.5") -} ] nodes edges []
        let
            graphBB = dotGraphBoundingBox graph
            translateCoord = translateDotCoord (boundingBoxBottom graphBB)

            termNodes = mapMaybe (isTermNode translateCoord) $ dotGraphNodes graph
            resultEdges = map (isResultEdge translateCoord) $ dotGraphEdges graph
            input = mapMaybe (isIONode translateCoord "input") $ dotGraphNodes graph
            output = mapMaybe (isIONode translateCoord "output") $ dotGraphNodes graph

        return $ OGraphical graphBB (listToMaybe input) (listToMaybe output) termNodes resultEdges
        where
            termName i = "T" ++ show i

            fromPortName 0 = "input"
            fromPortName i = termName i ++ ":output:s"

            toPortName i sliceNo = termName i ++ ":slice" ++ show sliceNo ++ ":n"

            escapeLabel = escapeString "|"

            consTermNode :: Int -> String -> String -> [Int] -> DotNode
            consTermNode i label oshape sliceIndices = DotNode (termName i)
                $ [("shape", "record"), ("termlabel", escapeLabel label),
                    ("oshape", oshape), ("height", "0.4")] ++
                    if null sliceIndices
                        then [("width", "0.4"), ("label", "{<output>" ++ escapeLabel label ++ "}")]
                        else [("fontsize", "2"), ("label", "{{" ++ joinWith "|"
                            (map (\sliceI -> "<slice" ++ show sliceI ++ ">") sliceIndices) ++ "}|<output>}")]

            makeTermNode (i, TeakOConstant _ num) = consTermNode i (show num) "const" []
            makeTermNode (i, TeakOAppend count slices) = consTermNode i
                (if count == 1 then " " else "x" ++ show count) "append" (reverse (makeSliceIndices slices))
            makeTermNode (i, TeakOBuiltin name _ _ slices) = consTermNode i name "" (makeSliceIndices slices)
            makeTermNode (i, TeakOp op slices) = consTermNode i
                (snd (teakOOpNames op)) oshape (makeSliceIndices slices)
                where
                    oshape = case length slices of
                        2 -> "binop"
                        1 -> "unop"
                        _ -> "box"
            makeTermNode (i, TeakOMux spec (_:slices)) = consTermNode i
                (joinWith ";" (map (joinWith "," . map (showImp True True 4)) spec)) "mux" [1..length slices]
            makeTermNode (_, TeakOMux _ []) = error "makeTermNode: can't happen"

            makeSliceIndices slices = [0..length slices - 1]

            makeSliceEdges (to, term) = case term of
                TeakOMux _ (oSlice@(from, slice):slices) -> (DotEdge (fromPortName from)
                    (termName to ++ ":w") [("width", show (sliceWidth slice)), ("label", sliceLabel oSlice)])
                    : zipWith makeSliceEdge ([1..] :: [Int]) slices
                _ -> zipWith makeSliceEdge ([0..] :: [Int]) $ oTermExtractSlices term
                where
                    makeSliceEdge sliceNo oSlice@(from, slice) =
                        DotEdge (fromPortName from) (toPortName to sliceNo) [("width", show (sliceWidth slice)),
                            ("label", sliceLabel oSlice)]

                    sliceLabel (0, slice)
                        | sliceOffset slice == 0 && sliceWidth slice == inputWidth = ""
                    sliceLabel (from, slice) = fromMaybe (verilogShowSlice slice "") $ do
                        term <- lookup from terms
                        if sliceOffset slice == 0 && oTermResultWidth term == sliceWidth slice
                            then return ""
                            else fail ""

            nodes = map makeTermNode terms
            outputTerm = last terms
            edges = (DotEdge (fromPortName (fst outputTerm)) "output"
                [("width", show $ oTermResultWidth (snd outputTerm))]) : concatMap makeSliceEdges terms

            isTermNode translateCoord (DotNode ('T':_) nvs)
                | isJust termLabel = Just (fromJust termLabel, defaultCompNode {
                    compNodeOrigin = origin,
                    compNodeWidth = width,
                    compNodeHeight = height }, oshape)
                | otherwise = Nothing
                where
                    termLabel = lookup "termlabel" nvs
                    Just pos = lookup "pos" nvs
                    oshape = case lookup "oshape" nvs of
                        Just "binop" -> nickedTrapezium
                        Just "unop" -> trapezium
                        Just "append" -> trapezium
                        Just "mux" -> trapezium
                        Just "const" -> curveBoxProp [TL,TR,BL,BR] (1 / 3)
                        _ -> box
                    [x, y] = map strToDouble $ splitWith "," pos
                    origin = translateCoord (x, y)
                    width = 72 * (strToDouble $ fromJust $ lookup "width" nvs)
                    height = 72 * (strToDouble $ fromJust $ lookup "height" nvs)
            isTermNode _ _ = Nothing

            isIONode translateCoord name (DotNode nodeName nvs)
                | name == nodeName = Just origin
                | otherwise = Nothing
                where
                    Just pos = lookup "pos" nvs
                    [x, y] = map strToDouble $ splitWith "," pos
                    origin = translateCoord (x, y)

            isResultEdge translateCoord (DotEdge _ _ nvs) = (points, strToInt width, label)
                where
                    Just pos = lookup "pos" nvs
                    Just width = lookup "width" nvs
                    label = fromMaybe "" $ lookup "label" nvs
                    points = edgeParsePos translateCoord pos

    partGraphicalInfo :: NetworkIF network => Bool -> [PlotOption] -> String -> Part network -> IO PartGraphical
    partGraphicalInfo keepDotFile options tempBaseFilename part = do
        graph <- do
            g1 <- plotPart part $ [PlotHorizParts, PlotPartSubGraph, PlotUniformLatch] ++ options
            g2 <- graphPassThroughDot removeFiles tempBaseFilename g1
            return $ flattenDotGraph g2
        let
            graphBB = dotGraphBoundingBox graph
            translateCoord = translateDotCoord (boundingBoxBottom graphBB)

            compNodes = IM.fromList $ map resolveComp $ sortAndGroupByElem fst $
                mapMaybe (isCompNode translateCoord) $ dotGraphNodes graph
            linkEdges = IM.fromList $ mapMaybe resolveEdge $ sortAndGroupByElem fst $
                mapMaybe (isLinkEdge translateCoord) $ dotGraphEdges graph
            portNodes = IM.fromList $ mapMaybe (isPortNode translateCoord) $ dotGraphNodes graph
            ucNodes = mapMaybe (isUCNode translateCoord) $ dotGraphNodes graph

            resolveComp (compNo, comps) = (compNo, map snd comps)

            resolveEdge (linkNo, edges) = resolve edges
                where
                    resolve [(_, (LinkDirect, edge))] = Just (linkNo, LinkEdge [(LinkDirect, edge)] Nothing)
                    resolve [(_, (LinkBeforeLatch, edge1)), (_, (LinkAfterLatch, edge2))] =
                        Just (linkNo, LinkEdge [(LinkBeforeLatch, edge1), (LinkAfterLatch, edge2)] latchPoints)
                        where
                            -- Join the points with a straight line
                            latchPoints = Just (last edge1, head edge2)

                            -- combinedPoints = points1 ++ straightLine ++ points2tail
                            -- straightLine = [points2head, points2head, points2head]
                    resolve _ = Nothing

        return $ PartGraphical graphBB portNodes compNodes linkEdges ucNodes [] True
        where
            removeFiles file
                | keepDotFile = return ()
                | otherwise = rm file

    springRedoLinks :: NetworkIF network => Part network -> PartGraphical -> PartGraphical
    springRedoLinks part graphical = graphical { partLinkEdges = links (partCompNodes graphical) }
        where
            links compNodes = IM.fromList $ catMaybes $ tryPart part $ nwMapLinks $ \link -> runMaybeT $ do
                (pas, _) <- MaybeT $ nwLinkToComp Passive $ refLink link
                (act, _) <- MaybeT $ nwLinkToComp Active $ refLink link
                return (nwLinkIndex link, LinkEdge [(LinkDirect, [compOrigin act, compOrigin pas])] Nothing)
                where compOrigin comp = compNodeOrigin $ head $ compNodes IM.! (nwCompIndex comp)

    springStep :: NetworkIF network => Part network -> PartGraphical -> PartGraphical
    springStep part graphical = springMakeBB $ springRedoLinks part $ graphical { partCompNodes = compNodes' }
        where
            compNodes' = step $ partCompNodes graphical

            compConnections = IM.fromList $ tryPart part $ nwMapComps $ \comp -> do
                let
                    compRef = refComp comp
                    links = concatMap flattenSome $ nwCompLinks comp

                    makeConnection link = do
                        pas <- nwLinkToComp Passive link
                        act <- nwLinkToComp Active link
                        width <- nwGetLinkWidth link
                        let logWidth = min (intWidth width) 9
                        return $ case (pas, act) of
                            (Just (pasComp, _), _) | refComp pasComp /= compRef ->
                                Just (nwCompIndex pasComp, logWidth)
                            (_, Just (actComp, _)) | refComp actComp /= compRef ->
                                Just (nwCompIndex actComp, logWidth)
                            _ -> Nothing

                -- connectedComps <- nwConnectedComps 2 compRef
                -- return (nwCompIndex comp, map nwComp $ connectedComps \\ [compRef])
                connections <- liftM catMaybes $ mapM makeConnection links
                return (nwCompIndex comp, connections)

            step compNodes = IM.mapWithKey stepNode compNodes
                where
                    stepNode i [node] = [node { compNodeOrigin = origin', compNodeVelocity = velocity' }]
                        where
                            connections = compConnections IM.! i
                            origin = compNodeOrigin node
                            velocity = compNodeVelocity node
                            (origin', velocity')
                                | null connections = (origin, velocity)
                                | otherwise = (origin', velocity')
                                where
                                    acc = totalForce /~ (mass * fromIntegral (length connections))
                                    velocity' = (velocity +~ acc *~ tStep) --  *~ drag
                                    origin' = origin +~ velocity' *~ tStep

                                    totalForce = foldl' forceToPoint (0,0) connections +~
                                        foldl' repelPoint (0,0) (IM.keys compNodes)

                                    forceToPoint acc (compNo, width) = acc +~
                                        force (fromIntegral width) origin to
                                        where to = compNodeOrigin $ head $ compNodes IM.! compNo

                                    repelPoint acc compNo
                                        | compNo == i = acc
                                        | otherwise = acc +~ repel 0 origin to
                                        where to = compNodeOrigin $ head $ compNodes IM.! compNo
                    stepNode _ _ = error "springStep: must be exactly one node per component"

                    hooke = 1
                    charge = 1000
                    mass = 0.5
                    tStep = 0.01
                    -- drag = 0.9

                    repel :: Double -> DPoint -> DPoint -> DPoint
                    -- repel _hooke2 from to = unitRepel *~ (charge * (1 / (min 0.01 (sep * sep))))
                    repel _hooke2 from to = unitRepel *~ (charge * (1 / (max 0.01 (sep * sep))))
                        where
                            attractUnit = attract /~ sep
                            attract = to -~ from
                            unitRepel = attractUnit *~ (- 1)
                            sep = distDPoint to from

                    force :: Double -> DPoint -> DPoint -> DPoint
                    force _hooke2 from to
                        | otherwise = attract *~ (hooke * 1 {- hooke2 -})
                        where attract = to -~ from

    defaultBB :: BoundingBox
    defaultBB = BoundingBox {
        boundingBoxTop = 0,
        boundingBoxBottom = 72,
        boundingBoxLeft = 0,
        boundingBoxRight = 72 }

    springMakeBB :: PartGraphical -> PartGraphical
    springMakeBB graphical
        | null compYs = graphical { partGraphBB = defaultBB }
        | otherwise = graphical { partGraphBB = bb }
        where
            padding = 2 * 72
            bb = BoundingBox {
                boundingBoxTop = minimum compYs - padding,
                boundingBoxBottom = maximum compYs + padding,
                boundingBoxLeft = minimum compXs - padding,
                boundingBoxRight = maximum compXs + padding }
            (compXs, compYs) = unzip $ map (compNodeOrigin . head) $ IM.elems $ partCompNodes graphical

    partGraphicalSpringInfo :: NetworkIF network => Part network -> IO PartGraphical
    partGraphicalSpringInfo part = do
        let
            compIndices = tryPart part $ nwMapComps (return . nwCompIndex)
            compCount = length compIndices
            root :: Double
            root = sqrt $ fromIntegral compCount
            rowLength :: Int
            rowLength = max 1 $ floor root
            h = 2 * 72 :: Double
            w = 2 * 72 :: Double

            makeCompNode origin = [defaultCompNode {
                compNodeOrigin = origin,
                compNodeWidth = w / 2,
                compNodeHeight = h / 2,
                compNodeLabelLength = 144 }]

            compNodes0 = IM.fromList $ zip compIndices $ map makeCompNode
                [(fromIntegral column * w, row * h) | row <- [0..], column <- [0..rowLength - 1]]

            portNodes = IM.empty
            ucNodes = []

        return $ springMakeBB $ springRedoLinks part $
            PartGraphical undefined portNodes compNodes0 undefined ucNodes [] True

    eqComp :: NetworkComp -> NetworkComp -> Bool
    eqComp from@(InstanceComp {}) to@(InstanceComp {}) =
        nwPartName from == nwPartName to && nwCompPorts from == nwCompPorts to && nwCompLinks from == nwCompLinks to
    eqComp from@(TeakComp {}) to@(TeakComp {}) =
        nwTeakType from == nwTeakType to && nwCompLinks from == nwCompLinks to
    eqComp _ _ = False

    diffParts :: NetworkIF network => Int -> Part network -> Part network -> IO (PartGraphical, PartGraphical)
    diffParts depth fromPart toPart = do
        from <- partGraphicalInfo False plotOptions "from" fromDiffNl
        to <- partGraphicalInfo False plotOptions "to" toDiffNl
        return (from, to)
        where
            plotOptions = [PlotRatio 1, PlotSize (75, 75), PlotShowUnconnected]

            fromDiffNl = trimPart fromPart fromDiffComps
            toDiffNl = trimPart toPart toDiffComps

            fromDiffComps = tryPart fromPart $ do
                let fromDiffComps0 = nub $ removedComps ++ updatedComps
                liftM (nub . concat) $ mapM (nwConnectedComps depth) fromDiffComps0

            toDiffComps = tryPart toPart $ do
                let toDiffComps0 = nub $ newComps ++ updatedComps
                liftM (nub . concat) $ mapM (nwConnectedComps depth) toDiffComps0

            keptComps = fromComps `intersect` toComps
            removedComps = fromComps \\ toComps
            newComps = toComps \\ fromComps
            updatedComps = map (refComp . fst) $ filter (not . uncurry eqComp) $ zip fromKeptComps toKeptComps

            fromComps = tryPart fromPart $ nwMapComps $ return . refComp
            toComps = tryPart toPart $ nwMapComps $ return . refComp
            fromKeptComps = tryPart fromPart $ mapM (liftM fromJust . nwGetComp) keptComps
            toKeptComps = tryPart toPart $ mapM (liftM fromJust . nwGetComp) keptComps

    -- pixelsPerPoint : scaling from PartGraphical units to display units at scale 1.0
    pixelsPerPoint :: Double
    pixelsPerPoint = 1.0

    compNodeBoundingBox :: CompNode -> BoundingBox
    compNodeBoundingBox node = pointWHToBoundingBox (compNodeOrigin node) (compNodeWidth node) (compNodeHeight node)

    data {- (NetworkIF network, GuiGraphical graphical) => -} GuiLayout network graphical = GuiLayout {
        layoutPart :: Maybe (Part network),
        layoutGraphical :: Maybe graphical,
        layoutDrawingArea :: Gtk.DrawingArea,
        layoutZoom :: LayoutZoom,
        layoutMoving :: Maybe Gtk.HandlerId,
        layoutBB :: BoundingBox,
        layoutViewAlwaysScaled :: Bool,
        layoutViewScale :: Double, -- view units / `graphical' units
        layoutViewOrigin :: DPoint, -- view centre in graphical units
        layoutVAdj :: Gtk.Adjustment,
        layoutHAdj :: Gtk.Adjustment,
        layoutUpdateScrollbars :: IO (),
        layoutPreRender :: GuiMiscRender network graphical,
        layoutPostRender :: GuiMiscRender network graphical
        }

    data LayoutZoomMove = LayoutZoomMove {
        layoutZoomStep :: Gtk.ToolButton,
        layoutZoomPlay :: Gtk.ToolButton,
        layoutZoomStop :: Gtk.ToolButton }

    data LayoutZoom = LayoutZoom {
        layoutZoomLabel :: Maybe Gtk.ToolButton,
        layoutZoomIn :: Maybe Gtk.ToolButton,
        layoutZoomOut :: Maybe Gtk.ToolButton,
        layoutZoomFit :: Maybe Gtk.ToolButton,
        layoutZoomMove :: Maybe LayoutZoomMove }

    noLayoutZoom :: LayoutZoom
    noLayoutZoom = LayoutZoom Nothing Nothing Nothing Nothing Nothing

    type GuiMiscRender network graphical = GuiLayout network graphical -> IO (Double -> Render ())

    noGuiMiscRender :: (NetworkIF network, GuiGraphical graphical) => GuiMiscRender network graphical
    noGuiMiscRender _ = return (const (return ()))

    layoutHeight :: (NetworkIF network, GuiGraphical graphical) => GuiLayout network graphical -> Double
    layoutHeight = boundingBoxHeight . layoutBB

    layoutWidth :: (NetworkIF network, GuiGraphical graphical) => GuiLayout network graphical -> Double
    layoutWidth = boundingBoxWidth . layoutBB

    layoutCentre :: (NetworkIF network, GuiGraphical graphical) => GuiLayout network graphical -> DPoint
    layoutCentre = boundingBoxCentre . layoutBB

    layoutViewPointToGraphicalPoint :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> DPoint -> IO DPoint
    layoutViewPointToGraphicalPoint layoutRef viewPoint = do
        layout <- readIORef layoutRef
        (viewWidth, viewHeight) <- Gtk.widgetGetSize $ layoutDrawingArea layout
        let
            viewCentre = (fromIntegral viewWidth / 2, fromIntegral viewHeight / 2)
        return $ (viewPoint -~ viewCentre) /~ layoutViewScale layout
            +~ layoutViewOrigin layout

    getLayoutProportionalOrigin :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> IO DPoint
    getLayoutProportionalOrigin layoutRef = do
        layout <- readIORef layoutRef
        let
            width = layoutWidth layout
            height = layoutHeight layout
            viewOrigin = layoutViewOrigin layout
            centre = layoutCentre layout
        debugPrint $ "OO " ++ show viewOrigin ++ " " ++ show centre
        return $ (viewOrigin -~ centre) .~ (1 / width, 1 / height)

    setLayoutProportionalOrigin :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> DPoint -> IO DPoint
    setLayoutProportionalOrigin layoutRef propOrigin = do
        layout <- readIORef layoutRef
        let
            width = layoutWidth layout
            height = layoutHeight layout
            centre = layoutCentre layout
        let origin = centre +~ propOrigin .~ (width, height)
        setLayoutViewOrigin layoutRef origin
        return origin

    setLayoutPreRender :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> GuiMiscRender network graphical -> IO ()
    setLayoutPreRender layoutRef render = modifyIORef layoutRef $ \layout -> layout { layoutPreRender = render }

    setLayoutMoving :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Maybe Gtk.HandlerId -> IO ()
    setLayoutMoving layoutRef moving = modifyIORef layoutRef $ \layout -> layout { layoutMoving = moving }

    setLayoutPostRender :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> GuiMiscRender network graphical -> IO ()
    setLayoutPostRender layoutRef render = modifyIORef layoutRef $ \layout -> layout { layoutPostRender = render }

    clearLayout :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) -> IO ()
    clearLayout layoutRef = do
        layout <- readIORef layoutRef
        when (isJust (layoutGraphical layout)) $ do
            dw <- Gtk.widgetGetDrawWindow $ layoutDrawingArea layout
            -- Gtk.drawWindowClear dw
            Gtk.renderWithDrawable dw $ do
                (uncurry3 setSourceRGB) white
                paint
        debugPrint "Clear"
        return ()

    setLayoutPart :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Maybe (Part network) -> IO ()
    setLayoutPart layoutRef part =
        modifyIORef layoutRef $ \layout -> layout { layoutPart = part }

    setLayoutGraphical :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Maybe graphical -> IO ()
    setLayoutGraphical layoutRef graphical = do
        modifyIORef layoutRef $ \layout -> layout { layoutGraphical = graphical }
        when (isJust graphical) $ do
            let bb = graphicalBB $ fromJust graphical
            debugPrint $ "Layout BoundingBox " ++ show bb
            setLayoutBoundingBox layoutRef $ boundingBoxScale 1.05 bb

    setLayoutViewScale :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Double -> IO ()
    setLayoutViewScale layoutRef scale =
        modifyIORef layoutRef $ \layout -> layout { layoutViewScale = scale }

    setLayoutViewOrigin :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> DPoint -> IO ()
    setLayoutViewOrigin layoutRef origin =
        modifyIORef layoutRef $ \layout -> layout { layoutViewOrigin = origin }

    -- exposeLayout : schedule a layout redraw
    exposeLayout :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) -> IO ()
    exposeLayout layoutRef = do
        layout <- readIORef layoutRef
        Gtk.widgetQueueDraw $ layoutDrawingArea layout

    -- redrawLayout : actually do redrawing of a layout widget
    redrawLayout :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) -> IO ()
    redrawLayout layoutRef = do
        debugPrint "Expose"
        do
            layout <- readIORef layoutRef
            when (layoutViewAlwaysScaled layout) $ fitLayout layoutRef
            return ()
        layout <- readIORef layoutRef
        let
            part = layoutPart layout
            Just graphical = layoutGraphical layout
        if isNothing (layoutGraphical layout)
            then renderGraphicalInLayout layoutRef $ const $ return ()
            else when (isJust part) $ do
                let render = graphicalDraw graphical $ fromJust part
                pre <- layoutPreRender layout layout
                post <- layoutPostRender layout layout
                renderGraphicalInLayout layoutRef $ \minLineWidth -> do
                    save; newPath
                    pre minLineWidth
                    restore
                    save; newPath
                    render minLineWidth
                    restore
                    save; newPath
                    post minLineWidth
                    restore

    a4Paper :: DPoint
    a4Paper = (210, 297)

    mmToPointsScale :: Double
    mmToPointsScale = 72 / 25.4

    renderToPS :: DPoint -> FilePath -> [Render ()] -> IO ()
    renderToPS paperSize fileName renders = uncurry (withPSSurface fileName)
        (paperSize *~ mmToPointsScale) $ \surface -> renderWith surface $ do
            forM_ renders $ \render -> do
                render
                showPage

    renderToPDF :: DPoint -> FilePath -> [Render ()] -> IO ()
    renderToPDF paperSize fileName renders = uncurry (withPDFSurface fileName)
        (paperSize *~ mmToPointsScale) $ \surface -> renderWith surface $ do
            forM_ renders $ \render -> do
                render
                showPage

    renderToSVG :: DPoint -> FilePath -> Render () -> IO ()
    renderToSVG paperSize fileName render = uncurry (withSVGSurface fileName)
        (paperSize *~ mmToPointsScale) $ \surface -> renderWith surface $ do
            render
            showPage

    renderPartForPaper :: (NetworkIF network, GuiGraphical graphical) => DPoint -> [PlotOption] ->
        Maybe (Double -> Render ()) -> Maybe (Double -> Render ()) ->
        Part network -> graphical -> Render ()
    renderPartForPaper paperSize opts pre post part graphical = do
        let
            border = 10
            labelHeight = 5
            defaultLayoutBorder = border + 5

            drawableSize = fromMaybe (paperSize -~ (defaultLayoutBorder, defaultLayoutBorder) *~ 2) $ do
                PlotSize (width, height) <- findSubOption opts $ PlotSize undefined
                return (width, height)

            showTitle = findBoolSubOption opts PlotShowTitle

            borderBoxSize@(borderBoxWidth, borderBoxHeight) = paperSize -~ (border, border) *~ 2
            (width, height) = drawableSize *~ mmToPointsScale

            bb = graphicalBB graphical
            layoutScale = min
                (width / max minBoundingBoxDimension (boundingBoxWidth bb))
                (height / max minBoundingBoxDimension (boundingBoxHeight bb))
            minLineWidth = 0.25 / layoutScale
            render = graphicalDraw graphical part
        do
            save
            setSourceRGB 0 0 0
            -- Drawing in mm
            scale mmToPointsScale mmToPointsScale
            uncurry translate $ paperSize /~ 2
            setLineWidth 0.25
            when showTitle $ do
                -- moveTo (0 - (borderBoxWidth / 2)) (0 - ((borderBoxHeight / 2) - labelHeight))
                uncurry moveTo $ (0, 0) -~ (borderBoxSize /~ 2 -~ (0, labelHeight))
                relLineTo borderBoxWidth 0
                stroke
                setFontSize $ labelHeight * 0.8
                centredText (0, 0 - ((borderBoxHeight / 2) - (labelHeight / 2))) $ networkName part
                uncurry (box (0, 0)) borderBoxSize
                stroke
            -- Drawing in points/dot units
            scale (1 / mmToPointsScale) (1 / mmToPointsScale)
            -- Scale/translate to fit
            scale layoutScale layoutScale
            -- showBB uncurry (box (0, 0)) (boundingBoxWidth bb, boundingBoxHeight bb)
            uncurry translate $ (0, 0) -~ boundingBoxCentre bb
            stroke
            save; newPath
            when (isJust pre) $ fromJust pre minLineWidth
            restore
            save; newPath
            render minLineWidth
            restore
            save; newPath
            when (isJust post) $ fromJust post minLineWidth
            restore
            restore

    renderGraphicalInLayout :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) ->
        (Double -> Render a) -> IO ()
    renderGraphicalInLayout layoutRef render = do
        layout <- readIORef layoutRef
        let drawingArea = layoutDrawingArea layout
        (width, height) <- Gtk.widgetGetSize drawingArea
        let
            sc = layoutViewScale layout
            minLineWidth = 0.5 / sc
            widthD = fromIntegral width
            heightD = fromIntegral height
            viewCentre = (widthD / 2, heightD / 2)

        dw <- Gtk.widgetGetDrawWindow drawingArea
        -- Gtk.drawWindowClear dw
        Gtk.renderWithDrawable dw $ do
            save
            (uncurry3 setSourceRGB) white
            paint
            restore
            setSourceRGB 0 0 0
            (uncurry translate) viewCentre
            scale sc sc
            (uncurry translate) $ (0, 0) -~ layoutViewOrigin layout
            render minLineWidth
            return ()

    updateZoomLabel :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) -> IO ()
    updateZoomLabel layoutRef = do
        layout <- readIORef layoutRef
        when (isJust $ layoutZoomLabel $ layoutZoom layout) $ do
            let
                scale = layoutViewScale layout
                Just zoomLabel = layoutZoomLabel $ layoutZoom layout
            Gtk.toolButtonSetLabel zoomLabel $ Just $ " " ++ show ((truncate (scale * 100.0)) :: Int) ++ "%" ++ " "

    setLayoutButtonsSensitive :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Bool -> IO ()
    setLayoutButtonsSensitive layoutRef sensitive = do
        layout <- readIORef layoutRef
        mapM_ (\w -> Gtk.widgetSetSensitivity w sensitive) $ layoutZoomButtons $ layoutZoom layout

    setLayoutBoundingBox :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> BoundingBox -> IO ()
    setLayoutBoundingBox layoutRef bb = do
        modifyIORef layoutRef $ \layout -> layout { layoutBB = bb }
        layout <- readIORef layoutRef
        Gtk.adjustmentSetUpper (layoutVAdj layout) $ boundingBoxBottom bb
        Gtk.adjustmentSetLower (layoutVAdj layout) $ boundingBoxTop bb
        Gtk.adjustmentSetUpper (layoutHAdj layout) $ boundingBoxRight bb
        Gtk.adjustmentSetLower (layoutHAdj layout) $ boundingBoxLeft bb

    setLayoutSizeRequest :: (NetworkIF network, GuiGraphical graphical) =>
        IORef (GuiLayout network graphical) -> Int -> Int -> IO ()
    setLayoutSizeRequest layoutRef width height = do
        layout <- readIORef layoutRef
        Gtk.widgetSetSizeRequest (layoutDrawingArea layout) width height

    initLayout :: (NetworkIF network, GuiGraphical graphical) =>
        Gtk.DrawingArea -> Gtk.Scrollbar -> Gtk.Scrollbar ->
        Maybe graphical ->
        LayoutZoom -> Bool ->
        Maybe (IORef (GuiLayout network graphical) -> DPoint -> Event -> IO ()) ->
        IO (IORef (GuiLayout network graphical))
    initLayout drawingArea vScroll hScroll graphical zoom alwaysScaled buttonAction = do
        vsAdj <- Gtk.rangeGetAdjustment vScroll
        hsAdj <- Gtk.rangeGetAdjustment hScroll

        layoutRef <- newIORef $ GuiLayout
            Nothing graphical drawingArea zoom Nothing
            (BoundingBox 0 0 1 1) alwaysScaled 1 (0, 0) vsAdj hsAdj undefined
            noGuiMiscRender noGuiMiscRender
        readIORef layoutRef >>= (setLayoutViewOrigin layoutRef . layoutCentre)

        Gtk.widgetModifyBg drawingArea Gtk.StateNormal (Gtk.Color 65535 65535 65535)

        ignoreVScroll <- newIORef False
        ignoreHScroll <- newIORef False

        let
            updateScrollbars = do
                debugPrint "Updatescrollbars"
                (width, height) <- Gtk.widgetGetSize drawingArea
                -- exposeLayout layoutRef
                layout <- readIORef layoutRef
                let
                    pageV = min (layoutHeight layout) (fromIntegral height / layoutViewScale layout)
                    pageH = min (layoutWidth layout) (fromIntegral width / layoutViewScale layout)
                    (originX, originY) = layoutViewOrigin layout

                Gtk.adjustmentSetPageSize vsAdj pageV
                Gtk.adjustmentSetPageSize hsAdj pageH

                writeIORef ignoreVScroll True
                Gtk.adjustmentSetValue vsAdj $ max (boundingBoxTop (layoutBB layout)) (originY - (pageV / 2))
                writeIORef ignoreHScroll True
                Gtk.adjustmentSetValue hsAdj $ max (boundingBoxLeft (layoutBB layout)) (originX - (pageH / 2))

        modifyIORef layoutRef $ \li -> li { layoutUpdateScrollbars = updateScrollbars }

        let
            resizeDa _ = do
                updateScrollbars
                redrawLayout layoutRef
                return True

        Gtk.onExpose drawingArea resizeDa
        -- onConfigure drawingArea resizeDa

        Gtk.onValueChanged vsAdj $ do
            ignore <- readIORef ignoreVScroll
            if ignore
                then do
                    debugPrint "IgnoringV"
                    writeIORef ignoreVScroll False
                else do
                    layout <- readIORef layoutRef
                    val <- Gtk.adjustmentGetValue vsAdj
                    pageV <- Gtk.adjustmentGetPageSize vsAdj
                    let (originX, _) = layoutViewOrigin layout

                    modifyIORef layoutRef $ \li -> li { layoutViewOrigin = (originX, val + (pageV / 2)) }

                    exposeLayout layoutRef

        Gtk.onValueChanged hsAdj $ do
            ignore <- readIORef ignoreHScroll
            if ignore
                then do
                    debugPrint "IgnoringH"
                    writeIORef ignoreHScroll False
                else do
                    layout <- readIORef layoutRef
                    val <- Gtk.adjustmentGetValue hsAdj
                    pageH <- Gtk.adjustmentGetPageSize hsAdj
                    let (_, originY) = layoutViewOrigin layout

                    modifyIORef layoutRef $ \li -> li { layoutViewOrigin = (val + (pageH / 2), originY) }

                    exposeLayout layoutRef

        when (isJust $ layoutZoomIn zoom) $ do
            Gtk.onToolButtonClicked  (fromJust $ layoutZoomIn zoom) $ do
                modifyIORef layoutRef $ \li -> li { layoutViewScale = layoutViewScale li * 1.5 }
                updateScrollbars
                exposeLayout layoutRef
                updateZoomLabel layoutRef
            return ()

        when (isJust $ layoutZoomOut zoom) $ do
            Gtk.onToolButtonClicked (fromJust $ layoutZoomOut zoom) $ do
                modifyIORef layoutRef $ \li -> li { layoutViewScale = layoutViewScale li / 1.5 }
                updateScrollbars
                exposeLayout layoutRef
                updateZoomLabel layoutRef
            return ()

        when (isJust $ layoutZoomFit zoom) $ do
            Gtk.onToolButtonClicked (fromJust $ layoutZoomFit zoom) $ do
                fitLayout layoutRef
                exposeLayout layoutRef
            return ()

        let
            step = do
                layout <- readIORef layoutRef
                let
                    part = layoutPart layout
                    graphical = layoutGraphical layout
                -- FIXME, sensitive
                when (isJust part && isJust graphical) $ do
                  let graphical' = foldl' (\g _ -> graphicalStep (fromJust part) g) (fromJust graphical) [(1::Int)..1]
                  setLayoutGraphical layoutRef $ Just graphical'
                  layout' <- readIORef layoutRef
                  layoutUpdateScrollbars layout'
                  exposeLayout layoutRef

            stepIfMoving = do
                layout <- readIORef layoutRef
                let moving = isJust $ layoutMoving layout
                when moving step
                return moving

        when (isJust $ layoutZoomMove zoom) $ do
            let Just move = layoutZoomMove zoom

            Gtk.onToolButtonClicked (layoutZoomStep move) step

            Gtk.onToolButtonClicked (layoutZoomPlay move) $ do
                layout <- readIORef layoutRef
                when (isNothing (layoutMoving layout)) $ do
                    handler <- Gtk.timeoutAddFull stepIfMoving Gtk.priorityLow 50
                    setLayoutMoving layoutRef $ Just handler
                    return ()

            Gtk.onToolButtonClicked (layoutZoomStop move) $ do
                layout <- readIORef layoutRef
                let    handler = layoutMoving layout
                when (isJust handler) $ Gtk.timeoutRemove $ fromJust handler
                setLayoutMoving layoutRef Nothing
                return ()

            return ()

        when (isJust buttonAction) $ do
            Gtk.onButtonPress drawingArea $ \event -> do
                debugPrint "Pressed"
                let viewPoint = (eventX event, eventY event)
                modelPoint <- layoutViewPointToGraphicalPoint layoutRef viewPoint
                debugPrint $ "At " ++ show viewPoint ++ " => " ++ show modelPoint
                (fromJust buttonAction) layoutRef modelPoint event
                return True
            {-
            Gtk.onScroll drawingArea $ \event -> do
                putStrLn $ show (eventX event, eventY event) ++ show (eventDirection event)
                return True
            -}
            return ()

        return layoutRef

    minBoundingBoxDimension :: Double
    minBoundingBoxDimension = 1.0

    fitLayout :: (NetworkIF network, GuiGraphical graphical) => IORef (GuiLayout network graphical) -> IO ()
    fitLayout layoutRef = do
        layout <- readIORef layoutRef
        let drawingArea = layoutDrawingArea layout
        (width, height) <- Gtk.widgetGetSize drawingArea
        layout <- readIORef layoutRef
        let sc = min (fromIntegral width / max minBoundingBoxDimension (layoutWidth layout))
                 (fromIntegral height / max minBoundingBoxDimension (layoutHeight layout))
        modifyIORef layoutRef $ \li -> li { layoutViewScale = sc }
        let origin = layoutCentre layout
        debugPrint $ "New origin " ++ show origin
        setLayoutViewOrigin layoutRef origin
        layoutUpdateScrollbars layout
        updateZoomLabel layoutRef
        return ()

    nodeFontSize :: Double
    nodeFontSize = 10

    -- nodePortLabelFontSize :: Double
    -- nodePortLabelFontSize = 7

    portFontSize :: Double
    portFontSize = 10

    compGrey :: Colour
    compGrey = (0.95, 0.95, 0.95)

    setCompFont :: Double -> Render ()
    setCompFont height = do
        selectFontFace "Helvetica" FontSlantNormal FontWeightNormal
        setFontSize $ height / 2

    simpleCompShape :: DPoint -> Double -> Double -> SimpleShapeRender -> String -> Render ()
    simpleCompShape origin width height shape label = do
        shape origin width height
        strokeFill compGrey
        stroke
        centredText origin label

    drawComp :: CompNode -> IM.IntMap LinkEdge -> NetworkComp -> Render ()
    drawComp node linkEdges comp = do
        let
            origin@(originX, originY) = compNodeOrigin node
            width = compNodeWidth node
            height = compNodeHeight node
            labelLength = compNodeLabelLength node

            simpleShape shape label = do
                setCompFont height
                simpleCompShape origin width height shape label

            clipLabel label
                | labelLength == 0 = label
                | listAtLeastLength labelLength label = take (labelLength - 4) label ++ " ..."
                | otherwise = label

            linkHeadX (Link linkNo)
                | isJust linkEdge = Just scaledX
                | otherwise = Nothing
                where
                    linkEdge = IM.lookup linkNo linkEdges
                    (x, _) = last $ snd $
                        fromJust $ find edgeIsAtEndOfLink $ linkEdgePoints $ fromJust linkEdge

                    edgeIsAtEndOfLink (LinkDirect, _) = True
                    edgeIsAtEndOfLink (LinkAfterLatch, _) = True
                    edgeIsAtEndOfLink _ = False

                    scaledX = x

        case comp of
            (TeakComp { nwTeakType = TeakJ }) -> simpleShape trapezium "J"
            (TeakComp { nwTeakType = TeakM }) -> simpleShape trapezium "M"
            (TeakComp { nwTeakType = TeakA }) -> simpleShape trapezium "A"
            (TeakComp { nwTeakType = TeakS {} }) -> simpleShape invTrapezium "S"
            (TeakComp { nwTeakType = TeakX terms }) -> do
                trapezium origin width height
                strokeFill compGrey
                stroke
                {-
                moveTo (boundingBoxRight bb - inpWidth) (boundingBoxTop bb)
                relLineTo (inpWidth / 2) inpWidth
                stroke
                -}
                setCompFont height
                centredText origin "X"
                where
                    {- bb = pointWHToBoundingBox origin width height
                    termCount = fromIntegral $ length terms
                    inpWidth = width / (1 + termCount) -}
            (TeakComp { nwTeakType = TeakF {} }) -> simpleShape invTrapezium "F"
            (TeakComp { nwTeakType = TeakI {} }) -> simpleShape box "I"
            (TeakComp { nwTeakType = TeakR {} }) -> simpleShape box "R"
            (TeakComp { nwTeakType = TeakO terms }) -> do
                box origin width height
                strokeFill compGrey
                selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                setFontSize nodeFontSize

                blockTexts origin $ map clipLabel $ prettyPrintOTerms 0 (\_ -> showString "in") terms
            (TeakComp { nwTeakType = TeakV {} }) -> do
                let
                    TeakV name _ _ _ _ = nwTeakType comp
                    [Many wgs, Many _, Many rgs, Many _] = nwCompLinks comp

                    showLabel yFraction label (Just x) = centredText (x, y) label
                        where y = (yFraction - 0.5) * height + originY
                    showLabel _ _ Nothing = return ()

                    numberedLabel yFraction prefix (i, x) = showLabel yFraction (prefix ++ show i) x
                    separator x = vLine (x, originY) height

                    right = originX + (width / 2)
                    left = originX - (width / 2)

                    drawPortion portion i link = do
                        let
                            maybeX = linkHeadX link
                        when (isJust maybeX) $ do
                            let
                                Just x = maybeX
                                leftSep = x - (right - x)
                                nameX = mean [left, leftSep]
                            numberedLabel 0.5 [toUpper portion] (i, Just x)
                            separator leftSep
                            centredText (nameX, originY) name
                        return ()

                box origin width height
                strokeFill compGrey

                case compNodePortion node of
                    Nothing -> do
                        let
                            wXs = map linkHeadX wgs
                            rXs = map linkHeadX rgs
                        mapM_ (numberedLabel 0.5 "W") $ zip [(0::Int)..] wXs
                        mapM_ (numberedLabel 0.5 "R") $ zip [(0::Int)..] rXs
                        let
                            labelXs = sort $ catMaybes $ wXs ++ rXs
                            midLabels = midPoints labelXs
                            leftSep
                                | null labelXs = 0
                                | otherwise = head labelXs - (head midLabels - head labelXs)
                        when (leftSep /= 0) $ separator leftSep
                        mapM_ separator midLabels
                        let nameX = mean [left, leftSep]
                        centredText (nameX, originY) name
                    Just ('r', i) -> drawPortion 'r' i $ rgs !! i
                    Just ('w', i) -> drawPortion 'w' i $ wgs !! i
                    portion -> error $ "TeakV: bad portion `" ++ show portion ++ "'"

            (InstanceComp { }) -> do
                curveBox [TL,TR,BL,BR] curve origin width height
                strokeFill compGrey
                curveBox [TL,TR] curve titleOrigin width titleHeight
                strokeFill (0.4, 0.4, 0.4)
                save
                setSourceRGB 1 1 1
                selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                setFontSize $ boxHeight / 2
                centredText titleOrigin $ nwPartName comp
                setSourceRGB 0 0 0
                vboxText inputColY (const (left + xPadding))
                    inputBoxHeight (map nwPortName inputs)
                vboxText outputColY
                    (\textWidth -> right - (textWidth + xPadding))
                    outputBoxHeight (map nwPortName outputs)
                restore
                where
                    (x, y) = origin
                    curve = boxHeight / 2
                    titleHeight = boxHeight
                    titleOrigin = (x, (y - height / 2) + titleHeight / 2)

                    ports = nwCompPorts comp
                    (inputs, outputs) = partition ((== Input) . nwPortDirection) ports
                    inputCount = length inputs
                    outputCount = length outputs
                    boxesHigh = 1 + max inputCount outputCount
                    boxHeight = height / fromIntegral boxesHigh

                    xPadding = 6

                    inputBoxHeight = (height - boxHeight) / fromIntegral inputCount
                    outputBoxHeight = (height - boxHeight) / fromIntegral outputCount

                    top = y - height / 2
                    left = x - width / 2
                    right = x + width / 2

                    inputColY = top + boxHeight + inputBoxHeight / 2
                    outputColY = top + boxHeight + outputBoxHeight / 2
            {-
            _ -> do
                box origin width height
                strokeFill compGrey
                -}

    {-
    middlePoint :: [DPoint] -> DPoint
    middlePoint (from:curveToPoints) = findMidPoint 0 $ zip distances curveEndPoints
        where
            (_, distances) = mapAccumL (\from to -> (to, distDPoint from to)) from curveEndPoints
            halfWay = sum distances / 2

            findMidPoint _ [] = error "findMidPoint: can't happen"
            findMidPoint _ [(_, point)] = point
            findMidPoint dist ((dist1, point1):dp2:rest)
                | dist + dist1 > halfWay = point1
                | otherwise = findMidPoint (dist + dist1) (dp2:rest)

            curveEndPoints = mapN 3 last curveToPoints
            -}

    partDrawLink :: NetworkIF network =>
        Bool -> Bool -> (LinkEnd -> Int -> Maybe (Int, Double, Colour)) ->
        PartGraphical -> NetworkLinkRef -> Double -> NetworkMonad network (Render ())
    partDrawLink withArrowhead withLatch widthColourF graphical link minLineWidth = do
        let
            linkEdges = partLinkEdges graphical
            linkEdge = IM.lookup (nwLink link) linkEdges
            Just (LinkEdge pointss latchPoints) = linkEdge
        width <- nwGetLinkWidth link
        latching <- nwGetLinkLatching link
        return $ if isJust linkEdge
            then do
                forM_ pointss $ \(end, points) -> do
                    let drawInfo = widthColourF end width
                    when (isJust drawInfo) $ do
                        let Just (lineWidth, a, (r,g,b)) = drawInfo
                        save
                        setSourceRGBA r g b a
                        setLineWidth $ max minLineWidth $ 1 + (fromIntegral lineWidth) / 2
                        if listAtLeastLength 3 points
                            then curveLine points
                            else line points
                        restore
                -- box (middlePoint points) 20 20
                when withArrowhead $ do
                    let
                        (end, points) = last pointss
                        arrowheadSize = 8
                        pointCount = length points
                        [nextToLastPoint, lastPoint] = drop (pointCount - 2) points
                        drawInfo = widthColourF end width
                    when (isJust drawInfo) $ do
                        save
                        let Just (_, a, (r,g,b)) = drawInfo
                        setSourceRGBA r g b a
                        arrowhead arrowheadSize 0.5 nextToLastPoint lastPoint
                        restore
                when (withLatch && isJust latchPoints) $ do
                    save
                    let (_, a, _) = fromMaybe (undefined, 1, undefined) $ widthColourF (fst (last pointss)) width
                    setLineWidth $ max minLineWidth 2
                    setSourceRGBA 0 0 0 a
                    let
                        -- FIXME, allow rotated latches?
                        Just (latchNorth, latchSouth) = latchPoints
                        (_, height) = diffDPoint latchNorth latchSouth
                        origin@(originX, originY) = meanDPoints [latchNorth, latchSouth]
                        depth = latchingDepth latching
                        latchWidth = 36
                    box origin latchWidth height
                    strokeFill compGrey
                    selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                    setFontSize nodeFontSize
                    let pad = height / 4
                    moveTo ((originX + (latchWidth / 2)) + pad) ((originY - (height / 2)) - pad)
                    showText $ show depth
                    restore
            else return ()

    drawPart :: NetworkIF network => PartGraphical -> Part network -> Double -> Render ()
    drawPart graphical part minLineWidth = tryPart part $ do
        let
            portNodes = partPortNodes graphical
            compNodes = partCompNodes graphical
            linkEdges = partLinkEdges graphical
            ucNodes = partUcNodes graphical
            highlightComps = partHighlightComps graphical

        highlights <- liftM (sequence . catMaybes) $ nwMapComps $ \comp -> do
            let
                compNode = IM.lookup (nwCompIndex comp) compNodes
            if (refComp comp) `elem` highlightComps && isJust compNode
                then return $ Just $ forM (fromJust compNode) $ \node -> do
                    let
                        origin = compNodeOrigin node
                        width = compNodeWidth node
                        height = compNodeHeight node
                        border = 0.4 * (min width height)
                    save
                    setSourceRGBA 0 1 0 0.3
                    box origin (width + border) (height + border)
                    fillPreserve
                    stroke
                    restore
                else return Nothing

        portRs <- liftM sequence_ $ (flip mapM) (zip [0..] (networkPorts part)) $ \(i, port) -> do
            let portNode = IM.lookup i portNodes
            if isNothing portNode
                then return $ return ()
                else do
                    let
                        Just (PortNode origin) = portNode
                    return $ do
                        selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                        setFontSize portFontSize
                        centredText origin $ nwPortName port
        let
            drawUCNode (UCNode link origin) = do
                linkName <- liftM (fromMaybe (show link)) $ nwGetLinkName link
                return $ do
                    box origin ucWidth ucHeight
                    save
                    setSourceRGB 1 0 0
                    strokeFill compGrey
                    restore
                    centredText origin linkName
                where
                    (ucWidth, ucHeight) = (72 * 0.4, 72 * 0.4)
        ucRs <- liftM sequence_ $ mapM drawUCNode ucNodes
        compRs <- liftM sequence_ $ nwMapComps $ \comp -> do
            let
                compNode = IM.lookup (nwCompIndex comp) compNodes
            if isJust compNode
                then return $ forM_ (fromJust compNode) $ \node -> do
                    (uncurry moveTo) $ compNodeOrigin node
                    save
                    setLineWidth $ max minLineWidth 2
                    drawComp node linkEdges comp
                    -- FIXME, show error markers if some ports are not connected
                    restore
                else return $ return ()
        compVs <- liftM sequence_ $ nwMapComps $ \comp -> do
            -- Show velocity lines
            let
                compNode = IM.lookup (nwCompIndex comp) compNodes
            if isJust compNode
                then return $ forM_ (fromJust compNode) $ \node -> do
                    let
                        origin = compNodeOrigin node
                        velocity = compNodeVelocity node
                    when (velocity /= atRest) $ do
                        save
                        (uncurry3 setSourceRGB) $ colours !! 2
                        setLineWidth 1
                        line [origin, origin +~ velocity *~ 1]
                        stroke
                        restore
                        return ()
                else return $ return ()
        let
            widthColoursF _ 0 = Just (1, 1, black)
            widthColoursF _ width = Just (lgWidth, 1, colours !! lgWidth)
                where lgWidth = min (intWidth width) 9

            widthFadedF _ _ = Just (1, 0.5, black)

            widthF
                | partShowLinkColours graphical = widthColoursF
                | otherwise = widthFadedF

        linkRs <- liftM sequence_ $ nwMapLinks $ \link ->
            partDrawLink True True widthF graphical (refLink link) minLineWidth

        return $ do
            highlights
            portRs
            linkRs
            compRs
            ucRs
            when False compVs -- Show velocity lines
            {- Show BB
            do
                let bb = graphicalBB graphical
                setLineWidth 1
                box (boundingBoxCentre bb) (boundingBoxWidth bb) (boundingBoxHeight bb)
                stroke -}
            return ()

    drawO :: OGraphical -> Double -> Render ()
    drawO (OGraphical _ input output termNodes resultEdges) minLineWidth = do
        let
            drawPort _ Nothing = return ()
            drawPort name (Just origin) = do
                selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                setFontSize portFontSize
                centredText origin name

            drawTerm (label, node, oshape) = do
                (uncurry moveTo) origin
                save
                setLineWidth $ max minLineWidth 2
                setCompFont height
                ext <- textExtents label
                let
                    width' = max width (textExtentsWidth ext + 20)
                simpleCompShape origin width' height oshape label
                restore
                where
                    origin = compNodeOrigin node
                    width = compNodeWidth node
                    height = compNodeHeight node

            drawEdge (points, width, label) = do
                let
                    (lineWidth, (r,g,b))
                        | width == 0 = (1, colours !! 0)
                        | otherwise = (lgWidth, colours !! lgWidth)
                            where lgWidth = min (intWidth width) 9

                    arrowheadSize = 8

                    pointCount = length points
                    [nextToLastPoint, lastPoint] = drop (pointCount - 2) points
                save
                setSourceRGB r g b
                setLineWidth $ max minLineWidth $ 1 + (fromIntegral lineWidth) / 2
                curveLine points
                when (not (null label)) $ do
                    selectFontFace "Helvetica" FontSlantNormal FontWeightBold
                    setFontSize portFontSize
                    -- centredText (last points -~ (0, portFontSize)) label
                    save
                    uncurry translate lastPoint
                    let angle = uncurry atan2 $ lastPoint -~ nextToLastPoint
                    -- rotate $ pi / 2
                    rotate $ (- angle) - pi / 2
                    moveTo 4 (- 8)
                    showText label
                    restore
                arrowhead arrowheadSize 0.5 nextToLastPoint lastPoint
                restore

        drawPort "input" input
        drawPort "output" output
        mapM_ drawEdge resultEdges
        mapM_ drawTerm termNodes

    fillZoomToolbar :: Int -> Bool -> Gtk.Toolbar -> IO LayoutZoom
    fillZoomToolbar pos _withZoomStep toolbar = do
        tooltips <- Gtk.tooltipsNew

        zoomIn <- makeToolButton toolbar False pos (Just Gtk.stockZoomIn) Nothing
        zoomOut <- makeToolButton toolbar False (pos + 1) (Just Gtk.stockZoomOut) Nothing
        zoomFit <- makeToolButton toolbar False (pos + 2) (Just Gtk.stockZoomFit) Nothing
        Gtk.toolItemSetTooltip zoomIn tooltips "Zoom In" ""
        Gtk.toolItemSetTooltip zoomOut tooltips "Zoom Out" ""
        Gtk.toolItemSetTooltip zoomFit tooltips "Zoom Fit" ""
        sep <- Gtk.separatorToolItemNew
        Gtk.toolbarInsert toolbar sep 3
        (zoomMove, pos') <- if True -- withZoomStep
            then do
                step <- makeToolButton toolbar False (pos + 4) (Just Gtk.stockGoForward) Nothing
                Gtk.toolItemSetTooltip step tooltips "Move Step" ""
                play <- makeToolButton toolbar False (pos + 5) (Just Gtk.stockMediaPlay) Nothing
                Gtk.toolItemSetTooltip play tooltips "Move Play" ""
                stop <- makeToolButton toolbar False (pos + 6) (Just Gtk.stockMediaStop) Nothing
                Gtk.toolItemSetTooltip stop tooltips "Move Stop" ""
                -- mapM_ (\b -> Gtk.widgetSetSensitivity b withZoomStep) [step, play, stop]
                sep <- Gtk.separatorToolItemNew
                Gtk.toolbarInsert toolbar sep 7
                return (Just (LayoutZoomMove step play stop), 8)
            else return (Nothing, 4)
        zoomLabel <- makeToolButton toolbar False pos' Nothing (Just " 100% ")
        return $ LayoutZoom (Just zoomLabel) (Just zoomIn) (Just zoomOut) (Just zoomFit) zoomMove

    layoutZoomButtons :: LayoutZoom -> [Gtk.Widget]
    layoutZoomButtons layoutZoom =
        (map Gtk.castToWidget (mapMaybe ($ layoutZoom)
            [layoutZoomIn, layoutZoomOut, layoutZoomFit, layoutZoomLabel])) ++
        (fromMaybe [] $ do
            move <- layoutZoomMove layoutZoom
            return $ map Gtk.castToWidget [layoutZoomStep move, layoutZoomPlay move, layoutZoomStop move])

    makeLayoutTable :: (NetworkIF network, GuiGraphical graphical) =>
        Maybe graphical -> Bool -> Bool -> Maybe (IORef (GuiLayout network graphical) -> DPoint -> Event -> IO ()) ->
            IO (IORef (GuiLayout network graphical), Gtk.Widget)
    makeLayoutTable graphical withZoom withZoomStep buttonAction = do
        drawingArea <- Gtk.drawingAreaNew
        vScroll <- liftM Gtk.castToScrollbar $ Gtk.vScrollbarNewDefaults
        hScroll <- liftM Gtk.castToScrollbar $ Gtk.hScrollbarNewDefaults
        cornerLabel <- Gtk.labelNew $ Just ""
        table <- Gtk.tableNew 2 2 False
        Gtk.tableAttach table cornerLabel 1 2 1 2 [Gtk.Fill] [Gtk.Fill] 0 0
        Gtk.tableAttach table drawingArea 0 1 0 1 [Gtk.Fill, Gtk.Expand, Gtk.Shrink]
            [Gtk.Fill, Gtk.Expand, Gtk.Shrink] 0 0
        Gtk.tableAttach table vScroll 1 2 0 1 [Gtk.Fill] [Gtk.Fill, Gtk.Expand, Gtk.Shrink] 0 0
        Gtk.tableAttach table hScroll 0 1 1 2 [Gtk.Fill, Gtk.Expand, Gtk.Shrink] [Gtk.Fill] 0 0

        let initLayout' = initLayout drawingArea vScroll hScroll graphical

        if withZoom
            then do
                toolbar <- Gtk.toolbarNew
                vBox <- Gtk.vBoxNew False 2
                Gtk.boxPackStart vBox toolbar Gtk.PackNatural 2
                Gtk.boxPackStart vBox table Gtk.PackGrow 2
                toolbarSetup toolbar Gtk.OrientationHorizontal Gtk.ToolbarIcons
                zoom <- fillZoomToolbar 0 withZoomStep toolbar

                layoutRef <- initLayout' zoom False buttonAction
                return (layoutRef, Gtk.castToWidget vBox)
            else do
                layoutRef <- initLayout' noLayoutZoom True buttonAction
                return (layoutRef, Gtk.castToWidget table)

    plotParts :: NetworkIF network => String -> Bool -> [PlotOption] -> Bool ->
        ([PlotOption] -> Part network -> IO PartGraphical) ->
        Maybe (Part network -> PartGraphical -> Double -> Render ()) ->
        [Part network] -> IO ()
    plotParts baseFilename addLangSuffix opts verbose makePartGraphical preRender parts = do
        let
            PlotPaperSize paperSize = getSubOption opts $ PlotPaperSize a4Paper
            PlotLanguage lang = getSubOption opts $ PlotLanguage PlotPS

            output pages = case lang of
                PlotPS -> do
                    let filename = baseFilename ++ if addLangSuffix then ".ps" else ""
                    when verbose $ putStrLn $ "*** Writing PostScript file `" ++ filename ++ "'"
                    renderToPS paperSize filename $ map snd pages
                PlotPDF -> do
                    let filename = baseFilename ++ if addLangSuffix then ".pdf" else ""
                    when verbose $ putStrLn $ "*** Writing PDF file `" ++ filename ++ "'"
                    renderToPDF paperSize filename $ map snd pages
                PlotSVG -> forM_ pages $ \(name, page) -> do
                    let filename = base ++ "-" ++ name ++ ext
                    when verbose $ putStrLn $ "*** Writing SVG file `" ++ filename ++
                        "' for part `" ++ name ++ "'"
                    renderToSVG paperSize (base ++ "-" ++ name ++ ext) page
                    where
                        commonFilename = baseFilename ++ if addLangSuffix then ".svg" else ""
                        splitFilename = splitWith "." commonFilename
                        base = joinWith "." $ init splitFilename

                        ext = case splitFilename of
                            [_] -> ""
                            elems -> "." ++ last elems

            PlotOnlyParts partNamesToPlot = getSubOption opts (PlotOnlyParts $ map networkName parts)
            partsToPlot = mapMaybe (nwFindPart parts) partNamesToPlot

        pages <- forM partsToPlot $ \part -> do
            graphical <- makePartGraphical opts part
            let preRender' = do
                  pre <- preRender
                  return $ pre part graphical
            return (networkName part, renderPartForPaper paperSize opts preRender' Nothing part graphical)
        output pages
        return ()

    -- trimPartForPartialPlot : trim a Part to select labels etc. required for a partial plot using
    --    the 'part' plot option.
    trimPartForPartialPlot :: NetworkIF network => [PlotOption] -> Part network -> Part network
    trimPartForPartialPlot plotOpts part = case partialOpt of
        Just (PlotPartial elems) -> let
            validElems = concatMap findPlotPartialElem elems
            in trimPart part $ nub $ concat $ tryPart part $ mapM compsForElem validElems
        _ -> part
        where
            compsForElem (posRef, depth) = liftM concat $ nwMapCompsIf
                (return . compHasPosRef posRef) (nwConnectedComps depth . refComp)

            findPlotPartialElem ([name, label], depth)
                | name == networkName part && isJust maybePosArray =
                    map (\i -> (i, depth)) $ mapMaybe findLabel $ indices posArray
                where
                    Just (PosArray posArray) = maybePosArray

                    findLabel i = case posArray ! i of
                        PosLabel _ labelName | label == labelName -> Just $ PosRef i
                        _ -> Nothing
            findPlotPartialElem _ = []

            compHasPosRef (PosRef ref) comp = ref `elem` posGetRefs (nwCompPos comp)
            compHasPosRef _ _ = False

            maybePosArray = tryPart part nwGetPosArray

            partialOpt = findSubOption plotOpts $ PlotPartial undefined
