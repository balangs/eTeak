{-
    Teak synthesiser for the Balsa language
    Copyright (C) 2007-2010 The University of Manchester

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,

    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
    School of Computer Science, The University of Manchester
    Oxford Road, MANCHESTER, M13 9PL, UK
-}

module Monitor (
    renderMonitorEvents,
    defaultMonitorStateColours,
    MonitorState (..),
    MonitorEvent (..),
    PartMonitorEvents,
    partMonitorEventsTimeLimits,
    readMonitorFile,
    findMonitorEventForTime,
    findBuiltins,
    FindBuiltinState (..),
    PartBuiltinState (..),
    LinkAndEnd,
    TimeSortedMonitorEvents
    ) where

    import GuiSupport
    import Graphics.Rendering.Cairo
    import Layout
    import NetParts
    import Report
    import Misc
    import State
    import Type

    import Data.Maybe
    import Control.Monad
    import Data.List
    import Data.Array
    import qualified Data.Map as M

    data MonitorState = HS_SPACER | HS_r | HS_R | HS_RA | HS_rA | HS_A
        deriving (Show, Ix, Eq, Ord, Enum)

    data MonitorEvent = MonitorEvent {
        monitorEventTime :: Integer,
        monitorEventData :: Integer,
        monitorEventEnd :: LinkEnd,
        monitorEventLink :: NetworkLinkRef,
        monitorEventState :: MonitorState }
        deriving Show

    type TimeSortedMonitorEvents = Array Int MonitorEvent
    type LinkAndEnd = (NetworkLinkRef, LinkEnd)
    type PartMonitorEvents = [(String, [(LinkAndEnd, TimeSortedMonitorEvents)])]

    linkForMonitorPort :: NetworkIF network => Part network -> String -> Maybe NetworkLinkRef
    linkForMonitorPort part portName = do
        port <- find ((portName ==) . portNameToSignalName . nwPortName) $ networkPorts part
        ref <- nwPortRef port
        let links = tryPart part $ nwGetPortAccess ref
        case links of
            [link] -> return link
            _ -> fail ""
        where
            -- FIXME, generalise somewhere
            portNameToSignalName port
                | isSuffixOf "]" port = name ++ "_" ++ joinWith "_" indices
                | otherwise = port
                where (name:indices) = map (dropSuffix "]") $ splitWith "[" port

    sortPartMonitorEvents :: [(String, [MonitorEvent])] -> PartMonitorEvents
    sortPartMonitorEvents partEvents = mapSnd sortEvents partEvents
        where
            sortEvents events = map sortTimes byLinkAndEnd
                where
                    byLinkAndEnd = sortAndGroupByElem (\ev -> (monitorEventLink ev, monitorEventEnd ev)) events
                    compareTime l r = monitorEventTime l `compare` monitorEventTime r
                    sortTimes (link, events) = (link, listArray (1, length sortedEvents) sortedEvents)
                        where sortedEvents = sortBy compareTime events

    -- findMonitorEventForTime : find an event at or before this time
    findMonitorEventForTime :: TimeSortedMonitorEvents -> Integer -> Maybe MonitorEvent
    findMonitorEventForTime events time = body (bounds events) >>= (return . (events !))
        where
            timeOf i = monitorEventTime $ events ! i

            body (from, to)
                | from == to = if timeOf to <= time then Just to else Nothing
                | timeOf (halfWay + 1) > time = body (from, halfWay)
                | otherwise = body (halfWay + 1, to)
                where halfWay = (from + to) `div` 2

    readMonitorFile :: NetworkIF network => [Part network] -> FilePath -> WhyT IO PartMonitorEvents
    readMonitorFile parts file = do
        fileContents <- whyTReadFile pos file
        events <- mapM' (\line -> WhyT $ return $ splitLine line) $ lines fileContents
        return $ sortPartMonitorEvents $ mapSnd (map snd) $ sortAndGroupByElem fst events
        where
            pos = PosLabel PosTopLevel $ "reading monitor file `" ++ file ++ "'"

            maybeRead :: Read a => String -> Maybe a
            maybeRead str = do
                (ret, restStr) <- listToMaybe $ reads str
                when (restStr /= "") $ fail ""
                return ret

            readTime ('#':timeStr) = maybeRead timeStr
            readTime _ = Nothing

            readState str = maybeToWhy (fail $ "bad monitor state `" ++ str ++ "'") $ case str of
                "SPACER" -> Just HS_SPACER
                "r" -> Just HS_r
                "R" -> Just HS_R
                "RA" -> Just HS_RA
                "rA" -> Just HS_rA
                "A" -> Just HS_A
                _ -> Nothing

            defaultTime = 0

            parseEnd "P" = LinkAfterLatch
            parseEnd "A" = LinkBeforeLatch
            parseEnd _ = LinkDirect

            readMonitorSignal linkStr = maybeToWhy (fail failStr) $ case splitWith "." linkStr of
                [partName, 'L':link] -> do
                    linkNo <- maybeRead link
                    return (partName, Link linkNo, LinkDirect)
                [partName, 'L':link, endStr] -> do
                    linkNo <- maybeRead link
                    let end = parseEnd endStr
                    return (partName, Link linkNo, end)
                [partName, portName, endStr] -> do
                    part <- nwFindPart parts partName
                    link <- linkForMonitorPort part portName
                    let end = parseEnd endStr
                    return (partName, link, end)
                _ -> Nothing
                where failStr = "bad monitor signal `" ++ linkStr ++ "'"

            splitLine line = case filter (/= "") (splitWith " " line) of
                (timeStr:linkStr:stateStr:rest) -> do -- FIXME, add values
                    time <- maybeToWhy (fail "bad time") $ readTime timeStr
                    (partName, link, end) <- readMonitorSignal linkStr
                    state <- readState stateStr
                    data_ <- case rest of
                        [value] -> maybeToWhy (fail "bad event data") $ maybeRead value
                        _ -> return 0
                    return (partName, MonitorEvent {
                        monitorEventTime = time,
                        monitorEventData = data_,
                        monitorEventEnd = end,
                        monitorEventLink = link,
                        monitorEventState = state })
                [linkStr, stateStr] -> do
                    (partName, link, end) <- readMonitorSignal linkStr
                    state <- readState stateStr
                    return (partName, MonitorEvent {
                        monitorEventTime = defaultTime,
                        monitorEventData = 0,
                        monitorEventEnd = end,
                        monitorEventLink = link,
                        monitorEventState = state })
                _ -> fail $ "bad monitor line `" ++ show line ++ "'"

    partMonitorEventsTimeLimits :: PartMonitorEvents -> (Integer, Integer)
    partMonitorEventsTimeLimits partEvents = limits
        where
            widenLimits (lowest, highest) (low, high) = (min low lowest, max high highest)

            limitList = concatMap getPartLimits partEvents

            limits
                | null limitList = (0, 1)
                | otherwise = foldl1' widenLimits limitList

            getPartLimits (_, byLinks) = mapMaybe latestLinkTime byLinks
                where
                    -- latestLinkTime (_, []) = Nothing
                    latestLinkTime (_, events) =
                        Just (monitorEventTime (events ! from), monitorEventTime (events ! to))
                        where (from, to) = bounds events

    data {- NetworkIF network => -} FindBuiltinState network = FindBuiltinsState {
        fbsParts :: M.Map String (Part network),
        fbsPart :: Maybe (Part network),
        fbsPartStates :: M.Map String PartBuiltinState
        }
        deriving Show

    data PartBuiltinState = PartBuiltinState {
        fbsLinkStates :: M.Map NetworkLinkRef [Int]
        }
        deriving Show

    type BuiltinMonad network a = State (FindBuiltinState network) a

    onPart :: NetworkIF network => Part network -> BuiltinMonad network a -> BuiltinMonad network a
    onPart part m = state op
        where op state = (ret, state' { fbsPart = fbsPart state })
                where (ret, state') = runState m (state { fbsPart = Just part })

    here :: NetworkIF network => NetworkMonad network a -> BuiltinMonad network a
    here nm = state op
        where op state = (tryPart (fromJust $ fbsPart state) nm, state)

    addLinkBuiltin :: NetworkIF network => NetworkLinkRef -> Int -> BuiltinMonad network Bool
    addLinkBuiltin link offset = state op
        where op state = (offset `elem` linkState, state {
            fbsPartStates = M.insert partName partStates' (fbsPartStates state) })
                where
                  partName = networkName $ fromJust $ fbsPart state
                  partStates = fbsPartStates state M.! partName
                  partStates' = partStates { fbsLinkStates = M.insert link linkState' $ fbsLinkStates partStates }
                  linkState = fromMaybe [] $ M.lookup link $ fbsLinkStates partStates
                  linkState' = mergeWith const linkState [offset]

    findBuiltins :: NetworkIF network => [Part network] -> FindBuiltinState network
    findBuiltins parts = snd $ runState allParts state0
        where
            allParts = forM_ parts $ \part -> onPart part startPart

            state0 = FindBuiltinsState {
                fbsParts = M.fromList $ map (\p -> (networkName p, p)) parts,
                fbsPart = Nothing,
                fbsPartStates = M.fromList $ map (\p -> (networkName p, partBuiltinState0)) parts
                }
            partBuiltinState0 = PartBuiltinState {
                fbsLinkStates = M.empty
                }

            {-
            followForward state link = do
                pasUsage <- nwGetLinkUsage Passive link
                case pasUsage of
                    Just (LinkComp comp addr) -> error "A"
                    Just (LinkAccess comp addr) -> error "P"
                    -}

            builtinO (TeakComp { nwTeakType = TeakO terms }) = any isBuiltinTerm terms
                where
                    isBuiltinTerm (_, TeakOBuiltin {}) = True
                    isBuiltinTerm _ = False
            builtinO _ = False

            -- builtinOffsets (_, Builtin builtin outputWidth params slices)
            -- builtinOffsets (_, Builtin "String" outputWidth params slices)
            -- FIXME, need to fill out
            builtinOffsets [(_, TeakOBuiltin "String" _ _ _)] = ([], [0])
            builtinOffsets [(_, TeakOBuiltin "StringAppend" _ _ _)] = ([0, builtinTypeWidth], [0])
            builtinOffsets _ = ([], [])

            startPart = do
                startComps <- here $ nwMapCompsIf (return . builtinO) return
                forM_ startComps $ \comp -> do
                    let
                        TeakComp { nwTeakType = TeakO terms, nwCompLinks = [One inp, One out] } = comp
                        (inputBs, outputBs) = builtinOffsets terms

                    forM_ inputBs $ \input -> do
                        addLinkBuiltin inp input

                    forM_ outputBs $ \output -> do
                        addLinkBuiltin out output
                    -- check for Builtin inputs
                    -- check for Builtin outputs
                    return ()

    defaultMonitorStateColours :: Array MonitorState Colour
    defaultMonitorStateColours = array (HS_SPACER, HS_A) [
        (HS_SPACER, white), (HS_r, pink), (HS_R, red), (HS_RA, blue), (HS_rA, paleBlue), (HS_A, green) ]

    -- renderMonitorEvents : render monitor events as wide background highlighting on monitored channel.
    --    This function is intended to be used in a layout pre-renderer
    renderMonitorEvents :: NetworkIF network =>
        Array MonitorState Colour -> [GuiOption] -> PartMonitorEvents -> Integer ->
        Part network -> PartGraphical -> Double -> Render ()
    renderMonitorEvents stateColours _ events time part graphical minLineWidth = do
        forM_ partEvents $ \((link, linkEnd), events) -> do
            let
                unused = tryPart part $ nwLinkIsUnused link
                -- FIXME, this function is linear timed and probably *way* too expensive
                maybeEvent = findMonitorEventForTime events time
            when (not unused && isJust maybeEvent) $ do
                let
                    Just event = maybeEvent
                    state = monitorEventState event
                    widthColourF arcEnd _
                        | arcEnd == linkEnd = Just (highlightWidth, highlightOpacity, stateColours ! state)
                        | otherwise = Nothing
                when (state /= HS_SPACER) $
                    tryPart part $ partDrawLink False False widthColourF graphical link minLineWidth
        where
            highlightOpacity = 0.5
            highlightWidth = 40
            partEvents = fromMaybe [] $ lookup (networkName part) events
