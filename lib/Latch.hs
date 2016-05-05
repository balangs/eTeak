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

module Latch (
    LinkDep,
    LatchStrategy (..),
    LatchOption (..),
    defaultLatchOptions,
    latchOptionUsage,
    nwInsertLatches,
    nwRemoveLatches,
    findLinkDeps,
    rootComponents,
    isLatchOption,
    PrioLink,
    Priority,
    makeEdges
    ) where

    import Misc
    import NetParts
    import Report
    import Graph
    import ParseTree
    import State
    import Options

    import Control.Monad
    import Data.Maybe
    import Data.List
    import Control.Monad.Trans

    data LatchStrategy =
          LatchSimple
        | LatchVar
        | LatchOpIn
        | LatchIIn
        | LatchBetweenInstances
        | LatchGo
        | LatchFork
        | LatchThrough
        | LatchLoop
        deriving (Show, Eq, Enum)

    data LatchOption =
          LatchOption { teakLatchStrategy :: LatchStrategy, teakLatchDepth :: Int }
        | LatchDoSCP
        | LatchSCPLimit Double
        deriving Show

    instance SubOption LatchOption where
        matchSubOption op1@(LatchOption {}) op2@(LatchOption {}) = teakLatchStrategy op1 == teakLatchStrategy op2
        matchSubOption LatchDoSCP LatchDoSCP = True
        matchSubOption (LatchSCPLimit {}) (LatchSCPLimit {}) = True
        matchSubOption _ _ = False

    defaultLatchOptions :: [LatchOption]
    defaultLatchOptions = [LatchSCPLimit 3000.0]

    showLatchOption :: LatchOption -> String
    showLatchOption opt@(LatchOption {}) = show $ teakLatchDepth opt
    showLatchOption (LatchSCPLimit limit) = show limit
    showLatchOption _ = ""

    isLatchOption :: LatchOption -> Bool
    isLatchOption (LatchOption {}) = True
    isLatchOption _ = False

    latchOptionUsage :: SubOptionUsages LatchOption
    latchOptionUsage = SubOptionUsages "latch" showLatchOption (Just []) (Just defaultLatchOptions) [
        ("scp", boolSubOption "Do SCP" LatchDoSCP),
        ("scp-limit", SubOptionUsage False "limit" "SCP limit" "0"
            (isJust . parseDouble) (\arg -> let
                Just limit = parseDouble arg
                in ([LatchSCPLimit limit], []))),
        ("simple", depthOption "latch every link" [LatchSimple] []),
        ("after-v", depthOption "latch after every V's read/write port" [LatchVar] []),
        ("after-o", depthOption "latch after every O" [LatchOpIn] []),
        ("before-i", depthOption "latch before every I" [LatchIIn] []),
        ("between-inst", depthOption "latch between every instance" [LatchBetweenInstances] []),
        ("through", depthOption "latch links that connect ports to ports" [LatchThrough] []),
        ("loop", depthOption "latch loops to guarantee deadlock-freedom" [LatchLoop] [])
        ]
        where
            parseDouble :: String -> Maybe Double
            parseDouble arg = case (reads :: String -> [(Double, String)]) arg of
                [(limit, "")] -> Just limit
                _ -> Nothing

            depthOption desc add remove = SubOptionUsage False "depth" desc "0"
                (isJust . parseDepth) (\arg -> let
                    Just depth = parseDepth arg
                    makeOpt strategy = LatchOption strategy depth
                    in (map makeOpt add, map makeOpt remove))

            parseDepth arg = case (reads :: String -> [(Int, String)]) arg of
                [(int, "")] | int >= 0 -> Just int
                _ -> Nothing

    latchStrategyToLinksFunc :: NetworkIF network => LatchStrategy -> LatchLinksFunc network
    latchStrategyToLinksFunc LatchSimple = findAllLinks
    latchStrategyToLinksFunc LatchLoop = findBackLinks
    latchStrategyToLinksFunc LatchFork = findZWForkLinks
    latchStrategyToLinksFunc LatchVar = findVarLinks
    latchStrategyToLinksFunc LatchOpIn = findOpLinks
    latchStrategyToLinksFunc LatchIIn = findILinks
    latchStrategyToLinksFunc LatchBetweenInstances = findBetweenInstanceLinks
    latchStrategyToLinksFunc LatchThrough = findThroughLinks
    latchStrategyToLinksFunc LatchGo = findGoLinks
    -- latchStrategyToLinksFunc _ = error "latchStrategyToLinksFunc: no links func"

    type LatchLinksFunc network = Int -> NetworkMonad network [(NetworkLinkRef, Int)]

    nwInsertLatches :: NetworkIF network => [LatchOption] -> WhyT (NetworkMonad network) ()
    nwInsertLatches opts = do
        -- Qualify opts with local (* latch="..." *) options
        latchAttr <- lift $ nwGetAttributeVal "latch"
        let
            Just (TeakParamString localOpts) = latchAttr
            usage = PosLabel NoPos $ "error parsing specified module latching options: " ++ localOpts
        opts' <- if isJust latchAttr
            then do
                opts' <- WhyT $ return $ decorateErrors usage $ parseSubOptionsString latchOptionUsage
                    (const fail) opts localOpts
                return opts'
            else return opts
        let
            insertForOption (LatchOption strategy depth) = do
                when (depth > 0) $ lift $ do
                    links <- (latchStrategyToLinksFunc strategy) depth
                    mapM_ (uncurry nwLatchLink) links
            insertForOption _ = return ()
        forM_ opts' insertForOption

    nwRemoveLatches :: NetworkIF network => NetworkMonad network ()
    nwRemoveLatches = nwMapLinks_ $ \link -> nwUpdateLink $ link { nwLinkLatching = HalfBuffer 0 }

    nwLatchLink :: NetworkIF network => NetworkLinkRef -> Int -> NetworkMonad network ()
    nwLatchLink link depth = do
        unused <- nwLinkIsUnused link
        when (not unused) $ do
            linkDecl <- nwGetLink link
            nwUpdateLink $ linkDecl { nwLinkLatching = HalfBuffer depth }
            return ()

    extraLatchesForLink :: NetworkIF network => Int -> NetworkLinkRef -> NetworkMonad network [(NetworkLinkRef, Int)]
    extraLatchesForLink finalDepth link = do
        currentDepth <- liftM latchingDepth $ nwGetLinkLatching link
        return [(link, max (finalDepth - currentDepth) 0)]

    findAllLinks :: NetworkIF network => LatchLinksFunc network
    findAllLinks depth = nwMapLinks $ \link -> return (refLink link, depth)

    -- returns a list of links connected to variables' readDone and writeDone (AB) ports
    findVarLinks :: NetworkIF network => LatchLinksFunc network
    findVarLinks depth = liftM concat $ nwMapCompsIf (return . isTeakV) getReadPortLinks
        where
            getReadPortLinks comp = do
                let
                    [_, Many wd, _, Many rd] = nwCompLinks comp
                rdL <- liftM concat $ mapM (extraLatchesForLink depth) rd
                wdL <- liftM concat $ mapM (extraLatchesForLink depth) wd
                return $ nub $ wdL ++ rdL

    -- returns a list of links connected to operator inputs
    findOpLinks :: NetworkIF network => LatchLinksFunc network
    findOpLinks depth = liftM concat $ nwMapCompsIf (return . isTeakO) getOpInputLink
        where
            getOpInputLink comp = do
                let [One i, _] = nwCompLinks comp
                extraLatchesForLink depth i

    -- returns a list of links connected to I outputs
    findILinks :: NetworkIF network => LatchLinksFunc network
    findILinks depth = liftM concat $ nwMapCompsIf (return . isTeakI) getIInputLink
        where
            getIInputLink comp = do
                let [One i, _] = nwCompLinks comp
                extraLatchesForLink depth i

    -- returns a list of links connected to I outputs
    findBetweenInstanceLinks :: NetworkIF network => LatchLinksFunc network
    findBetweenInstanceLinks depth = liftM concat $ nwMapLinks isInstToInstLink
        where
            isInstToInstLink link = runMaybeTD [] $ do
                (act, _) <- MaybeT $ nwLinkToComp Active $ refLink link
                (pas, _) <- MaybeT $ nwLinkToComp Passive $ refLink link
                if isInstanceComp act && isInstanceComp pas
                    then lift $ extraLatchesForLink depth $ refLink link
                    else fail ""

    -- returns a list of backlinks and their sucessors and predecessors
    findBackLinks :: NetworkIF network => LatchLinksFunc network
    findBackLinks depth = do
        dep <- findLinkDeps
        let
            rootComps = rootComponents dep
            edgesL = makeEdges dep
            loopL = loopLinks edgesL rootComps
        return $ zip (map id loopL) [depth, depth ..]

    -- returns a list of links that go from an input to an output port
    findThroughLinks :: NetworkIF network => LatchLinksFunc network
    findThroughLinks depth = do
        (ins, outs) <- portLinks
        let ioLinks = ins `intersect` outs
        return $ zip ioLinks [depth, depth ..]
        where
            portLinks = do
                accesses <- nwGetAccesses
                let (inss, outss) = unzip $ map getAccessLinks accesses
                return (concat inss, concat outss)
                where
                    getAccessLinks (Access _ bodys) = foldl' getPortLinks ([], []) bodys

                    getPortLinks (ins, outs) (PortLinkAccess Passive link) = (link:ins, outs)
                    getPortLinks (ins, outs) (PortLinkAccess Active link) = (ins, link:outs)
                    getPortLinks ios _ = ios

    -- returns all "go" ports
    findGoLinks :: NetworkIF network => LatchLinksFunc network
    findGoLinks depth = do
        goLinks <- nwGetPortAccess GoAccess
        initialLinks <- nwMapCompsIf (return . isTeakI) findIOut
        -- allGoLinks <- partFindGoLs network
        return $ zip (goLinks ++ initialLinks) [depth, depth ..]
        where
            findIOut (TeakComp { nwTeakType = (TeakI {}), nwCompLinks = [_, One out] }) = return out
            findIOut _ = error "findGoLinks findIlink: can't happen"

    -- returns all zero-width links connected to fork outputs
    findZWForkLinks :: NetworkIF network => LatchLinksFunc network
    findZWForkLinks depth = liftM concat $ nwMapLinks findOutSiblings
        where
            findOutSiblings link = do
                maybeOutComp <- nwLinkToComp Active linkRef
                if isJust maybeOutComp
                    then do
                        let Just (outComp, _) = maybeOutComp
                        if (isFork outComp && zeroWidth)
                            then extraLatchesForLink depth linkRef
                            else return []
                    else return []
                where
                    linkRef = refLink link
                    zeroWidth = (nwLinkWidth link) == 0

    type Priority = Int
    type PrioLink = (Priority, NetworkLinkRef)
    type LinkDep = (Vertex, ([PrioLink], PrioLink))

    -- nwCrossComp : give NetworkLinkRefs for all the passive, incoming links from this component reachable from
    --    output input port `addr' (which is a port address in the format of LinkComp's second argument).
    nwCrossComp :: Maybe NetworkComp -> [Int] -> [NetworkLinkRef]
    nwCrossComp (Just comp@(TeakComp {})) _addr = case nwTeakType comp of
        TeakR {} -> []
        -- TeakV {} -> let [port, subPort] = addr in [(portAt (port - 1)) !! subPort]
        TeakV {} -> portAt 0 ++ portAt 2
        _ -> portAt 0
        where portAt i = flattenSome ((nwCompLinks comp) !! i)
    nwCrossComp (Just comp@(InstanceComp {})) _ = concatMap flattenSome inputs
        where
            inputs = map fst $ filter isInput $ zip (nwCompLinks comp) (nwCompPorts comp)
            isInput (_, port) = nwPortDirection port == Input
    nwCrossComp _ _ = []

    -- returns a list of backlinks and their sucessors and predecessors
    findLinkDeps :: NetworkIF network => NetworkMonad network [LinkDep]
    findLinkDeps = do
        let
            portLinks = do
                accesses <- nwGetAccesses
                let (goss, portss) = unzip $ map getAccessLinks accesses
                return (concat goss, concat portss)
                where
                    getAccessLinks (Access GoAccess bodys) = (mapMaybe getPortLinks bodys, [])
                    getAccessLinks (Access _ bodys) = ([], mapMaybe getPortLinks bodys)

                    getPortLinks (PortLinkAccess _ link) = Just link
                    getPortLinks _ = Nothing

        (goLinks, ioLinks) <- portLinks
        let
            makeDep link = do
                usage <- nwGetLinkUsage Active linkRef
                case usage of
                    Just (LinkComp ref addr) -> do
                        comp <- nwGetComp ref
                        let inputLinks = map linkName $ nwCrossComp comp addr
                        return $ if null inputLinks -- Maybe an "r" component
                            then Just ([(0, Link (-3))], (2, linkRef))
                            else Just (inputLinks, linkName linkRef)
                    _ -> return Nothing
                where linkRef = refLink link

            -- linkName prefix linkRef = nwLink linkRef
            linkName linkRef = (namePriority, linkRef)
                where
                    namePriority
                        | linkRef `elem` goLinks = 0
                        | linkRef `elem` ioLinks = 1
                        | otherwise = 2
            
        liftM (zip [1..] . catMaybes) $ nwMapLinks makeDep

    rootComponents :: [LinkDep] -> [Vertex]
    rootComponents depList = nub (roots ++ nonRoots)
        where
            roots = nub $ map snd rootCPairs
            rootCPairs = sort $ foldl' isRootComp [] depList
            isRootComp rcl (name, (iList, outc))
                | hasLoopBack iList outc = (dominantInputType iList, name):rcl
                | dominantInputType iList > 1 = rcl
                | otherwise = (dominantInputType iList, name):rcl
            dominantInputType iList = fst $ head $ sort $ iList
            hasLoopBack ips op = op `elem` ips
            allComps = map fst depList
            nonRoots = nub $ allComps \\ roots

    makeEdges :: [LinkDep] -> [(Vertex, (Vertex, NetworkLinkRef))]
    makeEdges depList = concatMap (makeNodes depList) $ depList
        where
            makeNodes depList (thisComp, (_, thisOut)) =
                (foldl' (makeNode thisComp thisOut) [] depList)
                    where
                        makeNode from link toList (toComp, (inpList, _)) =
                            if link `elem` inpList && (from, (toComp, snd link)) `notElem` toList
                            then (from, (toComp, snd link)):toList
                            else toList
