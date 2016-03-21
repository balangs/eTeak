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

module Optim (
    Optim (..),
    OptimContext (..),
    OptimLog,
    OptimTest,
    OptimSummaryPrint,
    OptimAct,
    Lead (..),
    NewLead (..),
    optimPassesTest,
    newLeadsToLeads,
    applyLead,
    applyOptims,
    checkOTerms,
    makePartLeads,
    applyLeads,
    isOptim,
    removeGo,
    nextLinks,
    nextMLinks,
    prevLinks,
    networkRemoveAliases,
    shortenOTerms,
    noOptimLog,
    OptimOption (..),
    defaultOptimOptions,
    optimOptionUsage,
    separateOptimOptions
    ) where

    import Misc
    import NetParts
    import Report
    import State
    import ParseTree
    import Options

    import Data.List
    import Data.Maybe
    import Control.Monad.State
    import Data.Bits
    import Bits
    import qualified Data.IntMap as IM

    data {- NetworkIF network => -} OptimOption network = OptimVerbose
        -- OptimOptim is a wrapper to allow read-in optimisations to share a namespace with other optimisation options
        | OptimOptim { optimOptim :: Optim network }
        deriving Show

    defaultOptimOptions :: NetworkIF network => [OptimOption network]
    defaultOptimOptions = []

    isOptimOptim :: NetworkIF network => OptimOption network -> Bool
    isOptimOptim (OptimOptim _) = True
    isOptimOptim _ = False

    -- separateOptimOptions : separate options into straight Optims and other options
    separateOptimOptions :: NetworkIF network => [OptimOption network] -> ([OptimOption network], [Optim network])
    separateOptimOptions opts = (opts', map optimOptim optimOpts)
        where (opts', optimOpts) = partition (not . isOptimOptim) opts

    instance NetworkIF network => SubOption (OptimOption network) where
        matchSubOption OptimVerbose OptimVerbose = True
        matchSubOption (OptimOptim l) (OptimOptim r) = matchSubOption l r
        matchSubOption _ _ = False

    optimOptionUsage :: NetworkIF network => SubOptionUsages (OptimOption network)
    optimOptionUsage = SubOptionUsages "optim" show (Just []) (Just defaultOptimOptions) [
        ("verbose", boolSubOption "verbose reporting of all optimisation steps" OptimVerbose)
        ]

    applyOptims :: (Monad m, NetworkIF network) => [Optim network] -> OptimContext network -> Part network ->
        OptimLog m network -> WhyT m (Part network)
    applyOptims optims context part log = applyLeads context part log optims $ makePartLeads part optims

    makeLeadsForOptim :: NetworkIF network => Optim network -> NetworkMonad network [Lead network]
    makeLeadsForOptim optim = nwMapCompsIf rightComp makeLead
        where
            rightComp comp = return $ optimTestComp optim comp
            makeLead comp = return $ Lead (refComp comp) optim

    makePartLeads :: NetworkIF network => Part network -> [Optim network] -> [Lead network]
    makePartLeads part optims = tryPart part $ liftM concat $ mapM makeLeadsForOptim optims

    data {- NetworkIF network => -} Lead network = Lead { leadCompNo :: NetworkCompRef, leadOptim :: Optim network }

    data NewLead = NewLead { newLeadCompNo :: NetworkCompRef, newLeadName :: String }
        deriving (Show)

    data {- NetworkIF network => -} OptimContext network = OptimContext { optimContextParts :: [Part network] }

    type OptimTest network summary = OptimContext network -> Part network -> NetworkComp -> Maybe summary
    type OptimSummaryPrint network summary = Part network -> summary -> String
    type OptimAct network summary = Part network -> NetworkComp -> summary -> Why ([NewLead], Part network)
    type OptimLog m network = Part network -> NetworkComp -> Optim network -> m ()

    noOptimLog :: (Monad m, NetworkIF network) => OptimLog m network
    noOptimLog _ _ _ = return ()

    data Optim network = forall summary . Optim {
        optimName :: String,
        optimOnByDefault :: Bool,
        optimDescription :: String,
        -- optimTestComp : this component is the right *type* of seed component (no param. checks)
        optimTestComp :: NetworkComp -> Bool,
        -- optimTest : full test and action summary
        optimTest :: OptimTest network summary,
        -- optimPrint : pretty print summary
        optimPrint :: OptimSummaryPrint network summary,
        -- optimAct : act on summary and actually *do* the optimisation
        optimAct :: OptimAct network summary }

    instance Show (Optim network) where
        showsPrec _ optim = showString (optimName optim)

    instance SubOption (Optim network) where
        matchSubOption optim1 optim2 = optimName optim1 == optimName optim2

    isOptim :: NetworkIF network => String -> Optim network -> Bool
    isOptim name opt = name == optimName opt

    optimPassesTest :: NetworkIF network => OptimContext network -> Part network -> Lead network -> Maybe String
    optimPassesTest context part (Lead compNo (Optim _ _ _ doComp test printSummary _)) = do
        comp <- tryPart part $ nwGetComp compNo
        when (not (doComp comp)) $ fail ""
        summary <- test context part comp
        return $ printSummary part summary

    newLeadsToLeads :: NetworkIF network => [Optim network] -> [NewLead] -> [Lead network]
    newLeadsToLeads optims newLeads = concatMap newLeadToLeads newLeads
        where
            newLeadToLeads (NewLead compNo leadName) = map (\optim -> Lead compNo optim) applicableOptims
                where applicableOptims = filter ((== leadName) . optimName) optims

    applyLead :: (Monad m, NetworkIF network) => OptimContext network -> Part network ->
        OptimLog m network ->
        Lead network -> WhyT m ([NewLead], Part network)
    applyLead context part log (Lead compNo optim@(Optim _ _ _ _ test _ act)) = fromMaybe (return ([], part)) $ do
        comp <- tryPart part $ nwGetComp compNo
        summary <- test context part comp
        return $ do
            lift $ log part comp optim
            WhyT $ return $ act part comp summary

    applyLeads :: (Monad m, NetworkIF network) => OptimContext network -> Part network ->
        OptimLog m network -> [Optim network] -> [Lead network] -> WhyT m (Part network)
    applyLeads context part log optims leads = defaultConnectWhyT part
        (body part leads) (\part' -> return $ runPart_ part' nwRemoveUnusedLinks)
        where
            body partIn [] = return partIn
            body partIn (l:ls) = applyLead context partIn log l `connect`
                (\(newLeads, partIn') -> body partIn' (newLeadsToLeads optims newLeads ++ ls))
                where connect = defaultConnectWhyT partIn

    -- Specific optimisation stuff below here --

    isConnectFork :: NetworkIF network => NetworkComp -> NetworkMonad network Bool
    isConnectFork (TeakComp _ (TeakF [0]) [One from, Many [to]] _) = do
        fromWidth <- nwGetLinkWidth from
        toWidth <- nwGetLinkWidth to
        return $ fromWidth == toWidth
    isConnectFork _ = return False

    networkRemoveAliases :: NetworkIF network => NetworkMonad network ()
    networkRemoveAliases = nwMapCompsIf_ isConnectFork networkRemoveAliasesAction

    networkRemoveAliasesAction :: NetworkIF network => NetworkComp -> NetworkMonad network ()
    networkRemoveAliasesAction alias = do
        let [One from, Many [to]] = nwCompLinks alias
        nwRemoveComp alias
        nwRemapLink from to

    removeGo :: NetworkIF network => [Part network] -> String -> [Part network]
    removeGo originalParts topLevelName = fst $ removePartGoAtIndex originalParts topLevelIndex
        where
            Just topLevelIndex = nwFindPartIndex originalParts topLevelName

            removePartGoAtIndex parts i = (replaceAt parts' i part', removed)
                where
                    Part name ports body = parts !! i
                    ((parts', ports', removed), body') = runNetwork body $ removePartGo parts ports
                    part' = Part name ports' body'

            removePartGo parts ports = runMaybeTD (parts, ports, False) $ do
                liftPred null $ nwGetPortAccess DoneAccess
                [go] <- lift $ nwRemovePortAccess GoAccess
                MaybeT $ nwLinkToComp Passive go
                parts' <- lift $ removeGoFollowing parts go
                return (parts', tail ports, True)

            useTeakI = True

            removeGoFollowing parts link = do
                pasComp <- nwLinkToComp Passive link
                if isJust pasComp
                    then do
                        let Just (comp, _) = pasComp
                        case comp of
                            TeakComp {} -> case nwTeakType comp of
                                TeakF {} -> do
                                    let [One _, Many fOuts] = nwCompLinks comp
                                    nwRemoveComp comp
                                    parts' <- foldM removeGoFollowing parts fOuts
                                    return parts'
                                TeakM {} | useTeakI -> do
                                    let
                                        [Many mIns, One mOut] = nwCompLinks comp
                                        otherMIns = mIns \\ [link]
                                    -- FIXME, can drop the first case as mal can mop it up
                                    case otherMIns of
                                        [mIn] -> do
                                            nwRemoveComp comp
                                            nwNewTeakComp TeakI [One mIn, One mOut] (nwCompPos comp)
                                            return ()
                                        _ -> do
                                            iIn <- nwNewLinkRef 0
                                            nwUpdateComp $ comp { nwCompLinks = [Many otherMIns, One iIn] }
                                            nwNewTeakComp TeakI [One iIn, One mOut] (nwCompPos comp)
                                            return ()
                                    return parts
                                _ -> do
                                    nwNewTeakComp TeakR [One link] (nwCompPos comp)
                                    return parts
                            InstanceComp {} | (nwCompLinks comp !! 0) == One link -> do
                                let
                                    Just partIndex = nwFindPartIndex originalParts (nwPartName comp)
                                    (parts', removedGo) = removePartGoAtIndex parts partIndex
                                if removedGo
                                    then do
                                        nwUpdateComp $ comp {
                                            nwCompPorts = tail (nwCompPorts comp),
                                            nwCompLinks = tail (nwCompLinks comp) }
                                    else do
                                        nwNewTeakComp TeakR [One link] NoPos
                                        return ()
                                return parts'
                            _ -> return parts
                    else do
                        error "FIXME, go connects to port, trouble"
                        return parts

    -- selectOSlices : extract field [selWidth+selOffset-1:selOffset] from the given list of slices which
    --    have a low index of 0
    selectOSlices :: [TeakOSlice] -> Int -> Int -> [TeakOSlice]
    selectOSlices _ _ 0 = []
    selectOSlices slices selOffset selWidth = body 0 slices
        where
            selBegin = selOffset
            selEnd = selBegin + selWidth - 1

            body _ [] = []
            body sliceBegin ((sliceTermNo, slice):ss)
                | sliceBegin > selEnd = []
                | sliceEnd < selBegin = next
                | otherwise = (sliceTermNo, (sliceOffset slice + start - sliceBegin) +: (1 + end - start))
                    : next
                where
                    end = min selEnd sliceEnd
                    start = max selBegin sliceBegin
                    next = body (sliceBegin + sliceWidth slice) ss
                    sliceEnd = sliceBegin + sliceWidth slice - 1

    data OTermState = OTermState {
        originalTerms :: [(Int, TeakOTerm)],
        resultTerms :: [(Int, TeakOTerm)],
        nextTermIndex :: Int,
        termSubsts :: [(Int, Int)],
        oTermStateFlatSlices :: IM.IntMap [TeakOSlice] }
        deriving Show

    type OTermMonad a = State OTermState a

    lookupTerm :: Int -> OTermMonad (Maybe TeakOTerm)
    lookupTerm index = do
        state <- get
        return $ lookup index (originalTerms state)

    lookupResultTerm :: Int -> OTermMonad (Maybe TeakOTerm)
    lookupResultTerm index = do
        state <- get
        return $ lookup index (resultTerms state)

    lookupFlatSlices :: Int -> OTermMonad (Maybe [TeakOSlice])
    lookupFlatSlices index = do
        state <- get
        return $ IM.lookup index (oTermStateFlatSlices state)

    addFlatSlices :: Int -> [TeakOSlice] -> OTermMonad ()
    addFlatSlices index slices = do
        state <- get
        let flatSlices' = IM.insert index slices (oTermStateFlatSlices state)
        put $ state { oTermStateFlatSlices = flatSlices' }
        return ()

    lastResultTermIndex :: OTermMonad Int
    lastResultTermIndex = do
        state <- get
        let ret
               | null (resultTerms state) = 0
               | otherwise = fst $ head $ resultTerms state
        return ret

    addTerm :: TeakOTerm -> OTermMonad Int
    addTerm term = do
        state <- get
        let index = nextTermIndex state
        put $ state { nextTermIndex = index + 1,
            resultTerms = (index, term) : resultTerms state }
        return index

    addTermWithSubst :: Int -> TeakOTerm -> OTermMonad Int
    addTermWithSubst origIndex term = do
        state <- get
        let index = nextTermIndex state
        put $ state { nextTermIndex = index + 1,
            resultTerms = (index, term) : resultTerms state,
            termSubsts = (origIndex, index) : termSubsts state }
        return index

    sliceSubst :: TeakOSlice -> OTermMonad TeakOSlice
    sliceSubst slice@(0, _) = return slice
    sliceSubst (termNo, slice) = do
        state <- get
        let Just termNo' = lookup termNo $ termSubsts state
        return (termNo', slice)

    mapOrigTerms :: ((Int, TeakOTerm) -> OTermMonad a) -> OTermMonad [a]
    mapOrigTerms f = getTerms >>= mapM f
        where getTerms = do
                state <- get
                return $ originalTerms state

    runOTermMonad :: String -> [(Int, TeakOTerm)] -> OTermMonad a -> [(Int, TeakOTerm)]
    runOTermMonad msg terms m = check ("AFTER " ++ msg ++ " from:" ++ showTerms terms ++ "\nto:") $
        reverse $ resultTerms $ snd $ runState m $ OTermState (check ("BEFORE " ++ msg) terms) [] 1 [] IM.empty
        where
            check msg terms
                | checkOTerms terms = terms
                | otherwise = error $ msg ++ " " ++ showTerms terms

            showTerms terms = "\n  " ++ joinWith "\n  " (map show terms)

    checkOTerms :: [(Int, TeakOTerm)] -> Bool
    checkOTerms terms = all checkTerm terms
        where
            checkTerm (i, term) = (not $ any badSlice termSlices) && okTerm term
                where
                    termSlices = slices term

                    okTerm (TeakOAppend {}) = length termSlices /= 0
                    okTerm _ = True

                    badSlice (index, slice)
                        | index >= i = True
                        | index == 0 = False
                        | isNothing termWidth = True
                        | (sliceOffset slice + sliceWidth slice) > fromJust termWidth = True
                        | otherwise = False
                        where termWidth = lookup index widths

            widths = mapSnd oTermResultWidth terms

            slices (TeakOConstant {}) = []
            slices slice = teakOSlices slice

    -- flattenOSlice : flatten an oslice through appends to give a slice list
    flattenOSlice :: TeakOSlice -> OTermMonad [TeakOSlice]
    flattenOSlice oSlice@(0, _) = return [oSlice]
    flattenOSlice oSlice@(i, slice) = do
        memoSlices <- lookupFlatSlices i
        finalSlices <- if isJust memoSlices
            then return $ select $ fromJust memoSlices
            else do
                term <- lookupTerm i
                if isNothing term
                    then do
                        state <- get
                        put $ error $ show oSlice ++ " " ++ show state
                        return []
                    else do
                        sl <- body $ fromJust term
                        -- addFlatSlices i sl
                        return sl
        -- return $ selectOSlices slices offset width
        when (width /= sum (map oSliceWidth finalSlices)) $ do
            term <- lookupTerm i
            flatSlices <- case term of
                (Just (TeakOAppend count slices)) -> liftM (concat . replicate count . concat)
                    $ mapM flattenOSlice slices
                _ -> return []
            error $ "SLICES " ++ show oSlice ++ " " ++ show flatSlices ++ " " ++ show finalSlices ++ " " ++ show term
                ++ " offset: " ++ show offset ++ " width: " ++ show width
                ++ " sos: " ++ show (select flatSlices)
        return finalSlices
        where
            width = sliceWidth slice
            offset = sliceOffset slice

            select slices = selectOSlices slices offset width

            body (TeakOAppend count slices) = do
                flatSlices <- liftM (concat . replicate count . concat) $ mapM flattenOSlice slices
                addFlatSlices i flatSlices
                return $ select flatSlices
            body _ = return [(i, slice)]

    flattenOSliceAsTerm :: TeakOSlice -> OTermMonad TeakOSlice
    flattenOSliceAsTerm slice = do
        flatSlices <- flattenOSlice slice
        flatSlices' <- compactOSlices flatSlices
        case flatSlices' of
            [oneSlice] -> return oneSlice
            _ -> do
                appendIndex <- addTerm $ TeakOAppend 1 flatSlices'
                return (appendIndex, 0 +: (oSliceWidth slice))

    oSlicesAreAdjacent :: TeakOSlice -> TeakOSlice -> Bool
    oSlicesAreAdjacent (index1, slice1) (index2, slice2) =
        index1 == index2 && sliceHigh slice1 + 1 == sliceLow slice2

    joinAdjacentOSlices :: TeakOSlice -> TeakOSlice -> TeakOSlice
    joinAdjacentOSlices (index1, slice1) (_, slice2) = (index1,
        (sliceOffset slice1) +: (sliceWidth slice1 + sliceWidth slice2))

    -- compactOSlices : the given slices are to be appended, return those slices modified to
    --    insert constants refered to, appending adjacent constants and adjacent contiguous
    --    slices of other, non constant, terms
    compactOSlices :: [TeakOSlice] -> OTermMonad [TeakOSlice]
    compactOSlices slices = do
        (unfinishedConst, unfinishedSlices) <- foldM handleSlice ((0, 0), []) slices
        retSlices <- popConst unfinishedConst unfinishedSlices
        return $ reverse retSlices
        where
            popConst (0, _) retSlices = return retSlices
            popConst (width, num) retSlices = do
                constIndex <- addTerm $ TeakOConstant width num
                return $ (constIndex, 0 +: width) : retSlices

            pushConst (oldWidth, oldNum) (width, num) = return (oldWidth + width, oldNum + (num `shiftL` oldWidth))

            handleSlice (accConst, retSlices) oSlice@(index, slice) = do
                term <- lookupTerm index
                case term of
                    Just (TeakOConstant _ constNum) -> do
                        accConst' <- pushConst accConst
                            (sliceWidth slice, extractBitfield slice constNum)
                        return (accConst', retSlices)
                    _ -> do
                        oSlice' <- sliceSubst oSlice
                        retSlices' <- popConst accConst retSlices
                        let retSlices'' = case retSlices' of
                              (prevSlice:otherSlices) | oSlicesAreAdjacent prevSlice oSlice' ->
                                joinAdjacentOSlices prevSlice oSlice' : otherSlices
                              _ -> oSlice' : retSlices'
                        return ((0, 0), retSlices'')

    flattenOTermMuxs :: TeakOSlice -> [Implicant] -> TeakOSlice -> OTermMonad [([Implicant], TeakOSlice)]
    flattenOTermMuxs _ imps slice@(0, _) = return [(imps, slice)]
    flattenOTermMuxs choiceSlice imps slice@(i, _) = do
        Just term <- lookupResultTerm i
        case term of
            TeakOMux spec (subChoiceSlice:slices) | choiceSlice == subChoiceSlice ->
                liftM concat $ zipWithM (flattenOTermMuxs choiceSlice) spec slices
            _ -> return [(imps, slice)]

    removeUnusedOTerms :: [(Int, TeakOTerm)] -> [(Int, TeakOTerm)]
    removeUnusedOTerms terms = filter ((`elem` reachableTerms) . fst) terms
        where
            lastIndex = oTermsLastIndex terms
            reachableTerms = findReachableTerms terms lastIndex

    findReachableTerms :: [(Int, TeakOTerm)] -> Int -> [Int]
    findReachableTerms _ 0 = [0]
    findReachableTerms terms termNo = nub $ termNo : (concatMap
        (\(i, _) -> findReachableTerms terms i) $ oTermExtractSlices term)
        where Just term = lookup termNo terms

    shortenOTerms :: [(Int, TeakOTerm)] -> [(Int, TeakOTerm)]
    shortenOTerms terms = removeUnusedOTerms $ runOTermMonad "shortenOTerms" terms $ mapOrigTerms flattenTerm
        where
            lastOrigTermIndex = oTermsLastIndex terms

            flattenTerm (i, TeakOp op slices) = do
                slices' <- mapM flattenOSliceAsTerm slices
                addTermWithSubst i $ TeakOp op slices'
                return ()
            flattenTerm (i, TeakOBuiltin func width params slices) = do
                slices' <- mapM flattenOSliceAsTerm slices
                addTermWithSubst i $ TeakOBuiltin func width params slices'
                return ()
            flattenTerm (i, TeakOAppend count slices)
                | i == lastOrigTermIndex = do
                    slices' <- liftM concat $ mapM flattenOSlice $ concat $ replicate count slices
                    slices'' <- compactOSlices slices'
                    resultIndex <- lastResultTermIndex
                    lastTerm <- lookupResultTerm resultIndex
                    let
                        lastTermWidth = oTermResultWidth (fromJust lastTerm)
                        aliasSlices = [(resultIndex, 0 +: lastTermWidth)]
                    when (not (count == 1 && resultIndex /= 0 && slices'' == aliasSlices)) $ do
                        addTermWithSubst i $ TeakOAppend 1 slices''
                        return ()
                | otherwise = return ()
            flattenTerm (i, t@(TeakOConstant {}))
                | i == lastOrigTermIndex = addTermWithSubst i t >> return ()
                | otherwise = return ()
            flattenTerm (i, TeakOMux spec slices) = do
                (choiceSlice':slices') <- mapM flattenOSliceAsTerm slices
                specXslices'' <- liftM concat $ zipWithM (flattenOTermMuxs choiceSlice') spec slices'
                let (spec', slices'') = unzip specXslices''
                addTermWithSubst i $ TeakOMux spec' (choiceSlice':slices'')
                return ()
            -- flattenTerm (i, t) = addTermWithSubst i t

    -- nextLinks : return successor links which can be reached by transactions on `fromLink' and is:
    --    followJ == False -> solely required
    --    followJ == True -> required
    --    Don't pass through `alreadyVisited' links or the `fromLink' again.
    nextLinks :: NetworkIF network => Bool -> [NetworkLinkRef] -> NetworkLinkRef ->
        NetworkMonad network [NetworkLinkRef]
    nextLinks followJ alreadyVisited fromLink = nextLinksBody alreadyVisited fromLink
        where
            nextLinksBody visited link
                | link `elem` visited = return visited
                | otherwise = runMaybeTD visited' $ do
                    (passiveComp, port:subPort) <- MaybeT $ nwLinkToComp Passive link
                    case passiveComp of
                        -- TeakA
                        -- TeakM
                        TeakComp { nwTeakType = TeakJ {}, nwCompLinks = [Many inps, One out] }
                            -- AB experimental
                            | followJ || all (`elem` visited') inps -> follow [out]
                        TeakComp { nwTeakType = TeakF {}, nwCompLinks = [_, Many outs] } -> follow outs
                        TeakComp { nwTeakType = TeakO {}, nwCompLinks = [_, One out] } -> follow [out]
                        TeakComp { nwTeakType = TeakV {}, nwCompLinks = [Many _, Many wds, Many _, Many rds] }
                            | port == 0 -> follow [wds !! head subPort]
                            | port == 2 -> follow [rds !! head subPort]
                        -- TeakComp { nwTeakType = (TeakI {}), nwCompLinks = [_, One out] } -> follow [out]
                        -- TeakR
                        _ -> fail ""
                where
                    follow = lift . foldM nextLinksBody visited'
                    visited' = link : visited

    -- nextMLinks : find the `out' links of Ms whose inputs are all contained in `visited'
    nextMLinks :: NetworkIF network => [NetworkLinkRef] -> NetworkMonad network [NetworkLinkRef]
    nextMLinks visited = liftM (nub . catMaybes) $ mapM findMerge visited
        where
            findMerge link = do
                passiveComp <- nwLinkToComp Passive link
                case passiveComp of
                    Just (TeakComp { nwTeakType = TeakM, nwCompLinks = [Many inps, One out] }, _)
                        | out `notElem` visited && all (`elem` visited) inps -> return $ Just out
                    _ -> return Nothing

    -- prevLinks : return predecessor links which can cause a transaction on `fromLink'
    --    Don't pass through `alreadyVisited' links or the `fromLink' again
    prevLinks :: NetworkIF network => [NetworkLinkRef] -> NetworkLinkRef -> NetworkMonad network [NetworkLinkRef]
    prevLinks alreadyVisited fromLink = prevLinksBody alreadyVisited fromLink
        where
            prevLinksBody visited link
                | link `elem` visited = return visited
                | otherwise = runMaybeTD visited' $ do
                    (activeComp, port:subPort) <- MaybeT $ nwLinkToComp Active link
                    case activeComp of
                        -- TeakComp { nwTeakType = (TeakA {}), nwCompLinks = [Many inps, _] } -> follow inps
                        TeakComp { nwTeakType = TeakM {}, nwCompLinks = [Many inps, _] } -> follow inps
                        TeakComp { nwTeakType = TeakJ {}, nwCompLinks = [Many inps, _] } -> follow inps
                        TeakComp { nwTeakType = TeakF {}, nwCompLinks = [One inp, _] } -> follow [inp]
                        TeakComp { nwTeakType = TeakO {}, nwCompLinks = [One inp, _] } -> follow [inp]
                        TeakComp { nwTeakType = TeakS {}, nwCompLinks = [One inp, _] } -> follow [inp]
                        TeakComp { nwTeakType = TeakX {}, nwCompLinks = [Many inps, _, _] } -> follow inps
                        TeakComp { nwTeakType = TeakV {}, nwCompLinks = [Many wgs, _, Many rgs, _] }
                            | port == 0 -> follow [wgs !! head subPort]
                            | port == 2 -> follow [rgs !! head subPort]
                        -- TeakComp { nwTeakType = (TeakI {}), nwCompLinks = [One inp, _] } -> follow [inp]
                        -- TeakR
                        _ -> fail ""
                where
                    follow = lift . foldM prevLinksBody visited'
                    visited' = link : visited
