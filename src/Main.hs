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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main (
    main
    ) where

    import Bind
    import Context
    import Eval
    import Finish
    import Gen
    import Misc
    import NetParts
    import Gui
    import Options
    import ParseTree
    import Report
    import Print
    import Sim
    import SimTypes
    import Teak
    import Config
    import Latch
    import Plot
    import Layout
    import Balsa
    import Rule
    import Optim
    import Monitor
    import ToolOptions
    import SimPN
    import State
    import Dot

    import System.Environment
    import System.Exit
    import System.Time
    import System.FilePath
    import Data.List hiding (group)
    import System.Directory
    import Control.Monad
    import System.IO
    import System.IO.Error
    import Data.Maybe
    import Control.Monad.Trans
    import qualified Data.Map as DM
    import qualified Network

    type TeakNetwork = Network.Network

    main :: IO ()
    main = do
        args <- getArgs
        --print "renhongguang-have-fun-with-teak"
        --args <- return ["add.balsa"]
        --print args
        mainWithArgs args

    data CommandLineState = CommandLineState {
        -- Flow changing options
        clDoSimulate :: Bool,
        clDoOptimUsage :: Bool,
        clDoOptimise :: Bool,
        clDoGui :: Bool,
        clDoPlot :: Bool,
        clDoGates :: Bool,
        clDoPNTestCard :: Maybe String,
        clDoRulesTestCard :: Maybe (FilePath, String),
        -- Miscellaneous options
        clNetworkFile :: Maybe String,
        clOptimPasses :: Int,
        clReadDefaultRules :: Bool,
        clTopLevel :: Maybe String,
        clPrettyNetwork :: Bool,
        -- Flat command line
        clFlat :: String,
        -- All other options
        clOpts :: ToolOptions TeakNetwork,
        clEventFiles :: [FilePath]
        }

    defaultCommandLineState :: CommandLineState
    defaultCommandLineState = CommandLineState {
        clDoSimulate = False,
        clDoOptimUsage = False,
        clDoOptimise = False,
        clDoGui = False,
        clDoPlot = False,
        clDoGates = False,
        clDoPNTestCard = Nothing,
        clDoRulesTestCard = Nothing,
        clNetworkFile = Nothing,
        clOptimPasses = 10,
        clReadDefaultRules = False,
        clTopLevel = Nothing,
        clPrettyNetwork = False,
        clFlat = "",
        clOpts = defaultToolOptions,
        clEventFiles = []
        }

    modifyToolOptions :: CommandLineState -> (ToolOptions TeakNetwork -> ToolOptions TeakNetwork) -> CommandLineState
    modifyToolOptions state f = state { clOpts = f (clOpts state) }

    onClOpts :: (ToolOptions TeakNetwork -> [String] -> IO (ToolOptions TeakNetwork)) -> CommandLineState -> [String]
        -> IO CommandLineState
    onClOpts f state args = do
        opts' <- f (clOpts state) args
        return $ state { clOpts = opts' }

    clOptions :: CommandLineOptions CommandLineState
    clOptions = CommandLineOptions usage [
        -- General options
        CommandLineOption "verbose" 'v' [] "verbose output (for simulation, prints state changes)"
            (onClOpts $ \opts [] -> return $ opts { optVerbose = True }),
        CommandLineOption "top-level" 't' ["procedure"] "top most procedure for simulation, initial-opt"
            (\state [top] -> return $ state { clTopLevel = Just top }),
        CommandLineOption "include" 'I' ["directory"] "add <directory> to Balsa module path"
            (onClOpts $ \opts [path] -> return $ opts { optBalsaSearchPath = path:(optBalsaSearchPath opts) }),
        CommandLineOption "teak-opt" 'k' ["opt"] "component-generation sub-options"
            (onClOpts $ \opts [opt] -> do
                teakOpts' <- parseSubOptionsString teakOptionUsage teakUsage (optTeak opts) opt
                return $ opts { optTeak = teakOpts' }
                ),
        CommandLineOption "teak-usage" ' ' [] "display component-generation options usage"
            (\state [] -> teakUsage (optTeak $ clOpts state) "" >> return state),
        CommandLineOption "dump-parse-trees" ' ' [] "dump parse trees to .tree{,-bind,-eval,-finish} files"
            (onClOpts $ \opts [] -> return $ opts { optDumpParseTree = True }),
        -- Simulation
        CommandLineSeparator "",
        CommandLineOption "simulate" 'S' [] "simulate the last/top level procedure"
            (\state [] -> return $ state { clDoSimulate = True }),
        CommandLineOption "define" 'D' ["name", "value"] "bind <value> to <name> as a simulation defn."
            (onClOpts $ \opts [name, value] -> return $
                opts { optSimDefines = (name, value):(optSimDefines opts) }),
        CommandLineOption "pn-test-card" ' ' ["file"] "generate Petri net `test card' Dot file and exit"
            (\state [file] -> return $ state { clDoPNTestCard = Just file }),
        CommandLineOption "sim-opt" 's' ["opt"] "simulation sub-options"
            (onClOpts $ \opts [opt] -> do
                simOpts' <- parseSubOptionsString simOptionUsage simUsage (optSim opts) opt
                return $ opts { optSim = simOpts' }
                ),
        CommandLineOption "sim-usage" ' ' [] "display simulation sub-options usage"
            (\state [] -> simUsage (optSim $ clOpts state) "" >> return state),
        -- Network read/write
        CommandLineSeparator "",
        CommandLineOption "network-file" 'n' ["file"] "input network file name (rather than Balsa compilation)"
            (\state [file] -> return $ state { clNetworkFile = Just file }),
        CommandLineOption "output" 'o' ["file"] "output file name prefix (rather than using input file)"
            (onClOpts $ \opts [file] -> return $ opts { optBaseName = Just file }),
        CommandLineOption "pretty-network" ' ' [] "use `pretty' network format"
            (\state [] -> return $ state { clPrettyNetwork = True }),
        CommandLineSeparator "",
        -- Optimisation
        CommandLineOption "optimise" 'O' [] "optimise teak components."
            (\state [] -> return $ state { clDoOptimise = True }),
        CommandLineOption "named-opt" 'q' ["opt"]
            "apply named optimisation (`all' adds all, `none' removes all)"
            (\state [opt] -> do
                Why comp state' <- runWhyT $ do
                    state' <- readDefaultRules state
                    (otherOpts, enabledOptims) <- lift $ parseOptimSubOptions state' opt
                    return $ modifyToolOptions state' $ \opts -> opts
                        { optOptim = otherOpts, optEnabledOptims = enabledOptims }
                stop <- printCompleteness noPosContext comp
                when stop exitFailure
                return state'),
        CommandLineOption "rules" 'r' ["file"] "load (another) optimisation rules file (drops default rules)"
            (\state [file] -> do
                Why comp opts' <- runWhyT $ appendRulesFile (clOpts state) file
                stop <- printCompleteness noPosContext comp
                when stop exitFailure
                return $ state { clOpts = opts', clReadDefaultRules = True }
                ),
        CommandLineOption "rules-test-card" ' ' ["file", "sectionword"] "print rules test card"
            (\state [file, sectionWord] -> return state { clDoRulesTestCard = Just (file, sectionWord) }),
        CommandLineOption "optim-passes" ' ' ["int"] "perform this many optimisation passes"
            (\state [passes] -> return $ state { clOptimPasses = (read passes :: Int) }),
        CommandLineOption "optim-usage" ' ' [] "display optimisation usage"
            (\state [] -> do return state { clDoOptimUsage = True } ),
        -- Latching
        CommandLineSeparator "",
        CommandLineOption "latch" 'L' [] "insert latches" (onClOpts $ \opts [] ->
            return $ opts { optDoLatch = True }),
        CommandLineOption "latch-opt" 'l' ["opt"] "latch sub-options"
            (onClOpts $ \opts [opt] -> do
                latchOpts' <- parseSubOptionsString latchOptionUsage latchUsage (optLatch opts) opt
                return $ opts { optLatch = latchOpts' }
                ),
        CommandLineOption "latch-usage" ' ' [] "display latch options usage"
            (\state [] -> latchUsage (optLatch $ clOpts state) "" >> return state),
        -- Plotting
        CommandLineSeparator "",
        CommandLineOption "plot" ' ' [] "make a postscript plot of the compiled parts"
            (\state [] -> return $ state { clDoPlot = True }),
        CommandLineOption "plot-opt" 'p' ["opt"] "plot sub-options"
            (onClOpts $ \opts [opt] -> do
                plotOpts' <- parseSubOptionsString plotOptionUsage plotUsage (optPlot opts) opt
                return $ opts { optPlot = plotOpts' }),
        CommandLineOption "plot-usage" ' ' [] "display plot options usage"
            (\state [] -> plotUsage (optPlot $ clOpts state) "" >> return state),
        -- Gate-level netlisting
        CommandLineSeparator "",
        CommandLineOption "gates" ' ' [] "make a gate level netlist"
            (\state [] -> return $ state { clDoGates = True }),
        CommandLineOption "technology" 'X' ["tech"] "technology for gate level netlist"
            (onClOpts $ \opts [tech] -> return $ opts { optTech = tech }),
        CommandLineOption "test-protocol" ' ' [] "test channel protocol violations in Verilog"
            (onClOpts $ \opts [] -> return $ opts {
                optGenPartToGate = GenPartToGateProtocolTest : optGenPartToGate opts }),
        CommandLineOption "gates-usage" ' ' [] "display gates options usage"
            (\state [] -> gatesUsage state "" >> return state),
        -- Operating modes other than just compile->plot->gates
        CommandLineSeparator "",
        CommandLineOption "gui" ' ' [] "launch GUI"
            (\state [] -> return $ state { clDoGui = True }),
        CommandLineOption "gui-opt" 'g' ["opt"] "gui sub-options"
            (onClOpts $ \opts [opt] -> do
                guiOpts' <- parseSubOptionsString guiOptionUsage guiUsage (optGui opts) opt
                return $ opts { optGui = guiOpts' }),
        CommandLineOption "monitor-events" 'e' ["file"] "read monitor events file"
            (\state [file] -> return $ state { clEventFiles = file : clEventFiles state }),
        CommandLineOption "gui-usage" ' ' [] "display gui options usage"
            (\state [] -> guiUsage (optGui $ clOpts state) "" >> return state)
        ]

    header :: IO ()
    header = do
        putStrLn ""
        putStrLn "   _ _|_  _  _  |    eTeak synthesiser for the Balsa language"
        putStrLn $ "|_| |  |_| _| |/ . version: " ++ teakVersion
        putStrLn "  |_  |_ |_ |_| |\\ . (C) 2007-2016 The University of Manchester"
        putStrLn ""
        putStrLn "  This program comes with ABSOLUTELY NO WARRANTY;"
        putStrLn "  This is free software, and you are welcome to redistribute it"
        putStrLn "  under the conditions of version 3 (or greater) of the GNU GPL"
        putStrLn ""
        putStrLn "  (Graph library (C) 2002-2004 The University of Glasgow)"
        putStrLn ""

    usage :: String -> IO ()
    usage message = do
        header
        progName <- getProgName
        putStrLn $ "  usage: " ++ progName ++ " <switch>* [<file>]"
        putStrLn ""
        putStrLn "  switch:"
        putStrLn $ showOptionUsage "    " "    " clOptions
        when (message /= "") $ putStrLn $ "*** " ++ message ++ "\n"
        exitFailure

    optimsToSubOptionUsages :: [Optim TeakNetwork] -> [RuleGroup] -> SubOptionUsages (OptimOption TeakNetwork)
    optimsToSubOptionUsages optims groups = SubOptionUsages "optimisation" show (Just []) Nothing $
        (if null groups then [] else [allRule, noneRule] ++ map groupToSOU groups) ++ map optimToSOU optims
        where
            optimToSOU optim = (optimName optim, boolSubOption (optimDescription optim) (OptimOptim optim))
            groupToSOU group = (ruleGroupName group, SubOptionUsage True "" (ruleGroupDescription group)
                "" (const True) (const (map OptimOptim $ groupToOptims group, [])))

            allRule = ("all", SubOptionUsage True "" "all rules" "" (const True) (const (map OptimOptim optims, [])))
            noneRule = ("none", SubOptionUsage True "" "no rules" "" (const True) (const ([], map OptimOptim optims)))

            findOptims name
                | isJust optim = [fromJust optim]
                | isJust group = groupToOptims $ fromJust group
                | otherwise = []
                where
                    optim = find (isOptim name) optims
                    group = find ((== name) . ruleGroupName) groups

            groupToOptims group = concatMap findOptims $ ruleGroupMembers group

    optimiseUsage :: RuleSet TeakNetwork -> [Optim TeakNetwork] -> [OptimOption TeakNetwork] -> String -> IO ()
    optimiseUsage ruleSet allOptims opts message = do
        header
        let groups = ruleSetGroups ruleSet
        when (not (null groups)) $ do
            putStrLn "  Option group abbreviations\n"
            let
                names = map makeName groups
                descs = map (\group -> ruleGroupDescription group
                    ++ " (" ++ joinWith "," (ruleGroupMembers group) ++ ")") groups

                makeName group = (if optimOn then "   " else "no-") ++ ruleGroupName group
                    where optimOn = all (\name -> isJust (find (isOptimName name) opts)) $
                            ruleGroupMembers group

                isOptimName name (OptimOptim optim) = optimName optim == name
                isOptimName _ _ = False

            mapM_ putStrLn $ columnFormat ["   all":"   none":names, "all rules":"no rules":descs]
                ["  ", " -- "] ["  ", "    "]
        putStrLn ""
        subOptionsUsage (appendSubOptionUsages optimOptionUsage (optimsToSubOptionUsages allOptims []))
            opts "-q" message
        exitFailure

    plotUsage :: [PlotOption] -> String -> IO ()
    plotUsage opts message = do
        header
        subOptionsUsage plotOptionUsage opts "-p" message
        exitFailure

    guiUsage :: [GuiOption] -> String -> IO ()
    guiUsage opts message = do
        header
        subOptionsUsage guiOptionUsage opts "-g" message
        exitFailure

    latchUsage :: [LatchOption] -> String -> IO ()
    latchUsage opts message = do
        header
        subOptionsUsage latchOptionUsage opts "-l" message
        exitFailure

    teakUsage :: [TeakOption] -> String -> IO ()
    teakUsage opts message = do
        header
        subOptionsUsage teakOptionUsage opts "-k" message
        exitFailure

    simUsage :: [SimOption] -> String -> IO ()
    simUsage opts message = do
        header
        subOptionsUsage simOptionUsage opts "-s" message
        exitFailure

    gatesUsage :: CommandLineState -> String -> IO ()
    gatesUsage state _message = do
        header
        techs <- teakFindTechs $ clOpts state
        putStrLn $ "  Available technologies: " ++ joinWith ", " techs
        putStrLn ""
        exitFailure

    readDefaultRules :: CommandLineState -> WhyT IO CommandLineState
    readDefaultRules state = do
        optimPath <- lift $ teakOptimPath
        let defaultRulesFile = optimPath </> "default" <.> "rules"
        if not (clReadDefaultRules state)
            then do
                opts' <- appendRulesFile (clOpts state) defaultRulesFile
                return $ state { clReadDefaultRules = True, clOpts = opts' }
            else return state

    readDefaultRulesIO :: CommandLineState -> IO CommandLineState
    readDefaultRulesIO state = do
        Why comp state' <- runWhyT $ readDefaultRules state
        stop <- printCompleteness noPosContext comp
        when stop exitFailure
        return state'

    appendRulesFile :: ToolOptions TeakNetwork -> FilePath -> WhyT IO (ToolOptions TeakNetwork)
    appendRulesFile opts file = do
        ruleSet <- readRulesFile (optVerbose opts) file
        let
            rules' = concatRuleSets [ruleSet, optRules opts]
            optims = map ruleToOptim $ ruleSetRules rules'

        return $ opts { optRules = rules',
            optEnabledOptims = optEnabledOptims opts ++ filter optimOnByDefault optims,
            optAllOptims = optAllOptims opts ++ optims
            }

    readRulesFile :: Bool -> String -> WhyT IO (RuleSet TeakNetwork)
    readRulesFile verbose file = do
        exists <- lift $ doesFileExist file
        if exists
            then do
                contents <- lift $ readFile file
                let comp@(Why _ ruleSet) = parseRules (PosLC (PosFile file (ImportFile file)) 1 1) contents
                when verbose $ lift $ putStrLn $ "*** read " ++ summariseRuleSet ruleSet (" from file " ++ file)
                {-
                lift $ mapM (\rule -> do
                    print $ ruleName rule
                    mapM_ print $ ruleFromPatternLinks rule
                    -- mapM_ print $ ruleFromLeadsTos rule
                    print "From"
                    mapM_ (\comp -> do
                        print $ compPatternName comp
                        mapM_ print $ compPatternPortPatterns comp
                        ) $ ruleFrom rule
                    print "To"
                    mapM print $ ruleTo rule
                    ) $ ruleSetRules ruleSet
                    -}
                WhyT $ return comp
            else failPos PosTopLevel $ "can't open optimisation rules file `" ++ file ++ "'"

    parseOptimSubOptions :: CommandLineState -> String -> IO ([OptimOption TeakNetwork], [Optim TeakNetwork])
    parseOptimSubOptions state optString = do
        subOpts <- parseSubOptionsString
            (appendSubOptionUsages optimOptionUsage
                (optimsToSubOptionUsages (optAllOptims opts) (ruleSetGroups (optRules opts))))
            (optimiseUsage (optRules opts) (optAllOptims opts))
            (optOptim opts ++ (map OptimOptim (optEnabledOptims opts)))
            optString
        return $ separateOptimOptions subOpts
        where opts = clOpts state

    mainWithArgs :: [String] -> IO ()
    mainWithArgs args = do
        teakLibrary <- teakLibraryPath
        let state0 = modifyToolOptions defaultCommandLineState $ setTeakHome teakLibrary
        (stateWithoutFlat, remainingArgs) <- parseOptions clOptions state0 args

        let
            flatArgs = joinWith " " args
            state = stateWithoutFlat { clFlat = flatArgs }

        when (isJust $ clDoPNTestCard state) $ do
            pnTestCard $ fromJust $ clDoPNTestCard state
            exitWith ExitSuccess

        when (clDoGui state && clDoSimulate state) $
            usage "Don't use --simulate and --gui together"

        when (clDoGui state && (clDoGates state || clDoPlot state)) $
            usage "Don't use --plot, --gate, --simulate with --gui"

        when (clDoSimulate state && (clDoGates state || clDoPlot state)) $
            usage "Don't use --plot, --gate, with --simulate"

        when (isJust $ clDoRulesTestCard state) $ do
            state' <- readDefaultRulesIO state
            let Just (file, sectionWord) = clDoRulesTestCard state'
            rulesTestCard (optRules $ clOpts state') file sectionWord
            exitWith ExitSuccess

        when (clDoOptimUsage state) $ do
            state' <- readDefaultRulesIO state
            optimiseUsage (optRules $ clOpts state')
                (optAllOptims $ clOpts state')
                (optOptim (clOpts state') ++ (map OptimOptim (optEnabledOptims $ clOpts state'))) ""

        compileFlow state remainingArgs

    putTimeStamped :: String -> IO ()
    putTimeStamped str = do
        time <- getClockTime
        putStrLn $ show time ++ ": " ++ str

    compileFlow :: CommandLineState -> [String] -> IO ()
    compileFlow state remainingArgs = do
        let
            searchPath = optBalsaSearchPath $ clOpts state
            networkFile = clNetworkFile state
            verbose = optVerbose $ clOpts state

            putVerboseObj Nothing str = putVerbose str
            putVerboseObj (Just obj) str = when verbose $ do
                let len = show $ length $ show obj
                putStr $ "*** (" ++ len ++ ") "
                putTimeStamped str

            putVerbose str = when verbose $ do
                putStr $ "*** "
                putTimeStamped str

        (contextUnbound, state') <- case remainingArgs of
            [] | clDoGui state -> return (emptyContext, state)
            [] | isJust networkFile -> return (emptyContext, state)
            [inputFile]
                | isNothing networkFile && isSuffixOf ".teak" inputFile -> do
                    -- Use this as the input network.  Don't set base name
                    return (emptyContext, state { clNetworkFile = Just inputFile })
                | isNothing networkFile -> do
                    let baseFilename = fromMaybe (dropSuffix ".balsa" inputFile) $ optBaseName $ clOpts state
                    Why r balsaContext <- runWhyT $ parseBalsaFile searchPath inputFile
                    putVerboseObj (Just balsaContext) "Parsed Balsa input (if any)"
                    stop <- printCompleteness (Just balsaContext) r
                    when stop exitFailure
                    return (balsaContext, state {
                        clOpts = (clOpts state) { optBaseName = Just baseFilename } })
            _ -> do
                usage "pass exactly one <file> argument, or a network file, or use --gui"
                return (emptyContext, state)

        when (optDumpParseTree (clOpts state') && isNothing (optBaseName (clOpts state'))) $
            usage "Must have a base filename (use -o) to dump parse trees"

        contextFinish <- do
            let
                Why bindingR contextBound = bind contextUnbound
                Why evalR contextEval = evalContext contextBound
                contextFinish = finish contextEval

                dumpParseTree = optDumpParseTree $ clOpts state'

                treeFilename = fromJust (optBaseName (clOpts state')) <.> "tree"

                writeTree suffix context = do
                    treeFile <- openFile (treeFilename ++ suffix) WriteMode
                    hPutStrLn treeFile $ showTree context
                    hClose treeFile

            when dumpParseTree $ writeTree "-parse" contextUnbound

            bindingStop <- printCompleteness (Just contextBound) bindingR
            when bindingStop exitFailure

            when dumpParseTree $ do
                writeTree "-bind" contextBound
                writeTree "-eval" contextEval

            evalStop <- printCompleteness (Just contextEval) evalR
            when evalStop exitFailure

            when dumpParseTree $ writeTree "-finish" contextFinish

            return contextFinish

        putVerboseObj (Just contextFinish) "Finished elaboration"

        let
            SimType simType = getSubOption (optSim $ clOpts state') $ SimType SimTypeLanguage

        if clDoSimulate state' && simType == SimTypeLanguage
            then simulateFlow state' contextFinish
            -- Actually committing to Teak network/gates
            else do
                state'' <- readDefaultRulesIO state'
                backendFlow state'' verbose putVerbose putVerboseObj contextFinish

    simulateFlow :: CommandLineState -> Context Decl -> IO ()
    simulateFlow state context = do
        let
            maybeTopLevelName = clTopLevel state
            Just topLevelName = maybeTopLevelName
            maybeRef = findRefByName [context] ProcNamespace topLevelName

        let maybeSimWritePN = findSubOption (optSim $ clOpts state) $ SimWritePN ""
        when (isJust maybeSimWritePN) $ putStrLn $
            "*** Can't output a Petri net, wrong simulation type, use -s type=pn"

        maybeBinding <- if isJust maybeTopLevelName
            then if isJust maybeRef
                then return $ Just $ findBindingByRef [context] $ fromJust maybeRef
                else do
                    usage $ "No procedure `" ++ topLevelName ++ "' to simulate"
                    return Nothing
            else if isEmptyContext context
                then do
                    usage "No top-level procedures to simulate"
                    return Nothing
                else return $ Just $ findBindingByRef [context] $ Ref $ contextsLastIndex [context]

        when (isJust maybeBinding && findBoolSubOption (optSim $ clOpts state) SimSimulate) $ do
            let Just binding = maybeBinding
            putStrLn $ "*** Simulating BEGIN: " ++ bindingName binding
            simulate (optSimDefines $ clOpts state) (optSim $ clOpts state) context binding
            putStrLn $ "*** Simulating END " ++ bindingName binding

    -- backendFlow : things to do with parse trees to generate reports, Teak component implementations
    backendFlow :: CommandLineState -> Bool -> (String -> IO ()) ->
        (Maybe [Part TeakNetwork] -> String -> IO ()) -> Context Decl -> IO ()
    backendFlow state verbose putVerbose putVerboseObj context = do
        let networkFile = clNetworkFile state

        Why compileComp compiledParts <- if isJust networkFile
            then do
                Why comp readParts <- runWhyT $ readNetworkFile (fromJust networkFile)
                stop <- printCompleteness noPosContext comp
                when stop exitFailure
                return $ Why Complete (readParts :: [Part TeakNetwork])
            else return $ teak (optTeak $ clOpts state) context
        stop <- printCompleteness (Just context) compileComp
        when stop exitFailure

        putVerboseObj (Just compiledParts) "Finished compilation/reading network"

        finalParts <- do
            let
                maybeTopLevelName = clTopLevel state
                topLevelName = fromJust maybeTopLevelName

                unaliasedParts = map ((flip runPart_) networkRemoveAliases) compiledParts

            when (isJust maybeTopLevelName && isNothing (nwFindPart unaliasedParts topLevelName)) $
                usage $ "can't find top-level: `" ++ topLevelName ++ "'"

            uniqueParts <- if isJust maybeTopLevelName
                then do
                    let uniqueParts = uniquifyPart unaliasedParts topLevelName
                    hFlush stdout
                    return $ removeGo uniqueParts topLevelName
                else return unaliasedParts

            let
                optimisePart :: [Part TeakNetwork] -> Part TeakNetwork -> IO (Part TeakNetwork)
                optimisePart parts part = do
                    -- FIXME, make localOptims stuff a part of Balsa.hs
                    optimisations <- case localOptims of
                        Just (TeakParamString str) -> liftM snd $ parseOptimSubOptions state str
                        Nothing -> return $ optEnabledOptims $ clOpts state
                        _ -> do
                            optUsage "bad parameter type, must be string"
                            return $ optEnabledOptims $ clOpts state
                    let
                        optimContext = OptimContext { optimContextParts = parts }

                        optimLog _part comp optim = putStrLn $
                            (show (refComp comp)) ++ " " ++ optimName optim

                        optRun partIn runNo = do
                            putVerboseObj (Just [partIn]) $ "Run " ++ show runNo
                            Why comp partOut <- runWhyT $ applyOptims optimisations optimContext partIn
                                (if verbose && findBoolSubOption (optOptim $ clOpts state) OptimVerbose
                                    then optimLog
                                    else noOptimLog)
                            stop <- printCompleteness noPosContext comp
                            when stop exitFailure
                            return partOut
                    putVerbose $ "Optimising part " ++ networkName part
                    part' <- foldM optRun part [1..clOptimPasses state]
                    putVerboseObj (Just [part']) $ "Optimised " ++ networkName part
                    return part'
                    where
                        localOptims = tryPart part $ nwGetAttributeVal "optim"

                        optUsage msg = putStrLn $ "*** in part `" ++ networkName part ++
                            "' " ++ msg

                optimisePartsElem parts (i, part) = do
                    part' <- optimisePart parts part
                    return $ replaceAt parts i part'

            optimisedParts <- if clDoOptimise state
                then do
                    untrimmedOptimisedParts <- foldM optimisePartsElem uniqueParts $ zip [0..] uniqueParts
                    -- remove unused parts
                    return $ if isJust maybeTopLevelName
                        then uniquifyPart untrimmedOptimisedParts topLevelName
                        else untrimmedOptimisedParts
                else return uniqueParts

            putVerboseObj (Just optimisedParts) "Finished optimisation "

            finalParts <- if not (optDoLatch (clOpts state))
                then return optimisedParts
                else do
                    let
                        latchOpts = optLatch $ clOpts state

                        Why comp latchedParts = gatherFail $ map
                            (\part -> runWhyTPart_ part $ nwInsertLatches latchOpts) optimisedParts

                    exit <- printCompleteness noPosContext comp
                    when exit exitFailure

                    let
                        doscp = (findBoolSubOption latchOpts LatchDoSCP) -- || findBoolSubOption latchOpts (LatchSCPLimit 3000)
                    return latchedParts
{-

                    putVerboseObj (Just latchedParts) "Finished latch insertion"
                    print latchOpts
                    print $ findBoolSubOption latchOpts LatchDoSCP
                    print $ findBoolSubOption latchOpts (LatchSCPLimit 3000)
                    print $ findSubOption latchOpts (LatchSCPLimit undefined)
                    return latchedParts
-}
            return finalParts

        putVerboseObj (Just finalParts) "Checking networks"

        do -- check
            let comp = gatherCompleteness $ map checkPart finalParts
            _exit <- printCompleteness noPosContext comp
            -- exitFailure
            return ()

        putVerboseObj (Just finalParts) "Finished generating networks"

        if clDoGui state
            then do
                -- Read event files first
                let
                    readEventFile :: FilePath -> IO PartMonitorEvents
                    readEventFile file = do
                        Why comp events <- runWhyT $ readMonitorFile finalParts file
                        stop <- printCompleteness noPosContext comp
                        when stop exitFailure
                        return events

                events <- case clEventFiles state of
                    [] -> return []
                    [file] -> readEventFile file
                    _ -> do
                        usage "more than one monitor events file given"
                        return []

                let opts = (clOpts state) { optPartMonitorEvents = events }
                gui opts finalParts
                return ()
            else backend2 state verbose putVerbose finalParts

    backend2 :: CommandLineState -> Bool -> (String -> IO ()) -> [Part TeakNetwork] -> IO ()
    backend2 state verbose putVerbose partsIn = do
        let
            baseFilename = optBaseName $ clOpts state

            writeTeak parts = do
                let teakFilename = fromJust baseFilename ++ ".teak"
                if isNothing baseFilename
                    then putVerbose "Must have a base filename (use -o) to write network files, skipping"
                    else do
                        putVerbose $ "Writing Teak network file " ++ teakFilename
                        writeNetworkFile (clPrettyNetwork state) (clFlat state) teakFilename parts
                        putVerbose $ "Finished writing Teak network file"

        writeTeak partsIn

        when (clDoGates state) $ do
            when (isNothing baseFilename) $ usage "Must have a base filename (use -o) to write gate netlists"
            progName <- getProgName
            Why comp techMapping <- runWhyT $
                teakFindTechMapping (clOpts state) (optTech $ clOpts state)
            case comp of
                Complete -> genMakeGatesFile verbose (optGenPartToGate $ clOpts state)
                    ("from command: " ++ progName ++ " " ++ clFlat state)
                    techMapping -- (optTech $ clOpts state)
                    (fromJust baseFilename ++ ".v")
                    partsIn
                _ -> do
                    stop <- printCompleteness noPosContext comp
                    when stop exitFailure

        when (clDoPlot state) $ do
            let
                plotOpts = optPlot $ clOpts state
                trimmedParts = map (trimPartForPartialPlot plotOpts) partsIn
            plotParts (fromJust baseFilename) True plotOpts verbose
                (\opts part -> partGraphicalInfo False opts (networkName part) part)
                Nothing
                trimmedParts

        let SimType simType = getSubOption (optSim $ clOpts state) $ SimType SimTypeLanguage
        when (clDoSimulate state && simType == SimTypePN) $ do
            putVerbose "Petri net simulation"
            let
                maybeTopLevelName = clTopLevel state
                Just topLevelName = maybeTopLevelName
                maybeTopLevelPart = nwFindPart partsIn topLevelName

            Just part <- if isJust maybeTopLevelName
                then if isJust maybeTopLevelPart
                    then return maybeTopLevelPart
                    else do
                        usage $ "No procedure `" ++ topLevelName ++ "' to simulate"
                        return Nothing
                else if null partsIn
                    then do
                        usage "No top-level procedures to simulate"
                        return Nothing
                    else return $ Just $ last partsIn

            let SimWriteEvents logFilename = getSubOption (optSim $ clOpts state) $ SimWriteEvents "/dev/null"
            logFile <- openFile logFilename WriteMode

            let
                pn = makeTopLevelPartPN partsIn part
                setTime = simFlowSet TimeRef . TimeState

                initState = [[(TimeRef, TimeState 0),
                    (DefinesRef, DefinesState $ optSimDefines $ clOpts state),
                    (LogFileRef, FileState logFile)]]

                isNonEmptyMarking (_, marking) = length marking /= 0

                simTimeRange = [0..200000]

                maybeSimWritePN = findSubOption (optSim $ clOpts state) $ SimWritePN ""
                SimWritePNFormat format = getSubOption (optSim $ clOpts state) $ SimWritePNFormat SimWritePNDot

            when (isJust maybeSimWritePN) $ do
                putVerbose "Writing Petri net file"
                let Just (SimWritePN file) = maybeSimWritePN
                case format of
                    SimWritePNDot -> writeGraphsToFile file [
                        -- dotGraphGroupNodesByLabel (Just 1) [] $
                        pnToDot Nothing (networkName part) pn]
                    SimWritePNHLNet -> writeHLNet file (networkName part) pn
                putVerbose "Finished writing Petri net file"

            when (findBoolSubOption (optSim $ clOpts state) SimSimulate) $ do
                let otherPorts = filter (fromMaybe False . liftM (`notElem` [GoAccess, DoneAccess]) . nwPortRef) $
                      networkPorts part
                when (not (null otherPorts)) $ usage "Top-level part must have no ports"

                let marking0 = pnInitialMarking pn
                (marking', _) <- runStateT (runPN setTime pn marking0 simTimeRange) initState
                hClose logFile

                mapM_ (putStrLn . (\placeMarking -> showMarkingElem pn placeMarking "")) $
                    filter isNonEmptyMarking $ DM.assocs marking'
