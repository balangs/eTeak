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

module Sim (
	SimOption (..),
	defaultSimOptions,
	simOptionUsage,
	SimType (..),
	SimWritePNFormat (..),
	simulate
	) where

	import ParseTree
	import Context
	import Bits
	import Type
	import Misc
	import Traverse
	import SimTypes
	import SimBuiltin
	import Report
	import Show
	import Print
	import Options

	import Data.Maybe
	import Data.Ix
	import Control.Monad
	import State
	import Control.Monad.Trans
	import Data.Bits

	data SimOption =
		  SimVerbose
		| SimSimulate
		| SimChanCapacity Int
		| SimType SimType
		| SimWritePN FilePath
		| SimWritePNFormat SimWritePNFormat
		| SimWriteEvents FilePath
		deriving Show

	data SimType = SimTypeLanguage | SimTypePN
		deriving Eq

	data SimWritePNFormat = SimWritePNDot | SimWritePNHLNet
		deriving Eq

	instance Show SimType where
		showsPrec _ SimTypeLanguage = showString "language"
		showsPrec _ SimTypePN = showString "pn"

	instance Read SimType where
		readsPrec _ str = maybeToList $ do
			(token, rest) <- maybeLex str
			case token of
				"language" -> return (SimTypeLanguage, rest)
				"pn" -> return (SimTypePN, rest)
				_ -> fail ""

	instance Show SimWritePNFormat where
		showsPrec _ SimWritePNDot = showString "dot"
		showsPrec _ SimWritePNHLNet = showString "hl_net"

	instance Read SimWritePNFormat where
		readsPrec _ str = maybeToList $ do
			(token, rest) <- maybeLex str
			case token of
				"dot" -> return (SimWritePNDot, rest)
				"hl_net" -> return (SimWritePNHLNet, rest)
				_ -> fail ""

	instance SubOption SimOption where
		matchSubOption (SimChanCapacity {}) (SimChanCapacity {}) = True
		matchSubOption SimVerbose SimVerbose = True
		matchSubOption SimSimulate SimSimulate = True
		matchSubOption (SimType {}) (SimType {}) = True
		matchSubOption (SimWritePN {}) (SimWritePN {}) = True
		matchSubOption (SimWriteEvents {}) (SimWriteEvents {}) = True
		matchSubOption (SimWritePNFormat {}) (SimWritePNFormat {}) = True
		matchSubOption _ _ = False

	showSimOptionValue :: SimOption -> String
	showSimOptionValue (SimChanCapacity cap) = show cap
	showSimOptionValue (SimType typ) = show typ
	showSimOptionValue (SimWritePN file) = file
	showSimOptionValue (SimWriteEvents file) = file
	showSimOptionValue (SimWritePNFormat format) = show format
	showSimOptionValue _ = ""

	parseInt :: String -> Maybe Int
	parseInt arg = case (reads :: String -> [(Int, String)]) arg of
		[(int, "")] -> Just int
		_ -> Nothing

	defaultSimOptions :: [SimOption]
	defaultSimOptions = [SimChanCapacity 3, SimType SimTypeLanguage, SimWritePNFormat SimWritePNDot,
		SimSimulate]

	simOptionUsage :: SubOptionUsages SimOption
	simOptionUsage = SubOptionUsages "sim" showSimOptionValue (Just []) (Just defaultSimOptions) [
		("verbose", boolSubOption "verbosely report state changes" SimVerbose),
		("capacity", SubOptionUsage False "capacity" "channel token capacity" "3"
			(isJust . parseInt) (\arg -> ([SimChanCapacity $ fromJust $ parseInt arg], []))),
		("type", SubOptionUsage False "type" "simulation type (language|pn)" "language"
			(isJust . listToMaybe . (reads :: String -> [(SimType, String)])) (\arg -> ([SimType $ read arg], []))),
		("write-pn", SubOptionUsage False "file" "Generate file with the simulation Petri net" "pn.dot"
			(const True) (\arg -> ([SimWritePN arg], []))),
		("write-pn-format", SubOptionUsage False "format" "Format for Petri net file (dot|hl_net)" "dot"
			(isJust . listToMaybe . (reads :: String -> [(SimWritePNFormat, String)]))
			(\arg -> ([SimWritePNFormat $ read arg], []))),
		("write-events", SubOptionUsage False "file" "Write a monitor events file from link activity in a PN sim."
			"pn.report"
			(const True) (\arg -> ([SimWriteEvents arg], []))),
		("simulate", boolSubOption "actually simulate (no- means just do other sim. activities, such as writing files)"
			SimSimulate)
		]

	inlineCalls :: ParseTreeTraverse node => node -> node
	inlineCalls node = node'
		where
			(_, _, node') = traverse inlineApply [] () node

			inlineApply = emptyApply { applyContextCons = (:), preCmd = preNoAccum inlinePreCmd }

			inlinePreCmd cs (SharedCallCmd _ (Callable ref)) = (cs, declCmd $ bindingValue $ findBindingByRef cs ref)
			inlinePreCmd cs cmd = (cs, cmd)

	simulate :: [(String, String)] -> [SimOption] -> Context Decl -> Binding Decl -> IO ()
	simulate defines opts context binding = body $ bindingValue binding
		where
			body (ProcDecl _ ports _ cmd)
				| isEmptyContext ports = do
					putStrLn $ "+++ Simulating " ++ bindingName binding ++ ":"
					simRep defines opts context (inlineCalls cmd)
			body _ = putStrLn $ "+++ Simulation top level must be a procedure with no ports"

	simRep :: [(String, String)] -> [SimOption] -> Context Decl -> Cmd -> IO ()
	simRep defines opts context cmd = do
		runStateT (do
			state <- cmdInitialState context cmd
			ChanCapacityState capacity <- simFlowGet ChanCapacityRef
			when (verbose) $ lift $ putStrLn $ "Channel capacity: " ++ show capacity
			-- lift $ putStrLn $ "Init: " ++ take 120 (show state)
			body state)
			[initGlobals]
		return ()
		where
			SimChanCapacity chanCapacity = getSubOption opts (SimChanCapacity 3)
			verbose = findBoolSubOption opts SimVerbose

			initGlobals = [
				(RunRef, RunState True),
				(TimeRef, TimeState 0),
				(DefinesRef, DefinesState defines),
				(UidCountRef, UidState 0),
				(ChanCapacityRef, ChanCapacityState chanCapacity)]

			body state = do
				TimeState time <- simFlowGet TimeRef
				(done, state') <- simCmd state cmd
				simFlowSet TimeRef (TimeState (time + 1))
				-- This may be *really* inefficient but it's so beautiful
				-- It doesn't handle livelock, but what does!
				RunState run <- simFlowGet RunRef
				case (done, run, state == state') of
					(Done, _, _) -> do
						lift $ putStrLn $ "*** Done at step " ++ show time
						return ()
					(Redo, _, True) -> do
						lift $ putStrLn $ "*** No more progress at step " ++ show time
					(Redo, False, False) -> do
						lift $ putStrLn $ "BalsaSimulationStop called at step " ++ show time
					(Redo, True, False) -> do
						let diff = diffCmdState time context cmd state state'
						when (verbose && not (null diff)) $ do
							lift $ putStrLn $ "DIFF " ++ show time ++ "\n" ++ joinWith "\n" diff
						body state'

	findGuard :: Int -> Integer -> [[Implicant]] -> Maybe Int
	findGuard width test impss = body 0 impss
		where
			body _ [] = Nothing
			body i (imps:impss)
				| guardMatches = Just i
				| otherwise = body (i + 1) impss
				where guardMatches = any (impIsCoveredByImp width (Imp test 0)) imps

	simExpr :: Expr -> SimFlow SimValue
	simExpr (ValueExpr _ typ value) = return $ valueToSimValue typ value
	simExpr (VarRead _ _ slice ref) = do
		value <- varRead ref
		return $ simExtractBitfield slice value
	simExpr (OpenChanRead _ _ slice ref) = do
		value <- varRead ref
		return $ simExtractBitfield slice value
	simExpr expr@(BuiltinCallExpr _ _ _ args _) = do
		argsValues <- mapM simExpr args
		simBuiltinCall argsValues expr
	simExpr (CaseExpr _ test impss exprs) = do
		testValue <- simExpr test
		let
			int = svToInt testValue
			width = widthOfType [] $ typeOfExpr [] test
			guard = findGuard width int impss

		when (isNothing guard) $
			error $ "No matching expr guard for implicants " ++ show impss ++ " with test value " ++ show int

		simExpr $ exprs !! fromJust guard
	simExpr expr = do
		argsValues <- mapM simExpr $ subExprs expr
		return $ simPureExpr expr argsValues

	simLvalue :: Lvalue -> SimValue -> SimFlow ()
	simLvalue (VarWrite _ _ slice ref) value = do
		readValue <- varRead ref
		let value' = simInsertBitfield slice readValue value
		varWrite ref value'
	simLvalue (CaseLvalue _ expr impss lvalues) value = do
		exprValue <- simExpr expr
		let
			int = svToInt exprValue
			width = widthOfType [] $ typeOfExpr [] expr
			guard = findGuard width int impss

		when (isNothing guard) $
			error $ "No matching lvalue guard for implicants " ++ show impss ++ " with test value " ++ show int

		simLvalue (lvalues !! fromJust guard) value
	simLvalue lvalue _ = error $ "\n\nUnhandled lvalue: " ++ show lvalue

	simCmd :: SimState -> Cmd -> SimFlow (Repeat, SimState)
	simCmd _ NoCmd = return (Done, AtomicState)
	simCmd cmdState (LabelCmd _ _ cmd) = simCmd cmdState cmd
	simCmd _ (PrintCmd _ exprs) = do
		exprValues <- mapM simExpr exprs
		let types = map (typeOfExpr []) exprs
		simFlowIo $ do
			-- putStr "--- "
			putStrLn $ foldr (uncurry $ showSimValue True) "" $ zip types exprValues
		return (Done, AtomicState)
	simCmd _ (AssignCmd _ lvalue expr) = do
		value <- simExpr expr
		simLvalue lvalue value
		return (Done, AtomicState)
	simCmd (SeqState doneUpto cmdStates) (SeqCmd _ cmds) = do
		(doneUpto', cmdStates') <- progressSeq simCmd doneUpto cmdStates cmds

		if doneUpto' == length cmds
			then return (Done, SeqState 0 cmdStates')
			else return (Redo, SeqState doneUpto' cmdStates')
	simCmd (ParState cmdDones cmdStates) (ParCmd _ cmds) = do
		(dones, cmdStates') <- progressPar simCmd cmdDones cmdStates cmds

		if all (== Done) dones
			then return (Done, ParState (replicate (length cmds) Redo) cmdStates')
			else return (Redo, ParState dones cmdStates')
	simCmd (ContextState ctx cmdState) (BlockCmd _ _ cmd) = do
		(ctx', (done, cmdState')) <- simFlowLocalContext ctx $ do
			simCmd cmdState cmd
		return (done, ContextState ctx' cmdState')
	simCmd (WhileState st cmdState1 cmdState2) cmd@(WhileCmd _ cmd1 expr cmd2) = case st of
		0 -> do -- trying to do cmd1
			(doneCmd1, cmdState1') <- simCmd cmdState1 cmd1
			case doneCmd1 of
				Done -> simCmd (WhileState 1 cmdState1' cmdState2) cmd
				Redo -> return (Redo, WhileState 0 cmdState1' cmdState2)
		1 -> do -- test expr
			exprValue <- simExpr expr
			case svToInt exprValue of
				1 -> simCmd (WhileState 2 cmdState1 cmdState2) cmd
				_ -> return (Done, WhileState 0 cmdState1 cmdState2)
		_ -> do -- trying to do cmd2
			(doneCmd2, cmdState2') <- simCmd cmdState2 cmd2
			case doneCmd2 of
				Done -> simCmd (WhileState 0 cmdState1 cmdState2') cmd
				Redo -> return (Redo, WhileState 2 cmdState1 cmdState2')
	simCmd (ChanOutputState Nothing _) cmd@(OutputCmd _ _ expr) = do
		exprValue <- simExpr expr
		simCmd (ChanOutputState (Just exprValue) BetweenHandshakes) cmd
	simCmd state@(ChanOutputState (Just value) BetweenHandshakes) (OutputCmd _ chan _) = do
		pushed <- chanPush chan value
		if pushed
			then return (Redo, ChanOutputState (Just value) InHandshake)
			else return (Redo, state)
	simCmd state@(ChanOutputState (Just _) InHandshake) (OutputCmd _ chan _) = do
		beenRead <- chanBeenRead chan
		if beenRead
			then return (Done, ChanOutputState Nothing BetweenHandshakes)
			else return (Redo, state)
	simCmd _ (InputCmd _ chan lvalue) = do
		maybeValue <- chanProbe chan
		if isJust maybeValue
			then do
				simLvalue lvalue $ fromJust maybeValue
				chanPop chan
				return (Done, AtomicState)
			else return (Redo, AtomicState)
	simCmd (cmdState) (LoopCmd _ cmd) = do
		(_, cmdState') <- simCmd cmdState cmd
		return (Redo, cmdState')
	simCmd (CaseState Nothing cmdStates) cmd@(CaseCmd _ expr impss _) = do
		exprValue <- simExpr expr
		let
			int = svToInt exprValue
			width = widthOfType [] $ typeOfExpr [] expr
			guard = findGuard width int impss

		when (isNothing guard) $
			error $ "No matching command guard for implicants " ++ show impss ++ " with test value " ++ show int

		simCmd (CaseState guard cmdStates) cmd
	simCmd (CaseState (Just guard) cmdStates) (CaseCmd _ _ _ cmds) = do
		let
			cmdState = cmdStates !! guard
			cmd = cmds !! guard
		(cmdDone, cmdState') <- simCmd cmdState cmd
		let cmdStates' = replaceAt cmdStates guard cmdState'

		case cmdDone of
			Done -> return (Done, CaseState Nothing cmdStates')
			Redo -> return (Redo, CaseState (Just guard) cmdStates')
	simCmd state (EncInputCmd _ guard) = do
		(_, ret) <- simChanGuard state guard
		return ret
	simCmd (SelectState (Just index) guardStates) (SelectCmd _ False guards) = do
		let
			state = guardStates !! index
			guard = guards !! index
		(_, (done, guardState')) <- simChanGuard state guard
		let guardStates' = replaceAt guardStates index guardState'
		case done of
			Done -> return (Done, SelectState Nothing guardStates')
			Redo -> return (Redo, SelectState (Just index) guardStates')
	simCmd state@(SelectState Nothing guardStates) (SelectCmd _ False guards) = tryGuards 0 guards
		where
			tryGuards _ [] = return (Redo, state)
			tryGuards i (guard:guards) = do
				taken <- liftM (all isJust) $ probeChanGuard guard
				if taken
					then return (Redo, SelectState (Just i) guardStates)
					else tryGuards (i + 1) guards
	simCmd (ArbState last (Just index) guardStates) (SelectCmd _ True guards) = do
		let
			state = guardStates !! index
			guard = guards !! index
		(_, (done, guardState')) <- simChanGuard state guard
		let guardStates' = replaceAt guardStates index guardState'
		case done of
			Done -> return (Done, ArbState index Nothing guardStates')
			Redo -> return (Redo, ArbState last (Just index) guardStates')
	simCmd state@(ArbState last Nothing guardStates) (SelectCmd _ True guards) = do
		-- tryGuards 0 guardStates guards
		readys <- liftM (map (all isJust)) $ mapM probeChanGuard guards
		if or readys
			then tryReady 0 readys
			else return (Redo, state)
		where
			tryReady i (True:readys)
				-- Try a different on if we went this way last time
				| i == last && or readys = tryReady (i + 1) readys
				| otherwise = return (Redo, ArbState last (Just i) guardStates)
			tryReady i (False:readys) = tryReady (i + 1) readys
			tryReady _ _ = return (Redo, state)
	simCmd _ (SinkCmd _ expr) = do
		simExpr expr
		return (Done, AtomicState)
	simCmd (InstanceCallState ctx cmdState cmd) (InstanceCallCmd {}) = do
		(done, cmdState') <- simFlowCallContext ctx $ do
			simCmd cmdState cmd
		return (done, InstanceCallState ctx cmdState' cmd)
	simCmd state cmd = error $ "Don't understand that state: " ++ show state ++ " cmd: " ++ show cmd

	probeChanGuard :: ChanGuard -> SimFlow [Maybe SimValue]
	probeChanGuard (ChanGuard _ _ ctx _) = do
		mapM chanProbe chans
		where
			chans = map getChan $ contextBindingsList ctx
				where getChan binding = Chan NoPos ref
					where OpenChanDecl _ ref _ = bindingValue binding

	simChanGuard :: SimState -> ChanGuard -> SimFlow (Bool, (Repeat, SimState))
	simChanGuard state@(EncInputState BetweenHandshakes _ cmdState) chanGuard@(ChanGuard _ _ ctx _) = do
		allChans <- probeChanGuard chanGuard
		if all isJust allChans
			then do
				let
					values = map fromJust allChans
					allVars = zip openChanRefs $ map VarState values
				simChanGuard (EncInputState InHandshake allVars cmdState) chanGuard
			else return (False, (Redo, state))
		where
			openChanRefs = map makeRef $ contextBindingsList ctx
				where makeRef binding = SimRef (bindingIndex binding)
	simChanGuard (EncInputState InHandshake ctxState cmdState) (ChanGuard _ _ ctx cmd) =
		simFlowConstContext ctxState $ do
			(cmdDone, cmdState') <- simCmd cmdState cmd
			case cmdDone of
				Done -> do
					mapM_ chanPop chans
					return (True, (Done, EncInputState BetweenHandshakes [] cmdState'))
					where
						(_, chans) = unzip $ map makeRef $ contextBindingsList ctx
							where makeRef binding = (SimRef (bindingIndex binding), Chan NoPos ref)
								where OpenChanDecl _ ref _ = bindingValue binding
				Redo -> return (True, (Redo, EncInputState InHandshake ctxState cmdState'))
	simChanGuard _ _ = error "simChanGuard: can't happen"

	valueToSimValue :: Type -> Value -> SimValue
	valueToSimValue typ (IntValue int) = intToSv $ cropInt (widthOfType [] typ) int
	valueToSimValue _ (StringValue str) = stringToSv str
	valueToSimValue _ val = error $ "valueToSimValue: unrecognised value: " ++ show val

	diffContext :: PosContext c => c -> Context Decl -> SimContext -> SimContext -> [Maybe String]
	diffContext posContext ctx bindings1 bindings2 = -- joinWith ",\n" $
		map showPair $ filter (not . bindingEq) $ zip bindings1 bindings2
		where
			showValList typ vals = showChar '[' .
				showListWithSep (showString ", ") (showSimValue False typ) vals . showChar ']'

			showPair ((SimRef i, ChanState _ from), (_, ChanState _ to)) = Just $
				"(" ++ showPos (Just posContext) (bindingPos binding) ++ ") " ++
				"channel " ++
				bindingName binding ++ ": " ++
				showValList typ from "" ++ " -> " ++ showValList typ to ""
				where
					binding = findBindingByRef [ctx] (Ref i)
					typ = declType $ bindingValue binding
			showPair ((SimRef i, VarState from), (_, VarState to)) = Just $
				"(" ++ showPos (Just posContext) (bindingPos binding) ++ ") " ++
				"variable " ++
				bindingName binding ++ ": " ++
				showSimValue False typ from "" ++ " -> " ++ showSimValue False typ to ""
				where
					binding = findBindingByRef [ctx] (Ref i)
					typ = declType $ bindingValue binding
			showPair _ = Nothing

			bindingEq ((_, b1), (_, b2)) = b1 == b2

	diffCmdState :: PosContext c => Integer -> c -> Cmd -> SimState -> SimState -> [String]
	diffCmdState time posContext cmd state1 state2 = catMaybes $ body cmd state1 state2
		where
			posStr pos = "(@" ++ show time ++ ":" ++ showPos (Just posContext) pos ++ ") "

			self (c, s1, s2) = body c s1 s2
			selfs cs s1s s2s = concatMap self $ zip3 cs s1s s2s

			body :: Cmd -> SimState -> SimState -> [Maybe String]
			body (LabelCmd _ _ cmd) state1 state2 = body cmd state1 state2
			body (SeqCmd pos cmds) (SeqState at1 cmdStates1) (SeqState at2 cmdStates2) = [atMsg]
				++ selfs cmds cmdStates1 cmdStates2
				where atMsg
					| at1 /= at2 = Just $ posStr pos ++ "seq " ++ show at1 ++ " -> " ++ show at2
					| otherwise = Nothing
			body (ParCmd pos cmds) (ParState redos1 cmdStates1) (ParState redos2 cmdStates2) = [atMsg]
				++ selfs cmds cmdStates1 cmdStates2
				where atMsg
					| redos1 /= redos2 = Just $ posStr pos ++ "par " ++ show redos1 ++ " -> " ++ show redos2
					| otherwise = Nothing
			body (BlockCmd _ ctx cmd) (ContextState bindings1 state1) (ContextState bindings2 state2)
				= diffContext posContext ctx bindings1 bindings2 ++ body cmd state1 state2
			body (EncInputCmd _ chanGuard) state1 state2 = diffChanGuardState time posContext chanGuard state1 state2
			body (OutputCmd pos chan _) state1 state2
				| state1 /= state2 = [Just $ posStr pos ++ "output " ++ show chan ++ " : " ++
					show state1 ++ " -> " ++ show state2]
			body (SelectCmd _ _ guards) (SelectState _ guardStates1) (SelectState _ guardStates2)
				= concatMap (\(cg, s1, s2) -> diffChanGuardState time posContext cg s1 s2)
					(zip3 guards guardStates1 guardStates2)
			-- FIXME, do last choice too?
			body (SelectCmd _ _ guards) (ArbState _ _ guardStates1) (ArbState _ _ guardStates2)
				= concatMap (\(cg, s1, s2) -> diffChanGuardState time posContext cg s1 s2)
					(zip3 guards guardStates1 guardStates2)
			body (CaseCmd pos _ _ cmds) (CaseState guard1 cmdStates1) (CaseState guard2 cmdStates2) =
				[guardMsg] ++ selfs cmds cmdStates1 cmdStates2
				where
					guardMsg	
						| guard1 /= guard2 = Just $ posStr pos ++ "case " ++ show guard1 ++ " -> " ++ show guard2
						| otherwise = Nothing
			body (InstanceCallCmd {}) (InstanceCallState _ cmdState1 cmd1) (InstanceCallState _ cmdState2 _) =
				body cmd1 cmdState1 cmdState2
			-- FIXME, show something for loop
			body (LoopCmd _ cmd) state1 state2 = body cmd state1 state2
			body (WhileCmd pos cmd1 _ cmd2) state1 state2 = [stMsg] ++
				body cmd1 cmdState11 cmdState21 ++ body cmd2 cmdState12 cmdState22
				where
					WhileState st1 cmdState11 cmdState12 = state1
					WhileState st2 cmdState21 cmdState22 = state2

					stMsg
						| st1 /= st2 = Just $ posStr pos ++ "while " ++ show st1 ++ " -> " ++ show st2
						| otherwise = Nothing
			body _ _ _ = []

	diffChanGuardState :: PosContext c => Integer -> c -> ChanGuard -> SimState -> SimState -> [Maybe String]
	diffChanGuardState time posContext (ChanGuard pos _ _ cmd)
		(EncInputState inHs1 _ cmdState1) (EncInputState inHs2 _ cmdState2) =
		[inHsMsg] ++ map Just (diffCmdState time posContext cmd cmdState1 cmdState2)
		where
			posStr = "(@" ++ show time ++ ":" ++ showPos (Just posContext) pos ++ ") "

			inHsMsg
				| inHs1 /= inHs2 = Just $ posStr ++ "enc-input " ++ show inHs1 ++ " -> " ++ show inHs2
				| otherwise = Nothing
	diffChanGuardState _ _ _ _ _ = error "diffChanGuardState: must be ChanGuard"

	cmdInitialState :: Context Decl -> Cmd -> SimFlow SimState
	cmdInitialState context cmd = body cmd where
		body (LabelCmd _ _ cmd) = body cmd
		body (OutputCmd {}) = return $ ChanOutputState Nothing BetweenHandshakes
		body (SeqCmd _ cmds) = do
			cmdsStates <- mapM body cmds
			return $ SeqState 0 cmdsStates
		body (ParCmd _ cmds) = do
			cmdsStates <- mapM body cmds
			return $ ParState (replicate count Redo) cmdsStates
			where count = length cmds
		body (BlockCmd _ ctx cmd) = do
			cmdState <- body cmd
			bindingStates <- mapM bindingIS $ contextBindingsList ctx
			return $ ContextState bindingStates cmdState
			where bindingIS binding = do
				declState <- declInitialState (bindingValue binding)
				return (SimRef (bindingIndex binding), declState)
		body (WhileCmd _ cmd1 _ cmd2) = do
			cmd1State <- body cmd1
			cmd2State <- body cmd2
			return $ WhileState 0 cmd1State cmd2State
		body (LoopCmd _ cmd) = body cmd
		body (EncInputCmd _ guard) = chanGuardInitialState context guard
		body (SelectCmd _ False guards) = do
			guardsStates <- mapM (chanGuardInitialState context) guards
			return $ SelectState Nothing guardsStates
		body (SelectCmd _ True guards) = do
			guardsStates <- mapM (chanGuardInitialState context) guards
			return $ ArbState (-1) Nothing guardsStates
		body (CaseCmd _ _ _ cmds) = do
			cmdsStates <- mapM body cmds
			return $ CaseState Nothing cmdsStates
		body (InstanceCallCmd _ (Callable ref) chans)
			| isEmptyContext formals = body inlinedCmd
			| otherwise = do
				inlinedCmdState <- body inlinedCmd
				return $ InstanceCallState mappings inlinedCmdState inlinedCmd
			where
				binding = findBindingByRef [context] ref
				ProcDecl _ formals _ cmd = bindingValue binding
				mappings = zip (map SimRef (range (contextIndexRange formals))) (map chanRef chans)
				inlinedCmd = inlineCalls cmd
		body _ = return AtomicState

	chanGuardInitialState :: Context Decl -> ChanGuard -> SimFlow SimState
	chanGuardInitialState context (ChanGuard _ _ _ cmd) = do
		cmdState <- cmdInitialState context cmd
		return $ EncInputState BetweenHandshakes [] cmdState

	declInitialState :: Decl -> SimFlow DeclState
	declInitialState (VarDecl {}) = return $ VarState (SimValue 0 [])
	declInitialState (ChanDecl {}) = do
		ChanCapacityState capacity <- simFlowGet ChanCapacityRef
		return $ ChanState capacity []
	-- Not needed/supported decls
	declInitialState (ProcDecl {}) = return NoDeclState
	declInitialState (SharedDecl {}) = return NoDeclState
	declInitialState decl = error $ "\n\nCan't handle decl initial state of: " ++ show decl

	subExprs :: Expr -> [Expr]
	subExprs (BuiltinCallExpr _ _ _ args _) = args
	subExprs (BitfieldExpr _ _ _ expr) = [expr]
	subExprs (BinExpr _ _ _ lExpr rExpr) = [lExpr, rExpr]
	subExprs (UnExpr _ _ _ rExpr) = [rExpr]
	subExprs (ConsExpr _ _ _ exprs) = exprs
	subExprs (ExtendExpr _ _ _ _ expr) = [expr]
	subExprs (AppendExpr _ _ l r) = [l, r]
	subExprs expr = error $ "Can't find subexprs for: " ++ show expr

	simPureExpr :: Expr -> [SimValue] -> SimValue
	simPureExpr (BitfieldExpr _ _ slice _) [rhs] = simExtractBitfield slice rhs
	simPureExpr (BinExpr _ typ op lExpr rExpr) [lVal, rVal] = intToSv $ cropInt width result
		where
			width = widthOfType [] typ

			recoverExprSign expr val = recoverValue [] typ int
				where
					typ = typeOfExpr [] expr
					int = svToInt val

			l = recoverExprSign lExpr lVal
			r = recoverExprSign rExpr rVal
			ineq op = boolToBit $ l `op` r
			-- FIXME, use code from Expr.hs
			result = case op of
				BinAdd -> l + r
				BinSub -> l - r 
				BinLT -> ineq (<)
				BinGT -> ineq (>)
				BinLE -> ineq (<=)
				BinGE -> ineq (>=)
				BinEQ -> boolToBit (l == r)
				BinNE -> ineq (/=)
				BinXor -> l `xor` r
				BinOr -> l .|. r
				BinAnd -> l .&. r
				_ -> error $ "Unhandled binary operation: " ++ binOpSymbol op
	simPureExpr (UnExpr _ typ op _) [rVal] = intToSv $ cropInt width result
		where
			width = widthOfType [] typ
			r = svToInt rVal
			result = case op of
				UnNeg -> 0 - r
				UnNot -> bitNot width r
				_ -> error $ "Unhandled unary operation: " ++ unOpSymbol op
	simPureExpr (ConsExpr _ _ _ exprs) values = simAppendValues widths values
		where widths = map (widthOfType [] . typeOfExpr []) exprs
	simPureExpr (ExtendExpr _ _ _ False _) [arg] = arg
	simPureExpr (ExtendExpr _ _ toWidth True expr) [SimValue arg specials] = result
		where
			fromWidth = widthOfType [] $ typeOfExpr [] expr
			result = SimValue (cropInt toWidth $ recoverSign fromWidth arg) specials
	simPureExpr (AppendExpr pos typ l r) args = simPureExpr (ConsExpr pos typ typ [l, r]) args
	simPureExpr _ _ = error "simPureExpr: bad Expr"
