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

module Teak (
	teak,
	TeakOption,
	teakOptionUsage,
	visitCmd,
	defaultTeakOptions
	) where

	import Misc
	import NetParts
	import ParseTree
	import Context
	import Type
	import Report
	import Bits
	import Options
	import Show
	import Print

	import Data.List
	import Data.Maybe
	import Control.Monad
	import Data.Bits
	import Control.Monad.Trans

	data TeakChoiceEncoding = TeakChoice1Hot | TeakChoiceBinary
		deriving Eq

	data TeakOption =
		  TeakChoiceEncoding TeakChoiceEncoding
		| TeakBreakVariablesOnWrite
		| TeakSingleWritePort
		| TeakUseMux
		--  | TeakPushThrough

	instance SubOption TeakOption where
		matchSubOption (TeakChoiceEncoding enc1) (TeakChoiceEncoding enc2) = enc1 == enc2
		matchSubOption TeakBreakVariablesOnWrite TeakBreakVariablesOnWrite = True
		matchSubOption TeakSingleWritePort TeakSingleWritePort = True
		matchSubOption TeakUseMux TeakUseMux = True
		-- matchSubOption TeakPushThrough TeakPushThrough = True
		matchSubOption _ _ = False

	instance Show TeakOption where
		showsPrec _ (TeakChoiceEncoding enc) = shows enc
		showsPrec _ _ = id

	instance Show TeakChoiceEncoding where
		showsPrec _ TeakChoice1Hot = showString "onehot"
		showsPrec _ TeakChoiceBinary = showString "binary"

	instance Read TeakChoiceEncoding where
		readsPrec _ str = maybeToList $ do
			(token, rest) <- maybeLex str
			case token of
				"onehot" -> return (TeakChoice1Hot, rest)
				"binary" -> return (TeakChoiceBinary, rest)
				_ -> fail ""

	defaultTeakOptions :: [TeakOption]
	defaultTeakOptions = [TeakChoiceEncoding TeakChoice1Hot, TeakBreakVariablesOnWrite, TeakSingleWritePort]

	teakOptionUsage :: SubOptionUsages TeakOption
	teakOptionUsage = SubOptionUsages "teak" show Nothing (Just defaultTeakOptions) [
		("choice", SubOptionUsage False "encoding" "data encoding for steering values (onehot|binary)" "onehot"
			(not . null . (reads :: String -> [(TeakChoiceEncoding, String)])) (\arg ->
				([TeakChoiceEncoding (read arg)], []))),
		("break-v", boolSubOption "break Vs on boundaries on write accesses" TeakBreakVariablesOnWrite),
		("single-write-port", boolSubOption "only use on write port on Vs where all the writes cover the whole V"
			TeakSingleWritePort),
		("use-mux", boolSubOption "use S-X components for steered choices rather than J-S-M-F"
			TeakUseMux)
		-- ("push-through", boolSubOption "push variable write data through control" TeakPushThrough)
		]

	teak :: NetworkIF network => [TeakOption] -> Context Decl -> Why [Part network]
	teak opts context = parts
		where
			posArray = contextToPosArray context
			procs = filter isCompleteProcDeclBinding $ contextBindingsList context
			parts = gatherFail $ map (bindingToPart opts [context] posArray) procs

			bindingToPart
				--  | findBoolSubOption opts TeakPushThrough = ptProcBindingToPart
				| otherwise = procBindingToPart

			isCompleteProcDeclBinding binding = isProcDecl (bindingValue binding)
				&& bindingCompleteness binding == Complete

	j :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkLinkRef -> NetworkMonad network NetworkComp
	j pos is o = nwNewTeakComp TeakJ [Many is, One o] pos

	m :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkLinkRef -> NetworkMonad network NetworkComp
	m pos is o = nwNewTeakComp TeakM [Many is, One o] pos

	f :: NetworkIF network => Pos -> [Int] -> NetworkLinkRef -> [NetworkLinkRef] -> NetworkMonad network NetworkComp
	f pos spec i os = nwNewTeakComp (TeakF spec) [One i, Many os] pos

	s :: NetworkIF network => Pos -> Slice Int -> [([Implicant], Int)]
		-> NetworkLinkRef -> [NetworkLinkRef] -> NetworkMonad network NetworkComp
	s pos slice spec i os = nwNewTeakComp (TeakS slice spec) [One i, Many os] pos

	o :: NetworkIF network => Pos -> [(Int, TeakOTerm)] ->
		NetworkLinkRef -> NetworkLinkRef -> NetworkMonad network NetworkComp
	o pos spec i o = nwNewTeakComp (TeakO spec) [One i, One o] pos

	v :: NetworkIF network => Pos -> String -> Int -> [Int] -> [Int] -> [Int] ->
		[NetworkLinkRef] -> [NetworkLinkRef] -> [NetworkLinkRef] -> [NetworkLinkRef] ->
		NetworkMonad network NetworkComp
	v pos name w bs wws rws ws wds rs rds = nwAddComp $
		TeakComp 0 (TeakV name w bs wws rws) [Many ws, Many wds, Many rs, Many rds] pos

	a :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkLinkRef -> NetworkMonad network NetworkComp
	a pos is o = nwNewTeakComp TeakA [Many is, One o] pos

	x :: NetworkIF network => Pos -> [[Implicant]] -> [NetworkLinkRef]
		-> NetworkLinkRef -> NetworkLinkRef -> NetworkMonad network NetworkComp
	x pos spec is s o = nwNewTeakComp (TeakX spec) [Many is, One s, One o] pos

	-- connect : connect from to to with a (possibly narrowing) fork that can be removed in optimisation
	connect :: NetworkIF network => Pos -> NetworkLinkRef -> NetworkLinkRef -> NetworkMonad network ()
	connect pos from to = f pos [0] from [to] >> return ()

	makeJoin :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkMonad network NetworkLinkRef
	makeJoin pos [] = error $ "zero link join not allowed" ++ show pos
	makeJoin _ [signal] = return signal
	makeJoin pos signals = do
		outWidth <- liftM sum $ mapM nwGetLinkWidth signals
		link <- nwNewLinkRef outWidth
		j pos signals link
		return link

	makeFork :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkMonad network NetworkLinkRef
	makeFork pos [] = error $ "zero link fork not allowed" ++ show pos
	makeFork _ [signal] = return signal
	makeFork pos signals = do
		width <- nwGetLinkWidth $ head signals
		link <- nwNewLinkRef width
		f pos (replicate (length signals) 0) link signals
		return link

	makeMerge :: NetworkIF network => Pos ->
		(Pos -> [NetworkLinkRef] -> NetworkLinkRef -> NetworkMonad network NetworkComp) ->
		[NetworkLinkRef] -> NetworkMonad network NetworkLinkRef
	makeMerge _ _ [] = error "zero link merge not allowed"
	makeMerge _ _ [signal] = return signal
	makeMerge pos m signals = do
		width <- nwGetLinkWidth $ head signals
		link <- nwNewLinkRef width
		m pos signals link
		return link

	makeDoneMerge :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkMonad network (Maybe NetworkLinkRef)
	makeDoneMerge _ [] = return Nothing
	makeDoneMerge pos links = do
		link <- makeMerge pos m links
		return $ Just link

	makeSteerMux :: NetworkIF network => [TeakOption] -> Pos -> Slice Int -> [([Implicant], Int)] ->
		NetworkLinkRef -> [(NetworkLinkRef, Maybe NetworkLinkRef)] ->
		NetworkMonad network (Maybe NetworkLinkRef)
	-- makeSteerMux pos slice terms inp bodys
	makeSteerMux _ _ _ _ _ [] = return Nothing
	makeSteerMux opts pos slice terms inp bodys = if findBoolSubOption opts TeakUseMux
		then
			case dones' of
				[] -> do
					s pos slice terms inp gos
					return Nothing
				[done] -> do
					s pos slice terms inp gos
					return $ Just done
				_ -> do
					inpWidth <- nwGetLinkWidth inp
					outWidth <- nwGetLinkWidth $ head dones'
					sel <- nwNewLinkRef selWidth
					inp' <- nwNewLinkRef inpWidth
					out <- nwNewLinkRef outWidth
					f pos [selOffset, 0] inp [sel, inp']
					s pos slice terms inp' gos
					x pos xImpssAdjusted dones' sel out
					return $ Just out
		else do
			s pos slice terms inp gos
			makeDoneMerge pos $ catMaybes dones
		where
			selWidth = sliceWidth slice
			selOffset = sliceOffset slice
		
			maybePair _ Nothing = Nothing
			maybePair imps (Just done) = Just (imps, done)

			xImpsXDones = catMaybes $ zipWith maybePair impss dones 
			(xImpss, dones') = unzip xImpsXDones
			xImpssAdjusted = map (map (\imp -> impShiftRight imp selOffset)) xImpss

			(gos, dones) = unzip bodys
			(impss, _) = unzip terms

	makeDoneJoin :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkMonad network (Maybe NetworkLinkRef)
	makeDoneJoin _ [] = return Nothing
	makeDoneJoin pos links = do
		link <- makeJoin pos links
		return $ Just link

	-- forkSignal : fork inp to signal and a new signal of the same width as inp which is returned
	forkSignal :: NetworkIF network => Pos -> NetworkLinkRef -> NetworkLinkRef -> NetworkMonad network NetworkLinkRef
	forkSignal pos inp signal = do
		width <- nwGetLinkWidth inp
		out <- nwNewLinkRef width
		f pos [0, 0] inp [signal, out]
		return out

	-- forkNewSignal : forkSignal but make the signal here
	forkNewSignal :: NetworkIF network => Pos -> NetworkLinkRef -> NetworkMonad network (NetworkLinkRef, NetworkLinkRef)
	forkNewSignal pos inp = do
		signal <- nwNewLinkRef 0
		out <- forkSignal pos inp signal
		return (signal, out)

	-- makeDecision : steer `inp' to one of `outs' under a choice from one of `gos'
	makeDecision :: NetworkIF network => [TeakOption] -> Pos -> Bool ->
		NetworkLinkRef -> [NetworkLinkRef] -> [NetworkLinkRef] -> NetworkMonad network ()
	makeDecision _ pos _ inp [go] [out] = do
		j pos [inp, go] out
		return ()
	makeDecision opts pos doArb inp gos outs = do
		inpWidth <- nwGetLinkWidth inp
		let choiceSlice = inpWidth +: choiceWidth
		choices <- mapM makeChoiceO $ zip gos choiceValues
		choice <- makeMerge pos (if doArb then a else m) choices
		choiceAndData <- makeJoin pos [inp, choice]
		s pos choiceSlice (zip (map (:[]) $ choiceImps inpWidth) (repeat 0)) choiceAndData outs
		return ()
		where
			TeakChoiceEncoding encoding = getSubOption opts (TeakChoiceEncoding TeakChoice1Hot)
			(choiceWidth, choiceCount, choiceValues, choiceImps)
				| encoding == TeakChoice1Hot = (goCount, goCount, map bit [0..choiceCount - 1], imps)
				| otherwise = (intWidth (choiceCount - 1), goCount, [0..toInteger choiceCount - 1], imps)
				where imps _ = map makeImp choiceValues

			goCount = length gos

			makeImp value = Imp value 0

			makeChoiceO (go, value) = do
				choice <- nwNewLinkRef choiceWidth
				o pos [(1, TeakOConstant choiceWidth value)] go choice
				return choice

	insertExpr :: NetworkIF network => [TeakOption] -> [Context Decl] -> Pos -> Expr ->
		NetworkMonad network (NetworkLinkRef, NetworkLinkRef)
	insertExpr opts cs pos expr = do
		(gos@(_:_), done) <- visitExpr opts cs expr
		go <- makeFork pos gos
		return (go, done)

	-- chainCmds : chain from a given go through commands nls giving a network with a set go
	--	and a single done (if the last command had a done)
	chainCmds :: NetworkIF network => [TeakOption] -> [Context Decl] -> Pos -> NetworkLinkRef -> [Cmd] ->
		WhyT (NetworkMonad network) (Maybe NetworkLinkRef)
	chainCmds opts cs pos goIn cmds = do
		done <- foldM chainCmd (Just goIn) cmds
		return done
		where
			chainCmd Nothing cmd = WhyT $ return $ Why (parseNodeError cs pos cmd "inaccessible command") Nothing
			chainCmd (Just goInL) cmd = do
				(cmdGo, cmdDone) <- visitCmd opts cs cmd
				lift $ connect pos goInL cmdGo
				return cmdDone

	-- teakOFit : adapt term `indexIn' from width `widthIn' to `widthOut', possibly by adding
	--	terms to `terms'.  Returns (indexOfNewResult, newTerms)
	teakOFit :: [(Int, TeakOTerm)] -> Int -> Int -> Int -> Bool -> (Int, [(Int, TeakOTerm)])
	teakOFit terms indexIn widthIn widthOut signed
		| widthOut == widthIn = (indexIn, terms)
		| widthOut > widthIn && signed = (termNo + 1, terms ++ [
			(termNo, TeakOAppend extendWidth [(indexIn, (widthIn - 1) +: 1)]),
			(termNo + 1, TeakOAppend 1 [(indexIn, 0 +: widthIn), (termNo, 0 +: extendWidth)]) ])
		| widthOut > widthIn && not signed = (termNo + 1, terms ++ [
			(termNo, TeakOConstant extendWidth 0),
			(termNo + 1, TeakOAppend 1 [(indexIn, 0 +: widthIn), (termNo, 0 +: extendWidth)]) ])
		| otherwise {- widthOut < widthIn -} = (termNo, terms ++
			[(termNo, TeakOAppend 1 [(indexIn, 0 +: widthOut)]) ])
		where
			extendWidth = widthOut - widthIn
			termNo = oTermsLastIndex terms + 1

	stringExpr :: NetworkIF network => Pos -> String -> NetworkMonad network ([NetworkLinkRef], NetworkLinkRef)
	stringExpr pos str = do
		go <- nwNewLinkRef 0
		done <- nwNewLinkRef builtinTypeWidth
		o pos [(1, TeakOBuiltin "String" builtinTypeWidth [TeakParamString str] [])] go done
		return ([go], done)

	stringAppend :: NetworkIF network => Pos -> [NetworkLinkRef] -> NetworkMonad network NetworkLinkRef
	stringAppend _ [link] = return link
	stringAppend pos (link1:links) = do
		result <- nwNewLinkRef builtinTypeWidth
		right <- stringAppend pos links
		arg <- makeJoin pos [link1, right]
		o pos [(1, TeakOBuiltin "StringAppend" builtinTypeWidth
			[] [(0, 0 +: builtinTypeWidth), (0, builtinTypeWidth +: builtinTypeWidth)])] arg result
		return result
	stringAppend _ _ = error "stringAppend: no links"

	--- Tree functions start here ---

	visitExpr :: NetworkIF network => [TeakOption] -> [Context Decl] -> Expr ->
		NetworkMonad network ([NetworkLinkRef], NetworkLinkRef)
	visitExpr _ _ (VarRead _ _ bitRange ref) = varRead bitRange ref
	visitExpr _ _ (OpenChanRead _ _ bitRange ref) = varRead bitRange ref
	visitExpr _ cs (ValueExpr pos typ (IntValue int)) = do
		go <- nwNewLinkRef 0
		done <- if width == 0 -- Don't need a Constant for a token value
			then return go
			else do
				done <- nwNewLinkRef (widthOfType cs typ)
				o pos [(1, TeakOConstant width $ cropInt width int)] go done
				return done
		return ([go], done)
		where width = widthOfType cs typ
	visitExpr _ _ (ValueExpr pos _ (StringValue str)) = stringExpr pos str
	visitExpr opts cs (AppendExpr pos _ l r) = do
		(lGos, lDone) <- visitExpr opts cs l
		(rGos, rDone) <- visitExpr opts cs r
		done <- makeJoin pos [lDone, rDone]
		return (lGos ++ rGos, done)
	visitExpr opts cs (BinExpr pos typ op l r) = do
		(lGos, lDone) <- visitExpr opts cs l
		(rGos, rDone) <- visitExpr opts cs r
		args <- makeJoin pos [lDone, rDone]
		done <- nwNewLinkRef widthQ
		o pos terms args done
		return (lGos ++ rGos, done)
		where
			widthQ = widthOfType cs typ
			typeL = typeOfExpr cs l
			typeR = typeOfExpr cs r
			widthL = widthOfType cs typeL
			widthR = widthOfType cs typeR
			signedL = typeIsSigned cs typeL
			signedR = typeIsSigned cs typeR
			anySigned = signedL || signedR

			termL = TeakOAppend 1 [(0, 0 +: widthL)]
			termR = TeakOAppend 1 [(0, widthL +: widthR)]
			inputTerms = [(1, termL), (2, termR)]

			extendEqualOp widthMax op = terms'''
				where
					terms = inputTerms
					(left, terms') = teakOFit terms 1 widthL widthMax signedL
					(right, terms'') = teakOFit terms' 2 widthR widthMax signedR
					opTerm = (oTermsLastIndex terms'' + 1, TeakOp op $ orderInputs
						[(left, 0 +: widthMax), (right, 0 +: widthMax)])
					terms''' = terms'' ++ [opTerm]

			arith = extendEqualOp widthQ
			compare = extendEqualOp $ max widthL widthR

			(orderInputs, terms) = case op of
				BinAdd -> (id, arith TeakOpAdd)
				BinSub -> (id, arith TeakOpSub)
				BinAnd -> (id, arith TeakOpAnd)
				BinOr -> (id, arith TeakOpOr)
				BinXor -> (id, arith TeakOpXor)
				BinLT -> (reverse, compare $ if anySigned then TeakOpSignedGT else TeakOpUnsignedGT)
				BinGT -> (id, compare $ if anySigned then TeakOpSignedGT else TeakOpUnsignedGT)
				BinLE -> (reverse, compare $ if anySigned then TeakOpSignedGE else TeakOpUnsignedGE)
				BinGE -> (id, compare $ if anySigned then TeakOpSignedGE else TeakOpUnsignedGE)
				BinEQ -> (id, compare TeakOpEqual)
				BinNE -> (id, compare TeakOpNotEqual)
				_ -> error $ "Unhandled binary func `" ++ binOpSymbol op ++ "'"
	visitExpr opts cs (UnExpr pos typ op expr) = do
		done <- nwNewLinkRef widthQ
		(gos, rDone) <- visitExpr opts cs expr
		o pos terms rDone done
		return (gos, done)
		where
			widthQ = widthOfType cs typ
			typeR = typeOfExpr cs expr
			widthR = widthOfType cs typeR
			signedR = typeIsSigned cs typeR

			extendOp widthMax op = terms''
				where
					terms = []
					(right, terms') = teakOFit terms 0 widthR widthMax signedR
					opTerm = (oTermsLastIndex terms' + 1, TeakOp op [(right, 0 +: widthMax)])
					terms'' = terms' ++ [opTerm]

			negate widthMax = terms''
				where
					terms = [(1, TeakOConstant widthMax 0)]
					(right, terms') = teakOFit terms 0 widthR widthMax signedR
					opTerm = (oTermsLastIndex terms' + 1,
						TeakOp TeakOpSub [(1, 0 +: widthMax), (right, 0 +: widthMax)])
					terms'' = terms' ++ [opTerm]

			arith = extendOp widthQ

			terms = case op of
				UnNot -> arith TeakOpNot
				UnNeg -> negate widthQ
				_ -> error $ "Unhandled unary func `" ++ unOpSymbol op ++ "'"

	visitExpr opts cs (ExtendExpr pos typ _ extend expr) = do
		done <- nwNewLinkRef widthQ
		(gos, rDone) <- visitExpr opts cs expr
		let
			extendWidth = widthQ - widthExpr
			oSpecExtension
				| extend = TeakOAppend extendWidth [(0, (widthExpr - 1) +: 1)]
				| otherwise = TeakOConstant extendWidth 0
			oSpec = [(1, oSpecExtension), (2, TeakOAppend 1 [(0, 0 +: widthExpr), (1, 0 +: extendWidth)])]
		o pos oSpec rDone done
		return (gos, done)
		where
			widthQ = widthOfType cs typ
			widthExpr = widthOfType cs $ typeOfExpr cs expr

	visitExpr opts cs (BitfieldExpr pos typ slice expr)
		| widthQ == widthExpr = visitExpr opts cs expr
		| otherwise = do
			done <- nwNewLinkRef widthQ
			(gos, rDone) <- visitExpr opts cs expr
			f pos [sliceOffset slice] rDone [done]
			return (gos, done)
		where
			widthQ = widthOfType cs typ
			widthExpr = widthOfType cs $ typeOfExpr cs expr

	visitExpr opts cs node@(ConsExpr _ _ _ exprConsElems) = do
		(goss, dones) <- mapAndUnzipM (visitExpr opts cs) exprConsElems
		done <- makeJoin pos dones
		return (concat goss, done)
		where pos = exprPos node
	visitExpr opts cs (BuiltinCallExpr pos name params exprs returnType)
		| argCount == 0 = do
			go <- nwNewLinkRef 0
			done <- nwNewLinkRef width
			o pos [(1, TeakOBuiltin name width teakParams slices)] go done
			return ([go], done)
		| otherwise = do
			(goss, args) <- mapAndUnzipM (visitExpr opts cs) exprs
			done <- nwNewLinkRef width
			arg <- makeJoin pos args
			o pos [(1, TeakOBuiltin name width teakParams slices)] arg done
			return (concat goss, done)
		where
			argCount = length exprs
			width = widthOfType cs returnType
			argWidths = map (widthOfType cs . typeOfExpr cs) exprs
			makeSlice offset width = (offset + width, (0, offset +: width))
			slices = snd $ mapAccumL makeSlice 0 argWidths
			teakParams = map funcActualToTeakParam params

	visitExpr opts cs (CaseExpr pos expr impss exprs) = do
		(exprGos, exprDones) <- mapAndUnzipM (insertExpr opts cs pos) exprs
		(indexGos, indexDone) <- visitExpr opts cs expr
		let slice = 0 +: indexWidth
		Just done <- makeSteerMux opts pos slice (zip impss (repeat 0)) indexDone
			(zip exprGos (map Just exprDones))
		return (indexGos, done)
		where
			-- FIXME, need better way to type Case
			indexWidth = widthOfType cs $ typeOfExpr cs expr

			-- Another way to do this is to compute all the sub-expressions and choose from them
			--	with a big J then S
	visitExpr _ _ node = error $ "Teak: unsupported expr node " ++ show node

	visitLvalue :: NetworkIF network => [TeakOption] -> [Context Decl] -> Lvalue ->
		NetworkMonad network (NetworkLinkRef, NetworkLinkRef)
	visitLvalue _ _ (VarWrite _ _ slice ref) = do
		go <- nwNewLinkRef (sliceWidth slice)
		done <- nwNewLinkRef 0
		nwAddAccess $ Access (InstanceAccess ref) [VarAccess Write done go slice]
		return (go, done)
	visitLvalue opts cs (CaseLvalue pos expr impss lvalues) = do
		go <- nwNewLinkRef width
		(exprGo, exprDone) <- insertExpr opts cs pos expr
		(lvalueGos, lvalueDones) <- mapAndUnzipM (visitLvalue opts cs) lvalues

		dataToSteer <- forkSignal pos go exprGo
		steerIn <- makeJoin pos [dataToSteer, exprDone]
		Just done <- makeSteerMux opts pos slice (zip impss (repeat 0)) steerIn
			(zip lvalueGos (map Just lvalueDones))

		return (go, done)
		where
			width = widthOfType cs $ typeOfLvalue cs $ head lvalues
			indexWidth = widthOfType cs $ typeOfExpr cs expr
			slice = width +: indexWidth
	visitLvalue _ _ node = error $ "Teak: unsupported lvalue node " ++ show node

	-- Chans before nwSetChanFlow have uncertain direction and so just a carry a single link which will
	--	become the data-bearing link in the final channel access
	visitChan :: NetworkIF network => [Context Decl] -> Chan -> NetworkMonad network (AccessRef, NetworkLinkRef)
	visitChan cs (Chan _ ref) = do
		link <- nwNewLinkRef width
		return (InstanceAccess (unaliasRef cs ref), link)
		where width = widthOfType cs $ declType $ bindingValue $ findBindingByRef cs ref
	visitChan _ node = error $ "Teak: unsupported chan node " ++ show node

	visitCmd :: NetworkIF network => [TeakOption] -> [Context Decl] -> Cmd ->
		WhyT (NetworkMonad network) (NetworkLinkRef, Maybe NetworkLinkRef)
	visitCmd _ _ (NoCmd {}) = lift $ do
		go <- nwNewLinkRef 0
		return (go, Just go)
	visitCmd opts cs (ParCmd pos cmds) = do
		(gos, dones) <- mapAndUnzipM (visitCmd opts cs) cmds
		go <- lift $ makeFork pos gos
		done <- lift $ makeDoneJoin pos $ catMaybes dones
		return (go, done)
	visitCmd opts cs (SeqCmd pos cmds) = do
		go <- lift $ nwNewLinkRef 0
		done <- chainCmds opts cs pos go cmds
		return (go, done)
	visitCmd opts cs (LoopCmd pos cmd) = do
		(cmdGo, cmdDone) <- visitCmd opts cs cmd
		go <- if isJust cmdDone
			then lift $ do
				go <- nwNewLinkRef 0
				m pos [go, fromJust cmdDone] cmdGo
				return go
			else return cmdGo
		return (go, Nothing)
	visitCmd opts cs (InputCmd pos chan lvalue) = lift $ do
		(ref, chanData) <- visitChan cs chan
		chanGo <- nwNewLinkRef 0
		nwAddAccess $ Access ref [ChanInputAccess chanGo chanData]
		(lvalueGo, lvalueDone) <- visitLvalue opts cs lvalue
		connect pos chanData lvalueGo
		return (chanGo, Just lvalueDone)
	visitCmd opts cs (OutputCmd pos chan expr) = lift $ do
		(exprGo, exprDone) <- insertExpr opts cs pos expr
		(ref, chanData) <- visitChan cs chan
		chanDone <- nwNewLinkRef 0
		nwAddAccess $ Access ref [ChanOutputAccess chanData chanDone]
		connect pos exprDone chanData
		return (exprGo, Just chanDone)
	visitCmd opts cs (AssignCmd pos lvalue expr) = lift $ do
		(exprGo, exprDone) <- insertExpr opts cs pos expr
		(lvalueGo, lvalueDone) <- visitLvalue opts cs lvalue
		connect pos exprDone lvalueGo
		return (exprGo, Just lvalueDone)
	visitCmd opts cs (BlockCmd _ context cmd) = do
		(go, done) <- visitCmd opts (context:cs) cmd
		-- FIXME, find better concurrent solution than just reversing bindings to process SharedDecls
		mapM completeDecl $ reverse $ contextBindingsList context
		return (go, done)
		where
			completeDecl binding = do
				case decl of
					VarDecl varPos typ -> makeVar opts varPos ref width name $ typeBuiltinOffsets cs 0 typ
					ChanDecl {} -> do
						makeChan opts pos False ref width name
						return ()
					SharedDecl {} -> makeShared opts (context:cs) pos ref binding
					_ -> return ()
				where
					ref = refBinding binding
					name = bindingName binding
					decl = bindingValue binding
					width = widthOfType cs $ declType decl
					pos = bindingPos binding
	visitCmd opts cs (EncInputCmd pos guard) = do
		(chans, writes, [signal], [cmdGo], cmdDones) <- completeEncInputs opts cs [guard]
		let
			makeInputJoin (chanRef, writeLink) = do
				goLink <- nwNewLinkRef 0
				nwAddAccess $ Access (InstanceAccess chanRef) [ChanInputAccess goLink writeLink]
				return goLink

		gos <- lift $ mapM makeInputJoin $ zip chans writes
		go <- lift $ makeFork pos gos
		lift $ connect pos signal cmdGo
		done <- lift $ makeDoneMerge pos cmdDones
		return (go, done)
	visitCmd opts cs (SelectCmd pos arbitrate guards) = do
		(requests, grants, signals, gos, dones) <- liftM unzip5 $ mapM makeGuardSelect $ guards
		go <- lift $ nwNewLinkRef 0
		lift $ makeDecision opts pos arbitrate go requests grants
		lift $ mapM_ alias $ zip signals gos
		done <- lift $ makeDoneMerge pos $ concat dones
		return (go, done)
		where
			addReadAccess (chanRef, varWriteLink) = do
				request <- nwNewLinkRef 0
				width <- nwGetLinkWidth varWriteLink
				chanWriteLink <- nwNewLinkRef width
				forkedData <- forkSignal pos chanWriteLink request
				grant <- nwNewLinkRef 0
				j pos [grant, forkedData] varWriteLink
				nwAddAccess $ Access (InstanceAccess chanRef) [ChanBareInputAccess chanWriteLink]
				return (request, grant)

			makeGuardSelect guard = do
				(chans, writes, [signal], [go], dones) <- completeEncInputs opts cs [guard]
				(requests, grants) <- lift $ mapAndUnzipM addReadAccess $ zip chans writes
				request <- lift $ makeJoin pos requests
				grant <- lift $ makeFork pos grants
				return (request, grant, signal, go, dones)

			alias (from, to) = do
				connect pos from to
				return from
	visitCmd opts cs (SinkCmd pos expr) = lift $ do
		(exprGo, exprDone) <- insertExpr opts cs pos expr
		done <- nwNewLinkRef 0
		connect pos exprDone done
		return (exprGo, Just done)
	visitCmd opts cs (CaseCmd pos expr impss cmds) = do
		(exprGo, exprDone) <- lift $ insertExpr opts cs pos expr
		(cmdGos, cmdDones) <- mapAndUnzipM (visitCmd opts cs) cmds
		width <- lift $ nwGetLinkWidth exprDone
		let slice = 0 +: width
		done <- lift $ makeSteerMux opts pos slice
			(zip impss (repeat 0)) exprDone (zip cmdGos cmdDones)
		return (exprGo, done)
	visitCmd _ _ (SharedCallCmd _ (Callable ref)) = lift $ do
		go <- nwNewLinkRef 0
		done <- nwNewLinkRef 0
		nwAddAccess $ Access (InstanceAccess ref) [SharedCallAccess go done]
		return (go, Just done)
	visitCmd _ cs (InstanceCallCmd pos (Callable ref) chans) = lift $ do
		chanDatas <- mapM setChanFlow $ zip ports chans
		if reallyInstantiate
			then do
				go <- nwNewLinkRef 0
				done <- nwNewLinkRef 0
				nwAddComp $ InstanceComp 0 (bindingName binding)
					(goPort : donePort : otherPorts)
					(map One (go : done : chanDatas)) pos
				return (go, Just done)
			else do
				go <- nwNewLinkRef 0
				nwAddComp $ InstanceComp 0 (bindingName binding)
					(goPort : otherPorts)
					(map One (go : chanDatas)) pos
				return (go, Nothing)
		where
			binding = findBindingByRef cs ref
			proc = unaliasDecl cs $ bindingValue $ binding
			reallyInstantiate = isJust $ findAttr "reallyInstantiate" $ declAttrs proc

			ports = contextBindingsList $ declFormals proc
			goPort = NetworkPort "go" Input 0 Nothing
			donePort = NetworkPort "done" Output 0 Nothing
			otherPorts = (map portToNetworkPort ports)

			portToNetworkPort portBinding = NetworkPort (bindingName portBinding)
				direction width Nothing
				where
					port = bindingValue portBinding
					direction = declDirection port
					width = widthOfType cs $ declType port

			setChanFlow (port, chan) = do
				(ref, dataLink) <- visitChan cs chan
				nwAddAccess $ Access ref [accessBodyCons dataLink]
				return dataLink
				where accessBodyCons = case declDirection $ bindingValue port of
					Input -> ChanBareInputAccess
					Output -> ChanBareOutputAccess

	-- FIXME, inplace call, do referencing call as well
	visitCmd opts cs (PrintCmd pos exprs) = lift $ do
		(go, string) <- if null exprs
			then do
				([go], done) <- stringExpr pos ""
				return (go, done)
			else do
				(exprGos, exprDones) <- mapAndUnzipM (insertExpr opts cs pos) exprs
				go <- makeFork pos exprGos
				string <- stringAppend pos exprDones
				return (go, string)
		done <- nwNewLinkRef 0
		o pos [(1, TeakOBuiltin "tWriteMessage" 0 [] [(0, 0 +: builtinTypeWidth)])] string done
		return (go, Just done)
	visitCmd opts cs (WhileCmd pos NoCmd expr c2) = do
		(exprGo, exprDone) <- lift $ insertExpr opts cs pos expr
		(cmd2Go, cmd2Done) <- visitCmd opts cs c2
		done <- lift $ nwNewLinkRef 0
		lift $ s pos (0 +: 1) [([Imp 0 0], 0), ([Imp 1 0], 0)] exprDone [done, cmd2Go]
		go <- if isJust cmd2Done
			then lift $ do
				go <- nwNewLinkRef 0
				m pos [go, fromJust cmd2Done] exprGo
				return go
			else return exprGo
		return (go, Just done)
	visitCmd opts cs (WhileCmd pos c1 expr NoCmd) = do
		(cmd1Go, cmd1Done) <- visitCmd opts cs c1
		if isJust cmd1Done
			then lift $ do
				go <- nwNewLinkRef 0
				done <- nwNewLinkRef 0
				again <- nwNewLinkRef 0
				(exprGo, exprDone) <- insertExpr opts cs pos expr
				m pos [go, again] cmd1Go
				s pos (0 +: 1) [([Imp 0 0], 0), ([Imp 1 0], 0)] exprDone [done, again]
				connect pos (fromJust cmd1Done) exprGo
				return (go, Just done)
			else return (cmd1Go, Nothing)
	visitCmd opts cs (WhileCmd pos c1 expr c2) = do
		(exprGo, exprDone) <- lift $ insertExpr opts cs pos expr
		(cmd1Go, cmd1Done) <- visitCmd opts cs c1
		if isJust cmd1Done
			then do
				(cmd2Go, cmd2Done) <- visitCmd opts cs c2
				if isJust cmd2Done
					then lift $ do
						go <- nwNewLinkRef 0
						done <- nwNewLinkRef 0
						s pos (0 +: 1) [([Imp 0 0], 0), ([Imp 1 0], 0)] exprDone [done, cmd2Go]
						m pos [go, fromJust cmd2Done] cmd1Go
						connect pos (fromJust cmd1Done) exprGo
						return (go, Just done)
					else lift $ do
						done <- nwNewLinkRef 0
						s pos (0 +: 1) [([Imp 0 0], 0), ([Imp 1 0], 0)] exprDone [done, cmd2Go]
						connect pos (fromJust cmd1Done) exprGo
						return (cmd1Go, Just done)
				else return (cmd1Go, Nothing)
	visitCmd opts cs (LabelCmd _ _ cmd) = visitCmd opts cs cmd
	visitCmd _ _ node = error $ "Teak: unsupported cmd node " ++ show node

	attrToNameTeakParam :: Attr -> (String, TeakParam)
	attrToNameTeakParam (ExprAttr name expr) = (name, exprToTeakParam expr)

	procBindingToPart :: NetworkIF network => [TeakOption] -> [Context Decl] -> Maybe PosArray ->
		Binding Decl -> Why (Part network)
	procBindingToPart opts cs posArray binding = part
		where
			name = bindingName binding
			ProcDecl pos formals attrs cmd = bindingValue binding

			formalsBindings = contextBindingsList formals

			formalPorts = filter isPort formalsBindings
				where
					isPort binding = case bindingValue binding of
						PortDecl {} -> True
						_ -> False

			go = Binding 0 "go" OtherNamespace Complete $ PortDecl pos Input (Bits 0)
			done = Binding 0 "done" OtherNamespace Complete $ PortDecl pos Output (Bits 0)

			toNetworkPort (ref, binding, used) = (used, NetworkPort name direction width ref)
				where
					name = bindingName binding
					decl = bindingValue binding
					direction = declDirection decl
					width = widthOfType cs $ declType decl

			completeDecl binding = makeChan opts (bindingPos binding) True (refBinding binding) width name
				where
					name = bindingName binding
					decl = bindingValue binding
					width = widthOfType cs $ declType decl

			part = defaultConnectWhy undefined ports $ \ports -> return $ Part name (map snd ports) body

			(ports, body) = runNetwork0 $ runWhyT $ do
				(cmdGo, cmdDone) <- visitCmd opts (formals:cs) cmd
				let cmdHasDone = isJust cmdDone
				lift $ nwAddPortAccess Passive GoAccess cmdGo
				lift $ when cmdHasDone $ nwAddPortAccess Active DoneAccess $ fromJust cmdDone
				lift $ nwAddProperty $ NetworkAttrs $ map attrToNameTeakParam attrs
				lift $ when (isJust posArray) $ nwAddProperty $ NetworkPosArray $ fromJust posArray
				portsUsed <- gatherFailMap completeDecl formalPorts
				hasDone <- lift $ liftM isJust $ nwGetAccess DoneAccess
				let
					goDoneFormals
						| hasDone = [(Just GoAccess, go, True), (Just DoneAccess, done, True)]
						| otherwise = [(Just GoAccess, go, True)]

					ports = map toNetworkPort $ goDoneFormals ++
						zip3 (map (Just . InstanceAccess . refBinding) formalPorts) formalPorts portsUsed

					unusedPorts = map snd $ filter (not . fst) ports
				when (not (null unusedPorts)) $ WhyT $ return $
					Why (Wrong [Report pos $ "unused " ++ plural "port" unusedPorts ++ " `" ++
					joinWith ", " (map nwPortName unusedPorts) ++ "' in procedure `" ++ name ++ "'"]) undefined
				return ports

	-- completeEncInputs : complete a set of channel guarded commands and return a NetworkMonad of
	--	(chans, writes, signals, gos, dones) where `signals' are input-valid-signalling Link refs,
	--  and `gos/dones' are command go/done Link refs.  `chans' are chanRefs from the guards.  `writes' are
	--	variable write signals for each chanRef.
	completeEncInputs :: NetworkIF network => [TeakOption] -> [Context Decl] -> [ChanGuard] ->
		WhyT (NetworkMonad network) ([Ref], [NetworkLinkRef], [NetworkLinkRef], [NetworkLinkRef], [NetworkLinkRef])
	completeEncInputs opts cs guards = do -- TODO
		(chans, writes, signals, gos, dones) <- liftM unzip5 $ mapM makeChoice guards

		return (concat chans, concat writes, signals, gos, catMaybes dones)
		where
			makeChoice (ChanGuard pos _ context cmd) = do
				(cmdNlGo, cmdNlDone) <- visitCmd opts cs cmd
				(chans, writes, signals) <- lift $ liftM (unzip3 . catMaybes) $
					mapM completeDecl $ contextBindingsList context
				syncLink <- lift $ makeJoin pos signals
				return (chans, writes, syncLink, cmdNlGo, cmdNlDone)

			completeDecl binding = case decl of
				OpenChanDecl pos chanRef _ -> do
					(write, signal) <- makeFalseVar pos
						(refBinding binding) width builtinOffsets (bindingName binding)
					return $ Just (chanRef, write, signal)
				_ -> return Nothing
				where
					decl = bindingValue binding
					width = widthOfType cs $ declType decl
					builtinOffsets = typeBuiltinOffsets cs 0 $ declType decl

	varRead :: NetworkIF network => Slice Int -> Ref -> NetworkMonad network ([NetworkLinkRef], NetworkLinkRef)
	varRead slice ref = do
		go <- nwNewLinkRef 0
		done <- nwNewLinkRef (sliceWidth slice)
		nwAddAccess $ Access (InstanceAccess ref) [VarAccess Read go done slice]
		return ([go], done)

	-- splitVarAccess : split a read/write into possibly many sub-reads/write split across splitVarIntervals
	--	where there is only one matching interval, the returned access will use the original access's
	--	links, otherwise, new links are made and returned.  Returns (interval, access) where interval
	--	is one of the intervals from splitVarIntervals, in the same order as splitVarIntervals.
	splitVarAccess :: NetworkIF network => [Slice Int] -> AccessBody ->
		NetworkMonad network [(Slice Int, AccessBody)]
	splitVarAccess splitVarIntervals (VarAccess dir ctrlLink dataLink fullInterval) = body splitIntervals
		where
			splitIntervals = filterIntervals splitVarIntervals fullInterval

			body [] = error "splitAccess: can't happen"
			body [interval] = do
				let Just overlap = sliceOverlap interval fullInterval
				return [(interval, VarAccess dir ctrlLink dataLink overlap)]
			body intervals = forM intervals $ \interval -> do
				let Just overlap = sliceOverlap interval fullInterval
				data_ <- nwNewLinkRef $ sliceWidth interval
				ctrl <- nwNewLinkRef 0
				return (interval, VarAccess dir ctrl data_ overlap)
	splitVarAccess _ access = error $ "splitVarAccess: bad access `" ++ show access ++ "'"

	makeVar :: NetworkIF network => [TeakOption] -> Pos -> Ref -> Int -> String -> [Int] ->
		WhyT (NetworkMonad network) ()
	makeVar opts pos ref width name builtinOffsets = do
		access <- lift $ nwRemoveAccess (InstanceAccess ref)
		let
			breakOnWrite = findBoolSubOption opts TeakBreakVariablesOnWrite

			sepRw (ws, rs) access@(VarAccess Read _ _ _) = (ws, access:rs)
			sepRw (ws, rs) access@(VarAccess Write _ _ _) = (access:ws, rs)
			sepRw (ws, rs) _ = (ws, rs)

			(writes, reads) = foldl' sepRw ([], []) $ accessBodys $ fromJust access

			wholeVarInterval = 0 +: width
			splitVarIntervals
				| breakOnWrite = foldl' splitIntervalsByInterval [wholeVarInterval] $ map accessRange writes
				| otherwise = [wholeVarInterval]

			splitWrite access = do
				split <- splitVarAccess splitVarIntervals access
				let
					(_, accesses') = unzip split
					dones = map accessCtrl accesses'
					datas = map accessData accesses'
					offsets = map (sliceLow . accessRange) accesses'
				-- For single go/done cases, connections are made to accessCtrl/accessData access by splitVarAccess
				when (listAtLeastLength 2 dones) $ do
					j pos dones (accessCtrl access)
					return ()
				when (listAtLeastLength 2 datas) $ do
					f pos offsets (accessData access) datas
					return ()
				return split

			splitRead access = do
				split <- splitVarAccess splitVarIntervals access
				let
					(_, accesses') = unzip split
					gos = map accessCtrl accesses'
					datas = map accessData accesses'
				-- For single go/done cases, connections are made to accessCtrl/accessData access by splitVarAccess
				when (listAtLeastLength 2 gos) $ do
					f pos (replicate (length gos) 0) (accessCtrl access) gos
					return ()
				when (listAtLeastLength 2 datas) $ do
					j pos datas (accessData access)
					return ()
				return split

			allSplits = map (\interval -> (interval, [])) $ splitVarIntervals

			-- mergeSplits : merge a list of (interval, thing) into a list of (interval, [thing]) by appending
			mergeSplits prevSplits newSingleSplits = mergeByWith compareFst (\(i, l) (_, r) -> (i, l ++ r))
				prevSplits $ map (\(i, l) -> (i, [l])) newSingleSplits

			splitWrites = liftM (map snd) $ foldM splitAndMerge allSplits writes
				where splitAndMerge prevSplits write = splitWrite write >>= return . mergeSplits prevSplits

			splitReads = liftM (map snd) $ foldM splitAndMerge allSplits reads
				where splitAndMerge prevSplits read = splitRead read >>= return . mergeSplits prevSplits

		if isNothing access
			then return ()
			else do
				writes' <- lift $ splitWrites
				reads' <- lift $ splitReads
				sequence_ $ zipWith3 (newVar opts pos name builtinOffsets wholeVarInterval)
					splitVarIntervals writes' reads'

	newVar :: NetworkIF network => [TeakOption] -> Pos -> String -> [Int] -> Slice Int ->
		Slice Int -> [AccessBody] -> [AccessBody] -> WhyT (NetworkMonad network) ()
	newVar opts pos varName builtinOffsets wholeVarInterval interval writes reads =
		case (writes, reads) of
			([], []) -> return ()
			([], _) -> failPos pos $ "reads but no writes to variable `" ++ varName ++
				"' (in bit range " ++ (balsaShowSlice interval "") ++ ")"
			(_, []) -> do
				lift $ mapM writeContinue writes
				return ()
			_ -> lift $ do
				if mergeSteerForWrites
					then do
						newWg <- nwNewLinkRef width
						newWd <- nwNewLinkRef 0 
						(writeGos, writeDatas) <- mapAndUnzipM (forkNewSignal pos) wg
						m pos writeDatas newWg
						makeDecision opts pos False newWd writeGos wd
						return ([newWg], [newWd])
						v pos name' width thisBuiltinOffsets [0] readOffsets [newWg] [newWd] rg rd
					else v pos name' width thisBuiltinOffsets writeOffsets readOffsets wg wd rg rd
				return ()
		where
			writeContinue (VarAccess Write doneLink writeLink _) = connect pos writeLink doneLink
			writeContinue access = error $ "writeContinue: " ++ show access

			useSingleWritePort = findBoolSubOption opts TeakSingleWritePort

			wg = map accessData writes
			wd = map accessCtrl writes
			rg = map accessCtrl reads
			rd = map accessData reads

			width = sliceWidth interval

			name' = if interval == wholeVarInterval
				then varName
				else varName ++ verilogShowSlice interval ""

			accessRangeOffset accessSlice = sliceLow accessSlice - sliceLow interval

			allWritesAreAcrossWholeV = all ((== interval) . accessRange) writes
			mergeSteerForWrites = useSingleWritePort && allWritesAreAcrossWholeV &&
				listAtLeastLength 2 writes

			builtinSlices = map (\offset -> offset +: builtinTypeWidth) builtinOffsets

			thisBuiltinOffsets = map sliceOffset $ mapMaybe (sliceOverlap interval) builtinSlices
			writeOffsets = map (accessRangeOffset . accessRange) writes
			readOffsets = map (accessRangeOffset . accessRange) reads

	makeFalseVar :: NetworkIF network => Pos -> Ref -> Int -> [Int] -> String ->
		NetworkMonad network (NetworkLinkRef, NetworkLinkRef)
	makeFalseVar pos ref width builtinOffsets name = do
		access <- nwRemoveAccess (InstanceAccess ref)

		if isJust access
			then do
				chanReadLink <- nwNewLinkRef width
				signal <- nwNewLinkRef 0
				let
					bodys = accessBodys $ fromJust access
					ctrlLinks = map accessCtrl bodys
					readLinks = map accessData bodys
					readOffsets = map (sliceLow . accessRange) bodys

				v pos name width builtinOffsets [0] readOffsets
					[chanReadLink] [signal] ctrlLinks readLinks
				return (chanReadLink, signal)
			else if width == 0
				then do
					chanReadLink <- nwNewLinkRef 0
					return (chanReadLink, chanReadLink)
				else do
					chanReadLink <- nwNewLinkRef width
					signal <- nwNewLinkRef 0
					connect pos chanReadLink signal
					return (chanReadLink, signal)

	-- makeChan : connect up a channel's accesses.  Returns True is the channel is used, False otherwise
	makeChan :: NetworkIF network => [TeakOption] -> Pos -> Bool -> Ref -> Int -> String ->
		WhyT (NetworkMonad network) Bool
	makeChan opts pos isPort ref width name = do
		maybeAccess <- lift $ nwRemoveAccess (InstanceAccess ref)
		let
			writes = mapMaybe findOutputs bodys ++ mapMaybe findBareOutputs bodys
			reads = mapMaybe findInputs bodys
			bareInputs = mapMaybe findBareInputs bodys
			bodys = accessBodys $ fromJust maybeAccess

		if isJust maybeAccess
			then makeChanRW writes reads bareInputs
			else return False
		where
			findOutputs (ChanOutputAccess go done) = Just (Just done, go)
			findOutputs _ = Nothing

			findInputs (ChanInputAccess go done) = Just (go, done)
			findInputs _ = Nothing

			findBareInputs (ChanBareInputAccess dat) = Just dat
			findBareInputs _ = Nothing

			findBareOutputs (ChanBareOutputAccess dat) = Just (Nothing, dat)
			findBareOutputs _ = Nothing

			chanError msg = failPos pos $ "on channel `" ++ name ++ "', " ++ msg

			port msg action
				| isPort = action
				| otherwise = chanError msg

			-- writes writeDones
			makeChanW [] = port "incomplete channel, no write" $ return Nothing
			makeChanW [(Nothing, write)] = return $ Just write
			makeChanW [(Just done, write)] = do
				writeSignal <- lift $ forkSignal pos write done
				return $ Just writeSignal
			makeChanW writes = do
				let (writeDones, writeDatas) = unzip writes
				if any isNothing writeDones
					then chanError "some writes are bare"
					else do
						(writeGos, writeDatas') <- lift $ mapAndUnzipM (forkNewSignal pos) writeDatas
						write <- lift $ makeMerge pos m writeDatas'
						(writeMerged, write') <- lift $ forkNewSignal pos write
						lift $ makeDecision opts pos False writeMerged writeGos (map fromJust writeDones)
						return $ Just write'

			-- write readGos reads bareInputs
			makeChanR Nothing [] [] = do
				chanError "unused channel" -- Can't happen?, won't be an access
				-- return False
			makeChanR Nothing reads [] = do -- Only reads
				let (readGos, readDatas) = unzip reads
				write <- lift $ nwNewLinkRef width
				lift $ nwAddAccess $ Access (InstanceAccess ref) [PortLinkAccess Passive write]
				lift $ makeDecision opts pos False write readGos readDatas
				return True
			makeChanR Nothing _ [choice] = do -- Only choice
				lift $ nwAddAccess $ Access (InstanceAccess ref) [PortLinkAccess Passive choice]
				return True
			makeChanR (Just write) [] [] = port "incomplete channel, no reads" $ do
				lift $ nwAddAccess $ Access (InstanceAccess ref) [PortLinkAccess Active write]
				return True
			makeChanR (Just write) reads [] = do
				let (readGos, readDatas) = unzip reads
				lift $ makeDecision opts pos False write readGos readDatas
				return True
			makeChanR (Just _) (_:_) (_:_) = do
				chanError $ "can't use a channel in select/arbitrate/instance input port "
					++ "and also in a channel input statement"
				-- return False
			makeChanR (Just write) _ [choice] = do
				lift $ connect pos write choice
				return True
			makeChanR _ _ (_:_) = do
				chanError $ "channel used in more than one select/arbitrate/instance input port"
				-- return False

			makeChanRW [] [] [] = return False
			makeChanRW writes reads bareInputs = do
				write <- makeChanW writes
				makeChanR write reads bareInputs

	-- makeShared :: NetworkIF network =>
	--	Position -> [Context Decl] -> Ref -> Binding -> NetworkMonad network ()
	makeShared :: NetworkIF network => [TeakOption] -> [Context Decl] -> Pos -> Ref -> Binding Decl ->
		WhyT (NetworkMonad network) ()
	makeShared opts cs pos ref binding = do
		maybeAccess <- lift $ nwRemoveAccess (InstanceAccess ref)

		if isNothing maybeAccess
			then return ()
			else do
				let
					bodys = accessBodys $ fromJust maybeAccess
					cmd = declCmd $ bindingValue binding
					gos = map accessGo bodys
					dones = map accessDone bodys

					insertShared = do
						(sharedGo, sharedDone) <- visitCmd opts cs cmd
						-- FIXME, this isn't true allow no dones in shared call cmds
						if isNothing sharedDone
							then failPos pos "Shared blocks *must* terminate"
							else return (sharedGo, fromJust sharedDone)

				case (gos, dones) of
					([], []) -> return ()
					([go], [done]) -> do
						(sharedGo, sharedDone) <- insertShared
						lift $ connect pos go sharedGo
						lift $ connect pos sharedDone done
						return ()
					(_, _) -> do
						(sharedGo, sharedDone) <- insertShared
						(newGos, newDones) <- lift $ mapAndUnzipM (forkNewSignal pos) gos
						lift $ m pos newGos sharedGo
						lift $ makeDecision opts pos False sharedDone newDones dones
						return ()

