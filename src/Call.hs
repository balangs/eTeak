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

module Call (
	callExprEvalMid,
	callExprEvalPost,
	callCmdEvalMid,
	callCmdEvalPost,
	declIsPermanent,
	cmdIsPermanent,
	cmdIsNoCmd,
	Eval,
	procAliasEvalMid,
	procAliasEvalPost
	) where

	-- import Debug.Trace

        import Prelude hiding (traverse)
	import ParseTree
	import Context
	import Type
	import Misc
	import Traverse
	import Chan
	import Report
	import Print

	import Data.Maybe
	import Data.Array
	import Data.List

	type Eval t = [Context Decl] -> t -> (Completeness, t)

	evalError :: ShowParseNode node => [Context Decl] -> Pos -> node -> String -> (Completeness, node)
	evalError cs pos node message = (parseNodeError cs pos node message, node)

	actualExprToType :: ShowParseNode node =>
		Pos -> [Context Decl] -> Expr -> node -> (Type -> node) -> (Completeness, node)
	actualExprToType pos cs expr errorNode typeCons = body expr
		where
			expectingAType = evalError cs pos errorNode "expecting a type"

			body (MaybeTypeExpr _ ref) = (Complete, typeCons $ Type ref)
			body otherExpr
				| isJust maybeRef && isJust maybeTypeRef = case bindingValue binding of
					TypeDecl {} -> (Complete, typeCons $ Type typeRef)
					TypeParamDecl {} -> (Complete, typeCons $ Type typeRef)
					_ -> expectingAType
				| otherwise = expectingAType
				where
					maybeRef = exprRef otherExpr
					ref = fromJustDebug "B" maybeRef
					maybeTypeRef = findRefByName cs TypeNamespace $ bindingName $ findBindingByRef cs ref
					typeRef = fromJustDebug "C" maybeTypeRef
					binding = findBindingByRef cs typeRef

	prepareCallExprMidActual :: Pos -> [Context Decl] -> Binding Decl -> FuncActual -> (Completeness, FuncActual)
	prepareCallExprMidActual pos cs formal actual = body actual $ bindingValue formal
		where
			body (TypeFuncActual {}) (TypeParamDecl {}) = (Complete, actual)
			body (TypeFuncActual {}) _ = evalError cs pos actual "not expecting a type actual"
			body (ExprFuncActual _ expr) (TypeParamDecl {}) =
				actualExprToType (exprPos expr) cs expr actual TypeFuncActual
			body (ExprFuncActual _ expr) (ParamDecl _ isParam typ) = (Complete, ExprFuncActual isParam $
				TypeCheckExpr (exprPos expr) Transient typ expr)
			-- FIXME, not sure if this needs to be here
			body (ExprFuncActual isParam expr) decl = (Complete, ExprFuncActual isParam $
				TypeCheckExpr (exprPos expr) Transient typ expr)
				where typ = typeOfDecl decl

	substTypes :: ParseTreeTraverse t => [(Ref, Type)] -> t -> t
	substTypes refToTypePairs node = node'
		where
			(_, _, node') = traverse substApply () () node

			substApply = emptyApply { preType = preNoAccum substPreType }

			substPreType cs (Type ref)
				| isJust subst = (cs, fromJustDebug "D" subst)
				where subst = lookup ref refToTypePairs
			substPreType cs typ = (cs, typ)

	-- contextUnaliasedTypes : returns a list of (Ref, Type) mappings for removing aliased type
	--	refs in the given context.  Using this list with substTypes can remove references to aliased
	--	types in context from a node.
	contextUnaliasedTypes :: [Context Decl] -> Context Decl -> [(Ref, Type)]
	contextUnaliasedTypes cs context = mapMaybe replaceType $ contextBindingsList context
		where
			replaceType binding = body $ bindingValue binding
				where
					body (TypeDecl _ (AliasType _ typ)) = Just (refBinding binding, unaliasType cs typ)
					body _ = Nothing

	-- makeBuiltinCall : makes all referred-to types structural in each element of a builtin call's arguments
	--	and returns a BuiltinCallExpr node
	makeBuiltinCall :: [Context Decl] -> Pos -> String -> [FuncActual] -> [Expr] -> Type -> Expr
	makeBuiltinCall cs pos name params exprs returnType = BuiltinCallExpr pos name params' exprs' returnType
		where
			apply = emptyApply { applyContextCons = (:), postType = structPostType }
			structPostType localCs (_, typ) = ((), typeToStructType localCs $ unaliasType localCs typ)

			(_, _, params') = traverse apply cs () params
			(_, _, exprs') = traverse apply cs () exprs

	actualExprToChan :: [Context Decl] -> Expr -> Chan -> (Completeness, Chan)
	actualExprToChan cs expr errorNode = (result, actual)
		where
			expectingAChan = (parseNodeError cs (exprPos expr) expr "expecting a channel", errorNode)
			(result, actual) = body expr

			body (SliceExpr pos subExpr li ri)
				| subExprR == Complete = (Complete, SliceChan pos subExpr' li ri)
				| otherwise = (subExprR, undefined)
				where (subExprR, subExpr') = actualExprToChan cs subExpr errorNode
			body (ConsExpr pos _ NoType exprs)
				| exprR == Complete = (Complete, FlatArrayedChan pos interval exprs')
				| otherwise = (exprR, undefined)
				where
					(exprRs, exprs') = unzip $ map (\e -> actualExprToChan cs e errorNode) exprs
					exprR = gatherCompleteness exprRs
					interval = intervalFromRange (0, toInteger (length exprs) - 1)
			body (ConsExpr {}) = (parseNodeError cs (exprPos expr)
				errorNode "invalid arrayed channel expression", undefined)
			body (NameExpr pos string) = (Complete, NameChan pos string)
			body (IndexExpr pos subExpr i) = (subExprR, IndexChan pos subExpr' i)
				where (subExprR, subExpr') = body subExpr
			body (PartialArrayedRead pos ref) = (Complete, PartialArrayedChan pos ref)
			body (MaybeOtherExpr pos ref) = case decl of
				ChanDecl {} -> (Complete, Chan pos ref)
				PortDecl {} -> (Complete, Chan pos ref)
				ArrayedDecl {} -> (Complete, PartialArrayedChan pos ref)
				_ -> expectingAChan
				where
					binding = findBindingByRef cs ref
					decl = bindingValue binding
			body _ = expectingAChan

	-- topLevelProcedure : Complete implies all parameters have been applied
	-- 	Permanence is important to allow `local channel' behaviour across ports where modules aren't
	--	permanent
	topLevelProcedure :: [Context Decl] -> Bool -> Completeness -> Decl -> Bool
	topLevelProcedure cs True Complete decl = declIsPermanent cs decl
	topLevelProcedure _ _ _ _ = False

	isChanProcActual :: ProcActual -> Bool
	isChanProcActual (ChanProcActual {}) = True
	isChanProcActual _ = False

	-- FIXME, need to pass on errors
	-- FIXME, this should be *much* cleaner and integrated with stuff in Chan.hs
	-- consider getting rid of Partial... for Chan after decls have been completed
	-- FIXME, check multidimensional support
	flattenChanIntoAliasedDecl :: Pos -> [Context Decl] -> Chan -> Decl
	flattenChanIntoAliasedDecl pos cs (Chan _ ref) = AliasDecl pos ref $ typeOfRef cs ref
	flattenChanIntoAliasedDecl pos cs (FlatArrayedChan _ interval chans) = FlatArrayedDecl pos interval chans'
		where chans' = listArray (intervalRange interval) $ map (flattenChanIntoAliasedDecl pos cs) chans
	flattenChanIntoAliasedDecl pos cs chan@(PartialArrayedChan _ ref) = ret
		where
			indexRange = intervalRange $ arrayedDeclInterval cs $ bindingValue $ findBindingByRef cs ref
			(_, ret) = arrayedRefSliceEval cs ref indexRange makeFlatArrayedDecl
				partial (makeAliasForIndexOfDecl cs pos chan) noError
			noError str = (Complete, error str)
			partial arrRef = (Complete, flattenChanIntoAliasedDecl pos cs (PartialArrayedChan pos arrRef))
			makeFlatArrayedDecl interval decls = FlatArrayedDecl pos interval decls'
				where decls' = listArray (intervalRange interval) decls
	flattenChanIntoAliasedDecl _ _ chan = error $ "flattenChanIntoAliasedDecl: can't handle " ++ show chan

	makeAliasForIndexOfDecl :: ShowParseNode node =>
		[Context Decl] -> Pos -> node -> Decl -> Ref -> (Completeness, Decl)
	makeAliasForIndexOfDecl cs pos _ (ChanDecl {}) indexRef = (Complete, AliasDecl pos indexRef $ typeOfRef cs indexRef)
	makeAliasForIndexOfDecl cs pos _ (PortDecl {}) indexRef = (Complete, AliasDecl pos indexRef $ typeOfRef cs indexRef)
	makeAliasForIndexOfDecl cs pos node decl@(OpenChanDecl {}) (IndexRef _ i) = (parseNodeError cs pos node $
		"index `" ++ show i ++ "' cannot be used as a channel when enclosing this command", decl)
	makeAliasForIndexOfDecl cs pos node decl (IndexRef _ i) = (parseNodeError cs pos node $
		"index `" ++ show i ++ "' is not a channel", decl)
	makeAliasForIndexOfDecl _ _ _ _ _ = error "makeAliasForIndexOfDecl: bad args"

	prepareProcActual :: Eval Decl -> Pos -> [Context Decl] -> Binding Decl -> ProcActual ->
		(Completeness, (ProcActual, Decl))
	prepareProcActual _evalDecl pos cs formal actual = body actual decl
		where
			evalError' str = (comp, (actual, decl))
				where (comp, _) = evalError cs pos actual str
			decl = bindingValue formal
			typ = declType decl

			exprToChan decl expr
				| chanR == Complete = (Complete, (ChanProcActual $ CheckChan pos decl chan, decl))
				| otherwise = (chanR, (actual, decl))
				where (chanR, chan) = actualExprToChan cs expr undefined

			body (TypeProcActual {}) decl@(TypeParamDecl {}) = (Complete, (actual, decl))
			body (TypeProcActual {}) _ = evalError' "not expecting a type actual"
			body (ExprProcActual expr) decl@(TypeParamDecl {}) = (comp, (actual', decl))
				where (comp, actual') = actualExprToType pos cs expr actual TypeProcActual
			body (ExprProcActual expr) decl@(ParamDecl {}) =
				(Complete, (ExprProcActual $ ConstCheckExpr pos $ TypeCheckExpr pos Transient typ expr, decl))
			body (ExprProcActual expr) decl@(PortDecl {}) = exprToChan decl expr
			body (ExprProcActual expr) decl@(ArrayedDecl {}) = exprToChan decl expr
			{- FIXME, need to make conditional ports work
			body actual@(ExprProcActual {}) decl@(CaseDecl {}) = whyToPairDefault (actual, decl) $ do
				-- decl' <- pairToWhy $ evalDecl cs decl
				let decl' = PortDecl NoPos Output (Bits 1)
				pairToWhy $ body actual decl'
				-}
			body (ExprProcActual _) _ = evalError' "not expecting an expression"
			body (ChanProcActual _) _ = (Complete, (actual, decl))

	-- makeActualsArray : make an array of (Just actual) elements based on matching actuals to
	--	their corresponding formals in the given formals list.  Populates all other elements of
	--	the returned array with Nothing
	makeActualsArray :: Context Decl -> [a] -> Array Int (Maybe a)
	makeActualsArray formals actuals = (array formalsIntervals $ map (\i -> (i, Nothing)) formalsIndices) //
		(snd $ mapAccumL insertActuals actuals formalsIndices)
		where
			formalsIntervals = contextIndexRange formals
			formalsIndices = range formalsIntervals

			insertActuals [] i = ([], (i, Nothing))
			insertActuals (a:as) i
				| isNoDecl (bindingValue (findBindingByRef [formals] (Ref i))) =
					(a:as, (i, Nothing))
				| otherwise = (as, (i, Just a))

	prepareCombine :: (Completeness, [a]) -> (Completeness, [a]) -> (Completeness, [a])
	prepareCombine (lComplete, lActuals) (rComplete, rActuals) = (andCompleteness lComplete rComplete,
		lActuals ++ rActuals)

	callCmdPrepareContextAndActuals :: Eval Decl -> Pos -> [Context Decl] -> Binding Decl -> [ProcActual] ->
		(Completeness, Context Decl, [ProcActual])
	callCmdPrepareContextAndActuals eval pos cs procBinding actuals
		| actualsCount > formalsCount = (Wrong [Report pos $ "more actual arguments (" ++
			show actualsCount ++ ") than formal arguments (" ++ show formalsCount ++ ")"],
			formals, actuals)
		| otherwise = (result, formals', actuals')
		where
			actualsCount = length actuals
			formalsCount = validDeclContextSize formals
			decl = bindingValue procBinding
			formals = rebaseContextRefs cs $ declFormals decl

			actualsArray = makeActualsArray formals actuals

			preparePostBinding localCs (_, binding)
				-- FIXME, think about this
				--  | isNothing actual = ((bindingCompleteness binding, []), binding)
				-- skip rest of unapplied args, considering them `Complete' for the moment
				| isNothing actual = ((Complete, []), binding)
				| actualR == Complete = case {- trace (show decl') -} decl' {- bindingValue binding -} of
					ParamDecl {} -> (r, binding
						{ bindingValue = ExprDecl pos (procActualExpr actual') })
					TypeParamDecl {} -> (r, binding
						{ bindingValue = TypeDecl pos $ AliasType pos (procActualType actual')})
					-- PortDecl {} -> (r, binding { bindingValue = ChanExprDecl $ chan })
					_ -> (r, binding { bindingValue = decl' })
				| otherwise = (r, binding)
				where
					actual = bangDebug "callCmdPrepareContextAndActuals.getActual"
						actualsArray (bindingIndex binding)
					(actualR, (actual', decl')) = prepareProcActual eval pos localCs binding $ fromJust actual
					r = (actualR, [actual'])
					-- ChanProcActual chan = actual'

			bindActualsApply = emptyApply {
				applyContextCons = (:),
				nullResult = (Complete, []),
				combineResult = prepareCombine,
				postBinding = preparePostBinding }

			(_, (result, actuals'), formals') = traverse bindActualsApply cs () formals

	callExprEvalMid :: Eval Expr
	callExprEvalMid cs node@(CallExpr pos (Callable ref) _ actuals)
		| funcCompleteness < Incomplete = evalError cs pos node $ "skipping call to function `" ++ funcName ++ "'"
		| not (isFuncDecl decl || isBuiltinFuncDecl decl) = evalError cs pos node "not a function"
		| formalsCount /= length actuals =
			evalError cs pos node "length of function arguments different from formal argument list"
		| result == Complete = (Complete, CallExpr pos (Callable ref) formals' actuals')
		| otherwise = (result, node)
		where
			funcName = bindingName funcBinding
			funcBinding = findBindingByRef cs ref
			decl = bindingValue funcBinding
			funcCompleteness = bindingCompleteness funcBinding
			formalsCount = validDeclContextSize formals
			formals = rebaseContextRefs cs $ declFormals decl

			actualsArray = makeActualsArray formals actuals

			preparePostBinding localCs (_, binding)
				| actualR == Complete = case bindingValue binding of
					ParamDecl {} -> (r, binding { bindingValue = ExprDecl pos expr })
					TypeParamDecl {} -> (r, binding { bindingValue = TypeDecl pos $ AliasType pos typ })
					_ -> (r, binding)
				| otherwise = (r, binding)
				where
					actual = bangDebug "callExprEvalMid.getActual"
						actualsArray (bindingIndex binding)
					(actualR, actual') = prepareCallExprMidActual pos localCs binding $ fromJust actual
					r = (actualR, [actual'])
					ExprFuncActual _ expr = actual'
					TypeFuncActual typ = actual'

			bindActualsApply = emptyApply {
				applyContextCons = (:),
				nullResult = (Complete, []),
				combineResult = prepareCombine,
				postBinding = preparePostBinding }

			(_, (result, actuals'), formals') = traverse bindActualsApply cs () formals
	callExprEvalMid _ _ = error "callExprEvalMid: not a CallExpr"

	callExprEvalPost :: Eval Expr -> Eval Type -> Eval Expr
	callExprEvalPost evalExpr evalType cs (CallExpr pos (Callable ref) formals actuals)
		| isFuncDecl decl = (funcResult, funcExpr''')
		| isBuiltinFuncDecl decl = (returnTypeResult,
			makeBuiltinCall (formals:cs) pos name structParams builtinExprs returnType)
		where
			binding = findBindingByRef cs ref
			name = bindingName binding
			decl = bindingValue binding
			funcExpr = declExpr decl
			funcExpr'
				| isEmptyContext formals = funcExpr
				| otherwise = rebaseRefToContext cs ref funcExpr
			(funcResult, funcExpr'') = evalExpr (formals:cs) funcExpr'
			funcExpr''' = substTypes replacementTypes funcExpr''
			replacementTypes = contextUnaliasedTypes cs formals

			isNonParamActual (ExprFuncActual False _) = True
			isNonParamActual _ = False

			(builtinActuals, params) = partition isNonParamActual actuals
			builtinExprs = map funcActualExpr builtinActuals

			structParams = map makeStructParam params

			makeStructParam (TypeFuncActual typ) = TypeFuncActual $ typeToStructType cs typ
			makeStructParam param = param

			(returnTypeResult, returnType) = evalType (formals:cs) typ
				-- where typ = rebaseRefToContext cs ref $ declType decl
				where typ = substTypes replacementTypes $ rebaseRefToContext cs ref $ declType decl
	callExprEvalPost _ _ _ _ = error "callExprEvalPost: not a CallExpr"

	callCmdEvalMid :: Eval Decl -> [Context Decl] -> Cmd -> (Completeness, Cmd)
	callCmdEvalMid eval cs node@(CallCmd pos (Callable ref) _ actuals)
		| procCompleteness < Incomplete = evalError cs pos node $ "skipping call to procedure `" ++ procName ++ "'"
		| isSharedDecl decl = (Complete, SharedCallCmd pos (Callable ref))
		| not $ isProcDecl decl = evalError cs pos node "not a procedure"
		| result /= Complete = (result, CallCmd pos (Callable ref) formals' actuals')
		| topLevelProcedure cs topLevel procCompleteness decl = if allActualsAreChans
			then (Complete, InstanceCallCmd pos (Callable ref) chans) -- FIXME, need flatten chans
			else evalError cs pos node $
				"not all arguments resolve to channels in non-inlining call"
		| otherwise = (result, CallCmd pos (Callable ref) formals' actuals')
		where
			topLevel = bindingIsTopLevel cs ref
			chans = map (\(ChanProcActual chan) -> chan) actuals'
			allActualsAreChans = all isChanProcActual actuals'
			binding = findBindingByRef cs ref
			decl = bindingValue binding
			procCompleteness = bindingCompleteness binding
			procName = bindingName binding
			(result, formals', actuals') = callCmdPrepareContextAndActuals eval pos cs binding actuals
	callCmdEvalMid _ _ _ = error "callCmdEvalMid: not a CallCmd"

	callCmdEvalPost :: Eval Cmd -> Eval Cmd
	callCmdEvalPost eval cs node@(CallCmd pos (Callable ref) formals actuals)
		| procCompleteness < Incomplete = evalError cs pos node $ "skipping call to procedure `" ++ procName ++ "'"
		| actualsCount /= formalsCount = evalError cs pos node $ "incorrect number of actual parameters, " ++
			"expecting " ++ show formalsCount ++ ", got " ++ show actualsCount
		| otherwise = (r, node')
		where
			actualsCount = length actuals
			formalsCount = validDeclContextSize formals
			binding = findBindingByRef cs ref
			procCompleteness = bindingCompleteness binding
			procName = bindingName binding
			procedure = bindingValue binding

			actualsArray = makeActualsArray formals actuals

			-- Note that formals are already context rebased to cs
			formals' = contextMap substInPort formals
				where
					substInPort formal = case (bindingValue formal, actual) of
						(PortDecl {}, Just (ChanProcActual chan)) -> formal
							{ bindingValue = flattenChanIntoAliasedDecl pos (formals:cs) chan }
						(ArrayedDecl _ _ (PortDecl {}), Just (ChanProcActual chan)) -> formal
							{ bindingValue = flattenChanIntoAliasedDecl pos (formals:cs) chan }
						_ -> formal
						where actual = bangDebug "callCmdEvalPost.substInPort" actualsArray (bindingIndex formal)

			(r, node') = eval cs $ BlockCmd pos formals' $ rebaseRefToContext cs ref $ declCmd procedure
	callCmdEvalPost _ _ _ = error "callCmdEvalPost: not a CallCmd"

	procAliasEvalMid :: Eval Decl -> [Context Decl] -> Decl -> (Completeness, Decl)
	procAliasEvalMid eval cs node@(ProcAliasDecl pos (Callable ref) _ actuals)
		| procCompleteness < Incomplete = evalError cs pos node $ "skipping call to procedure `" ++ procName ++ "'"
		| not $ isProcDecl decl = evalError cs pos node "not a procedure"
		| otherwise = (result, ProcAliasDecl pos (Callable ref) formals' actuals')
		where
			binding = findBindingByRef cs ref
			decl = bindingValue binding
			procCompleteness = bindingCompleteness binding
			procName = bindingName binding
			(result, formals', actuals') = callCmdPrepareContextAndActuals eval pos cs binding actuals
	procAliasEvalMid _ _ _ = error "procAliasEvalMid: not a ProcAlias"

	procAliasEvalPost :: Eval Decl -> Eval Decl
	procAliasEvalPost eval cs node@(ProcAliasDecl pos (Callable ref) formals actuals)
		| procCompleteness < Incomplete = evalError cs pos node $ "skipping call to procedure `" ++ procName ++ "'"
		| otherwise = (r, node')
		where
			binding = findBindingByRef cs ref
			procCompleteness = bindingCompleteness binding
			procName = bindingName binding
			-- FIXME, remove aliasing in Callables?
			proc = bindingValue binding
			procCmd = {- substTypes replacementTypes $ -} rebaseRefToContext cs ref $ declCmd proc
			replacementTypes = contextUnaliasedTypes cs formals

			-- FIXME, implement with ProcDecl port port (BlockCmd pos expr type (AliasDecl ...) (AliasDecl ...)
			--                                  `----`====================================='---------------'
			-- type structure to make port skipping easier?
			-- FIXME, leaves a structure with `missing' refs.  Also modify to allow named actuals
			-- maybe make the actuals a [Maybe ProcActual], or [(Maybe String, ProcActual)], or
			-- a naming wrapper for ProcActuals
			(lowFormals, highFormals) = contextIndexRange formals
			actualsCount = length actuals

			(r, ProcDecl _ formalsEvaled procAttrsEvaled procCmdEvaled) = eval cs $
				ProcDecl pos formals (declAttrs proc) procCmd
			formalsEvaledList = contextBindingsList formalsEvaled

			-- droppedFormalCount : number of formals to skip, including those which are NoDecl
			droppedFormalCount = body actualsCount 0 formalsEvaledList
				where
					body 0 ret _ = ret
					body toDrop ret (b:bs)
						| isNoDecl (bindingValue b) = body toDrop (ret + 1) bs
						| otherwise = body (toDrop - 1) (ret + 1) bs
					body toDrop ret [] = error $ "procAliasEvalPost: run out of actuals; toDrop: "
						++ show toDrop ++ " ret: " ++ show ret

			formalsWoActuals = bindingsToContext (lowFormals + droppedFormalCount, highFormals) $
				drop droppedFormalCount formalsEvaledList
			-- FIXME, have a more general (although, for Exprs, slightly unnecessary) substParam
			--	function instead of substTypes?
			node' = rebaseNodeRefs lowFormals (- droppedFormalCount) $ substTypes replacementTypes $
				ProcDecl pos formalsWoActuals procAttrsEvaled procCmdEvaled
	procAliasEvalPost _ _ _ = error "procAliasEvalPost: not a ProcAlias"

	rebaseNodeRefs :: ParseTreeTraverse node => Int -> Int -> node -> node
	rebaseNodeRefs lowIndex adjustment node = node'
		where
			(_, _, node') = traverse rebaseApply () () node

			rebaseApply = emptyApply {
				preRef = preNoAccum rebasePreRef,
				preContext = preNoAccum rebasePreContext }

			rebasePreContext cs context = (cs, rebaseContextIndices adjustment context)

			rebasePreRef cs (Ref int)
				-- Only renumber refs to *this* context, not others
				| int >= lowIndex = (cs, Ref $ int + adjustment)
			rebasePreRef cs ref = (cs, ref)

	-- rebaseRefToContext : rebase a node, which is in the context of the declaration referred to by ref,
	--	to the context `toContext(s)'
	rebaseRefToContext :: ParseTreeTraverse node => [Context Decl] -> Ref -> node -> node
	rebaseRefToContext [] _ _ = error "rebaseRefToContext: need a non-root toContext"
	rebaseRefToContext toContexts ref node = rebaseNodeRefs firstOldIndex adjustment node
		where
			fromContext = findBindingContextByRef toContexts $ unaliasRef toContexts ref
			firstOldIndex = contextsNextIndex [fromContext]
			firstNewIndex = contextsNextIndex toContexts
			adjustment = firstNewIndex - firstOldIndex

	-- rebaseContextRefs : change the range of Refs within a context to bring it under the context list `cs'
	--	references to bindings within `context' have their reference numbers changed but other Refs do not.
	rebaseContextRefs :: [Context Decl] -> Context Decl -> Context Decl
	rebaseContextRefs cs context = rebaseNodeRefs firstOldIndex adjustment context
		where
			firstNewIndex = contextsNextIndex cs
			(firstOldIndex, _) = bounds $ contextBindings context
			adjustment = firstNewIndex - firstOldIndex

	cmdIsNoCmd :: Cmd -> Bool
	cmdIsNoCmd NoCmd = True
	cmdIsNoCmd (LabelCmd _ _ NoCmd) = True
	cmdIsNoCmd _ = False

	cmdIsPermanent :: [Context Decl] -> Cmd -> Bool
	cmdIsPermanent cs cmd = isPermanentCmd cs cmd
		where
			isPermanentDecl cs node = r where (_, r, _) = traverse permApply cs () node
			isPermanentCmd cs node = r where (_, r, _) = traverse permApply cs () node
			isPermanentGuard cs node = r where (_, r, _) = traverse permApply cs () node

			permApply = emptyApply {
				applyContextCons = (:),
				nullResult = False,
				combineResult = (||),
				postCmd = permPostCmd }

			-- permanent
			permPostCmd _ (_, node@(LoopCmd {})) = (True, node)
			-- maybe
			permPostCmd cs (_, node@(InstanceCallCmd _ (Callable ref) _)) =
				(isPermanentDecl cs $ bindingValue calledBinding, node)
				where calledBinding = findBindingByRef cs ref
			-- combined commands aren't necessarily
			permPostCmd _ (_, node@(WhileCmd {})) = (False, node)
			permPostCmd _ (_, node@(CaseCmdE {})) = (False, node)
			permPostCmd cs (_, node@(CaseCmd _ _ _ cmds)) = (all (isPermanentCmd cs) cmds, node)
			permPostCmd cs (_, node@(SelectCmd _ _ guards)) = (all (isPermanentGuard cs) guards, node)
			-- SharedCall?
			-- if sub commands are
			permPostCmd _ rn = rn

	declIsPermanent :: [Context Decl] -> Decl -> Bool
	declIsPermanent cs proc@(ProcDecl {})
		| isJust $ findAttr "reallyInstantiate" $ declAttrs proc = True
		| otherwise = cmdIsPermanent cs $ declCmd proc
	declIsPermanent _ _ = False
