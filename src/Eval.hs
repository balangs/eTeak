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

module Eval (
	evalContext,
	eval
	) where

	-- import Debug.Trace

	import ParseTree
	import Misc
	import Traverse
	import Context
	import Type
	import Expr
	import Report
	import Chan
	import Bits
	import Call
	import Print

	import Data.Maybe
	import Data.List
	import Data.Bits

	eval :: ParseTreeTraverse a => [Context Decl] -> a -> Why a
	eval cs node = Why r node'
		where (_, r, node') = traverse evalApply cs () node

	type Post node = ApplyPostFunc [Context Decl] Completeness node

	evalContext :: Context Decl -> Why (Context Decl)
	evalContext context = eval [context] context

	evalContextApply :: Apply [Context Decl] () Completeness
	evalContextApply = emptyApply {
		applyContextCons = (:),
		nullResult = Complete,
		combineResult = andCompleteness }

	evalApply :: Apply [Context Decl] () Completeness
	evalApply = evalContextApply {
		midInterval = evalMidInterval, postInterval = evalPostInterval,
		midChan = evalMidChan,
		midCmd = evalMidCmd, postCmd = evalPostCmd,
		midExpr = evalMidExpr, postExpr = evalPostExpr, finalExpr = evalFinalExpr,
		postCaseDeclGuard = evalPostCaseDeclGuard,
		midCaseCmdGuard = evalMidCaseCmdGuard,
		postCaseCmdGuard = evalPostCaseCmdGuard,
		postBinding = evalPostBinding,
		postChan = evalPostChan,
		midDecl = evalMidDecl, postDecl = evalPostDecl,
		midLvalue = evalMidLvalue, postLvalue = evalPostLvalue,
		postType = evalPostType,
		postTypeBody = evalPostTypeBody,
		postCaseMatch = evalPostCaseMatch,
		postCallable = evalPostCallable,
		postRef = evalPostRef }

	appendResult :: Completeness -> (Completeness, node) -> (Completeness, node)
	appendResult r1 (r2, node) = (andCompleteness r1 r2, node)	

	typeCheckExpr :: Type -> Expr -> Expr
	typeCheckExpr typ expr = TypeCheckExpr (exprPos expr) Transient typ expr

	typeCheckLvalue :: Type -> Lvalue -> Lvalue
	typeCheckLvalue typ lvalue = TypeCheckLvalue (lvaluePos lvalue) Transient typ lvalue

	plainEvalError :: Pos -> node -> String -> (Completeness, node)
	plainEvalError pos node message = (Wrong [Report pos message], node)

	evalError :: ShowParseNode node => [Context Decl] -> Pos -> node -> String -> (Completeness, node)
	evalError cs pos node message = (parseNodeError cs pos node message, node)

	unboundError :: String -> String -> Pos -> node -> (Completeness, node)
	unboundError typ name pos node = (Wrong [Report pos $ typ ++ "name `" ++ name ++ "' is unbound"], node)

	unboundRefError :: [Context Decl] -> String -> Ref -> Pos -> node -> (Completeness, node)
	unboundRefError cs typ ref pos node = (Wrong [Report pos $ typ ++ "name `" ++ name ++ "' is unbound"], node)
		where name = bindingName $ findBindingByRef cs ref

	isArrayType :: [Context Decl] -> Type -> Bool
	isArrayType cs typ = classOfType cs typ == ArrayClass

	traverseNoAccum :: ParseTreeTraverse node => Apply context () result -> context -> node -> (result, node)
	traverseNoAccum apply context node = (r, node')
		where (_, r, node') = traverse apply context () node

	-- -- Ref

	evalPostRef :: Post Ref
	evalPostRef cs (_, ref@(Ref {}))
		| comp == Complete = ok ref
		| otherwise = (Incomplete, ref)
		where
			comp = bindingCompleteness binding
			binding = findBindingByRef cs ref
	evalPostRef _ rn = rn

	-- -- Binding

	evalPostBinding :: Post (Binding Decl)
	-- FIXME, think about the 'Complete' on the next line very hard
	evalPostBinding _ (r@Incomplete, binding@(Binding {})) = ok $ binding { bindingCompleteness = r }
	evalPostBinding _ (r@Complete, binding@(Binding {})) = ok $ binding { bindingCompleteness = r }
	evalPostBinding _ (r, binding@(Binding {})) = (r, binding { bindingCompleteness = r })

	-- -- Interval

	evalMidInterval :: Post Interval
	evalMidInterval cs (Complete, IntervalE pos left right)
		| classOfType cs typ == EnumClass = ok $ IntervalE pos left $ typeCheckExpr typ right
		where typ = typeOfExpr cs left
	evalMidInterval _ rn = rn

	evalPostInterval :: Post Interval
	evalPostInterval cs (Complete, node@(IntervalE pos left right)) = makeInterval pos cs node left right
	evalPostInterval cs (Complete, node@(IntervalOver pos typ))
		| typeClass == NumClass = ok $ Interval numRange typ'
		| typeClass == EnumClass = ok $ Interval enumRange typ'
		| otherwise = evalError cs pos node "`over' bounds can only apply to numeric or enumeration types"
		where
			typ' = unaliasType cs typ
			typeClass = classOfType cs typ'
			numRange = rangeOfNumType typ'
			enumRange = rangeOfEnumType $ typeGetUnaliasedBody cs typ'
	evalPostInterval _ (r, node@(Interval {})) = (r, node)
	evalPostInterval _ (r, node) = appendResult r $ (Incomplete, node)

	-- -- Decl

	evalMidDecl :: Post Decl
	evalMidDecl cs (Complete, node@(ProcAliasDecl {})) = procAliasEvalMid eval cs node
		where eval cs = traverseNoAccum evalApply cs
	evalMidDecl cs (comp, node@(CaseDecl pos expr gs elseDecl))
		| comp == Complete && isJust (constExpr expr) = ok node'
		| comp == Complete = evalError cs pos node "conditional declaration guards must be constant"
		| comp == Incomplete = (Incomplete, node')
		where
			node' = CaseDecl pos expr (map modGuard gs) (DeferDecl (declPos elseDecl) Complete elseDecl)

			modGuard (CaseDeclGuard pos matches decl) =
				CaseDeclGuard pos (map (typeCheckMatch typ) matches) (DeferDecl (declPos decl) Complete decl)
			modGuard _ = error "modGuard: can't happen"

			typ = typeOfExpr cs expr
	evalMidDecl _ (r, DeferDecl pos _ decl) = ok $ DeferDecl pos r decl
	evalMidDecl _ rn = rn

	unDeferDecl :: Decl -> Decl
	unDeferDecl (DeferDecl _ _ decl) = decl
	unDeferDecl decl = decl

	deferDeclResult :: Decl -> Completeness
	deferDeclResult (DeferDecl _ r _) = r
	deferDeclResult _ = error "deferDeclResult: bad decl"

	unDeferDeclGuard :: CaseDeclGuard -> CaseDeclGuard
	unDeferDeclGuard (CaseDeclGuard pos matches decl) = CaseDeclGuard pos matches $ unDeferDecl decl
	unDeferDeclGuard guard = guard

	flattenCaseDeclGuards :: CaseDeclGuard -> [CaseDeclGuard]
	flattenCaseDeclGuards (ListCaseDeclGuard gs) = concatMap flattenCaseDeclGuards gs
	flattenCaseDeclGuards g = [g]

	evalPostDecl :: Post Decl
	evalPostDecl cs (Complete, node@(OpenChanDeclE pos ref indicess))
		| completeness == Complete && isNothing indicess' =
			evalError cs pos node "not all channel indices are constant"
		| completeness == Complete && not (null badIndices) =
			evalError cs pos node $ "bad (or repeated) arrayed indices " ++ joinWith ", " (map showIndex badIndices)
		| completeness == Complete = ok flatDeclArray'
			where
				showIndex = concatMap (\i -> "[" ++ show i ++ "]")
				binding = findBindingByRef cs ref
				completeness = bindingCompleteness binding
				decl = bindingValue binding
				flatDeclArray = flattenArrayedDecl cs decl ref
				indicess' = mapM (mapM (\(l, r) -> do
					lc <- constExpr l
					rc <- constExpr r
					return (lc, rc))) indicess
				badIndices = concat badIndicess
				(flatDeclArray', badIndicess) = mapAccumL
					(insertArrayedIndices ref []) flatDeclArray $ fromJust indicess'
	-- evalPostDecl cs (Complete, VarDecl typ _) = ok $ VarDecl typ offsets
	--	where offsets = typeBuiltinOffsets cs 0 typ
	-- FIXME, ProcAliasDecl
	evalPostDecl cs (Complete, node@(ProcAliasDecl {})) = procAliasEvalPost eval cs node
		where eval cs = traverseNoAccum evalApply cs
	evalPostDecl _ (_, node@(ParamDecl {})) = (Incomplete, node)
	evalPostDecl _ (_, node@(TypeParamDecl {})) = (Incomplete, node)
	evalPostDecl _ (_, node@(BuiltinFuncDecl {})) = ok node
	-- FIXME, think about this very hard
	-- evalPostDecl cs (Complete, node@(ProcDecl {})) = (Complete, node)
	evalPostDecl _ (Complete, node@(ProcDecl _ ports _ _)) = (contextCompleteness ports, node)
	evalPostDecl _ (Complete, node@(FuncDecl _ ports _)) = (contextCompleteness ports, node)
	-- FIXME, CaseCmdE check overlap of guards, optimise guards
	evalPostDecl cs (Complete, node@(CaseDecl pos expr gs (DeferDecl _ elseComp elseDecl))) =
		tryConstCase unDeferedTrueDecls elseImps
		where
			flatGs = flattenCaseDeclGuards $ ListCaseDeclGuard gs

			allMatchImps = concatMap impsCaseDeclGuard flatGs
				where
					impsCaseDeclGuard (CaseDeclGuard _ [ImpCaseMatches _ imps] _) = imps
					impsCaseDeclGuard _ = error "evalPostDecl impsCaseDeclGuard: not a CaseDeclGuard"
			width = widthOfType cs $ typeOfExpr cs expr
			elseImps = removeImpsFromImp (dcInWidth width) allMatchImps

			guardChoices = map guardChoice flatGs
			guardChoice (CaseDeclGuard _ matches _) = caseMatchesExpr cs matches expr
			guardChoice _ = error "evalPostDecl guardChoice: not a CaseDeclGuard"
			guardCommand (CaseDeclGuard _ _ decl) = decl
			guardCommand _ = error "evalPostDecl guardCommand: not a CaseDeclGuard"

			trueDecls = map guardCommand $ snd $ unzip $ filter ((== Just True) . fst) $ zip guardChoices flatGs
			unDeferedTrueDecls = map unDeferDecl trueDecls
			trueResult = gatherCompleteness $ map deferDeclResult trueDecls

			-- splitGuard (CaseDeclGuard _ [ImpCaseMatches _ imps] decl) = (imps, decl)
			-- splitGuard _ = error "evalPostDecl splitGuard: not a CaseDeclGuard"

			-- FIXME, complain if unnecessary else is given
			tryConstCase [] _ = (elseComp, elseDecl) -- No guards left
			tryConstCase [decl] _ = (trueResult, decl) -- One true guard
			tryConstCase (_:_) _ = evalError cs pos node "more than one true case guard"
	evalPostDecl _ (r, CaseDecl pos expr gs elseDecl) = (r, CaseDecl pos expr gs' elseDecl')
		where
			flatGs = flattenCaseDeclGuards $ ListCaseDeclGuard gs

			elseDecl' = unDeferDecl elseDecl
			gs' = map unDeferDeclGuard flatGs
	evalPostDecl _ rn = rn

	-- -- Type

	-- Terminal types
	evalPostType :: Post Type
	evalPostType _ (_, node@(Bits {})) = ok node
	evalPostType _ (_, node@(SignedBits {})) = ok node
	-- Conversions
	evalPostType cs (Complete, node@(NumType pos int signed))
		| isJust width && intClass == NumClass && widthInt >= 0 = (Complete,
			(if signed && widthInt > 0 then SignedBits else Bits) widthInt)
		| otherwise = evalError cs pos node "numeric type expression's width is not a constant non-negative integer"
			where
				intClass = classOfType cs $ typeOfExpr cs int
				width = constExpr int
				widthInt = fromEnum $ fromJust width
	evalPostType cs (_, node@(Type ref))
		-- FIXME, position
		| isNoDeclRef cs ref = unboundRefError cs "type " ref NoPos node
	evalPostType _ (_, node@(NameType pos name)) = unboundError "type " name pos node
	-- ArrayType
	-- Others
	evalPostType _ (r, node) = (r, node)

	recordElemsWidth :: [Context Decl] -> [RecordElem] -> Int
	recordElemsWidth cs elems = foldl' (+) 0 $ map elemWidth elems
		where elemWidth (RecordElem _ _ typ) = widthOfType cs typ

	-- RecordType
	evalPostTypeBody :: Post TypeBody
	evalPostTypeBody cs (Complete, RecordType pos elems NoType) = ok $ RecordType pos elems overType
		where overType = Bits $ recordElemsWidth cs elems
	evalPostTypeBody cs (Complete, node@(RecordType pos elems typ))
		| elemsWidth > overWidth = evalError cs pos node ("width of elements (" ++ show elemsWidth ++
			" bits) is greater than width of over type (" ++ show overWidth ++ " bits)")
		| otherwise = ok $ RecordType pos elems overType
		where
			elemsWidth = recordElemsWidth cs elems
			overType = Bits overWidth
			overWidth = widthOfType cs typ
	evalPostTypeBody _ (Complete, node@(EnumType pos elems NoType)) = ok $ EnumType pos elems overType
		where overType = typeOfRange $ rangeOfEnumType node
	evalPostTypeBody cs (Complete, node@(EnumType pos elems typ))
		| enumLow >= overLow && enumHigh <= overHigh = ok $ EnumType pos elems numOverType
		| otherwise = evalError cs pos node ("over type `" ++ showTypeName cs typ ++
			"' is too narrow for all enumeration type elements")
		where
			(enumLow, enumHigh) = rangeOfEnumType node
			(overLow, overHigh) = rangeOfNumType numOverType
			numOverType = (if typeIsSigned cs typ then SignedBits else Bits) $ widthOfType cs typ
	evalPostTypeBody _ (r, node) = (r, node)

	-- -- Chan

	evalMidChan :: Post Chan
	evalMidChan cs (Complete, IndexChan pos array index)
		| isArrayedChan array && isJust maybeIndexType = ok $ IndexChan pos array $ typeCheckExpr indexType index
		where
			maybeIndexType = arrayedChanIndexType cs array
			indexType = fromJust maybeIndexType
	evalMidChan cs (Complete, SliceChan pos array li ri)
		| isArrayedChan array && isJust maybeIndexType =
			ok $ SliceChan pos array (typeCheckExpr indexType li) (typeCheckExpr indexType ri)
		where
			maybeIndexType = arrayedChanIndexType cs array
			indexType = fromJust maybeIndexType
	evalMidChan _ (r, node) = (r, node)

	evalPostChan :: Post Chan
	evalPostChan cs (Complete, node@(IndexChan {})) = arrayedChanIndexEval cs node
	evalPostChan cs (Complete, node@(SliceChan {})) = arrayedChanSliceEval cs node
	evalPostChan cs (Complete, CheckChan pos decl chan)
		| chanDeclR == Complete = (chanDeclsEquiv pos cs decl chanDecl chan, chan)
		| otherwise = (chanDeclR, chan)
		where
			(chanDeclR, chanDecl) = makeDeclForChan cs chan
	evalPostChan cs (_, node@(Chan pos ref))
		| completeness == Complete = chan'
		| otherwise = (Incomplete, node)
		where
			binding = findBindingByRef cs ref
			decl = bindingValue binding
			completeness = bindingCompleteness binding
			chan' = case decl of
				ChanExprDecl {} -> ok $ declChan decl
				NoDecl {} -> unboundError "channel " (bindingName binding) pos node
				_ -> ok $ Chan pos ref
	evalPostChan cs (Complete, node@(PartialArrayedChan pos ref))
		| isNoDeclRef cs ref = unboundRefError cs "type " ref pos node
		| otherwise = ok $ flattenPartialArrayedChan cs node
	evalPostChan _ (_, node@(NameChan pos name)) = unboundError "channel " name pos node
	evalPostChan _ (r, node@(FlatArrayedChan {})) = (r, node)
	evalPostChan _ (r, node) = appendResult r $ (Incomplete, node)

	-- -- Cmd

	isSingleChan :: Chan -> Bool
	isSingleChan (Chan {}) = True
	isSingleChan _ = False

	-- DeferCmd has to be early to prevent result being tested and so the defered command evaluated
	evalMidCmd :: Post Cmd
	evalMidCmd _ (r, DeferCmd _ cmd) = ok $ DeferCmd r cmd
	evalMidCmd cs (Complete, node@(InputCmd pos chan lvalue))
		| isSingleChan chan = ok $ InputCmd pos chan lvalue'
		| otherwise = evalError cs pos node "expecting a single channel"
		where lvalue' = typeCheckLvalue (typeOfChan cs chan) lvalue
	evalMidCmd cs (Complete, node@(OutputCmd pos chan expr))
		| isSingleChan chan = ok $ OutputCmd pos chan expr'
		| otherwise = evalError cs pos node "expecting a single channel"
		where expr' = typeCheckExpr (typeOfChan cs chan) expr
	evalMidCmd cs (Complete, AssignCmd pos lvalue expr) = ok $ AssignCmd pos lvalue expr'
		where expr' = typeCheckExpr (typeOfLvalue cs lvalue) expr
	evalMidCmd cs (comp, CaseCmdE pos expr gs elseCmd)
		| comp == Complete = ok node'
		| comp == Incomplete = (Incomplete, node')
		where
			node' = CaseCmdE pos expr (map modGuard gs) (DeferCmd Complete elseCmd)

			modGuard (CaseCmdGuard pos matches cmd) =
				CaseCmdGuard pos (map (typeCheckMatch typ) matches) (DeferCmd Complete cmd)
			modGuard (ForCaseCmdGuard pos n matches ctx cmd) =
				ForCaseCmdGuard pos n (map (typeCheckMatch typ) matches) ctx' cmd
				where
					ctxIndexRange@(low, _) = contextIndexRange ctx
					paramBinding = findBindingByRef [ctx] (Ref low)
					ctx' = bindingsToContext ctxIndexRange [ paramBinding { bindingValue =
						TypeDecl pos (AliasType pos typ) } ]
			modGuard guard = error $ "evalMidCmd modGuard: bad guard `" ++ show guard ++ "'"

			typ = typeOfExpr cs expr
	evalMidCmd cs (Complete, ForCmd pos parSeq interval ctx cmd)
		| parSeq == Seq = (comp, SeqCmd pos cmds)
		| parSeq == Par = (comp, ParCmd pos cmds)
		where
			ctxIndexRange@(low, _) = contextIndexRange ctx
			constantBinding = findBindingByRef [ctx] (Ref low)
			indexCmd int = traverseNoAccum evalApply cs $ BlockCmd pos ctx' cmd
				where ctx' = bindingsToContext ctxIndexRange [constantBinding { bindingValue = ExprDecl pos $
					ValueExpr pos (intervalType interval) (IntValue int) }]
			(comps, cmds) = unzip $ map indexCmd $ intervalIndices interval
			comp = gatherCompleteness comps
	evalMidCmd cs (Complete, node@(CallCmd {})) = {- ss cs $ -} callCmdEvalMid eval cs node
		where eval cs = traverseNoAccum evalApply cs
	evalMidCmd _ (r, node) = (r, node)

	-- ss cs (comp, a) = trace (show comp ++ "\n" ++ show {- showParseNode cs -} a) (comp, a)

	unDeferCmd :: Cmd -> Cmd
	unDeferCmd (DeferCmd _ cmd) = cmd
	unDeferCmd cmd = cmd

	deferCmdResult :: Cmd -> Completeness
	deferCmdResult (DeferCmd r _) = r
	deferCmdResult _ = error "deferCmdResult: not a DeferCmd"

	unDeferCmdGuard :: CaseCmdGuard -> CaseCmdGuard
	unDeferCmdGuard (CaseCmdGuard pos matches cmd) = CaseCmdGuard pos matches $ unDeferCmd cmd
	unDeferCmdGuard guard = guard

	cmdGuardDeferResult :: CaseCmdGuard -> Completeness
	cmdGuardDeferResult (CaseCmdGuard _ _ (DeferCmd r _)) = r
	cmdGuardDeferResult d = error $ show d

	flattenCmdGuards :: CaseCmdGuard -> [CaseCmdGuard]
	flattenCmdGuards (ListCaseCmdGuard gs) = concatMap flattenCmdGuards gs
	flattenCmdGuards g = [g]

	evalPostCmd :: Post Cmd
	evalPostCmd _ (_, NoCmd) = ok NoCmd
	-- FIXME, SelectCmd check overlap of guards
	evalPostCmd cs (Complete, SeqCmd pos cmds)
		| null cmds' = ok NoCmd
		| any (cmdIsPermanent cs) (init cmds') =
			evalError cs (cmdPos permanentCmd) permanentCmd "extra commands after non-terminating loop"
		| otherwise = ok $ SeqCmd pos cmds'
		where
			cmds' = filter (not . cmdIsNoCmd) cmds
			Just permanentCmd = find (cmdIsPermanent cs) (init cmds')
	-- FIXME, CaseCmdE check overlap of guards, optimise guards
	evalPostCmd cs (Complete, node@(CaseCmdE pos expr gs (DeferCmd elseComp elseCmd))) =
		tryConstCase unDeferedUndecidedGuards unDeferedTrueCmds elseImps
		where
			flatGs = flattenCmdGuards $ ListCaseCmdGuard gs

			allMatchImps = concatMap impsCaseCmdGuard flatGs
				where
					impsCaseCmdGuard (CaseCmdGuard _ [ImpCaseMatches _ imps] _) = imps
					impsCaseCmdGuard _ = error "evalPostCmd impsCaseCmdGuard: not a CaseCmdGuard"
			width = widthOfType cs $ typeOfExpr cs expr
			elseImps = removeImpsFromImp (dcInWidth width) allMatchImps

			guardChoices = map guardChoice flatGs
			guardChoice (CaseCmdGuard _ matches _) = caseMatchesExpr cs matches expr
			guardChoice _ = error "evalPostCmd guardChoice: not a CaseCmdGuard"
			guardCommand (CaseCmdGuard _ _ cmd) = cmd
			guardCommand _ = error "evalPostCmd guardCommand: not a CaseCmdGuard"

			undecidedGuards = snd $ unzip $ filter (isNothing . fst) $ zip guardChoices flatGs
			undecidedResult = gatherCompleteness $ map cmdGuardDeferResult undecidedGuards
			unDeferedUndecidedGuards = map unDeferCmdGuard undecidedGuards
			trueCmds = map guardCommand $ snd $ unzip $ filter ((== Just True) . fst) $ zip guardChoices flatGs
			unDeferedTrueCmds = map unDeferCmd trueCmds
			trueResult = gatherCompleteness $ map deferCmdResult trueCmds

			splitGuard (CaseCmdGuard _ [ImpCaseMatches _ imps] cmd) = (imps, cmd)
			splitGuard _ = error "evalPostCmd splitGuard: not a CaseCmdGuard"

			-- FIXME, complain if unnecessary else is given
			-- tryConstCase _ _ _ = ok NoCmd -- FIXME AB1
			tryConstCase _ [cmd] _ = (trueResult, cmd) -- One true guard
			tryConstCase [] _ _ = (elseComp, elseCmd) -- No guards left
			tryConstCase guards [] [] = (undecidedResult, CaseCmd pos expr impss cmds)
				where (impss, cmds) = unzip $ map splitGuard guards
				-- Undecided. No else, FIXME, check no else clause given
			tryConstCase guards [] elseImps = -- Undecided. Else
				(gatherCompleteness [elseComp, undecidedResult], node')
				where
					node' = CaseCmd pos expr (elseImps:impss) (elseCmd:cmds)
					(impss, cmds) = unzip $ map splitGuard guards
			tryConstCase _ _ _ = evalError cs pos node "more than one true case guard"
	evalPostCmd _ (r, CaseCmdE pos expr gs elseCmd) = (r, CaseCmdE pos expr gs' elseCmd')
		where
			flatGs = flattenCmdGuards $ ListCaseCmdGuard gs

			elseCmd' = unDeferCmd elseCmd
			gs' = map unDeferCmdGuard flatGs
	-- FIXME, PrintCmd
	evalPostCmd cs (Complete, node@(PrintCmd pos exprs))
		| any (isJust . constImpExpr) exprs = evalError cs pos node "cannot print implicants"
		| otherwise = ok $ PrintCmd pos exprs'
		where
			exprs' = map toString exprs

			toString expr = body typ
				where
					body (Type ref)
						| ref == stringType = expr
					body (BuiltinType "String") = expr
					body _ = BuiltinCallExpr pos "ToString" [TypeFuncActual structuralType] [expr] (Type stringType)

					typ = typeOfExpr cs expr
					structuralType = typeToStructType cs typ

			stringType = fromJust $ findRefByName cs TypeNamespace "String"
	evalPostCmd cs (Complete, node@(CallCmd pos (Callable ref) _ _))
		| isNoDeclRef cs ref = unboundRefError cs "procedure/function " ref pos node
		| otherwise = callCmdEvalPost eval cs node
		where eval cs = traverseNoAccum evalApply cs
	evalPostCmd _ (Complete, node@(CallCmd _ (NameCallable pos name) _ _)) = unboundError "procedure " name pos node
	evalPostCmd _ (r, node) = (r, node)

	-- -- Expr

	badType :: Pos -> [Context Decl] -> node -> Type -> Type -> (Completeness, node)
	badType pos cs expr expecting got = plainEvalError pos expr $
		"bad type; expecting `" ++ expectingStr ++ "' got `" ++ gotStr ++ "'"
		where
			expectingStr = showTypeName cs expecting
			gotStr = showTypeName cs got

	badElemType :: Pos -> [Context Decl] -> Expr -> Type -> Type -> (Completeness, Expr)
	badElemType pos cs expr expecting got = evalError cs pos expr $
		"bad type; expecting an array of `" ++ expectingStr ++ "' elements got `" ++ gotStr ++ "'"
		where
			expectingStr = showTypeName cs expecting
			gotStr = showTypeName cs got

	enumElem :: [Context Decl] -> Type -> String -> Maybe Expr
	enumElem cs typ name
		| isJust elemRef && bindingCompleteness elemBinding == Complete = Just $ typeExpr typ elemExpr
		| otherwise = Nothing
		where
			EnumType _ ctx _ = typeGetUnaliasedBody cs typ
			elemRef = findRefByName [ctx] OtherNamespace name
			elemBinding = findBindingByRef [ctx] $ fromJust elemRef
			elemExpr = declExpr $ bindingValue $ findBindingByRef [ctx] $ fromJust elemRef

	tryEnumElemRef :: [Context Decl] -> Expr -> Type -> Ref -> Expr
	tryEnumElemRef cs node typ ref
		| classOfType cs typ == EnumClass && isJust elemExpr = fromJust elemExpr
		| otherwise = node
		where
			name = bindingName $ findBindingByRef cs ref
			elemExpr = enumElem cs typ name

	ok :: node -> (Completeness, node)
	ok node = (Complete, node)

	-- Type threading
	evalMidExpr :: Post Expr
	evalMidExpr _ (r, node@(ConsExpr _ NoType NoType _)) = (r, node)
	evalMidExpr cs (Complete, node) = case node of
		TypeCheckExpr _ _ typ (NameExpr pos elem)
			| classOfType cs typ == EnumClass -> ok $ EnumElemExpr pos typ elem
			| otherwise -> ok $ NameExpr pos elem
		ArrayElemTypeCheckExpr pos elemType (ConsExpr _ NoType NoType elems) ->
			evalMidExpr cs $ ok $ typeExpr arrayType $ ConsExpr pos arrayType arrayType elems
				where arrayType = ArrayType (intervalFromRange (0, (toInteger $ length elems) - 1)) elemType
		TypeCheckExpr _ _ typ (ConsExpr pos NoType NoType elems)
			| typClass `elem` [RecordClass, ArrayClass] ->
				evalMidExpr cs $ ok $ typeExpr typ $ ConsExpr pos typ typ elems
			| otherwise -> ok $ ConsExpr pos NoType NoType elems
			where typClass = classOfType cs typ
		TypeCheckExpr _ _ typ (ValueExpr pos NoType DontCareValue) -> ok $ ValueExpr pos typ (ImpValue allDcs)
			where
				width = widthOfType cs typ
				allDcs = Imp 0 (bit width - 1)
		TypeCheckExpr _ _ typ expr
			| classOfType cs typ == EnumClass && isJust ref -> expr'
			where
				ref = exprRef expr
				-- expr' = evalMidExpr cs $ ok $ tryEnumElemRef cs node typ $ fromJust ref
				expr' = ok $ tryEnumElemRef cs node typ $ fromJust ref
		ConsExpr pos returnType consType elems
			| isJust $ elemTypes -> if length elems /= length (fromJust elemTypes)
				then evalError cs pos node "length of list of expressions not the same as type"
				else ok $ ConsExpr pos returnType consType $ map (uncurry (typeCheckExpr)) $
					zip (fromJust elemTypes) elems
			| otherwise -> evalError cs pos node "expression must be of record or array type"
			where elemTypes = consTypeElemTypes cs consType
		AppendExpr pos typ left right
			| isArrayType cs leftType -> ok $ AppendExpr pos typ left $ ArrayElemTypeCheckExpr pos elemType right
			| otherwise -> evalError cs pos node "append arguments must be arrays"
			where
				leftType = typeOfExpr cs left
				elemType = arrayElemType $ unaliasType cs leftType
		BinExpr pos typ op left right
			| leftClass `elem` [RecordClass, EnumClass, ArrayClass] -> rightTypeCheck
			| leftClass == NumClass && op `elem` [BinAnd, BinOr, BinXor] -> rightTypeCheck
			where
				rightTypeCheck = ok $ BinExpr pos typ op left $ typeCheckExpr leftType right
				leftType = typeOfExpr cs left
				leftClass = classOfType cs leftType
		IndexExpr pos array@(PartialArrayedRead _ ref) index
			| isJust indexType -> ok $ IndexExpr pos array $ typeCheckExpr (fromJust indexType) index
				where indexType = arrayedIndexType cs ref
		IndexExpr pos array index
			| isArrayType cs arrayType -> ok $ IndexExpr pos array $ typeCheckExpr indexType index
			| otherwise -> evalError cs pos node "array must be of array type"
			where
				arrayType = typeOfExpr cs array
				ArrayType interval _ = unaliasType cs arrayType
				indexType = intervalType interval
		SliceExpr pos (PartialArrayedRead {}) _ _ ->
			evalError cs pos node "can't slice an arrayed open channel"
		SliceExpr pos array li ri
			| isArrayType cs arrayType ->
				ok $ SliceExpr pos array (typeCheckExpr indexType li) (typeCheckExpr indexType ri)
			| otherwise -> evalError cs pos node "array must be of array type"
			where
				arrayType = typeOfExpr cs array
				ArrayType interval _ = unaliasType cs arrayType
				indexType = intervalType interval
		CallExpr {} -> callExprEvalMid cs node
		_ -> ok node
	evalMidExpr _ rn = rn

	evalPostExpr :: Post Expr
	evalPostExpr _ (_, node@(NameExpr pos name)) = unboundError "" name pos node
	evalPostExpr cs (_, node@(MaybeTypeExpr pos ref)) = (Wrong [Report pos $
		"not expecting type name `" ++ name ++ "' here"], node)
		where name = bindingName $ findBindingByRef cs ref
	evalPostExpr cs (_, node@(MaybeOtherExpr pos ref)) = (Wrong [Report pos $
		"name `" ++ name ++ "' is bound to something which cannot be used here"], node)
		where name = bindingName $ findBindingByRef cs ref
	evalPostExpr cs (r, node@(ConsExpr pos _ NoType _)) =
		appendResult r $ evalError cs pos node "can't determine type of construction expression"
	evalPostExpr cs (Complete, TypeCheckExpr pos _ toType expr)
		-- Expanding constant ints and imps (FIXME, need different check than constExpr)
		| intOrImp && typeCoercable cs fromType toType = (Complete, typeExpr toType expr)
		-- Contracting ints
		| isJust int && intCoercable cs (fromJust int) toType = (Complete, typeExpr toType expr)
		| typeEquiv cs fromType toType = (Complete, expr)
		| otherwise = badType pos cs expr toType fromType
		where
			intOrImp = isJust int || isJust (constImpExpr expr)
			int = constExpr expr
			fromType = typeOfExpr cs expr
	evalPostExpr _ (r, TypeCheckExpr _ _ _ expr) = (r, expr)
	evalPostExpr cs (Complete, ArrayElemTypeCheckExpr pos toType expr)
		| isArrayType cs fromType && typeEquiv cs toType fromTypeElem = (Complete, expr)
		| otherwise = badElemType pos cs expr toType fromType
		where
			fromType = typeOfExpr cs expr
			fromTypeElem = arrayElemType $ unaliasType cs fromType
	evalPostExpr _ (r, ArrayElemTypeCheckExpr _ _ expr) = (r, expr)
	evalPostExpr cs (Complete, node) = case node of
		-- Evaluation
		-- Need checking/evaluation to become complete
		ConstCheckExpr pos expr
			| isNothing $ exprValue expr -> evalError cs pos node "expression is not constant"
			| otherwise -> ok expr
		-- Reduce operations on constants
		BinExpr {} -> binExprEval cs node
		IndexExpr pos (PartialArrayedRead _ ref) index
			| isNoDeclRef cs ref -> unboundRefError cs "variable " ref pos node
			| isJust constIndex -> arrayedRefIndexEval cs ref (fromJust constIndex) partial match $
				evalError cs pos node
			| otherwise -> evalError cs pos node "arrayed index operations must have constant indices"
			where
				constIndex = constExpr index
				match (OpenChanDecl _ _ typ) indexRef = {- eval $ -} ok $
					OpenChanRead pos typ (0 +: width) indexRef
					where width = widthOfType cs typ
				match _ (IndexRef _ index) = evalError cs pos node $
					"index `" ++ show index ++ "' is not an enclosing channel"
				match _ _ = error "evalPostExpr IndexExpr match: bad args"
				-- eval = traverse cs evalApply
				partial ref = ok $ PartialArrayedRead pos ref
		IndexExpr pos array index
			| comp == Complete -> traverseNoAccum evalApply cs node'
			| otherwise -> (comp, node)
				where (comp, node') = indexExprEval pos cs node array index
		SliceExpr pos array li ri
			| comp == Complete -> traverseNoAccum evalApply cs node'
			| otherwise -> (comp, node)
				where (comp, node') = sliceEval pos cs BitfieldExpr node array (typeOfExpr cs array) li ri
		RecElemExpr pos recExpr elem
			| not isRecord -> evalError cs pos node "Left hand expression is not a record"
			| isNothing foundElem -> evalError cs pos node ("Record has no element `" ++ elem ++ "'")
			| otherwise -> eval $ BitfieldExpr pos elemType (sliceFromLH low high) recExpr
			where
				recType = typeOfExpr cs recExpr
				isRecord = classOfType cs recType == RecordClass
				foundElem = findRecElemByName cs (typeGetUnaliasedBody cs recType) elem
				Just (low, elemType) = foundElem
				high = low + (widthOfType cs elemType) - 1
				eval = traverseNoAccum evalApply cs
		UnExpr {} -> unExprEval cs node
		CastExpr {} -> eval $ castExprEval cs node
			where eval = evalPostExpr cs
		CallExpr {} -> callExprEvalPost evalExpr evalType cs node
			where
				evalExpr cs = traverseNoAccum evalApply cs
				evalType cs = traverseNoAccum evalApply cs
		ConsExpr pos returnType consType elems
			| all isConst elems -> ok $ ValueExpr pos returnType result
			| otherwise -> ok node
			where
				result = makeConsValue cs consType elems
				isConst expr = isJust (constExpr expr) || isJust (constImpExpr expr)
		AppendExpr pos _ left right
			| isJust leftConst && isJust rightConst -> ok $ ValueExpr pos resultType $ IntValue constValue
			| isJust leftImp && isJust rightImp -> ok $ ValueExpr pos resultType $ ImpValue constImp
			| otherwise -> ok $ typeExpr resultType node
			where
				leftConst = constExpr left
				rightConst = constExpr right
				leftType = unaliasType cs $ typeOfExpr cs left
				rightType = unaliasType cs $ typeOfExpr cs right
				rightWidth = widthOfType cs rightType

				leftImp = constImpOrIntExpr left
				rightImp = constImpOrIntExpr right

				high' = toInteger $ intervalSize (arrayInterval leftType) + intervalSize (arrayInterval rightType) - 1
				resultType = ArrayType (intervalFromRange (0, high')) $ arrayElemType leftType

				appendConsts l r = l + (r `shiftL` rightWidth)

				constValue = appendConsts (fromJust leftConst) (fromJust rightConst)
				constImp = Imp (appendConsts leftVal rightVal) (appendConsts leftDcs rightDcs)
					where
						Just (Imp leftVal leftDcs) = leftImp
						Just (Imp rightVal rightDcs) = rightImp
		EnumElemExpr pos typ elem -> case classOfType cs typ of
			EnumClass
				| isJust elemExpr -> ok $ fromJust elemExpr
				| otherwise -> evalError cs pos node "no such enumeration element"
					where elemExpr = enumElem cs typ elem
			_ -> evalError cs pos node "not an enumeration type in expression"
		BitfieldExpr pos typ outerSlice (BitfieldExpr _ _ innerSlice expr2) -> ok bitfield'
			where
				bitfield' = BitfieldExpr pos typ slice' expr2
				slice' = sliceShiftL outerSlice $ sliceOffset innerSlice
		BitfieldExpr pos typ outerSlice (VarRead _ _ innerSlice ref) -> ok bitfield'
			where
				bitfield' = VarRead pos typ slice' ref
				slice' = sliceShiftL outerSlice $ sliceOffset innerSlice
		BitfieldExpr pos typ outerSlice (OpenChanRead _ _ innerSlice ref) -> ok bitfield'
			where
				bitfield' = OpenChanRead pos typ slice' ref
				slice' = sliceShiftL outerSlice $ sliceOffset innerSlice
		BitfieldExpr pos typ slice expr
			| isJust (constExpr expr) -> ok $ ValueExpr pos typ $ IntValue $ recoverValue cs typ bitField
			| otherwise -> ok node
				where
					value = fromJust $ constExpr expr
					bitField = extractBitfield slice value
		ExtendExpr pos typ _ signed expr
			| isJust (constExpr expr) -> ok $ ValueExpr pos typ $ IntValue $ value'
			| otherwise -> ok node
				where
					value = fromJust $ constExpr expr
					exprWidth = widthOfType cs $ typeOfExpr cs expr
					value' = if not signed && value < 0 then cropInt exprWidth value else value
		SizeofExpr pos typ -> ok $ ValueExpr pos widthType $ IntValue width
			where
				typ' = unaliasType cs typ
				width = toInteger $ widthOfType cs typ'
				widthType = typeForInt width
		-- Generated nodes must be to have been generated
		CaseExpr {} -> ok node
		BuiltinCallExpr {} -> ok node
		-- Values
		ValueExpr pos NoType DontCareValue ->
			evalError cs pos node "can't determine type of don't care value"
		ValueExpr {} -> ok node
		-- Complete if ref is complete
		MaybeTypeExpr pos ref
			| isNoDeclRef cs ref -> unboundRefError cs "type " ref pos node
		MaybeOtherExpr pos ref
			| isNoDeclRef cs ref -> unboundRefError cs "" ref pos node
		VarRead pos typ slice ref
			| isNoDeclRef cs ref -> unboundRefError cs "variable " ref pos node
			| isEmptySlice slice -> (Complete, VarRead pos typ (0 +: width) ref)
			| otherwise -> ok node
			where width = widthOfType cs typ
		OpenChanRead pos typ slice ref
			| isNoDeclRef cs ref -> unboundRefError cs "open channel " ref pos node
			| isEmptySlice slice -> (Complete, OpenChanRead pos typ (0 +: width) ref)
			| otherwise -> ok node
			where width = widthOfType cs typ
		PartialArrayedRead pos ref
			| isNoDeclRef cs ref -> unboundRefError cs "" ref pos node
			| otherwise -> ok node
		Expr pos ref
			| isNoDeclRef cs ref -> unboundRefError cs "" ref pos node
			| otherwise -> ok $ declExpr $ bindingValue $ findBindingByRef cs ref
		_ -> (Incomplete, node)
	evalPostExpr _ (r, node) = appendResult r (Incomplete, node)

	evalFinalExpr :: Post Expr
	evalFinalExpr cs (Complete, node@(PartialArrayedRead {})) =
		evalError cs (exprPos node) node "incomplete arrayed indexing"
	evalFinalExpr _ (r, node) = (r, node)

	-- -- Callable

	-- refCompleteness not to be used here, leave that to the Calls
	evalPostCallable :: Post Callable
	evalPostCallable _ (_, Callable ref) = ok $ Callable ref
	evalPostCallable _ (r, node) = appendResult r $ (Incomplete, node)

	-- -- Lvalue

	evalMidLvalue :: Post Lvalue
	evalMidLvalue cs (Complete, node@(IndexLvalue pos array index))
		| isArrayType cs arrayType = ok $ IndexLvalue pos array $ typeCheckExpr indexType index
		| otherwise = evalError cs pos node "array must be of array type"
		where
			arrayType = typeOfLvalue cs array
			ArrayType interval _ = unaliasType cs arrayType
			indexType = intervalType interval
	evalMidLvalue cs (Complete, node@(SliceLvalue pos array li ri))
		| isArrayType cs arrayType = ok $ SliceLvalue pos array (typeCheckExpr indexType li)
			(typeCheckExpr indexType ri)
		| otherwise = evalError cs pos node "array must be of array type"
		where
			arrayType = typeOfLvalue cs array
			ArrayType interval _ = unaliasType cs arrayType
			indexType = intervalType interval
	evalMidLvalue _ (r, node) = (r, node)

	-- Need checking/evaluation to become complete
	evalPostLvalue :: Post Lvalue
	evalPostLvalue cs (Complete, TypeCheckLvalue pos _ typ2 lvalue)
		| typeEquiv cs typ1 typ2 = ok lvalue
		| otherwise = badType pos cs lvalue typ2 typ1
		where typ1 = typeOfLvalue cs lvalue
	evalPostLvalue _ (r, TypeCheckLvalue _ Transient _ lvalue) = (r, lvalue)
	evalPostLvalue cs (Complete, node@(IndexLvalue pos array index))
		| comp == Complete = traverseNoAccum evalApply cs node'
		| otherwise = (comp, node)
			where (comp, node') = indexLvalueEval pos cs node array index
	evalPostLvalue cs (Complete, node@(SliceLvalue pos array li ri))
		| comp == Complete = traverseNoAccum evalApply cs node'
		| otherwise = (comp, node)
			where (comp, node') = sliceEval pos cs BitfieldLvalue node array (typeOfLvalue cs array) li ri
	-- CastLvalue.  Only allow width-preserving casts for now
	evalPostLvalue cs (Complete, node@(CastLvalue pos toType lvalue))
		| widthOfType cs toType == widthOfType cs fromType = ok $ typeLvalue toType lvalue
		| otherwise = evalError cs pos node "lvalue casts must preserve width"
		where fromType = typeOfLvalue cs lvalue
	evalPostLvalue _ (Complete, BitfieldLvalue pos typ outerSlice
		(BitfieldLvalue _ _ innerSlice lvalue)) = {- eval -} ok bitfield'
		where
			bitfield' = BitfieldLvalue pos typ slice' lvalue
			slice' = sliceShiftL outerSlice $ sliceOffset innerSlice
			-- eval = traverseNoAccum evalApply cs
	evalPostLvalue _ (Complete, BitfieldLvalue pos typ outerSlice
		(VarWrite _ _ innerSlice ref)) = {- eval -} ok bitfield'
		where
			bitfield' = VarWrite pos typ slice' ref
			slice' = sliceShiftL outerSlice $ sliceOffset innerSlice
			-- eval = traverseNoAccum evalApplys cs
	evalPostLvalue cs (Complete, lvalue@(RecElemLvalue pos recLvalue elem))
		| not isRecord = evalError cs pos lvalue "Left hand expression is not a record"
		| isNothing foundElem = evalError cs pos lvalue ("Record has no element `" ++ elem ++ "'")
		| otherwise = eval $ BitfieldLvalue pos elemType (sliceFromLH low high) recLvalue
		where
			recType = typeOfLvalue cs recLvalue
			isRecord = classOfType cs recType == RecordClass
			foundElem = findRecElemByName cs (typeGetUnaliasedBody cs recType) elem
			Just (low, elemType) = foundElem
			high = low + (widthOfType cs elemType) - 1
			eval = traverseNoAccum evalApply cs
	evalPostLvalue cs (Complete, SmashLvalue _ lvalue) = ok $
		typeLvalue (bitArrayType (widthOfType cs typ)) lvalue
		where typ = typeOfLvalue cs lvalue
	-- Generated nodes must be complete to have been generated
	evalPostLvalue _ (_, lvalue@(BitfieldLvalue {})) = ok lvalue
	evalPostLvalue _ (_, lvalue@(CaseLvalue {})) = ok lvalue
	-- Complete if ref is complete
	evalPostLvalue cs (Complete, node@(VarWrite pos typ slice ref))
		| isNoDeclRef cs ref = unboundRefError cs "" ref NoPos node
		| isEmptySlice slice = ok $ VarWrite pos typ (0 +: width) ref
		| otherwise = ok node
		where width = widthOfType cs typ
	evalPostLvalue _ (_, node@(NameLvalue pos name)) = unboundError "" name pos node
	evalPostLvalue _ (r, node) = appendResult r $ (Incomplete, node)

	-- CaseMatch

	evalPostCaseMatch :: Post CaseMatch
	evalPostCaseMatch cs (Complete, node@(ExprCaseMatch pos expr))
		| isJust constInt = ok $ ImpCaseMatches pos [Imp (cropInt width $ fromJust constInt) 0]
		| isJust constImp = ok $ ImpCaseMatches pos [fromJust constImp]
		| otherwise = evalError cs pos node "case match value must be a constant or implicant"
		where
			constInt = constExpr expr
			constImp = constImpExpr expr
			width = widthOfType cs $ typeOfExpr cs expr
	evalPostCaseMatch cs (Complete, node@(RangeCaseMatch pos left right))
		| indicesAreConst = ok $ ImpCaseMatches pos $ sliceToImplicants $ uncurry sliceFromLH intervalRange
		| otherwise = (comp, node)
		where
			indicesAreConst = comp == Complete
			(comp, Interval intervalRange _) = makeInterval pos cs node left right
	evalPostCaseMatch _ (_, node@(ImpCaseMatches {})) = ok node
	evalPostCaseMatch _ (r, node) = (r, node)

	-- CaseDeclGuard

	evalPostCaseDeclGuard :: Post CaseDeclGuard
	evalPostCaseDeclGuard _ (Complete, CaseDeclGuard pos matches decl) =
		ok $ CaseDeclGuard pos [ImpCaseMatches pos flatMatches] decl
		where
			-- Matches should all be ImpCaseMatches by now
			flatMatches = sort $ concatMap fromImp matches
			fromImp (ImpCaseMatches _ matches) = matches
			fromImp _ = error "evalPostCaseDeclGuard fromImp: not an ImpCaseMatches"
	evalPostCaseDeclGuard _ (r, node) = (r, node)

	-- CaseCmdGuard

	evalMidCaseCmdGuard :: Post CaseCmdGuard
	evalMidCaseCmdGuard cs (Complete, ForCaseCmdGuard pos _ matches ctx cmd) =
		traverseNoAccum evalApply cs $ ListCaseCmdGuard guards
		where
			ctxIndexRange@(low, _) = contextIndexRange ctx
			constantBinding = findBindingByRef [ctx] (Ref low)
			TypeDecl _ (AliasType _ typ) = bindingValue constantBinding
			indexGuard :: Integer -> CaseCmdGuard
			indexGuard int = CaseCmdGuard pos [ImpCaseMatches pos [Imp int 0]] $
				DeferCmd Complete $ BlockCmd pos ctx' cmd
				where ctx' = bindingsToContext ctxIndexRange [constantBinding { bindingValue = ExprDecl pos $
					ValueExpr pos typ (IntValue int) }]
			guards = map indexGuard $ concatMap impExpand flatMatches
	
			flatMatches = sort $ concatMap fromImp matches
			fromImp (ImpCaseMatches _ impMatches) = impMatches
			fromImp _ = error "evalMidCaseCmdGuard fromImp: not an ImpCaseMatches"
			
	evalMidCaseCmdGuard _ rn = rn

	evalPostCaseCmdGuard :: Post CaseCmdGuard
	evalPostCaseCmdGuard _ (Complete, CaseCmdGuard pos matches cmd) =
		ok $ CaseCmdGuard pos [ImpCaseMatches pos flatMatches] cmd
		where
			-- Matches should all be ImpCaseMatches by now
			flatMatches = sort $ concatMap fromImp matches
			fromImp (ImpCaseMatches _ impMatches) = impMatches
			fromImp _ = error "evalPostCaseCmdGuard fromImp: not an ImpCaseMatches"
	evalPostCaseCmdGuard _ (r, node) = (r, node)

	-- Case

	-- FIXME, have a ValueExpr to Imp function?
	exprMatchesAnImplicant :: [Context Decl] -> Expr -> [Implicant] -> Bool
	exprMatchesAnImplicant cs expr implicants = isJust $ find (impIsCoveredByImp width impValue) implicants
		where
			width = widthOfType cs $ typeOfExpr cs expr
			value = fromJust $ exprValue expr
			impValue = getImp value

			getImp (IntValue int) = Imp (cropInt width int) 0
			getImp (ImpValue imp) = imp
			getImp value = error $ "exprMatchesAnImplicant getImp: bad value `" ++ show value ++ "'"

	typeCheckMatch :: Type -> CaseMatch -> CaseMatch
	typeCheckMatch typ (ExprCaseMatch pos expr) = ExprCaseMatch pos (typeCheckExpr typ expr)
	typeCheckMatch typ (RangeCaseMatch pos li ri) =
		RangeCaseMatch pos (typeCheckExpr typ li) (typeCheckExpr typ ri)
	typeCheckMatch _ match = match

	caseMatchesExpr :: [Context Decl] -> [CaseMatch] -> Expr -> Maybe Bool
	caseMatchesExpr cs [(ImpCaseMatches _ imps)] expr
		| isJust value = Just choice
		| otherwise = Nothing
		where
			value = exprValue expr
			choice = exprMatchesAnImplicant cs expr imps
	caseMatchesExpr _ _ _ = Nothing
