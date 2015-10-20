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

{-# LANGUAGE FlexibleInstances #-}

module Print (
	showTree,
	binOpSymbol,
	unOpSymbol,
	parseNodeError,
	showDirection,
	ShowParseNode (..)
	) where

	import ParseTree
	import Traverse
	import Misc
	import Context
	import Type
	import Bits
	import Report

	import Data.List
	import Data.Array
	import Data.Maybe
	import Data.Char
	import Numeric

	showTree :: ParseTreeTraverse node => node -> String
	showTree node = spaceCompose r ""
		where (_, r, _) = traverse showTreeApply 0 () node

	type PostShow node = ApplyPostFunc Int [ShowS] node
	type PreShow node = Int -> node -> (Int, node)

	showTreeApply :: Apply Int () [ShowS]
	showTreeApply = emptyApply {
		nullResult = [],
		combineResult = (++),
		gatherList = printGatherList,
		gatherArray = printGatherArray,
		gatherPair = printGatherPair,
		gatherMaybe = printGatherMaybe,
		postCmd = printPostCmd,
		postBinding = printPostBinding,
		postSimpleBinding = printPostSimpleBinding,
		postInterval = printPostInterval,
		postCaseMatch = printPostCaseMatch,
		postCaseCmdGuard = printPostCaseCmdGuard,
		postCaseDeclGuard = printPostCaseDeclGuard,
		postChan = printPostChan,
		postChanGuard = printPostChanGuard,
		postContext = printPostContext,
		postDecl = printPostDecl,
		postExpr = printPostExpr,
		postLvalue = printPostLvalue,
		postType = printPostType,
		postTypeBody = printPostTypeBody,
		postRecordElem = printPostRecordElem,
		postAttr = printPostAttr,
		postCallable = printPostCallable,
		postFuncActual = printPostFuncActual,
		postProcActual = printPostProcActual,
		postRef = printPostRef,
		postInteger = printPostInteger,
		postInt = printPostInt,
		postBinOp = printPostBinOp,
		postUnOp = printPostUnOp,
		postString = printPostString,
		postBool = printPostBool,
		preBinding = preNoAccum printPreBinding,
		preSimpleBinding = preNoAccum printPreSimpleBinding,
		preInterval = preNoAccum printPreInterval,
		preCaseMatch = preNoAccum printPreCaseMatch,
		preCaseCmdGuard = preNoAccum printPreCaseCmdGuard,
		preCaseDeclGuard = preNoAccum printPreCaseDeclGuard,
		preChan = preNoAccum printPreChan,
		preChanGuard = preNoAccum printPreChanGuard,
		preCmd = preNoAccum printPreCmd,
		preContext = preNoAccum printPreContext,
		preDecl = preNoAccum printPreDecl,
		preExpr = preNoAccum printPreExpr,
		preLvalue = preNoAccum printPreLvalue,
		preType = preNoAccum printPreType,
		preTypeBody = preNoAccum printPreTypeBody,
		preCallable = preNoAccum printPreCallable,
		preFuncActual = preNoAccum printPreFuncActual,
		preProcActual = preNoAccum printPreProcActual,
		postImplicant = printPostImplicant,
		postValue = printPostValue,
		postPos = printPostPos,
		postCompleteness = printPostCompleteness,
		postParSeq = printPostParSeq,
		postCheckMode = printPostCheckMode,
		postDirection = printPostDirection,
		postNamespace = printPostNamespace,
		postSense = printPostSense,
		postImportPath = printPostImportPath,
		postSliceInt = printPostSliceInt,
		preRef = preNoAccum printPreRef
		}	

	printPostInteger :: PostShow Integer
	printPostInteger _ (_, int) = ([showString $ show int], int)

	printPostInt :: PostShow Int
	printPostInt _ (_, int) = ([showString $ show int], int)

	printPostBinOp :: PostShow BinOp
	printPostBinOp _ (_, op) = ([showString $ show op], op)

	printPostUnOp :: PostShow UnOp
	printPostUnOp _ (_, op) = ([showString $ show op], op)

	printPostString :: PostShow String
	printPostString _ (_, str) = ([showString $ show str], str)

	printPostBool :: PostShow Bool
	printPostBool _ (_, bool) = ([showString $ show bool], bool)

	printPostPos :: PostShow Pos
	printPostPos _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostCompleteness :: PostShow Completeness
	printPostCompleteness _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostParSeq :: PostShow ParSeq
	printPostParSeq _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostDirection :: PostShow Direction
	printPostDirection _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostNamespace :: PostShow Namespace
	printPostNamespace _ (_, node) = ([showString $ show node], node)

	printPostSense :: PostShow Sense
	printPostSense _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostImportPath :: PostShow ImportPath
	printPostImportPath _ (_, node) = ([showParen True $ showString $ show node], node)

	printPostSliceInt :: PostShow (Slice Int)
	printPostSliceInt _ (_, node) = ([showParen True $ shows node], node)

	printPostCheckMode :: PostShow CheckMode
	printPostCheckMode _ (_, node) = ([showParen True $ showString $ show node], node)

	compose :: (a -> a) -> [a -> a] -> a -> a
	compose sep = foldr (.) id . intersperse sep

	spaceCompose :: [ShowS] -> ShowS
	spaceCompose = compose (showChar ' ')

	nlCompose :: Int -> [ShowS] -> ShowS
	nlCompose t = compose (nl t)

	nl :: Int -> ShowS
	nl t = showString $ '\n' : replicate (t * 2) ' '

	nlArray :: Int -> [[ShowS]] -> ShowS
	nlArray _ [] = showString "[ ]"
	nlArray level fs = showString "[" . nl level . compose (showChar ',' . nl level) (concat fs) . showString "]"

	printGatherList :: Int -> [[ShowS]] -> [[ShowS]]
	printGatherList level fs = [[nlArray level fs]]

	printGatherArray :: Int -> [ShowS] -> [[ShowS]] -> [[ShowS]]
	printGatherArray level [b] fs = [[showString "(listArray " . b . showChar ' ' . nlArray level fs . showChar ')']]
	printGatherArray _ _ _ = error "printGatherArray: bad args"

	printGatherPair :: [ShowS] -> [ShowS] -> [[ShowS]]
	printGatherPair [l] [r] = [[showChar '(' . l . showChar ',' . r . showChar ')']]
	printGatherPair _ _ = error "printGatherPair: bad args"

	printGatherMaybe :: Maybe [ShowS] -> [[ShowS]]
	printGatherMaybe Nothing = [[showString "Nothing"]]
	printGatherMaybe (Just [r]) = [[showString "(Just " . r . showChar ')']]
	printGatherMaybe _ = error "printGatherMaybe: bad args"

	tag :: String -> ShowS -> ShowS
	tag name r = showParen True $ showString name . showChar ' ' . r

	nlTag :: Int -> String -> ShowS -> ShowS
	nlTag level name r = showParen True $ showString name . nl level . r

	terminal :: String -> ShowS
	terminal = showString

	printPreContext :: PreShow (Context Decl)
	printPreContext level node = (level + 2, node)

	printPostContext :: PostShow (Context Decl)
	printPostContext _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			EmptyContext {} -> tagged "EmptyContext"
			Context {} -> tagged "Context"

	printPreRef :: PreShow Ref
	printPreRef level node = (level, node)

	printPostRef :: PostShow Ref
	printPostRef _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			Ref {} -> tagged "Ref"
			IndexRef {} -> tagged "IndexRef"

	printPreBinding :: PreShow (Binding Decl)
	printPreBinding level node = (level, node)

	printPostBinding :: PostShow (Binding Decl)
	printPostBinding _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			Binding {} -> tagged "Binding"

	printPreSimpleBinding :: PreShow SimpleBinding
	printPreSimpleBinding level node = (level, node)

	printPostSimpleBinding :: PostShow SimpleBinding
	printPostSimpleBinding _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			SimpleBinding {} -> tagged "SimpleBinding"

	printPreInterval :: PreShow Interval
	printPreInterval level node = (level, node)

	printPostInterval :: PostShow Interval
	printPostInterval _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			IntervalE {} -> tagged "IntervalE"
			IntervalOver {} -> tagged "IntervalOver"
			Interval {} -> tagged "Interval"

	printPreDecl :: PreShow Decl
	printPreDecl level node = (level + 1, node)

	printPostDecl :: PostShow Decl
	printPostDecl level (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)
		nlTagged name = ([nlTag (level + 1) name $ nlCompose (level + 1) rs], node)
		-- nlRs = nlCompose (level + 1) rs

		ret = case node of
			NoDecl {} -> tagged "NoDecl"
			PosDecl {} -> tagged "PosDecl"
			ExprDecl {} -> tagged "ExprDecl"
			ChanExprDecl {} -> tagged "ChanExprDecl"
			ProcDecl {} -> nlTagged "ProcDecl"
			ProcAliasDecl {} -> tagged "ProcAliasDecl"
			FuncDecl {} -> tagged "FuncDecl"
			BuiltinFuncDecl {} -> tagged "BuiltinFuncDecl"
			VarDecl {} -> tagged "VarDecl"
			ParamDecl {} -> tagged "ParamDecl"
			TypeParamDecl {} -> tagged "TypeParamDecl"
			ChanDecl {} -> tagged "ChanDecl"
			PortDecl {} -> tagged "PortDecl"
			OpenChanDecl {} -> tagged "OpenChanDecl"
			OpenChanDeclE {} -> tagged "OpenChanDeclE"
			FlatArrayedDecl {} -> tagged "FlatArrayedDecl"
			AliasDecl {} -> tagged "AliasDecl"
			SharedDecl {} -> tagged "SharedDecl"
			TypeDecl {} -> tagged "TypeDecl"
			ArrayedDecl {} -> tagged "ArrayedDecl"
			CaseDecl {} -> tagged "CaseDecl"
			DeferDecl {} -> tagged "DeferDecl"

	printPostRecordElem :: PostShow RecordElem
	printPostRecordElem _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			RecordElem {} -> tagged "RecordElem"

	printPostAttr :: PostShow Attr
	printPostAttr _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			ExprAttr {} -> tagged "ExprAttr"

	printPreTypeBody :: PreShow TypeBody
	printPreTypeBody level node = (level, node)

	printPostTypeBody :: PostShow TypeBody
	printPostTypeBody _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			AliasType {} -> tagged "AliasType"
			RecordType {} -> tagged "RecordType"
			EnumType {} -> tagged "EnumType"

	printPreType :: PreShow Type
	printPreType level node = (level, node)

	printPostType :: PostShow Type
	printPostType _ (rs, node) = ret where
		bare name = ([terminal name], node)
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			NoType -> bare "NoType"
			NameType {} -> tagged "NameType"
			NumType {} -> tagged "NumType"
			ArrayType {} -> tagged "ArrayType"
			Type {} -> tagged "Type"
			Bits {} -> tagged "Bits"
			SignedBits {} -> tagged "SignedBits"
			BuiltinType {} -> tagged "BuiltinType"
			StructRecordType {} -> tagged "StructRecordType"
			StructEnumType {} -> tagged "StructEnumType"
			StructArrayType {} -> tagged "StructArrayType"

	printPreLvalue :: PreShow Lvalue
	printPreLvalue level node = (level, node)

	printPostLvalue :: PostShow Lvalue
	printPostLvalue _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			CastLvalue {} -> tagged "CastLvalue"
			TypeCheckLvalue {} -> tagged "TypeCheckLvalue"
			NameLvalue {} -> tagged "NameLvalue"
			RecElemLvalue {} -> tagged "RecElemLvalue"
			SmashLvalue {} -> tagged "SmashLvalue"
			IndexLvalue {} -> tagged "IndexLvalue"
			SliceLvalue {} -> tagged "SliceLvalue"
			BitfieldLvalue {} -> tagged "BitfieldLvalue"
			CaseLvalue {} -> tagged "CaseLvalue"
			VarWrite {} -> tagged "VarWrite"

	printPreChan :: PreShow Chan
	printPreChan level node = (body node, node) where
		body FlatArrayedChan {} = level + 1
		body _ = level

	printPostChan :: PostShow Chan
	printPostChan _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			NameChan {} -> tagged "NameChan"
			IndexChan {} -> tagged "IndexChan"
			SliceChan {} -> tagged "SliceChan"
			PartialArrayedChan {} -> tagged "PartialArrayedChan"
			FlatArrayedChan {} -> tagged "FlatArrayedChan"
			CheckChan {} -> tagged "CheckChan"
			Chan {} -> tagged "Chan"

	printPreCallable :: PreShow Callable
	printPreCallable level node = (level, node)

	printPostCallable :: PostShow Callable
	printPostCallable _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			Callable {} -> tagged "Callable"
			NameCallable {} -> tagged "NameCallable"

	printPostImplicant :: PostShow Implicant
	printPostImplicant _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			Imp {} -> tagged "Imp"

	printPostValue :: PostShow Value
	printPostValue _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			IntValue {} -> tagged "IntValue"
			ImpValue {} -> tagged "ImpValue"
			StringValue {} -> tagged "StringValue"
			DontCareValue {} -> tagged "DontCareValue"

	printPreExpr :: PreShow Expr
	printPreExpr level node = (body node, node) where
		body BuiltinCallExpr {} = level + 1
		body CallExpr {} = level + 1
		body _ = level

	printPostExpr :: PostShow Expr
	printPostExpr _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			NameExpr {} -> tagged "NameExpr"
			BinExpr {} -> tagged "BinExpr"
			IndexExpr {} -> tagged "IndexExpr"
			SliceExpr {} -> tagged "SliceExpr"
			RecElemExpr {} -> tagged "RecElemExpr"
			UnExpr {} -> tagged "UnExpr"
			CastExpr {} -> tagged "CastExpr"
			TypeCheckExpr {} -> tagged "TypeCheckExpr"
			ConstCheckExpr {} -> tagged "ConstCheckExpr"
			ArrayElemTypeCheckExpr {} -> tagged "ArrayElemTypeCheckExpr"
			CallExpr {} -> tagged "CallExpr"
			BuiltinCallExpr {} -> tagged "BuiltinCallExpr"
			ConsExpr {} -> tagged "ConsExpr"
			AppendExpr {} -> tagged "AppendExpr"
			EnumElemExpr {} -> tagged "EnumElemExpr"
			SizeofExpr {} -> tagged "SizeofExpr"
			BitfieldExpr {} -> tagged "BitfieldExpr"
			ExtendExpr {} -> tagged "ExtendExpr"
			CaseExpr {} -> tagged "CaseExpr"
			ValueExpr {} -> tagged "ValueExpr"
			VarRead {} -> tagged "VarRead"
			OpenChanRead {} -> tagged "OpenChanRead"
			PartialArrayedRead {} -> tagged "PartialArrayedRead"
			MaybeTypeExpr {} -> tagged "MaybeTypeExpr"
			MaybeOtherExpr {} -> tagged "MaybeOtherExpr"
			Expr {} -> tagged "Expr"

	printPreCaseMatch :: PreShow CaseMatch
	printPreCaseMatch level node = (level, node)

	printPostCaseMatch :: PostShow CaseMatch
	printPostCaseMatch _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			ExprCaseMatch {} -> tagged "ExprCaseMatch"
			RangeCaseMatch {} -> tagged "RangeCaseMatch"
			ImpCaseMatches {} -> tagged "ImpCaseMatches"

	printPreCaseCmdGuard :: PreShow CaseCmdGuard
	printPreCaseCmdGuard level node = (level, node)

	printPostCaseCmdGuard :: PostShow CaseCmdGuard
	printPostCaseCmdGuard _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			CaseCmdGuard {} -> tagged "CaseCmdGuard"
			ForCaseCmdGuard {} -> tagged "ForCaseCmdGuard"
			ListCaseCmdGuard {} -> tagged "ListCaseCmdGuard"

	printPreCaseDeclGuard :: PreShow CaseDeclGuard
	printPreCaseDeclGuard level node = (level, node)

	printPostCaseDeclGuard :: PostShow CaseDeclGuard
	printPostCaseDeclGuard _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			CaseDeclGuard {} -> tagged "CaseDeclGuard"
			ListCaseDeclGuard {} -> tagged "ListCaseDeclGuard"

	printPreChanGuard :: PreShow ChanGuard
	printPreChanGuard level node = (level, node)

	printPostChanGuard :: PostShow ChanGuard
	printPostChanGuard _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			ChanGuard {} -> tagged "ChanGuard"

	printPreCmd :: PreShow Cmd
	printPreCmd level node = (body node, node) where
		body BlockCmd {} = level + 1
		body PrintCmd {} = level + 1
		body InstanceCallCmd {} = level + 1
		body SeqCmd {} = level + 2
		body ParCmd {} = level + 2
		body _ = level

	printPostCmd :: PostShow Cmd
	printPostCmd level (rs, node) = ret where
		bare name = ([terminal name], node)
		tagged name = ([tag name $ spaceCompose rs], node)
		nlTagged name = ([nlTag (level + 1) name $ nlCompose (level + 1) rs], node)

		ret = case node of
			NoCmd -> bare "NoCmd"
			LabelCmd {} -> tagged "LabelCmd"
			SeqCmd {} -> tagged "SeqCmd"
			ParCmd {} -> tagged "ParCmd"
			BlockCmd {} -> nlTagged "BlockCmd"
			EncInputCmd {} -> tagged "EncInputCmd"
			InputCmd {} -> tagged "InputCmd"
			OutputCmd {} -> tagged "OutputCmd"
			SelectCmd {} -> tagged "SelectCmd"
			AssignCmd {} -> tagged "AssignCmd"
			SinkCmd {} -> tagged "SinkCmd"
			CaseCmd {} -> tagged "CaseCmd"
			CaseCmdE {} -> tagged "CaseCmdE"
			CallCmd {} -> tagged "CallCmd"
			InstanceCallCmd {} -> tagged "InstanceCallCmd"
			SharedCallCmd {} -> tagged "SharedCallCmd"
			PrintCmd {} -> tagged "PrintCmd"
			ForCmd {} -> tagged "ForCmd"
			LoopCmd {} -> tagged "LoopCmd"
			WhileCmd {} -> tagged "WhileCmd"
			DeferCmd {} -> tagged "DeferCmd"

	printPreFuncActual :: PreShow FuncActual
	printPreFuncActual level node = (level, node)

	printPostFuncActual :: PostShow FuncActual
	printPostFuncActual _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			TypeFuncActual {} -> tagged "TypeFuncActual"
			ExprFuncActual {} -> tagged "ExprFuncActual"

	printPreProcActual :: PreShow ProcActual
	printPreProcActual level node = (level, node)

	printPostProcActual :: PostShow ProcActual
	printPostProcActual _ (rs, node) = ret where
		tagged name = ([tag name $ spaceCompose rs], node)

		ret = case node of
			TypeProcActual {} -> tagged "TypeProcActual"
			ExprProcActual {} -> tagged "ExprProcActual"
			ChanProcActual {} -> tagged "ChanProcActual"

	showValue :: [Context Decl] -> Type -> Value -> String
	showValue cs valueType val = body unaliasedValueType val
		where
			unaliasedValueType = unaliasType cs valueType

			showImpLocal = showImp True True 4

			body typ (IntValue val) | isNumericType typ = show $ recoverValue cs typ val
			body typ (ImpValue imp) | isNumericType typ = showImpLocal imp
			body (BuiltinType "String") (StringValue str) = "\"" ++ escBalsaString str ++ "\""
			body (StructArrayType (Interval arrayRange _) elemType) val | isImpOrIntValue val =
				showAggr (replicate elemCount elemType) val
				where elemCount = rangeSize arrayRange
			body (StructRecordType _ elems _) val | isImpOrIntValue val = showAggr types val
				where
					types = map recordElemType elems
					recordElemType (RecordElem _ _ typ) = typ
			body (StructEnumType name elems _) (IntValue val)
				| isJust found = simpleBindingName (fromJust found)
				| otherwise = show val ++ "'" ++ name
				where found = find ((== val) . simpleBindingValue) elems
			body typ DontCareValue = "(? : " ++ showTypeName cs typ ++ ")"
			body typ (IntValue val) = "(" ++ show (recoverValue cs typ val) ++ " as " ++ showTypeName cs typ ++ ")"
			body typ (ImpValue imp) = "(" ++ showImpLocal imp ++ " as " ++ showTypeName cs typ ++ ")"
			body typ val = "(-- " ++ show val ++ " as " ++ showTypeName cs typ ++ ")"

			showAggr types val = "{" ++ joinWith "," (snd $ mapAccumL showElem 0 types) ++ "}"
				where
					showElem offset typ = (offset + width, showValue cs typ bitField)
						where
							width = widthOfType cs typ
							bitField = case val of
								IntValue int -> IntValue (extract int)
								ImpValue (Imp int dcs) -> ImpValue (Imp (extract int) (extract dcs))
								_ -> error "showAggr: can't happen"

							extract = extractBitfield (sliceFromOW offset width)

			escBalsaString str = concatMap escChar str
				where
					escChar '"' = "\\\""
					escChar '\n' = "\\\n"
					escChar chr | ord chr < 32 = "\\x" ++ showHex (ord chr) ""
					escChar chr = [chr]

	class Show node => ShowParseNode node where
		showParseNode :: [Context Decl] -> node -> String
		showParseNode _ = show

	parseNodeError :: ShowParseNode a => [Context Decl] -> Pos -> a -> String -> Completeness
	parseNodeError cs pos node message = Wrong [Report pos $ message ++ " in: `" ++ showParseNode cs node ++ "'"]

	instance ShowParseNode Interval where
		showParseNode cs (IntervalE _ li ri) = showParseNode cs li ++ " .. " ++ showParseNode cs ri
		showParseNode cs (IntervalOver _ typ) = "over " ++ showParseNode cs typ
		showParseNode cs (Interval (li, ri) typ) = showValue cs typ (IntValue li) ++ " .. " ++
			showValue cs typ (IntValue ri)

	paren :: String -> String
	paren str = "(" ++ str ++ ")"
	comment :: String -> String
	comment str = "(-- " ++ str ++ " --)"

	instance ShowParseNode Decl where
		showParseNode = showDecl "?"

	instance ShowParseNode Attr where
		showParseNode cs (ExprAttr name expr) = name ++ " = " ++ showParseNode cs expr

	instance ShowParseNode (Binding Decl) where 
		showParseNode cs binding = showDecl (bindingName binding) cs (bindingValue binding)

	showDirection :: Direction -> ShowS
	showDirection Input = showString "input"
	showDirection Output = showString "output"

	showDecl :: String -> [Context Decl] -> Decl -> String
	showDecl name cs decl = body decl
		where
			typedDecl keyword typ = keyword ++ " " ++ name ++ " : " ++ showParseNode cs typ
		
			body (NoDecl {}) = comment "no decl"
			body (PosDecl {}) = comment "position decl"
			body (ExprDecl _ expr) = comment $ name ++ " expr " ++ showParseNode cs expr
			body (ChanExprDecl _ chan) = comment $ "ChanExpr " ++ name ++ showParseNode cs chan
			body (ProcDecl _ formals attrs cmd) = "procedure " ++ name ++ " ("
				++ joinWith ", " (map (showParseNode cs) (contextBindingsList formals))
				++ ") is " ++ showAttrs ++ "begin " ++ showParseNode cs cmd ++ " end"
				where
					showAttrs
						| null attrs = ""
						| otherwise = "(* " ++ joinWith ", " (map (showParseNode cs) attrs) ++ " *)"
			body (ProcAliasDecl _ callable _ actuals) = "procedure " ++ name ++ " is "
				++ showParseNode cs callable ++ " ("
				++ joinWith ", " (map (showParseNode cs) actuals)
				++ ")"
			body (FuncDecl _ formals expr) = "function " ++ name ++ " ("
				++ joinWith ", " (map (showParseNode cs) (contextBindingsList formals))
				++ ") = " ++ showParseNode cs expr
			body (BuiltinFuncDecl _ formals typ) = "function " ++ name ++ " ("
				++ joinWith ", " (map (showParseNode cs) (contextBindingsList formals))
				++ ") is builtin : " ++ showParseNode cs typ
			body (VarDecl _ typ) = typedDecl "variable" typ
			body (ParamDecl _ _ typ) = typedDecl "parameter" typ
			body (TypeParamDecl {}) = "type"
			body (ChanDecl _ typ) = typedDecl "channel" typ
			body (PortDecl _ dir typ) = typedDecl (showDirection dir "") typ
			body (OpenChanDecl _ ref _) = comment $ "Open channel to " ++ show ref
			body (OpenChanDeclE _ ref exprss) = comment $ "Open channel to " ++ show ref ++ ", indexed with "
				++ concatMap showIndex exprss
				where
					showIndex exprs = "[" ++ joinWith "," (map showPair exprs) ++ "]"
					showPair (l, r)
						| l == r = showParseNode cs l ++ ".." ++ showParseNode cs r
						| otherwise = showParseNode cs l
			body (FlatArrayedDecl _ interval elems) = comment ("Flat arrayed " ++ showParseNode cs interval)
				++ " {" ++ joinWith ", " (map showElem (assocs elems)) ++ "}"
				where showElem (i, decl) = show i ++ ": " ++ showParseNode cs decl
			body (AliasDecl _ ref _) = comment $ "Aliases " ++ show ref
			body (SharedDecl _ _ cmd) = "shared " ++ name ++ " is begin " ++ showParseNode cs cmd ++ " end"
			body (TypeDecl _ typeBody) = "type " ++ name ++ " is " ++ showParseNode cs typeBody
			body (ArrayedDecl _ interval decl) = "array " ++ showParseNode cs interval ++ " of "
				++ showDecl name cs decl
			body (CaseDecl _ expr guards elseDecl) = "case " ++ showParseNode cs expr ++ " of "
				++ joinWith " | " (map (showParseNode cs) guards) ++ (case elseDecl of
					NoDecl _ -> ""
					decl -> " else " ++ showParseNode cs decl) ++ " end"
			body (DeferDecl _ _ decl) = body decl

	instance ShowParseNode CaseDeclGuard where
		showParseNode cs (CaseDeclGuard _ matches decl) = joinWith ", " (map (showParseNode cs) matches)
			++ " then " ++ showParseNode cs decl
		showParseNode cs (ListCaseDeclGuard guards) = joinWith " | " (map (showParseNode cs) guards)

	instance ShowParseNode TypeBody where
		showParseNode cs = body
			where
				body (AliasType _ typ) = showParseNode cs typ
				body (RecordType _ elems overType) = "record " ++ joinWith "; " (map showElem elems) ++ end overType
					where showElem (RecordElem _ name typ) = name ++ " : " ++ showParseNode cs typ
				body (EnumType _ elems overType) = "enum " ++ joinWith ", " (map showElem elemsList) ++ end overType
					where
						elemsList = contextBindingsList elems
						showElem (Binding { bindingName = name, bindingValue = ExprDecl _ expr }) =
							name ++ " = " ++ showParseNode cs expr
						showElem _ = error "showParseNode TypeBody: can't happen"

				end overType
					| overType == NoType = " end"
					| otherwise = " over " ++ showParseNode cs overType

	instance ShowParseNode Type where
		showParseNode _ NoType = comment "no type"
		showParseNode _ (NameType _ name) = name
		showParseNode cs (NumType _ expr signed) = showParseNode cs expr ++
			(if signed then "signed" else "") ++ " bits"
		showParseNode cs (ArrayType interval typ) = "array " ++ showParseNode cs interval ++ " of "
			++ showParseNode cs typ
		showParseNode _ (BuiltinType name) = name
		showParseNode cs (Type ref) = bindingName binding
			where binding = findBindingByRef cs ref
		showParseNode _ (Bits int) = show int ++ " bits"
		showParseNode _ (SignedBits int) = show int ++ " signed bits"
		showParseNode cs (StructRecordType name recordElems overType) = comment name ++ " record " ++
			joinWith "; " (map showRecordElem recordElems) ++
			if noOverType then " end" else " over " ++ showParseNode cs overType
			where
				noOverType = overType == NoType
				showRecordElem (RecordElem _ name typ) = name ++ " : " ++ showParseNode cs typ
		showParseNode cs (StructEnumType name enumElems overType) = comment name ++ " enumeration " ++
			joinWith ", " (snd (mapAccumL showBinding 0 enumElems)) ++
			if noOverType then " end" else " over " ++ showParseNode cs overType
			where
				noOverType = overType == NoType
				showBinding i (SimpleBinding name value)
					| i == value = (i + 1, name)
					| otherwise = (value + 1, name ++ " = " ++ if noOverType
						then show value
						else showValue cs overType (IntValue value))
		showParseNode cs (StructArrayType (Interval (low, high) typ) elemType) = "array " ++
			showValue cs typ (IntValue low) ++ ".." ++ showValue cs typ (IntValue high) ++
			" of " ++ showParseNode cs elemType
		showParseNode _ node = show node

	instance ShowParseNode Lvalue where
		showParseNode cs lvalue = body lvalue
			where
				body (CastLvalue _ typ lvalue) = paren $ body lvalue ++ showType " as " typ
				body (TypeCheckLvalue _ mode typ lvalue) = paren $ comment (show mode) ++
					body lvalue ++ showType " : " typ
				body (NameLvalue _ name) = name
				body (RecElemLvalue _ lvalue elemName) = body lvalue ++ "." ++ elemName
				body (SmashLvalue _ lvalue) = paren $ "#" ++ body lvalue
				body (IndexLvalue _ lvalue expr) = paren $ body lvalue ++ "[" ++ showParseNode cs expr ++ "]"
				body (SliceLvalue _ lvalue li ri) = paren $ body lvalue ++ showExprSlice (showParseNode cs) li ri
				body (BitfieldLvalue _ typ slice lvalue) = paren $ "(#" ++ body lvalue ++ ")"
					++ showSlice slice ++ showType " : " typ
				body (CaseLvalue _ expr implicantss lvalues) = paren $ "case " ++ showParseNode cs expr ++ " of "
					++ joinWith "| " (map showMatch $ zip implicantss lvalues)
					where
						showMatch (implicants, lvalue) = joinWith ", " (map (showImp True True 4) implicants)
							++ " then " ++ body lvalue
				body (VarWrite _ _ slice ref)
					| width == sliceWidth slice && sliceOffset slice == 0 = name
					| otherwise = "#" ++ name ++ showSlice slice
					where
						name = bindingName binding
						binding = findBindingByRef cs ref
						width = widthOfType cs $ declType $ bindingValue binding

				showExprSlice show li ri = "[" ++ show li ++ ".." ++ show ri ++ "]"

				showSlice slice
					| isEmptySlice slice = "(-- empty slice --)"
					| otherwise = balsaShowSlice slice ""

				showType _ NoType = comment "no type"
				showType str typ = str ++ showParseNode cs typ

	instance ShowParseNode Chan where
		showParseNode _ (NameChan _ name) = name
		showParseNode cs (IndexChan _ chan expr) = showParseNode cs chan ++ "[" ++ showParseNode cs expr ++ "]"
		showParseNode cs (SliceChan _ chan li ri) = showParseNode cs chan ++ "["
			++ showParseNode cs li ++ showParseNode cs ri ++ "]"
		showParseNode _ (PartialArrayedChan _ ref) = comment $ "Partially arrayed " ++ show ref
		showParseNode cs (FlatArrayedChan _ interval chans) = comment ("Flat arrayed " ++ showParseNode cs interval)
			++ " {" ++ joinWith "," (map (showParseNode cs) chans) ++ "}"
		showParseNode cs (CheckChan _ decl chan) = comment ("Check against " ++ showDecl "?" cs decl)
			++ " " ++ showParseNode cs chan
		showParseNode cs (Chan _ ref) = name
			where name = bindingName $ findBindingByRef cs ref

	instance ShowParseNode Callable where
		showParseNode _ (NameCallable _ name) = name
		showParseNode cs (Callable ref) = name
			where name = bindingName $ findBindingByRef cs ref

	binOpSymbol :: BinOp -> String
	binOpSymbol BinMul = "*"
	binOpSymbol BinDiv = "/"
	binOpSymbol BinMod = "%"
	binOpSymbol BinPow = "^"
	binOpSymbol BinAdd = "+"
	binOpSymbol BinSub = "-"
	binOpSymbol BinAnd = "and"
	binOpSymbol BinOr = "or"
	binOpSymbol BinXor = "xor"
	binOpSymbol BinLT = "<"
	binOpSymbol BinGT = ">"
	binOpSymbol BinLE = "<="
	binOpSymbol BinGE = ">="
	binOpSymbol BinNE = "/="
	binOpSymbol BinEQ = "="

	unOpSymbol :: UnOp -> String
	unOpSymbol UnNot = "not"
	unOpSymbol UnNeg = "-"
	unOpSymbol UnLog = "log"
	unOpSymbol UnSmash = "#"

	instance ShowParseNode Expr where
		showParseNode cs exprNode = body exprNode
			where
				body (NameExpr _ name) = name
				body (BinExpr _ _ op l r) = paren $ body l ++ " " ++ binOpSymbol op ++ " " ++ body r
				body (IndexExpr _ expr i) = paren $ body expr ++ "[" ++ body i ++ "]"
				body (SliceExpr _ expr li ri) = paren $ body expr ++ "["
					++ body li ++ ".." ++ body ri ++ "]"
				body (RecElemExpr _ expr elemName) = paren $ body expr ++ "." ++ elemName
				body (UnExpr _ _ op expr) = paren $ unOpSymbol op ++ " " ++ body expr
				body (CastExpr _ typ expr) = paren $ body expr ++ showType " as " typ
				body (TypeCheckExpr _ _ typ expr) = paren $ body expr ++ showType " : " typ
				body (ConstCheckExpr _ expr) = comment "is const" ++ " " ++ body expr
				body (ArrayElemTypeCheckExpr _ typ expr) = comment ("array elements have type: "
					++ showType "" typ) ++ " " ++ body expr
				body (CallExpr _ callable _ funcActuals) = showParseNode cs callable
					++ " (" ++ joinWith ", " (map (showParseNode cs) funcActuals) ++ ")"
				body (BuiltinCallExpr _ name funcActuals exprs typ) = name ++ " (" ++
					joinWith ", " (map (showParseNode cs) funcActuals) ++
					joinWith ", " (map body exprs) ++
					")" ++ if typ /= NoType then comment (": " ++ showType "" typ) else ""
				body (ConsExpr _ exprType exprConsType exprs) = paren $ "{" ++
					joinWith ", " (map (body) exprs) ++ "}" ++
					showType " : " exprConsType ++ showType " as " exprType
				body (AppendExpr _ _ l r) = paren $ body l ++ " @ " ++ body r
				body (EnumElemExpr _ typ elemName) = paren $ showType "" typ ++ "'" ++ elemName
				body (SizeofExpr _ typ) = paren $ "sizeof " ++ showType "" typ
				body (BitfieldExpr _ _ slice expr) = paren $ sliceBitIndex (body expr) slice
				body (ExtendExpr _ _ width signed expr) = paren $ comment ("extend to "
					++ show width ++ (if signed then "signed" else "") ++ "bits") ++ " " ++ body expr
				body (CaseExpr _ expr implicantss exprs) = paren $ "case " ++ body expr ++ " of "
					++ joinWith "| " (map showMatch $ zip implicantss exprs)
					where
						showMatch (implicants, expr) = joinWith ", " (map (showImp True True 4) implicants)
							++ " then " ++ body expr
				body (ValueExpr _ typ value) = showValue cs typ value
				body (VarRead _ _ slice ref) = paren $ sliceBitIndex (refName ref) slice
				body (OpenChanRead _ _ slice ref) = paren $ sliceBitIndex (refName ref) slice
				body (PartialArrayedRead _ ref) = comment "partial arrayed read" ++ " " ++ refName ref
				body (MaybeTypeExpr _ ref) = comment "a type?" ++ " " ++ refName ref
				body (MaybeOtherExpr _ ref) = refName ref
				body (Expr _ ref) = comment "shared expr" ++ " " ++
					(if isFuncDecl decl then body (declExpr decl) else comment "not a function")
					where
						decl = bindingValue $ findBindingByRef cs ref

						isFuncDecl (FuncDecl {}) = True
						isFuncDecl _ = False

				sliceBitIndex expr slice = "(#" ++ expr ++ ")" ++ if isEmptySlice slice
					then "(-- empty slice --)"
					else balsaShowSlice slice ""

				refName ref = bindingName $ findBindingByRef cs ref

				showType _ NoType = comment "no type"
				showType str typ = str ++ showParseNode cs typ

	instance ShowParseNode CaseMatch where
		showParseNode cs (ExprCaseMatch _ e) = showParseNode cs e
		showParseNode cs (RangeCaseMatch _ li ri) = showParseNode cs li ++ " .. " ++ showParseNode cs ri
		showParseNode _ (ImpCaseMatches _ implicants) = joinWith ", " $ map (showImp True True 4) implicants

	instance ShowParseNode CaseCmdGuard where
		showParseNode cs (CaseCmdGuard _ matches cmd) = joinWith ", " (map (showParseNode cs) matches)
			++ " then " ++ showParseNode cs cmd
		showParseNode cs (ForCaseCmdGuard _ varName matches _ cmd) = "for " ++ varName ++ " in "
			++ joinWith ", " (map (showParseNode cs) matches)
			++ " then " ++ showParseNode cs cmd ++ " end"
		showParseNode cs (ListCaseCmdGuard guards) = joinWith " | " (map (showParseNode cs) guards)

	showChanGuard :: String -> [Context Decl] -> ChanGuard -> String
	showChanGuard sep cs (ChanGuard _ chans _ cmd) = joinWith ", " (map (showParseNode cs) chans)
		++ " " ++ sep ++ " " ++ showParseNode cs cmd

	instance ShowParseNode Cmd where
		showParseNode _ NoCmd = "continue"
		showParseNode cs (LabelCmd _ label cmd) = label ++ ": " ++ showParseNode cs cmd
		showParseNode cs (SeqCmd _ cmds) = joinWith "; " $ map (showParseNode cs) cmds
		showParseNode cs (ParCmd _ cmds) = joinWith " || " $ map (showParseNode cs) cmds
		showParseNode cs (BlockCmd _ context cmd) = defns ++ "begin " ++ showParseNode cs cmd ++ " end"
			where
				defns
					| contextSize context == 0 = ""
					| otherwise = "local " ++ joinWith " " (map (showParseNode cs) (contextBindingsList context))
		showParseNode cs (EncInputCmd _ chanGuard) = showChanGuard " -> then " cs chanGuard ++ " end"
		showParseNode cs (InputCmd _ chan lvalue) = showParseNode cs chan ++ " -> " ++ showParseNode cs lvalue
		showParseNode cs (OutputCmd _ chan expr) = showParseNode cs chan ++ " <- " ++ showParseNode cs expr
		showParseNode cs (SelectCmd _ arb chanGuards) = (if arb then "arbitrate " else "select ")
			++ joinWith " | " (map (showChanGuard " then " cs) chanGuards) ++ " end"
		showParseNode cs (AssignCmd _ lvalue expr) = showParseNode cs lvalue ++ " := " ++ showParseNode cs expr
		showParseNode cs (SinkCmd _ expr) = "sink " ++ showParseNode cs expr
		showParseNode cs (CaseCmdE _ expr caseCmdGuards cmd) = "case " ++ showParseNode cs expr ++ " of "
			++ joinWith " | " (map (showParseNode cs) caseCmdGuards) ++ elseCmd ++ " end"
			where
				elseCmd
					| cmd == NoCmd = ""
					| otherwise = "else " ++ showParseNode cs cmd
		showParseNode cs (CaseCmd _ expr implicantss cmds) = "case " ++ showParseNode cs expr ++ " of "
			++ joinWith " | " (map showMatch $ zip implicantss cmds) ++ " end"
			where
				showMatch (implicants, cmd) = joinWith ", " (map (showImp True True 4) implicants)
					++ " then " ++ showParseNode cs cmd
		showParseNode cs (CallCmd _ callable _ procActuals) = showParseNode cs callable
			++ " (" ++ joinWith ", " (map (showParseNode cs) procActuals) ++ ")"
		showParseNode cs (InstanceCallCmd _ callable chans) = showParseNode cs callable
			++ " (" ++ joinWith ", " (map (showParseNode cs) chans) ++ ")"
		showParseNode cs (SharedCallCmd _ callable) = showParseNode cs callable ++ " ()"
		showParseNode cs (PrintCmd _ exprs) = "print " ++ joinWith "," (map (showParseNode cs) exprs)
		showParseNode cs (ForCmd _ parSeq interval _ cmd) = "for " ++ (case parSeq of { Par -> "||" ; Seq -> ";" })
			++ " in " ++ showParseNode cs interval ++ " then " ++ showParseNode cs cmd ++ " end"
		showParseNode cs (LoopCmd _ cmd) = "loop " ++ showParseNode cs cmd ++ " end"
		showParseNode cs (WhileCmd _ preCmd expr postCmd) = "loop" ++
			(if preCmd == NoCmd then "" else (" " ++ showParseNode cs preCmd)) ++
			" while " ++ showParseNode cs expr ++
			(if postCmd == NoCmd then "" else ("then " ++ showParseNode cs postCmd)) ++
			" end"
		showParseNode cs (DeferCmd completeness cmd) = "(-- " ++ show completeness ++ "--) " ++ showParseNode cs cmd

	instance ShowParseNode FuncActual where
		showParseNode cs (TypeFuncActual typ) = showParseNode cs typ
		showParseNode cs (ExprFuncActual _ expr) = showParseNode cs expr

	instance ShowParseNode ProcActual where
		showParseNode cs (TypeProcActual typ) = showParseNode cs typ
		showParseNode cs (ExprProcActual expr) = showParseNode cs expr
		showParseNode cs (ChanProcActual chan) = showParseNode cs chan

	isNumericType :: Type -> Bool
	isNumericType (Bits {}) = True
	isNumericType (SignedBits {}) = True
	isNumericType (NumType {}) = True
	isNumericType _ = False

	isImpOrIntValue :: Value -> Bool
	isImpOrIntValue (IntValue {}) = True
	isImpOrIntValue (ImpValue {}) = True
	isImpOrIntValue _ = False
