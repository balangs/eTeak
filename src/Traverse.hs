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

module Traverse (
	emptyApply,
	preNoAccum,
	Apply (..),
	ApplyPreFunc,
	ApplyPostFunc,
	ParseTreeTraverse (traverse)
	) where

        import Prelude hiding (traverse)
	import ParseTree
	import Report
	import Context
	import Bits

	import Data.Array
	import Data.Maybe
	import Data.List

	type ApplyPreFunc context accum node = context -> accum -> node -> (context, accum, node)
	type ApplyPostFunc context result node = context -> (result, node) -> (result, node)
	type Descend context accum result node = (context, accum) -> (context, accum, result, node)
	type Cons context accum result t node = (context, accum, result, t -> node) -> (context, accum, result, node)

	preNoAccum :: (context -> node -> (context, node)) -> context -> accum -> node -> (context, accum, node)
	preNoAccum f context accum node = (context', accum, node')
		where (context', node') = f context node

	data Apply context accum result = Apply {
		-- useContextPreCsInEncNode : use the modified cs returned by preContext as the context for the
		--	rest of the node containing the Context.  This is useful for modifying the context not only
		--	for the Context's Bindings but also for the rest of any other node's elements which contains
		--	a Context binding values.
		useContextPreCsInEncNode :: Bool,
		applyContextCons :: Context Decl -> context -> context,
		nullResult :: result,
		-- combineResult : combine result elements from nodes.  The second argument is always the element to add
		--	where the function is not commutative
		combineResult :: result -> result -> result,
		gatherList :: context -> [result] -> [result],
		gatherArray :: context -> result -> [result] -> [result],
		gatherPair :: result -> result -> [result],
		gatherMaybe :: Maybe result -> [result],
		midInterval :: ApplyPostFunc context result Interval,
		midChan :: ApplyPostFunc context result Chan,
		midChanGuard :: ApplyPostFunc context result ChanGuard,
		midCmd :: ApplyPostFunc context result Cmd,
		midDecl :: ApplyPostFunc context result Decl,
		midExpr :: ApplyPostFunc context result Expr,
		midLvalue :: ApplyPostFunc context result Lvalue,
		midCaseCmdGuard :: ApplyPostFunc context result CaseCmdGuard,
		midCaseDeclGuard :: ApplyPostFunc context result CaseDeclGuard,
		postBinding :: ApplyPostFunc context result (Binding Decl),
		postSimpleBinding :: ApplyPostFunc context result SimpleBinding,
		postInterval :: ApplyPostFunc context result Interval,
		postCaseMatch :: ApplyPostFunc context result CaseMatch,
		postCaseCmdGuard :: ApplyPostFunc context result CaseCmdGuard,
		postCaseDeclGuard :: ApplyPostFunc context result CaseDeclGuard,
		postChan :: ApplyPostFunc context result Chan,
		postChanGuard :: ApplyPostFunc context result ChanGuard,
		postCmd :: ApplyPostFunc context result Cmd,
		postContext :: ApplyPostFunc context result (Context Decl),
		postDecl :: ApplyPostFunc context result Decl,
		postExpr :: ApplyPostFunc context result Expr,
		postLvalue :: ApplyPostFunc context result Lvalue,
		postType :: ApplyPostFunc context result Type,
		postTypeBody :: ApplyPostFunc context result TypeBody,
		postCallable :: ApplyPostFunc context result Callable,
		postFuncActual :: ApplyPostFunc context result FuncActual,
		postProcActual :: ApplyPostFunc context result ProcActual,
		postRef :: ApplyPostFunc context result Ref,
		postRecordElem :: ApplyPostFunc context result RecordElem,
		postAttr :: ApplyPostFunc context result Attr,
		finalExpr :: ApplyPostFunc context result Expr,
		preBinding :: ApplyPreFunc context accum (Binding Decl),
		preSimpleBinding :: ApplyPreFunc context accum SimpleBinding,
		preInterval :: ApplyPreFunc context accum Interval,
		preCaseMatch :: ApplyPreFunc context accum CaseMatch,
		preCaseCmdGuard :: ApplyPreFunc context accum CaseCmdGuard,
		preCaseDeclGuard :: ApplyPreFunc context accum CaseDeclGuard,
		preChan :: ApplyPreFunc context accum Chan,
		preChanGuard :: ApplyPreFunc context accum ChanGuard,
		preCmd :: ApplyPreFunc context accum Cmd,
		preContext :: ApplyPreFunc context accum (Context Decl),
		preDecl :: ApplyPreFunc context accum Decl,
		preExpr :: ApplyPreFunc context accum Expr,
		preLvalue :: ApplyPreFunc context accum Lvalue,
		preType :: ApplyPreFunc context accum Type,
		preTypeBody :: ApplyPreFunc context accum TypeBody,
		preCallable :: ApplyPreFunc context accum Callable,
		preFuncActual :: ApplyPreFunc context accum FuncActual,
		preProcActual :: ApplyPreFunc context accum ProcActual,
		preRef :: ApplyPreFunc context accum Ref,
		preRecordElem :: ApplyPreFunc context accum RecordElem,
		-- Terminals
		postInteger :: ApplyPostFunc context result Integer,
		postInt :: ApplyPostFunc context result Int,
		postBinOp :: ApplyPostFunc context result BinOp,
		postUnOp :: ApplyPostFunc context result UnOp,
		postString :: ApplyPostFunc context result String,
		postBool :: ApplyPostFunc context result Bool,
		postImplicant :: ApplyPostFunc context result Implicant,
		postValue :: ApplyPostFunc context result Value,
		postPos :: ApplyPostFunc context result Pos,
		postCompleteness :: ApplyPostFunc context result Completeness,
		postNamespace :: ApplyPostFunc context result Namespace,
		postParSeq :: ApplyPostFunc context result ParSeq,
		postDirection :: ApplyPostFunc context result Direction,
		postCheckMode :: ApplyPostFunc context result CheckMode,
		postSense :: ApplyPostFunc context result Sense,
		postImportPath :: ApplyPostFunc context result ImportPath,
		postSliceInt :: ApplyPostFunc context result (Slice Int) }

	preId :: ApplyPreFunc context accum node
	preId context accum node = (context, accum, node)
	postId :: ApplyPostFunc context result node
	postId _ node = node

	defaultGatherList :: context -> [result] -> [result]
	defaultGatherList _ = id

	defaultGatherArray :: context -> result -> [result] -> [result]
	defaultGatherArray _ = (:)

	defaultContextCons :: c -> context -> context
	defaultContextCons _ cs = cs

	emptyApply :: Apply context accum result
	emptyApply = Apply {
		useContextPreCsInEncNode = False,
		applyContextCons = defaultContextCons,
		nullResult = undefined,
		combineResult = const,
		gatherList = defaultGatherList,
		gatherArray = defaultGatherArray, gatherPair = (\l r -> [l, r]),
		gatherMaybe = maybeToList,
		preChanGuard = preId, midChanGuard = postId, postChanGuard = postId,
		preCmd = preId, midCmd = postId, postCmd = postId,
		preBinding = preId, postBinding = postId,
		preSimpleBinding = preId, postSimpleBinding = postId,
		preInterval = preId, midInterval = postId, postInterval = postId,
		preCaseMatch = preId, postCaseMatch = postId,
		preCaseCmdGuard = preId, midCaseCmdGuard = postId, postCaseCmdGuard = postId,
		preCaseDeclGuard = preId, midCaseDeclGuard = postId, postCaseDeclGuard = postId,
		preChan = preId, midChan = postId, postChan = postId,
		preContext = preId, postContext = postId,
		preDecl = preId, midDecl = postId, postDecl = postId,
		preExpr = preId, midExpr = postId, postExpr = postId, finalExpr = postId,
		preLvalue = preId, midLvalue = postId, postLvalue = postId,
		preType = preId, postType = postId,
		preTypeBody = preId, postTypeBody = postId,
		preCallable = preId, postCallable = postId,
		preFuncActual = preId, postFuncActual = postId,
		preProcActual = preId, postProcActual = postId,
		preRef = preId, postRef = postId,
		postAttr = postId,
		postInteger = postId, postInt = postId, postString = postId, postImplicant = postId, postValue = postId,
		postBinOp = postId, postUnOp = postId,
		postBool = postId, postPos = postId, preRecordElem = preId, postRecordElem = postId,
		postCompleteness = postId, postNamespace = postId, postParSeq = postId, postDirection = postId,
		postCheckMode = postId, postSense = postId, postImportPath = postId,
		postSliceInt = postId }

	class ParseTreeTraverse node where
		traverse :: Apply context accum result -> context -> accum -> node -> (accum, result, node)
		traverse apply contextIn accumIn node = (accum, finalResult, finalNode) where
			(accum, innerResult, innerNode) = innerTraverse apply contextIn accumIn node
			(finalResult, finalNode) = final apply contextIn (innerResult, innerNode)

		innerTraverse :: Apply context accum result -> context -> accum -> node -> (accum, result, node)
		innerTraverse apply context accum node = snd $ contextInnerTraverse apply context accum node

		contextInnerTraverse :: Apply context accum result -> context -> accum -> node ->
			(context, (accum, result, node))
		contextInnerTraverse apply contextIn accumIn node = (context, (descend2Accum, retResult, retNode)) where
			(context, preAccum, preNode) = pre apply contextIn accumIn node
			(_, descend1Accum, descend1Result, descend1Node) = descend1 apply preNode (context, preAccum)
			(midResult, midNode) = mid apply context (descend1Result, descend1Node)
			(_, descend2Accum, descend2Result, descend2Node) = descend2 apply midNode (context, descend1Accum)
			depthResult = combineResult apply midResult descend2Result
			(retResult, retNode) = post apply context (depthResult, descend2Node)

		pre :: Apply context accum result -> ApplyPreFunc context accum node
		pre _ = preId

		mid :: Apply context accum result -> ApplyPostFunc context result node
		mid _ = postId

		post :: Apply context accum result -> ApplyPostFunc context result node
		post _ = postId

		final :: Apply context accum result -> ApplyPostFunc context result node
		final _ = postId

		descend1 :: Apply context accum result -> node -> Descend context accum result node
		descend1 apply node = noDescend apply node

		descend2 :: Apply context accum result -> node -> Descend context accum result node
		descend2 apply node = noDescend apply node

	gatherResult :: Apply context accum result -> [result] -> result
	gatherResult apply results = foldl' (combineResult apply) (nullResult apply) results

	skipD :: Apply context accum result -> v -> Cons context accum result v node
	skipD _ v (context, accum, rs, cons) = (context, accum, rs, cons v)

	{- stepD :: ParseTreeTraverse a => Apply context accum result ->
		a -> (context, accum, result, a -> b) -> (context, accum, result, b) -}
	stepD :: ParseTreeTraverse e => Apply context accum result -> e ->
		Cons context accum result e node
	stepD apply e (context, accum, rs, cons) = (context, accum', combineResult apply rs r1, cons e')
		where (accum', r1, e') = traverse apply context accum e

	stringD :: Apply context accum result -> String ->
		Cons context accum result String node
	stringD apply str (context, accum, rs, cons) = (context, accum, combineResult apply rs r1, cons str')
		where (r1, str') = postString apply context (nullResult apply, str)

	innerD :: ParseTreeTraverse e => Apply context accum result -> e ->
		Cons context accum result e node
	innerD apply e (context, accum, rs, cons) = (context, accum', combineResult apply rs r1, cons e')
		where (accum', r1, e') = innerTraverse apply context accum e

	contextD :: Apply context accum result -> Context Decl ->
		Cons context accum result (Context Decl) node
	contextD apply ctx (context, accum, rs, cons) = (applyContextCons apply ctx' retContext,
		accum', combineResult apply rs r1, cons ctx')
		where
			(context', (accum', r1, ctx')) = contextInnerTraverse apply context accum ctx
			retContext
				| useContextPreCsInEncNode apply = context'
				| otherwise = context

	descend :: Apply context accum result -> cons ->
		((context, accum, result, cons) -> (context, accum, result, node)) ->
		Descend context accum result node
	descend apply cons f (context, accum) = f (context, accum, nullResult apply, cons)

	noDescend :: Apply context accum result -> node ->
		Descend context accum result node
	noDescend apply node = descend apply node id

	-- Any with I

	descendNIV :: (ParseTreeTraverse e, ParseTreeTraverse i) =>
		Apply context accum result ->
		(e -> i -> v1 -> node) -> e -> i -> v1 ->
		Descend context accum result node
	descendNIV apply cons e i v = descend apply cons $
		skipD apply v . innerD apply i . stepD apply e

	descendNIVV :: (ParseTreeTraverse e, ParseTreeTraverse i) =>
		Apply context accum result ->
		(e -> i -> v1 -> v2 -> node) -> e -> i -> v1 -> v2 ->
		Descend context accum result node
	descendNIVV apply cons e i v1 v2 = descend apply cons $
		skipD apply v2 . skipD apply v1 . innerD apply i . stepD apply e

	-- Just N

	descendN :: ParseTreeTraverse e =>
		Apply context accum result ->
		(e -> node) -> e ->
		Descend context accum result node
	descendN apply cons e = descend apply cons $ stepD apply e

	descendNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> e2 -> node) -> e1 -> e2 ->
		Descend context accum result node
	descendNN apply cons e1 e2 = descend apply cons $
		stepD apply e2 . stepD apply e1

	descendNNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> node) -> e1 -> e2 -> e3 ->
		Descend context accum result node
	descendNNN apply cons e1 e2 e3 = descend apply cons $
		stepD apply e3 . stepD apply e2 . stepD apply e1

	descendNNNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3,
		ParseTreeTraverse e4) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> e4 -> node) -> e1 -> e2 -> e3 -> e4 ->
		Descend context accum result node
	descendNNNN apply cons e1 e2 e3 e4 = descend apply cons $
		stepD apply e4 . stepD apply e3 . stepD apply e2 . stepD apply e1

	descendNNNNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3,
		ParseTreeTraverse e4, ParseTreeTraverse e5) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> e4 -> e5 -> node) -> e1 -> e2 -> e3 -> e4 -> e5 ->
		Descend context accum result node
	descendNNNNN apply cons e1 e2 e3 e4 e5 = descend apply cons $
		stepD apply e5 . stepD apply e4 . stepD apply e3 . stepD apply e2 . stepD apply e1

	-- N+V+

	descendNNV :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> e2 -> v1 -> node) -> e1 -> e2 -> v1 ->
		Descend context accum result node
	descendNNV apply cons e1 e2 v = descend apply cons $
		skipD apply v . stepD apply e2 . stepD apply e1

	descendNNVV :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> e2 -> v1 -> v2 -> node) -> e1 -> e2 -> v1 -> v2 ->
		Descend context accum result node
	descendNNVV apply cons e1 e2 v1 v2 = descend apply cons $
		skipD apply v2 . skipD apply v1 . stepD apply e2 . stepD apply e1

	descendNNNV :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> v1 -> node) -> e1 -> e2 -> e3 -> v1 ->
		Descend context accum result node
	descendNNNV apply cons e1 e2 e3 v = descend apply cons $
		skipD apply v . stepD apply e3 . stepD apply e2 . stepD apply e1

	descendNNNVV :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> v1 -> v2 -> node) -> e1 -> e2 -> e3 -> v1 -> v2 ->
		Descend context accum result node
	descendNNNVV apply cons e1 e2 e3 v1 v2 = descend apply cons $
		skipD apply v2 . skipD apply v1 . stepD apply e3 . stepD apply e2 . stepD apply e1

	descendNNNNV :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3,
		ParseTreeTraverse e4) =>
		Apply context accum result ->
		(e1 -> e2 -> e3 -> e4 -> v1 -> node) -> e1 -> e2 -> e3 -> e4 -> v1 ->
		Descend context accum result node
	descendNNNNV apply cons e1 e2 e3 e4 v1 = descend apply cons $
		skipD apply v1 . stepD apply e4 . stepD apply e3 . stepD apply e2 . stepD apply e1

	-- V+N*

	descendVVN :: ParseTreeTraverse e1 =>
		Apply context accum result ->
		(v1 -> v2 -> e1 -> node) -> v1 -> v2 -> e1 ->
		Descend context accum result node
	descendVVN apply cons v1 v2 e = descend apply cons $
		stepD apply e . skipD apply v2 . skipD apply v1

	descendVVNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(v1 -> v2 -> e1 -> e2 -> node) -> v1 -> v2 -> e1 -> e2 ->
		Descend context accum result node
	descendVVNN apply cons v1 v2 e1 e2 = descend apply cons $
		stepD apply e2 . stepD apply e1 . skipD apply v2 . skipD apply v1

	descendVVVN :: ParseTreeTraverse e1 =>
		Apply context accum result ->
		(v1 -> v2 -> v3 -> e1 -> node) -> v1 -> v2 -> v3 -> e1 ->
		Descend context accum result node
	descendVVVN apply cons v1 v2 v3 e = descend apply cons $
		stepD apply e . skipD apply v3 . skipD apply v2 . skipD apply v1

	descendVVVVN :: ParseTreeTraverse e1 =>
		Apply context accum result ->
		(v1 -> v2 -> v3 -> v4 -> e1 -> node) -> v1 -> v2 -> v3 -> v4 -> e1 ->
		Descend context accum result node
	descendVVVVN apply cons v1 v2 v3 v4 e = descend apply cons $
		stepD apply e . skipD apply v4 . skipD apply v3 . skipD apply v2 . skipD apply v1

	-- .*S.*

	descendS :: Apply context accum result ->
		(String -> node) -> String ->
		Descend context accum result node
	descendS apply cons str = descend apply cons $
		stringD apply str

	descendNS :: ParseTreeTraverse e1 =>
		Apply context accum result ->
		(e1 -> String -> node) -> e1 -> String ->
		Descend context accum result node
	descendNS apply cons e str = descend apply cons $
		stringD apply str . stepD apply e

	descendSN :: ParseTreeTraverse e1 =>
		Apply context accum result ->
		(String -> e1 -> node) -> String -> e1 ->
		Descend context accum result node
	descendSN apply cons str e = descend apply cons $
		stepD apply e . stringD apply str

	descendNNS :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> e2 -> String -> node) -> e1 -> e2 -> String ->
		Descend context accum result node
	descendNNS apply cons e1 e2 str = descend apply cons $
		stringD apply str . stepD apply e2 . stepD apply e1

	descendNSN :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> String -> e2 -> node) -> e1 -> String -> e2 ->
		Descend context accum result node
	descendNSN apply cons e1 str e2 = descend apply cons $
		stepD apply e2 . stringD apply str . stepD apply e1

	descendNSNVV :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> String -> e2 -> v1 -> v2 -> node) -> e1 -> String -> e2 -> v1 -> v2 ->
		Descend context accum result node
	descendNSNVV apply cons e1 str e2 v1 v2 = descend apply cons $
		skipD apply v2 . skipD apply v1 . stepD apply e2 . stringD apply str . stepD apply e1

	descendSNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(String -> e1 -> e2 -> node) -> String -> e1 -> e2 ->
		Descend context accum result node
	descendSNN apply cons str e1 e2 = descend apply cons $
		stepD apply e2 . stepD apply e1 . stringD apply str

	descendNSNNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3, ParseTreeTraverse e4) =>
		Apply context accum result ->
		(e1 -> String -> e2 -> e3 -> e4 -> node) -> e1 -> String -> e2 -> e3 -> e4 ->
		Descend context accum result node
	descendNSNNN apply cons e1 str e2 e3 e4 = descend apply cons $
		stepD apply e4 . stepD apply e3 . stepD apply e2 . stringD apply str . stepD apply e1

	-- CN

	descendNCN :: (ParseTreeTraverse e1, ParseTreeTraverse e2) =>
		Apply context accum result ->
		(e1 -> Context Decl -> e2 -> node) -> e1 -> Context Decl -> e2 ->
		Descend context accum result node
	descendNCN apply cons e1 ctx e2 = descend apply cons $
		stepD apply e2 . contextD apply ctx . stepD apply e1

	descendNCNN :: (ParseTreeTraverse e1, ParseTreeTraverse e2, ParseTreeTraverse e3) =>
		Apply context accum result ->
		(e1 -> Context Decl -> e2 -> e3 -> node) -> e1 -> Context Decl -> e2 -> e3 ->
		Descend context accum result node
	descendNCNN apply cons e1 ctx e2 e3 = descend apply cons $
		stepD apply e3 . stepD apply e2 . contextD apply ctx . stepD apply e1

	descendVVCN :: ParseTreeTraverse e => Apply context accum result ->
		(v1 -> v2 -> Context Decl -> e -> node) -> v1 -> v2 -> Context Decl -> e ->
		Descend context accum result node
	descendVVCN apply cons v1 v2 ctx e = descend apply cons $
		stepD apply e . contextD apply ctx . skipD apply v2 . skipD apply v1

	descendVVVCN :: ParseTreeTraverse e => Apply context accum result ->
		(v1 -> v2 -> v3 -> Context Decl -> e -> node) -> v1 -> v2 -> v3 -> Context Decl -> e ->
		Descend context accum result node
	descendVVVCN apply cons v1 v2 v3 ctx e = descend apply cons $
		stepD apply e . contextD apply ctx . skipD apply v3 . skipD apply v2 . skipD apply v1

	instance ParseTreeTraverse Decl where
		pre = preDecl
		mid = midDecl
		post = postDecl

		descend1 apply node = body node where
			body (NoDecl pos) = descendN apply NoDecl pos
			body (PosDecl pos) = descendN apply PosDecl pos
			body (ExprDecl pos expr) = descendNN apply ExprDecl pos expr
			body (ChanExprDecl pos chan) = descendNN apply ChanExprDecl pos chan
			body (ProcDecl pos ctx attrs cmd) = descendNCNN apply ProcDecl pos ctx attrs cmd
			body (ProcAliasDecl pos callable ctx actuals) =
				descendNNVV apply ProcAliasDecl pos callable ctx actuals
			body (FuncDecl pos formals expr) = descendNCN apply FuncDecl pos formals expr
			body (BuiltinFuncDecl pos formals typ) = descendNCN apply BuiltinFuncDecl pos formals typ
			body (VarDecl pos typ) = descendNN apply VarDecl pos typ
			body (ParamDecl pos isParam typ) = descendNNN apply ParamDecl pos isParam typ
			body (TypeParamDecl pos) = descendN apply TypeParamDecl pos
			body (ChanDecl pos typ) = descendNN apply ChanDecl pos typ
			body (PortDecl pos dir typ) = descendNNN apply PortDecl pos dir typ
			body (OpenChanDecl pos ref typ) = descendNNN apply OpenChanDecl pos ref typ
			body (OpenChanDeclE posE ref indices) = descendNNN apply OpenChanDeclE posE ref indices
			body (FlatArrayedDecl pos interval decls) = descendNNN apply FlatArrayedDecl pos interval decls
			body (AliasDecl pos ref typ) = descendNNN apply AliasDecl pos ref typ
			body (SharedDecl pos formals cmd) = descendNCN apply SharedDecl pos formals cmd
			body (TypeDecl pos body) = descendNN apply TypeDecl pos body
			body (ArrayedDecl pos interval decl) = descendNNN apply ArrayedDecl pos interval decl
			body (CaseDecl pos expr gs elseDecl) = descendNNVV apply CaseDecl pos expr gs elseDecl
			body (DeferDecl pos comp decl) = descendNNN apply DeferDecl pos comp decl

		descend2 apply node = body node where
			body (ProcAliasDecl pos callable ctx actuals) =
				descendVVCN apply ProcAliasDecl pos callable ctx actuals
			body (CaseDecl pos expr gs elseDecl) = descendVVNN apply CaseDecl pos expr gs elseDecl
			body other = noDescend apply other

	instance ParseTreeTraverse Interval where
		pre = preInterval
		mid = midInterval
		post = postInterval

		descend1 apply node = body node where
			body (IntervalE pos left right) = descendNNV apply IntervalE pos left right
			body (IntervalOver pos typ) = descendNN apply IntervalOver pos typ
			body (Interval range typ) = descendNN apply Interval range typ

		descend2 apply node = body node where
			body (IntervalE pos left right) = descendVVN apply IntervalE pos left right
			body other = noDescend apply other

	instance ParseTreeTraverse Expr where
		pre = preExpr
		mid = midExpr
		post = postExpr
		final = finalExpr

		descend1 apply node = body node where
			body (NameExpr pos name) = descendNS apply NameExpr pos name
			body (BinExpr pos typ op left right) = descendNNNNV apply BinExpr pos typ op left right
			body (IndexExpr pos expr index) = descendNIV apply IndexExpr pos expr index
			body (SliceExpr pos expr li ri) = descendNIVV apply SliceExpr pos expr li ri
			body (RecElemExpr pos expr elem) = descendNNS apply RecElemExpr pos expr elem
			body (UnExpr pos typ op expr) = descendNNNN apply UnExpr pos typ op expr
			body (CastExpr pos typ expr) = descendNNV apply CastExpr pos typ expr
			body (TypeCheckExpr pos mode typ expr) = descendNNNV apply TypeCheckExpr pos mode typ expr
			body (ConstCheckExpr pos expr) = descendNN apply ConstCheckExpr pos expr
			body (ArrayElemTypeCheckExpr pos typ expr) =
				descendNNV apply ArrayElemTypeCheckExpr pos typ expr
			body (CallExpr pos callable ctx actuals) =
				descendNNVV apply CallExpr pos callable ctx actuals
			body (BuiltinCallExpr pos name params exprs typ) =
				descendNSNNN apply BuiltinCallExpr pos name params exprs typ
			body (ConsExpr pos returnType consType exprs) =
				descendNNNV apply ConsExpr pos returnType consType exprs
			body (AppendExpr pos typ left right) = descendNNNV apply AppendExpr pos typ left right
			body (EnumElemExpr pos expr elem) = descendNNS apply EnumElemExpr pos expr elem
			body (SizeofExpr pos typ) = descendNN apply SizeofExpr pos typ
			body (BitfieldExpr pos typ indices expr) =
				descendNNNN apply BitfieldExpr pos typ indices expr
			body (ExtendExpr pos typ width signed expr) =
				descendNNNNN apply ExtendExpr pos typ width signed expr
			body (CaseExpr pos expr impss exprs) = descendNNNN apply CaseExpr pos expr impss exprs
			body (ValueExpr pos typ value) = descendNNN apply ValueExpr pos typ value
			body (VarRead pos typ range ref) = descendNNNN apply VarRead pos typ range ref
			body (OpenChanRead pos typ range ref) = descendNNNN apply OpenChanRead pos typ range ref
			body (PartialArrayedRead pos ref) = descendNN apply PartialArrayedRead pos ref
			body (MaybeTypeExpr pos ref) = descendNN apply MaybeTypeExpr pos ref
			body (MaybeOtherExpr pos ref) = descendNN apply MaybeOtherExpr pos ref
			body (Expr pos ref) = descendNN apply Expr pos ref

		descend2 apply node = body node where
			body (BinExpr pos typ op left right) = descendVVVVN apply BinExpr pos typ op left right
			body (AppendExpr pos typ left right) = descendVVVN apply AppendExpr pos typ left right
			body (IndexExpr pos expr index) = descendVVN apply IndexExpr pos expr index
			body (SliceExpr pos expr li ri) = descendVVNN apply SliceExpr pos expr li ri
			body (CastExpr pos typ expr) = descendVVN apply CastExpr pos typ expr
			body (TypeCheckExpr pos mode typ expr) = descendVVVN apply TypeCheckExpr pos mode typ expr
			body (ArrayElemTypeCheckExpr pos typ expr) =
				descendVVN apply ArrayElemTypeCheckExpr pos typ expr
			body (ConsExpr pos returnType consType exprs) =
				descendVVVN apply ConsExpr pos returnType consType exprs
			body (CallExpr pos callable ctx actuals) = descendVVCN apply CallExpr pos callable ctx actuals
			body other = noDescend apply other

	instance ParseTreeTraverse CaseMatch where
		pre = preCaseMatch
		post = postCaseMatch

		descend1 apply node = body node where
			body (ExprCaseMatch pos expr) = descendNN apply ExprCaseMatch pos expr
			body (RangeCaseMatch pos left right) = descendNNN apply RangeCaseMatch pos left right
			body (ImpCaseMatches pos imps) = descendNN apply ImpCaseMatches pos imps

	instance ParseTreeTraverse Implicant where
		post = postImplicant

		descend1 apply node = body node where
			body (Imp value dcs) = descendNN apply Imp value dcs

	instance ParseTreeTraverse CaseCmdGuard where
		pre = preCaseCmdGuard
		mid = midCaseCmdGuard
		post = postCaseCmdGuard

		descend1 apply node = body node where
			body (CaseCmdGuard pos matches cmd) = descendNNV apply CaseCmdGuard pos matches cmd
			body (ForCaseCmdGuard pos name matches ctx cmd) =
				descendNSNVV apply ForCaseCmdGuard pos name matches ctx cmd
			body other = noDescend apply other

		descend2 apply node = body node where
			body (CaseCmdGuard pos matches cmd) = descendVVN apply CaseCmdGuard pos matches cmd
			body (ForCaseCmdGuard pos name matches ctx cmd) =
				descendVVVCN apply ForCaseCmdGuard pos name matches ctx cmd
			-- body (ListCaseCmdGuard guards) = descendN apply ListCaseCmdGuard guards
			body (ListCaseCmdGuard guards) = descendN apply ListCaseCmdGuard guards

	instance ParseTreeTraverse CaseDeclGuard where
		pre = preCaseDeclGuard
		mid = midCaseDeclGuard
		post = postCaseDeclGuard

		descend1 apply node = body node where
			body (CaseDeclGuard pos matches decl) = descendNNV apply CaseDeclGuard pos matches decl
			body other = noDescend apply other

		descend2 apply node = body node where
			body (CaseDeclGuard pos matches decl) = descendVVN apply CaseDeclGuard pos matches decl
			body (ListCaseDeclGuard guards) = descendN apply ListCaseDeclGuard guards

	instance ParseTreeTraverse ChanGuard where
		pre = preChanGuard
		mid = midChanGuard
		post = postChanGuard

		descend1 apply node = body node where
			body (ChanGuard pos chans ctx cmd) = descendNNVV apply ChanGuard pos chans ctx cmd

		descend2 apply node = body node where
			body (ChanGuard pos chans ctx cmd) = descendVVCN apply ChanGuard pos chans ctx cmd

	instance ParseTreeTraverse Cmd where
		pre = preCmd
		mid = midCmd
		post = postCmd

		descend1 apply node = body node where
			body (LabelCmd pos label cmd) = descendNSN apply LabelCmd pos label cmd
			body (SeqCmd pos cmds) = descendNN apply SeqCmd pos cmds
			body (ParCmd pos cmds) = descendNN apply ParCmd pos cmds
			body (BlockCmd pos ctx cmd) = descendNCN apply BlockCmd pos ctx cmd
			body (EncInputCmd pos gs) = descendNN apply EncInputCmd pos gs
			body (InputCmd pos chan lvalue) = descendNNV apply InputCmd pos chan lvalue
			body (OutputCmd pos chan expr) = descendNNV apply OutputCmd pos chan expr
			body (SelectCmd pos arb gs) = descendNNN apply SelectCmd pos arb gs
			body (AssignCmd pos lvalue expr) = descendNNV apply AssignCmd pos lvalue expr
			body (SinkCmd pos expr) = descendNN apply SinkCmd pos expr
			body (CaseCmdE pos expr gs elseCmd) = descendNNVV apply CaseCmdE pos expr gs elseCmd
			body (CaseCmd pos expr impss cmds) = descendNNNN apply CaseCmd pos expr impss cmds
			body (CallCmd pos callable ctx actuals) = descendNNVV apply CallCmd pos callable ctx actuals
			body (InstanceCallCmd pos callable chans) =
				descendNNV apply InstanceCallCmd pos callable chans
			body (SharedCallCmd pos callable) = descendNN apply SharedCallCmd pos callable
			body (PrintCmd pos exprs) = descendNN apply PrintCmd pos exprs
			body (ForCmd pos parSeq interval ctx cmd) = descendNNNVV apply ForCmd pos parSeq interval ctx cmd
			body (LoopCmd pos cmd) = descendNN apply LoopCmd pos cmd
			body (WhileCmd pos c1 e c2) = descendNNNN apply WhileCmd pos c1 e c2
			body (DeferCmd comp cmd) = descendNN apply DeferCmd comp cmd
			body other = noDescend apply other

		descend2 apply node = body node where
			body (InputCmd pos chan lvalue) = descendVVN apply InputCmd pos chan lvalue
			body (OutputCmd pos chan expr) = descendVVN apply OutputCmd pos chan expr
			body (AssignCmd pos lvalue expr) = descendVVN apply AssignCmd pos lvalue expr
			body (CaseCmdE pos expr gs elseCmd) = descendVVNN apply CaseCmdE pos expr gs elseCmd
			body (CallCmd pos callable ctx actuals) = descendVVCN apply CallCmd pos callable ctx actuals
			body (InstanceCallCmd pos callable chans) =
				descendVVN apply InstanceCallCmd pos callable chans
			body (ForCmd pos parSeq interval ctx cmd) = descendVVVCN apply ForCmd pos parSeq interval ctx cmd
			body other = noDescend apply other

	instance ParseTreeTraverse Lvalue where
		pre = preLvalue
		mid = midLvalue
		post = postLvalue

		descend1 apply node = body node where
			body (NameLvalue pos name) = descendNS apply NameLvalue pos name
			body (CastLvalue pos typ lvalue) = descendNNV apply CastLvalue pos typ lvalue
			body (TypeCheckLvalue pos mode typ lvalue) =
				descendNNNV apply TypeCheckLvalue pos mode typ lvalue
			body (RecElemLvalue pos lvalue string) = descendNNS apply RecElemLvalue pos lvalue string
			body (SmashLvalue pos lvalue) = descendNN apply SmashLvalue pos lvalue
			body (IndexLvalue pos lvalue index) = descendNNV apply IndexLvalue pos lvalue index
			body (SliceLvalue pos lvalue li ri) = descendNNVV apply SliceLvalue pos lvalue li ri
			body (BitfieldLvalue pos typ indices lvalue) =
				descendNNNN apply BitfieldLvalue pos typ indices lvalue
			body (CaseLvalue pos expr impss lvalues) =
				descendNNNN apply CaseLvalue pos expr impss lvalues
			body (VarWrite pos typ range ref) = descendNNNN apply VarWrite pos typ range ref

		descend2 apply node = body node where
			body (CastLvalue pos typ lvalue) = descendVVN apply CastLvalue pos typ lvalue
			body (TypeCheckLvalue pos mode typ lvalue) =
				descendVVVN apply TypeCheckLvalue pos mode typ lvalue
			body (IndexLvalue pos lvalue index) = descendVVN apply IndexLvalue pos lvalue index
			body (SliceLvalue pos lvalue li ri) = descendVVNN apply SliceLvalue pos lvalue li ri
			body other = noDescend apply other

	instance ParseTreeTraverse Chan where
		pre = preChan
		mid = midChan
		post = postChan

		descend1 apply node = body node where
			body (NameChan pos name) = descendNS apply NameChan pos name
			body (IndexChan pos chan index) = descendNNV apply IndexChan pos chan index
			body (SliceChan pos chan li ri) = descendNNVV apply SliceChan pos chan li ri
			body (PartialArrayedChan pos ref) = descendNN apply PartialArrayedChan pos ref
			body (FlatArrayedChan pos interval chans) = descendNNN apply FlatArrayedChan pos interval chans
			body (CheckChan pos decl chan) = descendNNN apply CheckChan pos decl chan
			body (Chan pos ref) = descendNN apply Chan pos ref

		descend2 apply node = body node where
			body (IndexChan pos chan index) = descendVVN apply IndexChan pos chan index
			body (SliceChan pos chan li ri) = descendVVNN apply SliceChan pos chan li ri
			body other = noDescend apply other

	instance ParseTreeTraverse Type where
		pre = preType
		post = postType

		descend1 apply node = body node where
			body (NameType pos name) = descendNS apply NameType pos name
			body (NumType pos expr signedness) = descendNNN apply NumType pos expr signedness
			body (ArrayType interval typ) = descendNN apply ArrayType interval typ
			body (Type ref) = descendN apply Type ref
			body (Bits int) = descendN apply Bits int
			body (SignedBits int) = descendN apply SignedBits int
			body (BuiltinType name) = descendS apply BuiltinType name
			body (StructRecordType name elems typ) = descendSNN apply StructRecordType name elems typ
			body (StructArrayType interval typ) = descendNN apply StructArrayType interval typ
			body (StructEnumType name elems typ) = descendSNN apply StructEnumType name elems typ
			body other = noDescend apply other

	instance ParseTreeTraverse TypeBody where
		pre = preTypeBody
		post = postTypeBody

		descend1 apply node = body node where
			body (AliasType pos typ) = descendNN apply AliasType pos typ
			body (RecordType pos elems typ) = descendNNN apply RecordType pos elems typ
			body (EnumType pos ctx typ) = descendNCN apply EnumType pos ctx typ

	instance ParseTreeTraverse RecordElem where
		pre = preRecordElem
		post = postRecordElem

		descend1 apply node = body node where
			body (RecordElem pos name typ) = descendNSN apply RecordElem pos name typ

	instance ParseTreeTraverse Attr where
		post = postAttr

		descend1 apply node = body node where
			body (ExprAttr name expr) = descendSN apply ExprAttr name expr

	instance ParseTreeTraverse (Binding Decl) where
		pre = preBinding
		post = postBinding

		descend1 apply node = body node where
			body (Binding index name namespace comp decl) =
				descendNSNNN apply Binding index name namespace comp decl

	instance ParseTreeTraverse SimpleBinding where
		pre = preSimpleBinding
		post = postSimpleBinding

		descend1 apply node = body node where
			body (SimpleBinding name value) =
				descendSN apply SimpleBinding name value

	instance ParseTreeTraverse Callable where
		pre = preCallable
		post = postCallable

		descend1 apply node = body node where
			body (NameCallable pos name) = descendNS apply NameCallable pos name
			body (Callable ref) = descendN apply Callable ref

	instance ParseTreeTraverse FuncActual where
		pre = preFuncActual
		post = postFuncActual

		descend1 apply node = body node where
			body (TypeFuncActual typ) = descendN apply TypeFuncActual typ
			body (ExprFuncActual isParam expr) = descendNN apply ExprFuncActual isParam expr

	instance ParseTreeTraverse ProcActual where
		pre = preProcActual
		post = postProcActual

		descend1 apply node = body node where
			body (TypeProcActual typ) = descendN apply TypeProcActual typ
			body (ExprProcActual expr) = descendN apply ExprProcActual expr
			body (ChanProcActual chan) = descendN apply ChanProcActual chan

	instance ParseTreeTraverse Ref where
		pre = preRef
		post = postRef

		descend1 apply node = body node where
			body (Ref int) = descendN apply Ref int
			body (IndexRef ref int) = descendNN apply IndexRef ref int

	instance ParseTreeTraverse (Context Decl) where
		pre = preContext
		post = postContext

		descend1 apply node (context, accum) = body node where
			body EmptyContext = (context, accum, nullResult apply, EmptyContext)
			body ctx@(Context {}) = (context, accum'', r, ctx')
				where
					r = gatherResult apply $ intervalR : (gatherArray apply context intervalR $ reverse rs)
					(accum', intervalR, _) = traverse apply context accum $ contextVisibleNameRange ctx
					((accum'', rs), ctx') = contextProgressiveFold descendBinding (accum', []) ctx
					descendBinding (a, rs) pCtx binding = ((a', r:rs), binding')
						where (a', r, binding') = traverse apply (applyContextCons apply pCtx context) a binding

	traverseListStep :: ParseTreeTraverse node => Apply context accum result ->
		context -> accum -> node -> (accum, (result, node))
	traverseListStep apply context accum elem = (accum', (result, elem'))
		where (accum', result, elem') = traverse apply context accum elem

	instance ParseTreeTraverse a => ParseTreeTraverse [a] where
		traverse apply context accum list = (accum', gatherResult apply $ gatherList apply context rs, es)
			where
				(accum', rsXes) = mapAccumL (traverseListStep apply context) accum list
				(rs, es) = unzip rsXes

	instance (ParseTreeTraverse i, Ix i, ParseTreeTraverse a) => ParseTreeTraverse (Array i a) where
		traverse apply context accum arr = (accum'',
			gatherResult apply $ gatherArray apply context bsR rs, listArray bs' arr'List)
			where
				(accum', bsR, bs') = traverse apply context accum bs
				(accum'', rsXarrs) = mapAccumL (traverseListStep apply context) accum' (elems arr)
				(rs, arr'List) = unzip rsXarrs
				bs = bounds arr

	instance (ParseTreeTraverse a, ParseTreeTraverse b) => ParseTreeTraverse (a, b) where
		traverse apply context accum (l, r) = (accum'', gatherResult apply $ gatherPair apply rL rR, (l', r'))
			where
				(accum', rL, l') = traverse apply context accum l
				(accum'', rR, r') = traverse apply context accum' r

	instance ParseTreeTraverse a => ParseTreeTraverse (Maybe a) where
		traverse apply _ accum Nothing = (accum, gatherResult apply $ gatherMaybe apply Nothing, Nothing)
		traverse apply context accum (Just e) = (accum', gatherResult apply $ gatherMaybe apply (Just r), Just e')
			where (accum', r, e') = traverse apply context accum e

	instance ParseTreeTraverse Pos where
		post = postPos

	instance ParseTreeTraverse Completeness where
		post = postCompleteness

	instance ParseTreeTraverse Namespace where
		post = postNamespace

	instance ParseTreeTraverse Direction where
		post = postDirection

	instance ParseTreeTraverse ParSeq where
		post = postParSeq

	instance ParseTreeTraverse CheckMode where
		post = postCheckMode

	instance ParseTreeTraverse Value where
		post = postValue

		descend1 apply node = body node where
			body (IntValue int) = descendN apply IntValue int
			body (ImpValue imp) = descendN apply ImpValue imp
			body (StringValue str) = descendS apply StringValue str
			body DontCareValue = noDescend apply DontCareValue

	instance ParseTreeTraverse Integer where
		post = postInteger

	instance ParseTreeTraverse Int where
		post = postInt

	instance ParseTreeTraverse Bool where
		post = postBool

	instance ParseTreeTraverse Sense where
		post = postSense

	instance ParseTreeTraverse ImportPath where
		post = postImportPath

	instance ParseTreeTraverse (Slice Int) where
		post = postSliceInt

	instance ParseTreeTraverse BinOp where
		post = postBinOp

	instance ParseTreeTraverse UnOp where
		post = postUnOp
