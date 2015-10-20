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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TraverseBase (
	emptyApply,
	preNoAccum,
	Apply (..),
	ApplyPreFunc,
	ApplyPostFunc,
	Visit (..),
	showATraverse,
	stepD,

	traverse
	) where

	import Array
	import Maybe
	import Data.List

	type ApplyPreFunc context accum node = context -> accum -> node -> (context, accum, node)
	type ApplyPostFunc context result node = context -> (result, node) -> (result, node)
	type Descend context accum result node = (context, accum) -> (context, accum, result, node)
	type Cons context accum result t node = (context, accum, result, t -> node) -> (context, accum, result, node)

	preNoAccum :: (context -> node -> (context, node)) -> context -> accum -> node -> (context, accum, node)
	preNoAccum f context accum node = (context', accum, node')
		where (context', node') = f context node

	data Apply contextNode context accum result = Apply {
		-- useContextPreCsInEncNode : use the modified cs returned by preContext as the context for the
		--	rest of the node containing the Context.  This is useful for modifying the context not only
		--	for the Context's Bindings but also for the rest of any other node's elements which contains
		--	a Context binding values.
		useContextPreCsInEncNode :: Bool,
		applyContextCons :: contextNode -> context -> context,
		nullResult :: result,
		-- combineResult : combine result elements from nodes.  The second argument is always the element to add
		--	where the function is not commutative
		combineResult :: result -> result -> result,
		gatherList :: context -> [result] -> [result],
		gatherArray :: context -> result -> [result] -> [result],
		gatherPair :: result -> result -> [result],
		gatherMaybe :: Maybe result -> [result]
		}

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

	emptyApply :: Apply contextNode context accum result
	emptyApply = Apply {
		useContextPreCsInEncNode = False,
		applyContextCons = defaultContextCons,
		nullResult = undefined,
		combineResult = const,
		gatherList = defaultGatherList,
		gatherArray = defaultGatherArray,
		gatherPair = (\l r -> [l, r]),
		gatherMaybe = maybeToList
		}

	traverse :: Visit context accum result nodeFuncs node => Apply contextNode context accum result -> nodeFuncs ->
		context -> accum -> node -> (accum, result, node)
	traverse apply funcs contextIn accumIn node = (accum, finalResult, finalNode) where
		(accum, innerResult, innerNode) = innerTraverse apply funcs contextIn accumIn node
		(finalResult, finalNode) = final apply funcs contextIn (innerResult, innerNode)

	innerTraverse :: Visit context accum result nodeFuncs node => Apply contextNode context accum result ->
		nodeFuncs -> context -> accum -> node -> (accum, result, node)
	innerTraverse apply funcs context accum node = snd $ contextInnerTraverse apply funcs context accum node

	contextInnerTraverse :: Visit context accum result nodeFuncs node =>
		Apply contextNode context accum result -> nodeFuncs ->
		context -> accum -> node -> (context, (accum, result, node))
	contextInnerTraverse apply funcs contextIn accumIn node = (context, (descend2Accum, retResult, retNode)) where
		(context, preAccum, preNode) = pre apply funcs contextIn accumIn node
		(_, descend1Accum, descend1Result, descend1Node) = descend1 apply funcs preNode (context, preAccum)
		(midResult, midNode) = mid apply funcs context (descend1Result, descend1Node)
		(_, descend2Accum, descend2Result, descend2Node) = descend2 apply funcs midNode (context, descend1Accum)
		depthResult = combineResult apply midResult descend2Result
		(retResult, retNode) = post apply funcs context (depthResult, descend2Node)

	class {- Traverse node => -} Visit context accum result nodeFuncs node where
		pre :: Apply contextNode context accum result -> nodeFuncs -> ApplyPreFunc context accum node
		pre _ _ = preId

		mid :: Apply contextNode context accum result -> nodeFuncs -> ApplyPostFunc context result node
		mid _ _ = postId

		post :: Apply contextNode context accum result -> nodeFuncs -> ApplyPostFunc context result node
		post _ _ = postId
		
		final :: Apply contextNode context accum result -> nodeFuncs -> ApplyPostFunc context result node
		final _ _ = postId

		descend1 :: Apply contextNode context accum result -> nodeFuncs -> node -> Descend context accum result node
		descend1 apply _funcs node = noDescend apply node

		descend2 :: Apply contextNode context accum result -> nodeFuncs -> node -> Descend context accum result node
		descend2 apply _funcs node = noDescend apply node

	gatherResult :: Apply contextNode context accum result -> [result] -> result
	gatherResult apply results = foldl' (combineResult apply) (nullResult apply) results

	skipD :: v -> Cons context accum result v node
	skipD v (context, accum, rs, cons) = (context, accum, rs, cons v)

	descend :: Apply contextNode context accum result -> cons ->
		((context, accum, result, cons) -> (context, accum, result, node)) ->
		Descend context accum result node
	descend apply cons f (context, accum) = f (context, accum, nullResult apply, cons)

	noDescend :: Apply contextNode context accum result -> node ->
		Descend context accum result node
	noDescend apply node = descend apply node id

	stepD :: Visit context accum result nodeFuncs e =>
		Apply contextNode context accum result -> nodeFuncs -> e -> Cons context accum result e node
	stepD apply funcs e (context, accum, rs, cons) = (context, accum', combineResult apply rs r1, cons e')
		where (accum', r1, e') = traverse apply funcs context accum e

	stringD :: Visit context accum result nodeFuncs String =>
		Apply contextNode context accum result -> nodeFuncs -> String -> Cons context accum result String node
	stringD apply funcs str (context, accum, rs, cons) = (context, accum, combineResult apply rs r1, cons str')
		where (r1, str') = post apply funcs context (nullResult apply, str)

	innerD :: Visit context accum result nodeFuncs e =>
		Apply contextNode context accum result -> nodeFuncs -> e -> Cons context accum result e node
	innerD apply funcs e (context, accum, rs, cons) = (context, accum', combineResult apply rs r1, cons e')
		where (accum', r1, e') = innerTraverse apply funcs context accum e

	contextD :: Visit context accum result nodeFuncs contextNode =>
		Apply contextNode context accum result -> nodeFuncs -> contextNode ->
		Cons context accum result contextNode node
	contextD apply funcs ctx (context, accum, rs, cons) = (applyContextCons apply ctx' retContext,
		accum', combineResult apply rs r1, cons ctx')
		where
			(context', (accum', r1, ctx')) = contextInnerTraverse apply funcs context accum ctx
			retContext
				| useContextPreCsInEncNode apply = context'
				| otherwise = context

	data A = A Int
		| B String
		| C Int Int

	descendN :: Visit context accum result nodeFuncs e =>
		Apply contextNode context accum result -> nodeFuncs ->
		(e -> node) -> e ->
		Descend context accum result node
	descendN apply funcs cons e = descend apply cons $ stepD apply funcs e

	descendS :: Visit context accum result nodeFuncs String =>
		Apply contextNode context accum result -> nodeFuncs ->
		(String -> node) -> String ->
		Descend context accum result node
	descendS apply funcs cons s = descend apply cons $ stringD apply funcs s

	descendNN :: (Visit context accum result nodeFuncs e1, Visit context accum result nodeFuncs e2) =>
		Apply contextNode context accum result -> nodeFuncs ->
		(e1 -> e2 -> node) -> e1 -> e2 ->
		Descend context accum result node
	descendNN apply funcs cons e1 e2 = descend apply cons $
		stepD apply funcs e2 . stepD apply funcs e1

	data AFuncs context result = AFuncs {
		postInt :: ApplyPostFunc context result Int,
		postString :: ApplyPostFunc context result String,
		postA :: ApplyPostFunc context result A
		}

	instance Visit context accum result (AFuncs context result) Int where
		post _ f = postInt f

	instance Visit context accum result (AFuncs context result) String where
		post _ f = postString f

	instance Visit context accum result (AFuncs context result) A where
		post _ f = postA f

		descend1 apply funcs node = case node of
			A i -> descendN apply funcs A i
			B s -> descendS apply funcs B s
			C i j -> descendNN apply funcs C i j

	showA :: AFuncs () String
	showA = AFuncs {
		postInt = \context (_, node) -> (show node, node),
		postString = \context (_, node) -> (node, node),
		postA = \context (result, node) -> case (result, node) of
			(i, node@(A {})) -> ("A " ++ i, node)
			(s, node@(B {})) -> ("B " ++ s, node)
			(ij, node@(C {})) -> ("C " ++ ij, node)
		}

	showATraverse :: Visit () () String (AFuncs () String) node => node -> String
	showATraverse node = result
		where
			(_, result, _) = traverse
				(emptyApply { nullResult = "", combineResult = \l r -> l ++ " " ++ r }) showA () () node
