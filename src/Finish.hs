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

module Finish (
    finish
    ) where

    import Prelude hiding (traverse)
    import ParseTree
    import Traverse
    import Context
    import Type
    import Report

    import Data.List
    import Data.Array
    import Data.Maybe

    {-
        Remove:

        x Aliases
        x ExprDecls
        x ChanExprDecls
        x FuncDecls
        x TypeDecls
        x BuiltinFuncDecls
        x Arrayed everything -- FlatArrayed, ArrayedDecl, IndexRef
        x Typing - reduce to just Bits n and SignedBits n
        o Intermediate node types:
            ...

        o Migrate declarations upwards
        o Disabiguate names
        x Drop Poss

        o Remove substitutions from Contexts in:
            x BlockCmd
            o ProcDecl
            ...
    -}

    data FinishContext = FinishContext
        [Context Decl] -- finishContexts
        Int -- finishNextRef
        [(Ref, Ref)] -- finishRefSubsts

    emptyFinishContext :: FinishContext
    emptyFinishContext = FinishContext [] 1 []

    finish :: ParseTreeTraverse node => node -> node
    finish c = c'
        where
            (_, _, typeStrippedC) = traverse typeStripApply [] () c
            (_, _, c') = traverse finishApply emptyFinishContext () typeStrippedC

    finishApply :: Apply FinishContext () ()
    finishApply = emptyApply {
        useContextPreCsInEncNode = True,
        applyContextCons = contextCons,
        preRef = preNoAccum finishPreRef,
        preContext = preNoAccum finishPreContext,
        preCmd = preNoAccum finishPreCmd,
        postCmd = finishPostCmd,
        preBinding = preNoAccum finishPreBinding,
        preExpr = preNoAccum finishPreExpr }
        where contextCons c (FinishContext cs nr substs) = FinishContext (c:cs) nr substs

    type FinishPost node = ApplyPostFunc FinishContext () node
    type FinishPre node = FinishContext -> node -> (FinishContext, node)

    typeStripApply :: Apply [Context Decl] () ()
    typeStripApply = emptyApply {
        applyContextCons = (:),
        postType = typeStripPostType
        }

    typeStripPostType :: ApplyPostFunc [Context Decl] () Type
    typeStripPostType cs (_, typ)
        | isStructType unaliasedType = ((), unaliasedType)
        | hasBuiltins = ((), typeToStructType cs unaliasedType)
        | typeIsSigned cs typ = ((), SignedBits width)
        | otherwise = ((), Bits width)
        where
            width = widthOfType cs unaliasedType
            unaliasedType = unaliasType cs typ
            hasBuiltins = not $ null $ typeBuiltinOffsets cs 0 unaliasedType

    finishPreCmd :: FinishPre Cmd
    finishPreCmd cs cmd = (cs, cmd)

    finishPostCmd :: FinishPost Cmd
    finishPostCmd _ (_, InstanceCallCmd pos callable chans) = ((), InstanceCallCmd pos callable chans')
        where
            chans' = flattenChans chans

            flattenChans chans = concatMap flattenChan chans

            flattenChan chan@(Chan {}) = [chan]
            flattenChan (FlatArrayedChan _ _ chans) = flattenChans chans
            -- flattenChan (PartialArrayedChan _ chans) = chans
            flattenChan chan = error $ show chan
    finishPostCmd _ (_, cmd) = ((), cmd)

    -- FIXME, signedness lost.  This *really* should be an operator property, not a type one
    finishPreExpr :: FinishPre Expr
    finishPreExpr cs expr = (cs, expr)

    lookupRefSubst :: FinishContext -> Ref -> Maybe Ref
    lookupRefSubst cs ref = maybeRef'
        where
            FinishContext _ _ refSubsts = cs
            maybeRef' = lookup ref refSubsts

    -- FIXME, fail on no match
    finishPreRef :: FinishPre Ref
    finishPreRef cs ref
        | isJust maybeNewRef = (cs, fromJust maybeNewRef)
        | otherwise = (cs, ref)
        where maybeNewRef = lookupRefSubst cs ref

    finishPreBinding :: FinishPre (Binding Decl)
    finishPreBinding cs binding = (cs, binding)

    finishPreContext :: FinishPre (Context Decl)
    finishPreContext cs ctx = (cs', ctx')
        where
            bindings = contextBindingsList ctx

            FinishContext ctxs nextIndex csSubsts = cs
            cs' = FinishContext ctxs nextIndex' (unaliasedLocals ++ csSubsts)

            ctx' = (bindingsToContext (nextIndex, nextIndex' - 1) (reverse bindings'))

            unaliasSubst (from, to) = (from, unaliasRef (ctx':ctxs) to)
            unaliasedLocals = map unaliasSubst localSubsts

            (nextIndex', bindings', localSubsts) = foldl' (reworkBinding cs) (nextIndex, [], [])
                $ zip (map (Ref . bindingIndex) bindings) bindings

    reworkBinding :: FinishContext -> (Int, [Binding Decl], [(Ref, Ref)]) -> (Ref, Binding Decl) ->
        (Int, [Binding Decl], [(Ref, Ref)])
    reworkBinding cs (nextIndex, bindings, localSubsts) (ref, binding) = body decl
        where
            decl = bindingValue binding
            completeness = bindingCompleteness binding

            body (ProcDecl {})
                | completeness /= Complete = drop
            body (ArrayedDecl _ interval decl) = makeArrayElemBindings (intervalRefs interval) (repeat decl)
            body (FlatArrayedDecl _ interval array) = makeArrayElemBindings (intervalRefs interval) (elems array)
            body (BuiltinFuncDecl {}) = drop
            body (FuncDecl {}) = drop
            body (ExprDecl {}) = drop
            body (ChanExprDecl {}) = drop
            body (TypeDecl {}) = drop
            body (AliasDecl _ origRef _) = alias origRef
            body _ = insert

            intervalRefs interval = map (IndexRef ref) $ intervalIndices interval
            renamedBinding = binding { bindingIndex = 0, bindingName = bindingName binding ++ refToNameSuffix "" ref }

            refToNameSuffix suffix (Ref {}) = suffix
            refToNameSuffix suffix (IndexRef ref index) = refToNameSuffix ("[" ++ show index ++ "]" ++ suffix) ref

            insert = (nextIndex + 1, renamedBinding:bindings, (ref, Ref nextIndex):localSubsts)
            drop = (nextIndex, bindings, localSubsts)
            alias toRef = (nextIndex, bindings, (ref, toRef'):localSubsts)
                where toRef' = fromJust $ lookupRefSubst cs toRef

            makeArrayElemBindings indexRefs decls = foldl' makeBinding
                (nextIndex, bindings, localSubsts) $ zip indexRefs decls
                where makeBinding nbs (ref, decl) = reworkBinding cs nbs
                        (ref, binding { bindingIndex = 0, bindingValue = decl })
