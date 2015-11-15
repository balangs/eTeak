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

module Rule (
	CompPattern (..),
	CompReplacement (..),
	CompReplacements (..),
	LeadsToType,
	LinkLeadsTo,
	LinkParam (..),
	LinkWithUsage,
	ListPattern (..),
	NewLinkCons (..),
	Rule (..),
	RuleDefns,
	RuleFromMatch,
	RuleGroup (..),
	RuleMatch,
	RulePatternLink,
	RulePatternLinks,
	RuleRemaps,
	RuleSet (..),
	RuleToReplacement,
	RuleValue (..),
	TSRuleExpr,
	TSRuleVals,
	TSRuleValue,
	applyRule,
	concatRuleSets,
	emptyRuleSet,
	nwApplyMatchReplacement,
	parseRules,
	ruleParser,
	ruleToOptim,
	rulesTestCard,
	showMatchSummary,
	summariseRuleSet
	) where

	import NetParts
	import Parser
	import Misc
	import Report
	import ParseTree
	import Show
	import Optim
	import TeakScript
	import Bits
	import Layout
	import Plot
	import Network -- FIXME
	-- import Graphics.Rendering.Cairo
	-- import Dot

	import Control.Monad
	import Data.Maybe
	import Data.List
	import qualified Data.Map as DM
	import Data.Array (listArray, (!))
	import Control.Monad.ST
	import Control.Monad.ST.Unsafe
	import System.FilePath
	import System.IO

	data CompPattern = CompPattern {
		compPatternName :: String,
		compPatternTeakCompType :: Maybe TeakCompType,
		compPatternIsInstance :: Bool,
		compPatternTryAllComps :: Bool,
		compPatternTest :: (NetworkComp -> Bool),
		compPatternPorts :: [NetworkPortInfo], -- only for Teak components
		compPatternPortPatterns :: [Some ListPattern],
		compPermutations :: [String] }

	data {- NetworkIF network => -} CompReplacement network =
		  CompReplacement {
			compReplacementName :: String,
			compReplacementCompType :: String,
			compReplacementParams :: TSUpdates (RuleValue network),
			compReplacementPortPatterns :: [Some ListPattern] }
		deriving Show

	data {- NetworkIF network => -} CompReplacements network =
		  CompSingleReplacement {
		  	compReplacementComp :: CompReplacement network }
		| CompForeachReplacement {
			compReplacementsIterators :: [(String, TSRuleExpr network)],
			compReplacementComp :: CompReplacement network,
			compReplacementsWhere :: [(String, TSRuleExpr network)] }
		deriving Show

	data {- NetworkIF network => -} Rule network = Rule {
		ruleName :: String,
		ruleDescription :: String,
		ruleOnByDefault :: Bool,
		ruleFrom :: [CompPattern],
		ruleFromLeadsTos :: [LinkLeadsTo],
		ruleFromPatternLinks :: RulePatternLinks,
		ruleWhen :: TSRuleExpr network,
		ruleDebugWhen :: Maybe (TSRuleExpr network),
		ruleNewLinks :: [(String, NewLinkCons network)],
		ruleAliases :: [(String, String)],
		ruleDefns :: [(String, TSRuleExpr network)],
		ruleTo :: [CompReplacements network],
		ruleGlobalVals :: TSRuleVals network }

	data {- NetworkIF network => -} NewLinkCons network = NewLinkCons { newLinkUpdates :: TSUpdates (RuleValue network) }
		| NewLinksCons {
			newLinksIterators :: [(String, TSRuleExpr network)],
			newLinkUpdates :: TSUpdates (RuleValue network) }

	data LeadsToType = DiamondLeadsTo | SingleSuccessorLeadsTo | RequiredLeadsTo

	type LinkLeadsTo = (LeadsToType, String, String, [String])
	type LinkWithUsage = (LinkParam, NetworkLinkUsage)
	type RulePatternLink = (String, Some LinkWithUsage)
	type RulePatternLinks = [RulePatternLink]

	data LinkParam = LinkParam {
		paramLink :: NetworkLinkRef,
		fOffset :: Maybe Int,
		sImps :: Maybe [Implicant],
		sOffset :: Maybe Int,
		wOffset :: Maybe Int,
		rOffset :: Maybe Int }
		-- deriving Show

	instance Eq LinkParam where
		a == b = paramLink a == paramLink b

	instance Ord LinkParam where
		a `compare` b = paramLink a `compare` paramLink b

	noParamLink :: NetworkLinkRef -> LinkParam
	noParamLink link = LinkParam link Nothing Nothing Nothing Nothing Nothing

	data RuleGroup = RuleGroup {
		ruleGroupName :: String,
		ruleGroupDescription :: String,
		ruleGroupMembers :: [String] }
		deriving Show

	emptyRuleSet :: NetworkIF network => RuleSet network
	emptyRuleSet = RuleSet [] []

	data {- NetworkIF network => -} RuleSet network = RuleSet {
		ruleSetRules :: [Rule network],
		ruleSetGroups :: [RuleGroup] }

	type RuleFromMatch = ([(String, NetworkComp)], [(String, Some LinkParam)])
	type RuleDefns network = (TSRuleVals network, [NetworkLink])
	type RuleToReplacement = [(String, NetworkComp)]
	type RuleRemaps = [(NetworkLinkRef, NetworkLinkRef)]

	type RuleMatch network = (RuleFromMatch, RuleDefns network, RuleToReplacement, RuleRemaps)

	data ListPattern =
		  EllipsisPattern Bool String
		| SinglePattern Bool String
		deriving Show

	listPatternName :: ListPattern -> String
	listPatternName (EllipsisPattern _ name) = name
	listPatternName (SinglePattern _ name) = name

	data {- NetworkIF network => -} RuleValue network =
		  RuleComp NetworkComp
		| RuleLink LinkParam
		| RuleOSlice TeakOSlice
		| RuleSlice (Slice Int)
		| RuleOTerm Int TeakOTerm
		| RulePart (Part network)
		| RuleContext (OptimContext network)
		| RulePos Pos

	type TSRuleValue network = TSValue (RuleValue network)
	type TSRuleExpr network = TSExpr (RuleValue network)
	type TSRuleVals network = [(String, TSRuleValue network)]

	isRuleLink :: NetworkIF network => TSRuleValue network -> Bool
	isRuleLink (TSVRecord (RuleLink _)) = True
	isRuleLink _ = False

	isRuleOSlice :: NetworkIF network => TSRuleValue network -> Bool
	isRuleOSlice (TSVRecord (RuleOSlice {})) = True
	isRuleOSlice _ = False

	isRuleOTerm :: NetworkIF network => TSRuleValue network -> Bool
	isRuleOTerm (TSVRecord (RuleOTerm {})) = True
	isRuleOTerm _ = False

	partValName :: String
	partValName = "_"

	contextValName :: String
	contextValName = "_ctx"

	extractElems :: NetworkIF network => [(String, TSRuleValue network)] -> RuleValue network -> [String] ->
		[(String, TSRuleValue network)]
	extractElems vals ruleValue names = map (\name -> (name, fromJust $ tsExtractElem vals ruleValue name)) names

	findLinkWidth :: NetworkIF network => [(String, TSRuleValue network)] -> LinkParam -> Maybe Int
	findLinkWidth vals link
		| nwLink (paramLink link) < 0 = Nothing
		| otherwise = Just $ tryPart part $ nwGetLinkWidth $ paramLink link
		where Just (TSVRecord (RulePart part)) = lookup partValName vals

	instance NetworkIF network => TSVRecordClass (RuleValue network) where
		tsExtractElem vals (RuleComp comp) "part"
			| isInstanceComp comp = Just $ TSVRecord $ RulePart part
			where
				Just part = nwFindPart (optimContextParts context) (nwPartName comp)
				Just (TSVRecord (RuleContext context)) = lookup contextValName vals
		tsExtractElem _ (RuleComp comp) elemName = lookup elemName $ extractCompParams comp
		tsExtractElem vals (RuleLink link) elemName = lookup elemName $ extractLinkParams (findLinkWidth vals) link
		tsExtractElem _ (RuleOSlice (index, _)) "index" = Just $ intToTSVInt $ index
		tsExtractElem _ (RuleOSlice (_, slice)) "slice" = Just $ TSVRecord $ RuleSlice slice
		tsExtractElem _ (RuleSlice slice) "offset" = Just $ intToTSVInt $ sliceOffset slice
		tsExtractElem _ (RuleSlice slice) "width" = Just $ intToTSVInt $ sliceWidth slice
		tsExtractElem _ (RuleOTerm index _) "index" = Just $ intToTSVInt $ index
		tsExtractElem _ (RuleOTerm _ term) "slices" = Just $ TSVList $ map (TSVRecord . RuleOSlice) $
			oTermExtractSlices term
		tsExtractElem _ _ _ = Nothing

		tsExtractElems _ (RuleComp comp) = extractCompParams comp
		tsExtractElems vals (RuleLink link) = extractLinkParams (findLinkWidth vals) link
		tsExtractElems vals slice@(RuleOSlice {}) = extractElems vals slice ["index", "slice"]
		tsExtractElems vals slice@(RuleSlice {}) = extractElems vals slice ["offset", "width"]
		tsExtractElems vals term@(RuleOTerm {}) = extractElems vals term ["index", "slices"]
		tsExtractElems _ _ = []

		tsInsertElem _ (RuleLink l) name value = maybe Nothing (Just . RuleLink) $ insertLinkParam l name value
		tsInsertElem _ (RuleOSlice (_, slice)) "index" (TSVInt index) =
			Just $ RuleOSlice (fromInteger index, slice)
		tsInsertElem _ (RuleOSlice (index, _)) "slice" (TSVRecord (RuleSlice slice)) = Just $ RuleOSlice (index, slice)
		tsInsertElem _ (RuleOTerm _ term) "index" (TSVInt index) = Just $ RuleOTerm (fromInteger index) term
		tsInsertElem _ (RuleOTerm index term) "slices" (TSVList slices)
			| all isRuleOSlice slices = Just $ RuleOTerm index $ oTermInsertSlices term $
				map (\(TSVRecord (RuleOSlice slice)) -> slice) slices
		tsInsertElem _ _ _ _ = Nothing

		tsRecordCompare (RuleComp l) (RuleComp r) = nwCompIndex l `compare` nwCompIndex r
		tsRecordCompare (RuleLink l) (RuleLink r) = paramLink l `compare` paramLink r
		tsRecordCompare (RuleOSlice s1) (RuleOSlice s2) = s1 `compare` s2
		tsRecordCompare (RuleSlice s1) (RuleSlice s2) = s1 `compare` s2
		tsRecordCompare (RuleOTerm i1 t1) (RuleOTerm i2 t2) = (i1 `compare` i2) `compare` (t1 `compare` t2)
		tsRecordCompare (RulePos p1) (RulePos p2) = p1 `compare` p2
		tsRecordCompare _ _  = LT

		tsShowRecord (RuleLink l) = shows l
		tsShowRecord (RuleComp l) = shows l
		tsShowRecord (RulePart part) = showString "(part " . showString (networkName part) . showChar ')'
		tsShowRecord (RuleOSlice slice) = shows slice
		tsShowRecord (RuleSlice slice) = shows slice
		tsShowRecord (RuleOTerm index term) = showChar '(' . shows index . showChar ',' . shows term . showChar ')'
		tsShowRecord (RuleContext {}) = showString "!context!"
		tsShowRecord (RulePos pos) = showString $ showPos noPosContext pos

		tsRecordSameClass (RuleLink {}) (RuleLink {}) = True
		tsRecordSameClass (RuleComp {}) (RuleComp {}) = True
		tsRecordSameClass (RulePart {}) (RulePart {}) = True
		tsRecordSameClass (RuleOSlice {}) (RuleOSlice {}) = True
		tsRecordSameClass (RuleSlice {}) (RuleSlice {}) = True
		tsRecordSameClass (RuleOTerm {}) (RuleOTerm {}) = True
		tsRecordSameClass (RulePos {}) (RulePos {}) = True
		tsRecordSameClass _ _ = False

	{-
		compName ::= `A' | `F' | `J' | `L' | `M' | `O' | `S' | `V' | `I' | `R'

		linkName ::= tsName

		singleLinkPattern ::= `_'
			| linkName

		linkPatternElem ::= singleLinkPattern
			| ( label )? `...'

		linkPattern ::= ( linkPatternElem `,' )* linkPatternElem

		portConn ::= singleLinkPattern
			| `[' linkPattern `]'

		portConnList ::= e
			| ( portConn `,' )* portConn

		compPattern ::= ( label )? compName `(' portConnList `)'
	-}

	singleLinkPattern :: TSParser ListPattern
	singleLinkPattern = anyOneOfStr "single port connection" [
		tsName >>= return . SinglePattern True,
		tsKeyword "_" >> return (SinglePattern False "") ]

	linkPatternElem :: TSParser ListPattern
	linkPatternElem = anyOneOfStr "component port connection" [
		singleLinkPattern,
		do
			(userNamed, l) <- optional (False, "") (tsLabel >>= (\l -> return (True, l)))
			tsKeyword "..." >> return (EllipsisPattern userNamed l) ]

	linkPattern :: TSParser [ListPattern]
	linkPattern = sepList (tsKeyword ",") linkPatternElem

	portConn :: TSParser (Some ListPattern)
	portConn = anyOneOfStr "component port connection" [
		singleLinkPattern >>= return . One,
		do
			tsKeyword "["
			es <- linkPattern
			tsKeyword "]"
			return $ Many es ]

	portConnList :: TSParser [Some ListPattern]
	portConnList = sepList (tsKeyword ",") portConn

	adjustCompPatternForPorts :: CompPattern -> [NetworkPortInfo] -> Maybe CompPattern
	adjustCompPatternForPorts pattern ports = do
		-- teakType <- nwCompShortNameToSampleTeakType $ compPatternCompType pattern
		let
			-- ports = teakCompPortInfo $ nwTeakCompInfo teakType
			patternPorts = compPatternPortPatterns pattern
		when (length ports /= length patternPorts) $ fail ""

		patternPorts' <- zipWithM adjustPort (map networkPortIsArrayed ports) patternPorts

		return $ pattern { compPatternPortPatterns = patternPorts' }
		where
			-- make One/Many of component ports affect Single/Ellipsis in pattern
			adjustPort True (One (SinglePattern named name)) = Just $ One $ EllipsisPattern named name
			adjustPort False (One (EllipsisPattern named name)) = Just $ One $ SinglePattern named name
			adjustPort False (Many _) = Nothing
			adjustPort _ patternPort = Just patternPort

	linkLeadsToParser :: String -> TSParser LinkLeadsTo
	linkLeadsToParser _enclosingRuleName = do
		from <- tsName
		typ <- anyOneOfStr "leads to operator" [
			tsKeyword "->" >> return SingleSuccessorLeadsTo,
			tsKeyword "-|->" >> return RequiredLeadsTo,
			tsKeyword "-<>->" >> return DiamondLeadsTo
			]
		to <- tsName
		excluding <- optional [] $ do
			tsKeyword "excluding"
			tsKeyword "("
			links <- optional [] $ sepList (tsKeyword ",") tsName
			tsKeyword ")"
			return links
		return (typ, from, to, excluding)

	compPatternParser :: String -> TSParser CompPattern
	compPatternParser enclosingRuleName = do
		n <- tsLabel
		(compIsInstance, c) <- anyOneOfStr "comp type" [
			do
				c <- tsName
				return (False, c),
			do
				tsKeyword "instance"
				return (True, "") ]
		tsKeyword "("
		pcs <- portConnList
		tsKeyword ")"
		perms <- optional [] $ do
			tsKeyword "permute"
			optional [] $ do
				tsKeyword "("
				ps <- sepList (tsKeyword ",") tsName
				tsKeyword ")"
				return ps
		exhaustive <- optional False $ do
			tsKeyword "exhaustive"
			return True

		let
			(_, pcs') = mapAccumL (numberAnonPortConn n) 1 pcs
			errorPos = PosLabel PosTopLevel $ "parsing component pattern `" ++ n ++ "' of rule `"
				++ enclosingRuleName ++ "'"

		pattern <- if compIsInstance
			then do
				let
					test comp = isInstanceComp comp &&
						length (nwCompLinks comp) == length pcs'
				return $ Just $ CompPattern n Nothing True exhaustive test [] pcs' perms
			else do
				let
					maybeTest = nwCompShortNameToTest c
					Just test = maybeTest
					sample = nwCompShortNameToSampleTeakType c
					ports = teakCompPortInfo $ nwTeakCompInfo $ fromJust sample
					pattern = adjustCompPatternForPorts
						(CompPattern n sample False exhaustive test ports pcs' perms) ports

				if isNothing maybeTest
					then parserFail $ Report errorPos $
						"rule " ++ enclosingRuleName ++ ": bad component name `" ++ c ++ "'"
					else return pattern

		if isNothing pattern
			then parserFail (Report errorPos "bad component pattern")
			else return $ fromJust pattern

	parseOptionalList :: TSParser a -> TSParser [a]
	parseOptionalList parser = anyOneOfStr "a list" [
		do
			tsKeyword "("
			ret <- sepList (tsKeyword ",") parser
			tsKeyword ")"
			return ret,
		liftM (:[]) parser
		]

	compReplacementParser :: NetworkIF network => TSParser (CompReplacements network)
	compReplacementParser = anyOneOfStr "replacement component" [
		liftM CompSingleReplacement compReplacement,
		foreachCompReplacement ]
		where
			foreachCompReplacement = do
				tsKeyword "foreach"
				iters <- parseOptionalList tsName
				tsKeyword "in"
				overLists <- parseOptionalList tsExprParser
				when (length iters /= length overLists) $ parserFail
					$ Report NoPos $ "foreach iterators and value lists must be the same length `("
						++ showListWithSep (showString ", ") showString iters "" ++ ") in ("
						++ showListWithSep (showString ", ") (\_ -> showChar '_') overLists ""
						++ ")'"
				comp <- compReplacement
				whereBindings <- optional [] $ do
					tsKeyword "where"
					tsSemiList $ tsName >>= tsFuncBindingParser
				return $ CompForeachReplacement (zip iters overLists) comp whereBindings

			compReplacement = do
				do
					n <- tsLabel
					c <- tsName
					ps <- optional [] tsUpdatesParser
					tsKeyword "("
					pcs <- portConnList
					tsKeyword ")"

					let (_, pcs') = mapAccumL (numberAnonPortConn n) 1 pcs

					-- FIXME, check port structure of replacement
					return $ CompReplacement n c ps pcs'

	semiList :: TSParser a -> TSParser [a]
	semiList parser = do
		tsKeyword "{"
		ret <- optional [] $ sepList (tsKeyword ";") parser
		tsKeyword "}"
		return ret

	onOrOff :: String -> TSParser Bool
	onOrOff enclosingRuleName = do
		expr <- tsExprParser
		let
			Why comp ret = boolEval expr

			errorPos = PosLabel PosTopLevel $ "parsing rule default expression `" ++ show expr ++ "' of rule `"
				++ enclosingRuleName ++ "'"

			boolEval expr = do
				value <- tsvToWhy $ tsEvalExpr [] (extraMiscVals :: [(String, TSValue TSNoRecord)]) expr
				mustBeBool value
				let TSVBool bool = value
				return bool
				where
					mustBeBool (TSVBool _) = return ()
					mustBeBool value = failPos errorPos $ "must yield a boolean result, got `" ++ show value ++ "'"
		case comp of
			Complete -> return ret
			Wrong (report:_) -> parserFail report
			_ -> parserFail $ Report errorPos "other error"

	ruleParser :: NetworkIF network => TSParser (Rule network)
	ruleParser = do
		tsKeyword "rule"
		name <- tsName
		desc <- optional ("optimisation " ++ name) tsString
		onByDefault <- optional True $ do
			tsKeyword "default"
			onOrOff name
		tsKeyword "from"
		let
			unzipConcat lrs = (concat ls, concat rs)
				where (ls, rs) = unzip lrs

		(froms, leadsTos) <- liftM unzipConcat $ semiList (anyOneOfStr "from component or leads-to link" [
			do
				comp <- compPatternParser name
				return ([comp], []),
			do
				leadsTo <- linkLeadsToParser name
				return ([], [leadsTo])
			])
		whenCond <- optional (TSValue (TSVBool True)) $ do
			tsKeyword "when"
			tsExprParser
		tos <- optional [] $ do
			tsKeyword "to"
			semiList compReplacementParser
		aliases <- optional [] $ do
			tsKeyword "alias"
			rep $ do
				oldLink <- tsName
				tsKeyword "=>"
				newLink <- tsName
				return (oldLink, newLink)
		debugCond <- optional Nothing $ do
			tsKeyword "debug"
			expr <- tsExprParser
			return $ Just expr
		(newLinks, defns) <- optional ([], []) $ do
			tsKeyword "where"
			(newLinkss, defnss) <- liftM unzip $ semiList $ do
				bName <- tsName
				anyOneOfStr "binding right hand side" [
					do
						tsKeyword "="
						anyOneOfStr "`link {...' or expression" [
							tsExprParser >>= \l -> return ([], [(bName, l)]),
							do
								tsKeyword "newlink"
								updates <- tsUpdatesParser
								return ([(bName, NewLinkCons updates)], []),
							do
								tsKeyword "foreach"
								iters <- parseOptionalList tsName
								tsKeyword "in"
								overLists <- parseOptionalList tsExprParser
								tsKeyword "newlink"
								updates <- tsUpdatesParser
								return ([(bName, NewLinksCons (zip iters overLists) updates)], [])
							],
					tsFuncBindingParser bName >>= \b -> return ([], [b]) ]
			return (concat newLinkss, concat defnss)
		return $ Rule name desc onByDefault froms leadsTos
			(makeFromPatternLinks name froms) whenCond debugCond newLinks aliases defns tos []

	rulePatternLinksCompleteCount :: RulePatternLinks -> Int
	rulePatternLinksCompleteCount patterns = sum $ map linkCount patterns
		where
			linkCount (_, links) = sum $ map oneIfComplete $ map snd $ flattenSome links

			oneIfComplete usage
				| isNoConnUsage usage = 0
				| otherwise = 1

			isNoConnUsage (NetworkLinkUsage pas act) = act == NoConn || pas == NoConn

	showsRulePatternLink :: [CompPattern] -> RulePatternLink -> [ShowS]
	showsRulePatternLink comps (varName, usages) = case usages of
		One usage -> [showString varName . showString " = " . showUsage usage]
		Many subUsages -> zipWith (\i elem ->
			showString varName . showChar '[' . shows i . showString "] = " . showUsage elem) [(0::Int)..] subUsages
		_ -> [id]
		where
			showUsage (link, NetworkLinkUsage pas act) = (if isUnknownLinkParam link
				then id
				else showLinkParam [] link . showString ": ") .
				showConn act . showString " -> " . showConn pas

			showConn NoConn = showString "u/c"
			showConn (LinkComp (Comp compRef) addr)
				| compRef < 0 = showString (compPatternName (comps !! ((- compRef) - 1))) . showAddr addr
				| otherwise = showChar 'C' . shows compRef . showAddr addr
			showConn (LinkAccess accessRef addr) = showString "access " . shows accessRef . showAddr addr

			showAddr addr = shows addr

	makeFromPatternLinks :: String -> [CompPattern] -> RulePatternLinks
	makeFromPatternLinks ruleName froms = fromPatternLinks
		where
			merge = mergePatternLinksList (\_ _ _ _ -> True) []

			fromPatternLinks = fromMaybe [] $ do
				firstCompLinks:restCompLinks <- zipWithM extractCompLinks [-1,-2..] froms
				-- merge $ firstCompLinks:restCompLinks
				return $ foldl' stepPattern firstCompLinks $ zip (tail froms) restCompLinks
				where
					stepPattern acc (fromCompPattern, fromLinks)
						| isNothing next = error "Error in from pattern links"
						| not (compPatternTryAllComps fromCompPattern) && accCompleteCount == nextCompleteCount =
							error $ "in rule `" ++ ruleName ++ "' no progress, from " ++ show accCompleteCount ++
							" to " ++ show nextCompleteCount ++ " complete links" ++
							"\nacc:\n  " ++ showLinks acc ++
							"\nfrom component pattern `" ++ compPatternName fromCompPattern ++
								"':\n  " ++ showLinks fromLinks ++
							"\nTry using the `exhaustive' flag after thinking hard about it"

						| otherwise = fromJust next
						where
							showLinks links = showListWithSep (showString "\n  ") id
								(concatMap (showsRulePatternLink froms) links) ""

							accCompleteCount = rulePatternLinksCompleteCount acc
							nextCompleteCount = rulePatternLinksCompleteCount $ fromJust next
							next = mergePatternLinks (\_ _ _ _ -> True) acc fromLinks

			makeFromPatternLink compNo portNo sense pattern = case pattern of
				SinglePattern True pName -> Just (pName, One link)
				EllipsisPattern True pName -> Just (pName, Many [link])
				_ -> Nothing
				where link = (unknownLinkParam, makeUsage sense (LinkComp (Comp compNo) [portNo]))

			makeUsage Passive conn = NetworkLinkUsage conn NoConn
			makeUsage Active conn = NetworkLinkUsage NoConn conn

			extractCompLinks compNo from
				| compPatternIsInstance from = Just []
				| otherwise = do
					-- teakType <- nwCompShortNameToSampleTeakType $ compPatternCompType from
					let
						-- sampleComp = TeakComp 0 teakType [] NoPos
						-- senses = nwCompPortSenses sampleComp
						senses = map networkPortSense $ compPatternPorts from
					merge $ zipWith3 extractSome [0..] senses (compPatternPortPatterns from)
					where
						extractSome portNo sense someListPattern =
							sortAssocList $ mapMaybe (makeFromPatternLink compNo portNo sense) $
								flattenSome someListPattern

	ruleGroupParser :: TSParser RuleGroup
	ruleGroupParser = do
		tsKeyword "group"
		name <- tsName
		desc <- optional ("group " ++ name) tsString
		tsKeyword "("
		members <- optional [] $ sepList (tsKeyword ",") (anyOneOfStr "group member name" [tsName, tsString])
		tsKeyword ")"
		return $ RuleGroup name desc members

	ruleKeywords :: [String]
	ruleKeywords = ["alias", "default", "rule", "when", "newlink", "permute", "group", "instance",
		"exhaustive", "debug", "foreach", "in", "excluding"]

	ruleLayoutKeywords :: [String]
	ruleLayoutKeywords = ["from", "to", "where"]

	ruleSymbols :: [String]
	ruleSymbols = ["=>", "->", "-<>->", "-|->"]

	concatRuleSets :: NetworkIF network => [RuleSet network] -> RuleSet network
	concatRuleSets sets = RuleSet (concatMap (ruleSetRules) sets) (concatMap ruleSetGroups sets)

	parseRules :: NetworkIF network => Pos -> String -> Why (RuleSet network)
	parseRules pos = parseWithTSLexer ruleKeywords ruleLayoutKeywords ruleSymbols pos emptyRuleSet parser
		where
			insertGlobalVals vals rule = rule { ruleGlobalVals = vals }

			parser = do
				(valss, ruleSets) <- liftM unzip $ semiList elemParser
				let
					ruleSet = concatRuleSets ruleSets
					vals = mapSnd (tsEvalExpr [] vals) (concat valss) ++ globalVals
				return $ ruleSet { ruleSetRules = map (insertGlobalVals vals) $ ruleSetRules ruleSet }

			elemParser = anyOneOfStr "rule" [
				do
					r <- ruleParser
					return ([], RuleSet [r] []),
				do
					g <- ruleGroupParser
					return ([], RuleSet [] [g]),
				do
					tsKeyword "let"
					vals <- semiList $ tsName >>= tsFuncBindingParser
					return (vals, RuleSet [] []) ]

	summariseRuleSet :: NetworkIF network => RuleSet network -> ShowS
	summariseRuleSet ruleSet = shows (length rules) .
		showString " optimisation rules (" .
		showListWithSep (showString ",") (showString . ruleName) rules .
		showString ") and " . shows (length groups) . showChar ' ' . showString (plural "group" groups) .
			showString " (" .  showListWithSep (showString ",") (showString . ruleGroupName) groups . showString ")"
		where
			rules = ruleSetRules ruleSet
			groups = ruleSetGroups ruleSet

	-- numberAnonPortConn : number anonymous connections (_ or ...) with `compName/num' with
	--	accending `num' starting from i
	numberAnonPortConn :: String -> Int -> Some ListPattern -> (Int, Some ListPattern)
	numberAnonPortConn compName i (One conn) = (i', One conn')
		where (i', conn') = numberAnonLinkPattern compName i conn
	numberAnonPortConn compName i (Many conns) = (i', Many conns')
		where (i', conns') = mapAccumL (numberAnonLinkPattern compName) i conns
	numberAnonPortConn _ i conn = (i, conn)

	numberAnonLinkPattern :: String -> Int -> ListPattern -> (Int, ListPattern)
	numberAnonLinkPattern compName i (EllipsisPattern userNamed "") =
		(i + 1, EllipsisPattern userNamed (anonRename compName i))
	numberAnonLinkPattern compName i (SinglePattern userNamed "") =
		(i + 1, SinglePattern userNamed (anonRename compName i))
	numberAnonLinkPattern _ i pattern = (i, pattern)

	anonRename :: String -> Int -> String
	anonRename name i = name ++ "/" ++ show i

	-- matchList : find all matches of list to the given matches. Returns a list of (name, sublist of list) for
	--	each element of matches in sequence.
	matchList :: [ListPattern] -> [a] -> [[(String, Some a)]]
	matchList patterns list = map reverse $ body patterns [] list
		where
			body [] ret [] = [ret]
			body [EllipsisPattern _ name] ret ls = [(name, Many ls):ret]
			body ((SinglePattern _ name):ms) ret (l:ls) = body ms ((name, One l):ret) ls
			body ((EllipsisPattern _ name):ms) ret ls = concatMap bodySuffix $ partitions ls
				where bodySuffix (selection, rest) = body ms ((name, Many selection):ret) rest
			body _ _ _ = []

	sortAssocList :: [(String, a)] -> [(String, a)]
	sortAssocList = sortBy compareFst

	matchSome :: Some ListPattern -> Some a -> [[(String, Some a)]]
	matchSome pattern some = map sortAssocList $ body pattern some
		where
			body (One (SinglePattern _ name)) (One e) = [[(name, One e)]]
			body (One (EllipsisPattern _ name)) (Many es) = [[(name, Many es)]]
			body (Many patterns) (Many es) = matchList patterns es
			body _ _ = []

	mergeEqLinkWithUsages :: Some LinkWithUsage -> Some LinkWithUsage -> Some LinkWithUsage
	mergeEqLinkWithUsages l r = do body l r
		where
			body (One link1) (One link2)
				{- AB
				| isUnknownLinkWithUsages [link1] = One link2
				| isUnknownLinkWithUsages [link2] = One link1
				-}
				| otherwise = One $ addUsage link1 link2
			body (Many links1) (Many links2)
				{- AB
				-- Preserve (Many []) nature of `test' usages
				| isUnknownLinkWithUsages links1 = Many links2
				| isUnknownLinkWithUsages links2 = Many links1
				-}
				| test1 && test2 = Many [addUsage (head links1) (head links2)]
				| test1 = Many $ zipWith addUsage (subPortTestLinks (head links1)) links2
				| test2 = Many $ zipWith addUsage links1 (subPortTestLinks (head links2))
				| otherwise = Many $ zipWith addUsage links1 links2
				where
					subPortTestLinks testLink = map (\subPort -> addSubPort subPort testLink) [0..]
					
					addSubPort subPort (l, NetworkLinkUsage pas act) = (l, NetworkLinkUsage pas' act')
						where
							pas' = addConnSubPort pas
							act' = addConnSubPort act

							addConnSubPort (LinkComp comp port) = LinkComp comp (port ++ [subPort])
							addConnSubPort conn = conn

					test1 = isUnknownLinkWithUsages links1
					test2 = isUnknownLinkWithUsages links2
			body _ _ = error "mergeEqLinkWithUsages: unmatched links"

			addUsage (link1, usage1) (link2, usage2) =
				(newLink, addUsagePairUnsafe usage1 usage2)
				where
					newLink
						| isUnknownLinkParam link1 = link2
						| isUnknownLinkParam link2 = link1
						| otherwise = mergeLinkParam link1 link2

	unknownLinkRef :: NetworkLinkRef
	unknownLinkRef = Link 0

	isUnknownLinkParam :: LinkParam -> Bool
	isUnknownLinkParam lp = paramLink lp == unknownLinkRef

	isUnknownLinkWithUsages :: [LinkWithUsage] -> Bool
	isUnknownLinkWithUsages ((lp, _):_) = isUnknownLinkParam lp
	isUnknownLinkWithUsages _ = False

	unknownLinkParam :: LinkParam
	unknownLinkParam = noParamLink unknownLinkRef

	isPatternComp :: NetworkCompRef -> Bool
	isPatternComp (Comp i) = i < 0

	mergePatternLinks ::
		(LinkParam -> Sense -> NetworkCompRef -> [Int] -> Bool) ->
		RulePatternLinks -> RulePatternLinks ->
		Maybe RulePatternLinks
	mergePatternLinks otherEndOK = maybeMergeByWith compareFst
		(\(_, links1) (_, links2) -> eqLinks links1 links2)
		(\(name, links1) (_, links2) -> (name, mergeEqLinkWithUsages links1 links2))
		where
			eqLinks (One link1) (One link2) = compatableLink link1 link2
			eqLinks (Many links1) (Many links2)
				| isUnknownLinkWithUsages links1 = and $ zipWith compatableLink (repeat (head links1)) links2
				| isUnknownLinkWithUsages links2 = and $ zipWith compatableLink links1 (repeat (head links2))
				| otherwise = length links1 == length links2 && and (zipWith compatableLink links1 links2)
			eqLinks _ _ = False

			compatableLink l1@(link1, NetworkLinkUsage pas1 act1) l2@(link2, NetworkLinkUsage pas2 act2) = ret
				where
					ret = compatableUsage && (link1 == link2 || otherIsOK)

					otherIsOK = case (isUnknownLinkParam link1, isUnknownLinkParam link2) of
						(False, False) -> False -- must be same link
						(False, True) -> tryOtherSense l1 l2
						(True, False) -> tryOtherSense l2 l1
						(True, True) -> True -- both are tests

					tryOtherSense (linkK, NetworkLinkUsage pasK actK) (_, NetworkLinkUsage pasT actT) =
						tryOther Active pasK actT && tryOther Passive actK pasT
							where
								tryOther _ NoConn _ = True -- connected the other way around
								tryOther senseT _ (LinkComp compT addrT)
									| isPatternComp compT = otherEndOK linkK senseT compT addrT
								tryOther _ _ _ = True

					compatableUsage = compatableConn pas1 pas2 && compatableConn act1 act2
					
			compatableConn conn _ | incompleteConn conn = True
			compatableConn _ conn | incompleteConn conn = True
			compatableConn (LinkComp comp1 (addr1:_)) (LinkComp comp2 (addr2:_)) =
				comp1 == comp2 && addr1 == addr2
			compatableConn _ _ = error "compatableConn: unrecognised conns"

	mergePatternLinksList ::
		(LinkParam -> Sense -> NetworkCompRef -> [Int] -> Bool) ->
		RulePatternLinks ->
		[RulePatternLinks] ->
		Maybe RulePatternLinks
	mergePatternLinksList otherEndOK testPatternLinks patternLinkss =
		foldM (mergePatternLinks otherEndOK) testPatternLinks patternLinkss

	matchSomeLinks :: NetworkComp ->
		(LinkParam -> Sense -> NetworkCompRef -> [Int] -> Bool) ->
		RulePatternLinks ->
		[Some ListPattern] -> [Sense] -> [Some LinkParam] ->
		[String] ->
		[RulePatternLinks]
	matchSomeLinks comp otherEndOK testPatternLinks patterns senses links perms = matches
		where
			-- FIXME, do perms on individual matches?
			matches = mapMaybe (mergePatternLinks otherEndOK testPatternLinks) permedCombinations

			permedCombinations
				| null perms = allMatchCombinations
				| otherwise = concatMap makePermPatternLinks allMatchCombinations

			allMatchCombinations = mapMaybe (mergePatternLinksList otherEndOK []) $
				crossLists $ zipWith matchSome patterns decoratedLinks

			decoratedLinks = zipWith3 decorateLink [0..] senses links
			decorateLink i sense link = fmap (addSense i sense) link

			addSense i Passive link = (link, NetworkLinkUsage (LinkComp (refComp comp) [i]) NoConn)
			addSense i Active link = (link, NetworkLinkUsage NoConn (LinkComp (refComp comp) [i]))

			makePermPatternLinks patternLinks = map makePerm $ permute permBodys
				where
					makePerm perm = sortAssocList $ zip permNames perm ++ otherLinks

					(permNames, permBodys) = unzip permLinks
					(permLinks, otherLinks) = partition ((`elem` perms) . fst) patternLinks

	incompleteUsages :: NetworkLinkUsage -> [Sense]
	incompleteUsages (NetworkLinkUsage pas act) =
		(if incompleteConn pas then [Passive] else []) ++
		(if incompleteConn act then [Active] else [])
	-- incompleteUsages _ = []

	incompleteConn :: NetworkLinkConn -> Bool
	incompleteConn NoConn = True
	incompleteConn (LinkComp comp _) = isPatternComp comp
	incompleteConn _ = False

	-- matchCompPattern : test a [Some ListPattern] pattern list against the ports of the given component
	matchCompPattern :: NetworkIF network => Rule network -> Part network -> NetworkCompRef -> [RuleFromMatch]
	matchCompPattern rule part tryCompRef
		| isJust tryComp = tryMatch (zip [1..] patterns) [] (ruleFromPatternLinks rule) $ fromJust tryComp
		| otherwise = []
		where
			patterns = ruleFrom rule
			tryComp = findComp tryCompRef
			compNames = map compPatternName patterns

			compTests = listArray (1, length patterns) $ map compPatternTest patterns

			otherEndOK linkK senseT compT addrT = isJust $ do
				(otherEndComp, (otherEndPort:_)) <- findUsage senseT linkK
				let testComp = compTests ! (- (nwComp compT))
				when (not ([otherEndPort] == addrT && testComp otherEndComp)) $ fail ""

			match visited patternLinks = [(zip compNames (reverse visited), mapSnd dropUsage patternLinks)]
				where
					-- dropUsage (name, ls) = (name, map fst ls)
					dropUsage = fmap fst

			compSubstPatternLinks from to patternLinks = mapSnd (fmap substLink) patternLinks
				where
					substLink (linkParam, NetworkLinkUsage pas act) =
						(linkParam, NetworkLinkUsage (substConn pas) (substConn act))
						where
							substConn (LinkComp from1 addr)
								| from1 == from = LinkComp to addr
							substConn conn = conn

			tryMatch [] visited patternLinks _ = match visited patternLinks
			-- FIXME.  Exhaustive?
			tryMatch ((patternNo, CompPattern _ _ _compIsInstance _exhaustive compTest _ compPattern perms):ps)
				visited patternLinks comp
				| compTest comp = concatMap (tryNextComp ps visited') matches
				| otherwise = []
				where
					senses = nwCompPortSenses comp
					links = compParamLinks comp

					testPatternLinks = compSubstPatternLinks (Comp (- patternNo)) (refComp comp) patternLinks

					matches = matchSomeLinks comp otherEndOK testPatternLinks compPattern senses links perms

					visited' = comp : visited

			{- AB
			newComps patternLinks = nub $ mapMaybe (\(link, usage) -> do
				sense <- incompleteUsage usage
				liftM fst $ findUsage sense link) $ concatMap (flattenSome . snd) patternLinks
				-}
			newComps pattern patternLinks
				| compPatternTryAllComps pattern = allComps $ compPatternTest pattern
				| otherwise = nub $ concatMap tryLink $ concatMap (flattenSome . snd) patternLinks
				where
					tryLink (link, usage) = map fst $ mapMaybe (\sense -> findUsage sense link) $
						incompleteUsages usage

			tryNextComp [] visited patternLinks = match visited patternLinks
			tryNextComp ps@((_, p):_) visited patternLinks = concatMap (tryMatch ps visited patternLinks) comps
				where comps = newComps p patternLinks \\ visited

			findComp compRef = tryPart part $ nwGetComp compRef

			findUsage sense link = tryPart part $ nwLinkToComp sense $ paramLink link

			allComps test = tryPart part $ nwMapCompsIf (return . test) return

	extractLinkParams :: NetworkIF network => (LinkParam -> Maybe Int) -> LinkParam -> [(String, TSRuleValue network)]
	extractLinkParams getWidth link = mapMaybe ($ link) [
		extractLinkParam "fOffset" fOffset (TSVInt . toInteger),
		extractLinkParam "sImps" sImps (TSVList . map TSVImp),
		extractLinkParam "sOffset" sOffset (TSVInt . toInteger),
		extractLinkParam "wOffset" wOffset (TSVInt . toInteger),
		extractLinkParam "rOffset" rOffset (TSVInt . toInteger),
		\l -> do
			width <- getWidth l
			return ("width", TSVInt $ toInteger width)
		]
		where
			extractLinkParam name get convert l = do
				v <- get l
				return (name, convert v)

	insertLinkParam :: NetworkIF network => LinkParam -> String -> TSRuleValue network -> Maybe LinkParam
	insertLinkParam link "fOffset" (TSVInt o) = Just $ link { fOffset = Just (fromInteger o) }
	insertLinkParam link "sImps" (TSVList ls)
		| all isTSVImp ls = Just $ link { sImps = Just (map (\(TSVImp i) -> i) ls) }
	insertLinkParam link "sOffset" (TSVInt o) = Just $ link { sOffset = Just (fromInteger o) }
	insertLinkParam link "wOffset" (TSVInt o) = Just $ link { wOffset = Just (fromInteger o) }
	insertLinkParam link "rOffset" (TSVInt o) = Just $ link { rOffset = Just (fromInteger o) }
	insertLinkParam _ _ _ = Nothing

	extractCompParams :: NetworkIF network => NetworkComp -> TSRuleVals network
	extractCompParams comp@(TeakComp { nwTeakType = typ }) = posParam : teak typ
		where
			posParam = ("pos", TSVRecord $ RulePos $ nwCompPos comp)

			teak (TeakV name width bOffsets _ _) =
				[("bOffsets", TSVList $ map (TSVInt . toInteger) bOffsets),
				 ("name", TSVString name),
				 ("width", intToTSVInt width)]
			teak (TeakO terms) = [("terms", TSVList $ map (\(i, term) -> TSVRecord $ RuleOTerm i term) terms)]
			teak (TeakS slice _) = [("slice", TSVRecord $ RuleSlice slice)]
			teak _ = []
	extractCompParams _ = []

	insertCompParams :: NetworkIF network => NetworkComp -> TSRuleVals network -> Why NetworkComp
	insertCompParams comp params = case comp of
		TeakComp {} -> foldM teak comp params
		_ -> paramError
		where
			teak comp@(TeakComp { nwTeakType = TeakV _ width bOffsets wOffsets rOffsets })
				("name", TSVString name')
				= return $ comp { nwTeakType = TeakV name' width bOffsets wOffsets rOffsets }
			teak comp@(TeakComp { nwTeakType = TeakV name _ bOffsets wOffsets rOffsets })
				("width", TSVInt width')
				= return $ comp { nwTeakType = TeakV name (fromInteger width') bOffsets wOffsets rOffsets }
			teak comp@(TeakComp { nwTeakType = TeakV name width _ wOffsets rOffsets })
				("bOffsets", TSVList bOffsets') | all isTSVInt bOffsets'
				= return $ comp { nwTeakType = TeakV name width (map fromTSVInt bOffsets') wOffsets rOffsets }
			teak comp@(TeakComp { nwTeakType = TeakO {} })
				("terms", TSVList terms) | all isRuleOTerm terms
				= return $ comp { nwTeakType = TeakO $ map (\(TSVRecord (RuleOTerm i term)) -> (i, term)) terms }
			teak comp@(TeakComp { nwTeakType = TeakS _ matches })
				("slice", TSVRecord (RuleSlice slice)) = return $ comp { nwTeakType = TeakS slice matches }
			teak comp ("pos", TSVRecord (RulePos pos)) = return $ comp { nwCompPos = pos }
			teak _ _ = paramError

			paramError = fail $ "insertCompParams: can't insert parameters `" ++ show params ++
				"' into component `" ++ show comp ++ "'"

	-- Put this stuff in NetParts?

	instance Show LinkParam where
		showsPrec _ = showLinkParam []

	showLinkParam :: [(String, String)] -> LinkParam -> ShowS
	showLinkParam otherParams lp@(LinkParam { paramLink = Link link }) = showChar 'L' . shows link .
		(if null params then id else showString "{" . showListWithSep (showString ", ") id params . showString "}")
		where
			getParam name getter = do
				value <- getter lp
				return (name, show value)

			showRawParam (name, value) = showString name . showString " = " . showString value

			params = map showRawParam $ otherParams ++ catMaybes [
				getParam "fOffset" fOffset,
				getParam "sImps" sImps,
				getParam "sOffset" sOffset,
				getParam "wOffset" wOffset,
				getParam "rOffset" rOffset ]

	mergeLinkParam :: LinkParam -> LinkParam -> LinkParam
	mergeLinkParam linkParam1 linkParam2
		| paramLink linkParam1 /= paramLink linkParam2 = cantMerge
		| otherwise = LinkParam (paramLink linkParam1)
			(merge fOffset)
			(merge sImps) (merge sOffset)
			(merge wOffset) (merge rOffset)
		where
			merge getter = case (isJust param1, isJust param2) of
				(False, False) -> Nothing
				(True, False) -> param1
				(False, True) -> param2
				(True, True) -> cantMerge
				where
					param1 = getter linkParam1
					param2 = getter linkParam2

			cantMerge = errorFixme $ "mergeLinkParam: can't merge " ++ show linkParam1 ++ " and " ++ show linkParam2

	compParamLinks :: NetworkComp -> [Some LinkParam]
	compParamLinks (TeakComp { nwTeakType = TeakF offsets, nwCompLinks = [One inp, Many outs] }) =
		[One (noParamLink inp), Many $ zipWith addParam outs offsets]
		where addParam link offset = (noParamLink link) { fOffset = Just offset }
	-- TeakO
	compParamLinks (TeakComp { nwTeakType = TeakS _ spec, nwCompLinks = [One inp, Many outs] }) =
		[One (noParamLink inp), Many outs']
		where
			outs' = zipWith (\out (imps, offset) ->
				(noParamLink out) { sImps = Just imps, sOffset = Just offset }) outs spec
	compParamLinks (TeakComp { nwTeakType = TeakV _ _ _ wOffsets rOffsets,
		nwCompLinks = [Many wgs, Many wds, Many rgs, Many rds] }) =
		[Many wgs', Many (map noParamLink wds), Many (map noParamLink rgs), Many rds']
		where
			wgs' = zipWith (\wg offset -> (noParamLink wg) { wOffset = Just offset }) wgs wOffsets
			rds' = zipWith (\rd offset -> (noParamLink rd) { rOffset = Just offset }) rds rOffsets
	compParamLinks comp = map (fmap noParamLink) $ nwCompLinks comp

	insertMany :: [Bool] -> [[b]] -> Maybe [Some b]
	insertMany isManys lists = sequence $ zipWith insertElem isManys lists
		where
			insertElem False [l] = Just (One l)
			insertElem True ls = Just (Many ls)
			insertElem _ _ = Nothing

	compInsertParamLinks :: NetworkComp -> [[LinkParam]] -> Why NetworkComp
	compInsertParamLinks comp linkss = maybeToWhy (fail errorMsg) $ do
		comp' <- body comp
		let ports = teakCompPortInfo $ nwTeakCompInfo $ nwTeakType comp
		someLinks <- insertMany (map networkPortIsArrayed ports) (map (map paramLink) linkss)
		return $ comp' { nwCompLinks = someLinks }
		where
			errorMsg = "compInsertParamLinks: can't insert links `" ++ show linkss ++
				"' into component `" ++ show comp ++ "'"

			body (TeakComp { nwTeakType = typ }) = teak typ linkss
			body _ = Nothing

			teak (TeakF _) [_, outs] = do
				offsets' <- mapM fOffset outs
				return $ comp { nwTeakType = TeakF offsets' }
			teak (TeakS slice _) [_, outs] = do
				spec <- mapM makeSpecElem outs
				return $ comp { nwTeakType = TeakS slice spec }
				where
					makeSpecElem out = do
						s <- sImps out
						o <- sOffset out
						return (s, o)
			teak (TeakV name width bOffsets _ _) [wgs, _, _, rds] = do
				wOffsets <- mapM wOffset wgs
				rOffsets <- mapM rOffset rds
				return $ comp { nwTeakType = TeakV name width bOffsets wOffsets rOffsets }
				where
			teak _ _ = Just comp

	tsvLookupLinks :: NetworkIF network => [(String, TSRuleValue network)] -> String -> Why [LinkParam]
	tsvLookupLinks vals name = do
		val <- maybeToWhy (fail ("can't find link(s) `" ++ name ++ "'")) $ lookup name vals
		val' <- tsvToWhy val
		let badLinks = fail $ "not a link or list of links: `" ++ show val' ++ "'"
		case val' of
			TSVRecord (RuleLink link) -> return [link]
			TSVList links -> do
				links' <- mapM tsvToWhy links
				if all isRuleLink links'
					then return $ map (\(TSVRecord (RuleLink l)) -> l) links
					else badLinks
			_ -> badLinks

	ruleEval :: NetworkIF network => OptimContext network -> Part network -> Rule network -> TSRuleVals network ->
		TSRuleExpr network -> TSRuleValue network
	ruleEval context part rule localVals = tsEvalExpr [(partValName, TSVRecord (RulePart part)),
		(contextValName, TSVRecord (RuleContext context))]
		(localVals ++ ruleGlobalVals rule)

	extraMiscVals :: TSVRecordClass record => [(String, TSValue record)]
	extraMiscVals = [("off", TSVBool False)] ++ tsMiscVals

	leadsTo :: NetworkIF network => LeadsToType -> NetworkLinkRef -> NetworkLinkRef -> [NetworkLinkRef] ->
		NetworkMonad network Bool
	leadsTo _ from to exclude
		| from == to && from `notElem` exclude && to `notElem` exclude = return True
	leadsTo DiamondLeadsTo from to exclude = do
		nexts <- loop exclude [from]
		return $ to `elem` (nexts \\ exclude)
		{-
		prevs <- prevLinks exclude to
		return $ intersect (from:nexts) (to:prevs) \\ exclude /= []
		-}
		where
			loop visited froms = do
				nexts <- liftM (nub . concat) $ mapM (nextLinks True visited) froms
				mergeOuts <- nextMLinks nexts
				if null mergeOuts
					then return nexts
					else loop nexts mergeOuts

	leadsTo SingleSuccessorLeadsTo from to exclude = do
		wSuccessors <- nextLinks False exclude from
		return $ to `elem` (wSuccessors \\ exclude)
	leadsTo RequiredLeadsTo from to exclude = do
		wSuccessors <- nextLinks True exclude from
		return $ to `elem` (wSuccessors \\ exclude)

	globalVals :: NetworkIF network => TSRuleVals network
	globalVals = [
		("newOSlice", TSVFunc newOSliceF),
		("sliceFromOW", TSVFunc sliceFromOWF),
		("oAppend", TSVRecord $ RuleOTerm 0 $ TeakOAppend 1 []),
		("oConstant", TSVFunc oConstantF),
		("imp", TSVFunc impF),
		("oMux", TSVFunc oMuxF),
		("oTermWidth", TSVFunc oTermWidthF),
		("shortenOTerms", TSVFunc shortenOTermsF),
		("partPortAliases", TSVFunc partPortAliasesF),
		-- ("leadsTo", TSVFunc leadsToF),
		("zip", TSVFunc zipF),
		("listElem", TSVFunc listElemF),
		("checkOTerms", TSVFunc checkOTermsF),
		("isBuiltinCall", TSVFunc isBuiltinCallF),
		("posList", TSVFunc posListF)
		] ++ extraMiscVals
		where
			newOSliceF _ (TSVInt index)
				| index >= 0 = TSVFunc $ newOSliceF1 $ fromInteger index
			newOSliceF _ arg = tsBadArg "newOSlice" "a natural number" arg

			newOSliceF1 index _ (TSVRecord (RuleSlice slice)) = TSVRecord (RuleOSlice (index, slice))
			newOSliceF1 _ _ arg = tsBadArg "newOSlice _" "a slice" arg

			sliceFromOWF _ (TSVInt offset)
				| offset >= 0 = TSVFunc $ sliceFromOWF1 offset
			sliceFromOWF _ arg = tsBadArg "sliceFromOW" "a natural number" arg

			sliceFromOWF1 offset _ (TSVInt width)
				| width == 0 = TSVRecord $ RuleSlice emptySlice
				| width >= 0 = TSVRecord $ RuleSlice $ (fromInteger offset) +: (fromInteger width)
			sliceFromOWF1 _ _ arg = tsBadArg "sliceFromOW _" "a natural number" arg

			oConstantF _ (TSVInt width) = TSVFunc $ oConstantF1 width 
			oConstantF _ arg = tsBadArg "oConstant" "a natural number" arg

			oConstantF1 width _ (TSVInt value) = TSVRecord $ RuleOTerm 0 $
				TeakOConstant (fromInteger width) (fromInteger value)
			oConstantF1 _ _ arg = tsBadArg "oConstant _" "an integer" arg

			impF _ (TSVInt val ) = TSVFunc $ impF1 val
			impF _ arg = tsBadArg "imp" "a natural number" arg

			impF1 val _ (TSVInt dcs) = TSVImp $ Imp val dcs
			impF1 _ _ arg = tsBadArg "imp _" "a natural number" arg

			oTermWidthF _ (TSVRecord (RuleOTerm _ term)) = intToTSVInt $ oTermResultWidth term
			oTermWidthF _ arg = tsBadArg "oTermWidth" "an OTerm" arg

			shortenOTermsF _ (TSVList terms)
				| all isRuleOTerm terms = TSVList $ map toOTerm $ shortenOTerms $ map fromOTerm terms
			shortenOTermsF _ arg = tsBadArg "shortenOTerms" "a list of OTerms" arg

			oMuxF _ (TSVList impss)
				| all (isTSVListOf isTSVImp) impss = TSVRecord $ RuleOTerm 0 $ TeakOMux spec []
					where spec = map (map fromTSVImp . fromTSVList) impss
			oMuxF _ arg = tsBadArg "oMux" "a list of lists of Imps" arg

			fromOTerm (TSVRecord (RuleOTerm i term)) = (i, term)
			fromOTerm arg = error $ "fromOTerm: not an OTerm `" ++ show arg ++ "'"
			toOTerm (i, term) = (TSVRecord (RuleOTerm i term))

			partPortAliasesF _ (TSVRecord (RulePart part)) =
				TSVList $ map (TSVList . map (intToTSVInt . fst)) refGroups
				where
					ports = zip ([0..] :: [Int]) $ networkPorts part
					refGroups = filter (listAtLeastLength 2) $ groupBy eqRef $ sortBy compareRef ports

					eqRef (_, port1) (_, port2) = portLinks port1 == portLinks port2
					compareRef (_, port1) (_, port2) = portLinks port1 `compare` portLinks port2

					portLinks port = fromMaybe [] $ do
						ref <- nwPortRef port
						return $ tryPart part $ nwGetPortAccess ref
			partPortAliasesF _ arg = tsBadArg "oTermWidth" "an OTerm" arg

			{-
			leadsToF _ (TSVRecord (RuleLink from)) = TSVFunc $ leadsToF1 $ paramLink from
			leadsToF _ arg = tsBadArg "leadsTo" "a link" arg

			leadsToF1 from vals (TSVRecord (RuleLink to)) = TSVBool $ tryPart part $ leadsTo
				SingleSuccessorLeadsTo from (paramLink to)
				where Just (TSVRecord (RulePart part)) = lookup partValName vals
			leadsToF1 _ _ arg = tsBadArg "leadsTo" "a link" arg
			-}

			checkOTermsF _ termList@(TSVList terms) | all isRuleOTerm terms = if checkOTerms oTerms
				then termList
				else TSVError $ "bad oTerms `" ++ show oTerms ++ "'"
				where
					oTerms = map fromRuleOTerm terms

					fromRuleOTerm (TSVRecord (RuleOTerm i rule)) = (i, rule)
					fromRuleOTerm _ = error "fromRuleOTerm"
			checkOTermsF _ arg = tsBadArg "checkOTerms" "a list of OTerms" arg

			zipF _ (TSVList left) = TSVFunc $ zipF1 left
			zipF _ arg = tsBadArg "zip" "a list" arg

			zipF1 left _ (TSVList right) = TSVList $ zipWith (\l r -> TSVList [l, r]) left right
			zipF1 _ _ arg = tsBadArg "zip _" "a list" arg

			listElemF _ (TSVList l) = TSVFunc $ listElemF1 l
			listElemF _ arg = tsBadArg "listElem" "a list" arg

			listElemF1 l _ (TSVInt index) = l !! (fromInteger index)
			listElemF1 _ _ arg = tsBadArg "listElem _" "a list" arg

			isBuiltinCallF _ (TSVRecord (RuleOTerm _ (TeakOBuiltin {}))) = TSVBool True
			isBuiltinCallF _ (TSVRecord (RuleOTerm _ _)) = TSVBool False
			isBuiltinCallF _ arg = tsBadArg "isBuiltinCall" "an oTerm" arg

			isPos (TSVRecord (RulePos {})) = True
			isPos _ = False

			fromPos (TSVRecord (RulePos pos)) = pos
			fromPos _ = error "fromPos"

			flattenPosList (PosList poss) = poss
			flattenPosList pos = [pos]

			posListF _ l@(TSVList poss) | isTSVListOf isPos l = TSVRecord $ RulePos $ PosList $
				concatMap (flattenPosList . fromPos) poss
			posListF _ arg = tsBadArg "posList" "a list of poss" arg

	ruleEvalDefns :: NetworkIF network => OptimContext network -> Part network -> Rule network ->
		RuleFromMatch -> Why (RuleDefns network)
	ruleEvalDefns context part rule (comps, links) = do
		-- FIXME, The 1000 trick here is a mess
		(newLinkVals, makeNewLinks) <- liftM unzip $ mapM makeNewLinkVal $ zip [-1000,-2000..] $ ruleNewLinks rule
		let
			localEval extraVals = ruleEval context part rule (extraVals ++ vals)
			vals = linkVals ++ compVals ++ mapSnd (localEval []) (newLinkVals ++ ruleDefns rule)
		newLinks <- liftM concat $ mapM ($ localEval) makeNewLinks
		return (vals, newLinks)
		where
			linksToTSVAssoc (name, One l) = (name, TSVRecord (RuleLink l))
			linksToTSVAssoc (name, Many ls) = (name, TSVList (map (TSVRecord . RuleLink) ls))
			linksToTSVAssoc _ = error "linksToTSVAssoc: can't happen"
			compToTSVAssoc (name, comp) = (name, TSVRecord (RuleComp comp))

			linkVals = map linksToTSVAssoc links
			compVals = map compToTSVAssoc comps

			makeRuleLink updates i = TSExprMod (TSValue (TSVRecord (RuleLink (noParamLink (Link i))))) updates

			makeNewLinkVal (i, (name, NewLinkCons updates))
				| length widthUpdates /= 1 = fail $
					"ruleEvalDefns: new link `" ++ name ++ "' must have exactly one width"
				| otherwise = return (val, makeNewLink)
				where
					(widthUpdates, otherUpdates) = partition ((== "width") . fst) updates
					[(_, widthExpr)] = widthUpdates

					makeNewLink eval = case eval [] widthExpr of
						(TSVInt int) | int >= 0 -> return [NetworkLink i (fromInteger int) (HalfBuffer 0)]
						w -> fail $ "ruleEvalDefns: new link `" ++ name ++ "'s width must be a natural number, was `"
							++ show w ++ "'"

					val = (name, makeRuleLink otherUpdates i)
			makeNewLinkVal (i, (name, NewLinksCons {
				newLinksIterators = iters, newLinkUpdates = updates}))
				| length widthUpdates /= 1 = fail $
					"ruleEvalDefns: new links `" ++ name ++ "' must have exactly one width"
				| otherwise = return (val, makeNewLinks)
				where
					(widthUpdates, otherUpdates) = partition ((== "width") . fst) updates
					[(_, widthExpr)] = widthUpdates

					makeNewLinks eval = do
						let (iterNames, _) = unzip iters
						evalledIters <- forM iters $ \(iterName, expr) -> do
							val <- tsvToWhy $ eval [] expr
							case val of
								TSVList vals -> return vals
								_ -> fail $ "`over' list for iterator `" ++ iterName ++ "' must be a list"
						let iterCount = length $ head evalledIters
						when (any ((/= iterCount) . length) evalledIters) $ do
							let lenString name vals = name ++ ": " ++ show (length vals)
							fail $ "`over' lists must all be the same length `" ++ joinWith ","
								(zipWith lenString iterNames evalledIters) ++ "'"
						let iterVals = map (zip iterNames) $ transpose evalledIters
						mapM makeLink $ zip3 [(0::Int)..] links iterVals
						where
							(TSVList tsvLinks) = eval [] (TSName name) -- FIXME, relying on `name' to get link vals
							toLink (TSVRecord (RuleLink link)) = paramLink link
							toLink _ = error "not a link"

							links :: [NetworkLinkRef]
							links = map toLink tsvLinks

							makeLink (printedIndex, Link i, iterVals) = do
								width <- tsvToWhy $ eval iterVals widthExpr
								case width of
									(TSVInt int) -> return $ NetworkLink i (fromInteger int) (HalfBuffer 0)
									w -> fail $ "ruleEvalDefns: new link `" ++ name ++ "[" ++ show printedIndex
										++ "]'s width must be a natural number, was `" ++ show w ++ "'"

					(_, firstIterExpr:_) = unzip iters

					-- "map (\_link -> link { /updates/ }) (/mapParamsToLinks/ params)"
					val = (name, TSCall (TSCall (TSName "map")
						(TSLambda "_link" (TSExprMod (TSName "_link") otherUpdates)))
						(TSCall (TSValue (TSVFunc mapParamsToLinks)) firstIterExpr))

					mapParamsToLinks _ (TSVList paramValues) = TSVList $ map (TSVRecord . RuleLink . noParamLink . Link)
						$ take (length paramValues) [i,(i - 1)..]
					mapParamsToLinks _ params = tsBadArg "ruleEvalDefns of for ... link" "a list" params

	makeReplacementComps :: NetworkIF network => [(String, NetworkComp)] -> [(String, TSRuleValue network)] ->
		(TSRuleVals network -> TSRuleExpr network -> TSRuleValue network) ->
		CompReplacements network -> Why [(String, NetworkComp)]
	makeReplacementComps comps vals localEval (CompSingleReplacement replacement) =
		liftM (:[]) $ makeSingleReplacementComp comps vals (localEval []) replacement
	makeReplacementComps comps _ localEval (CompForeachReplacement {
		compReplacementsIterators = iters,
		compReplacementComp = replacement,
		compReplacementsWhere = whereBindings }) = decorateErrors decorate $ do
			let (iterNames, _) = unzip iters
			evalledIters <- forM iters $ \(iterName, expr) -> do
				val <- tsvToWhy $ localEval [] expr
				case val of
					TSVList vals -> return vals
					_ -> fail $ "`over' list for iterator `" ++ iterName ++ "' must be a list"
			let iterCount = length $ head evalledIters
			when (any ((/= iterCount) . length) evalledIters) $ do
				let lenString name vals = name ++ ": " ++ show (length vals)
				fail $ "`over' lists must all be the same length `" ++ joinWith ","
					(zipWith lenString iterNames evalledIters) ++ "'"
			let iterVals = map (zip iterNames) $ transpose evalledIters
			forM iterVals $ \iterVals -> do
				let
					allVals = iterVals ++ whereVals
					whereVals = map (evalBinding (localEval allVals)) whereBindings
				makeSingleReplacementComp comps allVals (localEval allVals) replacement
			where
				decorate = PosLabel PosTopLevel $ "in component replacement foreach"
				evalBinding eval (name, expr) = (name, eval expr)

	makeSingleReplacementComp :: NetworkIF network => [(String, NetworkComp)] -> [(String, TSRuleValue network)] ->
		(TSRuleExpr network -> TSRuleValue network) -> CompReplacement network -> Why (String, NetworkComp)
	makeSingleReplacementComp comps vals localEval replacement = do
		comp <- chooseComp
		compLinks <- liftM snd $ mapAccumM findSomePortMatch 1 portPatterns
		comp' <- compInsertParamLinks comp compLinks
		comp'' <- insertCompParams comp' compParams
		return (compReplacementName replacement, comp'')
		where
			compName = compReplacementName replacement
			maybeOrigComp = lookup compName comps
			sampleComp = nwCompShortNameToSampleTeakType $ compReplacementCompType replacement

			chooseComp
				| isJust maybeOrigComp = return $ fromJust maybeOrigComp
				| isJust sampleComp = return $ TeakComp 0 (fromJust sampleComp) [] NoPos
				| otherwise = fail $ "can't make replacement component `" ++ compReplacementName replacement
					++ "', can't find a pattern for it"

			findPortNameMatch i "" = do
				links <- tsvLookupLinks vals $ anonRename compName i
				return (i + 1, links)
			findPortNameMatch i name = do
				links <- tsvLookupLinks vals name
				return (i, links)

			findPortMatch i pattern = findPortNameMatch i $ listPatternName pattern

			findSomePortMatch i (One pattern) = findPortMatch i pattern
			findSomePortMatch i (Many patterns) = do
				(i', rets) <- mapAccumM findPortMatch i patterns
				return (i', concat rets)
			findSomePortMatch _ _ = error "findSomePortMatch: can't happen"

			portPatterns = compReplacementPortPatterns replacement

			compParams = mapSnd localEval $ compReplacementParams replacement

	showMatchSummary :: NetworkIF network => Part network -> RuleMatch network -> ShowS
	showMatchSummary part ((matchComps, _), (defns, newLinks), replacementComps, remaps) =
		showString "from\n" .
		makeTable (\comp -> showComp comp "") matchComps .
		showString "\nto\n" .
		makeTable (\comp -> showComp comp "") replacementComps .
		showString "\nwhere\n" .
		makeTable (\defn -> showDefn defn "") defns .
		(if null newLinks
			then id
			else showString "\nnew links\n" . makeTable show (map (\l -> (show (nwLinkIndex l), l)) newLinks)) .
		(if null remaps
			then id
			else showString "\nremaps\n" . showTable (columnFormat [map show froms, map show tos]
				["  ", " => "] ["  ", "  "])) .
		showChar '\n'
		where
			showTable = showListWithSep (showString "\n") showString
			(froms, tos) = unzip remaps

			makeTable eShow namesXes = showTable $ columnFormat [map (++ ":") names, map eShow es] sep sep
				where
					(names, es) = unzip namesXes
					sep = ["  ", " "]

			showDefn (TSVRecord (RuleComp comp)) = showComp comp
			showDefn (TSVRecord (RuleLink link)) = showLinkWithParams link
			showDefn (TSVList elems) = showString "[" .
				showListWithSep (showString ", ") showDefn elems . showString "]"
			showDefn defn = shows defn

			showLinkWithParams lp@(LinkParam { paramLink = link })
				| nwLink link <= 0 = showLinkParam [] lp
				| otherwise = showLinkParam [("width", show width)] lp
				where width = tryPart part $ nwGetLinkWidth link

			showComp (TeakComp i typ links _) = showString "C" . shows i . showString
				" (" . shows typ . showString ") " . showString (showSomeHaskell show (Some links))
			showComp otherComp = shows otherComp

	applyRule :: NetworkIF network => OptimContext network -> Part network -> Rule network -> NetworkCompRef ->
		[Why (RuleMatch network)]
	applyRule context part rule comp = filter (not . isIncomplete) $ map makeReplacement matches
		where
			decorate = PosLabel PosTopLevel $ "in optimisation rule " ++ ruleName rule
			matches = matchCompPattern rule part comp
			debug = ruleDebugWhen rule

			makeReplacement match@(comps, _) = decorateErrors decorate $ do
				defns@(vals, _) <- ruleEvalDefns context part rule match
				let
					localEval extraVals = ruleEval context part rule (extraVals ++ vals)

					boolEval exprType expr = do
						value <- tsvToWhy $ localEval [] $ expr
						mustBeBool value
						let TSVBool bool = value
						return bool
						where
							mustBeBool (TSVBool _) = return ()
							mustBeBool value = fail $ "applyRule: " ++ exprType ++ " expression `" ++
								show expr ++ "' must yield a boolean result, got `" ++ show value ++ "'"

					makeRemap (from, to) = do
						fromLinks <- tsvLookupLinks vals from
						toLinks <- tsvLookupLinks vals to
						when (length fromLinks /= length toLinks) $ fail
							$ "applyRule: bad remap `" ++ show from ++ " => " ++ show to ++ "'"
						return $ zip (map paramLink fromLinks) (map paramLink toLinks)

					leadsToLinkParam typ from to exclude =
						tryPart part $ leadsTo typ (paramLink from) (paramLink to) (map paramLink exclude)

				leadsToVal <- liftM and $ mapM (\(typ, from, to, exclude) -> do
					fromLinks <- tsvLookupLinks vals from
					toLinks <- tsvLookupLinks vals to
					excludeLinks <- liftM concat $ mapM (tsvLookupLinks vals) exclude

					let
						clauseError msg = fail $ "applyRule: in leads-to clause `" ++ show from ++ " -> " ++ show to
							++ "', " ++ msg

					case (fromLinks, toLinks, excludeLinks) of
						([from], tos, exclude) -> return $ all (\to -> leadsToLinkParam typ from to exclude) tos
						(froms, [to], exclude) -> return $ all (\from -> leadsToLinkParam typ from to exclude) froms
						([], _, _) -> clauseError "no `from' links"
						(_, [], _) -> clauseError "no `to' links"
						(_, _, _) -> clauseError "there isn't either exactly one `from' or `to' link"
					) $ ruleFromLeadsTos rule

				whenVal <- boolEval "when" $ ruleWhen rule

				if leadsToVal && whenVal
					then do
						remaps <- liftM concat $ mapM makeRemap $ ruleAliases rule
						newComps <- liftM concat $ mapM (makeReplacementComps comps vals localEval) $ ruleTo rule
						let ret = (match, defns, newComps, remaps)
						if isJust debug
							then do
								debugVal <- boolEval "debug" $ fromJust debug
								-- when debugVal $ fail $ showMatchSummary 1 1 ret ""
								if debugVal	
									then return $ runST $ do
										unsafeIOToST $ putStrLn $ (showString "rule " . showString (ruleName rule)
											. showString "\n" . showMatchSummary part ret) ""
										return ret
									else return ret
							else return ret
					else incomplete

	nwApplyMatchReplacement :: NetworkIF network => RuleMatch network -> NetworkMonad network ()
	nwApplyMatchReplacement ((fromComps, _), (_, newLinks), toComps, remaps) = do
		newLinks' <- mapM nwAddLink newLinks
		let
			linkSubsts = zipWith (\from to -> (refLink from, refLink to)) newLinks newLinks'
			substLink link = fromMaybe link $ lookup link linkSubsts
			substCompLinks comp = comp { nwCompLinks = map (fmap substLink) (nwCompLinks comp) }

		mapM_ nwRemoveComp $ map snd $ fromComps
		mapM_ nwUpdateComp $ map (substCompLinks . snd) updateComps
		mapM_ nwAddComp $ map (substCompLinks . snd) newComps
		mapM_ (\(from, to) -> nwRemapLink (substLink from) (substLink to)) remaps
		where
			isElem es e = isJust $ find (eqFst e) es
			(updateComps, newComps) = partition (isElem fromComps) toComps

	nothingIfNull :: [a] -> Maybe [a]
	nothingIfNull [] = Nothing
	nothingIfNull list = Just list

	ruleToOptim :: NetworkIF network => Rule network -> Optim network
	ruleToOptim rule = Optim (ruleName rule)
		(ruleOnByDefault rule)
		(ruleDescription rule)
		(compPatternTest $ head $ ruleFrom rule)
		(\context part comp -> nothingIfNull $ applyRule context part rule (refComp comp))
		(\part matches -> let
			count = length matches
			in showListWithSep (showString "\n") (\(i, Why comp match) ->
				if comp == Complete
					then showString "Match " . shows i . showChar '/' . shows count . showString ":\n"
						. showMatchSummary part match
					else showError comp) (zip [(1::Int)..] matches) "")
		(\part comp (match : _) -> defaultConnectWhy ([], part) match $ \matchM -> return
			-- FIXME, need to allow NewLeads from Rules
			([NewLead (refComp comp) (ruleName rule)], runPart_ part $ nwApplyMatchReplacement matchM))
		where
			showError (Wrong reports) = showString "*** " . showListWithSep (showString ",") showReport reports
				where showReport (Report _ str) = showString str
			showError _ = showString ""

	rulesTestCard :: NetworkIF network => RuleSet network -> FilePath -> String -> IO ()
	rulesTestCard ruleSet file sectionWord = do
		handle <- openFile file WriteMode
		ruleShows <- mapM (tcShowRule file sectionWord) rules
		hPutStrLn handle $ foldl' (.) id ruleShows ""
		hClose handle
		where
			rules = ruleSetRules ruleSet

	tcShowRule :: NetworkIF network => FilePath -> String -> Rule network -> IO ShowS
	tcShowRule baseName sectionWord rule = do
		putStrLn $ "Plotting: " ++ name
		-- print from
		plotNetwork fromName (name ++ "_from") from
		plotNetwork toName (name ++ "_to") to

		return $ loutSection sectionWord ("rule " ++ name) $
			loutPara (showString name . showString ": " . showString (capitalise $ ruleDescription rule)) .
			loutPara (showString $ "{5c @Wide @Scale @IncludeGraphic {" ++ fromName ++ "}}|1c" ++
				"{5c @Wide @Scale @IncludeGraphic {" ++ toName ++ "}}\n") .
			id
		where
			name = ruleName rule
			baseName2 = baseName ++ "-" ++ name
			fromName = baseName2 ++ "-from" <.> "eps"
			toName = baseName2 ++ "-to" <.> "eps"

			links :: [String]
			links = nub $ map listPatternName $ flattenSome $ Some $
				(concatMap compPatternPortPatterns $ ruleFrom rule) ++
				(concatMap (compReplacementPortPatterns . compReplacementComp) $ ruleTo rule)

			plotNetwork :: FilePath -> String -> Network -> IO ()
			plotNetwork fileName graphName network = do
				let
					width = 50
					height = 50
					opts = [PlotSize (width, height), PlotShowUnconnected]
					part = Part graphName [] network
					paperSize = (width, height)

					keepDot = False

				graphical <- partGraphicalInfo keepDot opts graphName part

				renderToPS paperSize fileName [do
					renderPartForPaper paperSize opts Nothing Nothing part graphical]

				return ()

			from :: Network
			(_, from) = runNetwork linkNW $ do
				mapM (\(name, ref) -> nwSetLinkName ref name) $ DM.assocs linkMappings
				forM_ (ruleFrom rule) $ \pattern -> tcNwAddCompPattern
					linkMappings
					(compPatternName pattern)
					(compPatternPortPatterns pattern)
					(compPatternTeakCompType pattern)

			to :: Network
			-- FIXME, handle iterated replacements
			(_, to) = runNetwork linkNW $ do
				mapM (\(name, ref) -> nwSetLinkName ref name) $ DM.assocs linkMappings
				forM_ (ruleTo rule) $ \replacement -> tcNwAddCompPattern
					linkMappings
					(compReplacementName $ compReplacementComp replacement)
					(compReplacementPortPatterns $ compReplacementComp replacement)
					(nwCompShortNameToSampleTeakType $ compReplacementCompType $ compReplacementComp replacement)

			(linkMappings, linkNW) = runNetwork0 $ do
				linkRefAssocs <- mapM makeLink links
				return $ DM.fromList linkRefAssocs

			makeLink name = do
				ref <- nwNewLinkRef 0
				return (name, ref)

	tcNwAddCompPattern :: NetworkIF network => DM.Map String NetworkLinkRef ->
		String -> [Some ListPattern] -> Maybe TeakCompType ->
		NetworkMonad network ()
	tcNwAddCompPattern _linkMappings _name _portPatterns Nothing = return () -- FIXME, handle instances
	tcNwAddCompPattern linkMappings name portPatterns (Just sampleComp) = do
		nwNewTeakComp typ links NoPos
		return ()
		where
			links = zipWith makeLinkFromPattern portPatterns $ teakCompPortInfo $ nwTeakCompInfo typ

			makeLinkFromPattern (One pattern) info
				| networkPortIsArrayed info = Many [patternToLink pattern]
				| otherwise = One (patternToLink pattern)
			makeLinkFromPattern (Many patterns) _ = Many (map patternToLink patterns)
			makeLinkFromPattern _ _ = error "makeLinkFromPattern"

			patternToLink (SinglePattern _ name) = linkMappings DM.! name
			patternToLink (EllipsisPattern _ name) = linkMappings DM.! name

			-- FIXME, need to make the lengths of wgs/wds and rgs/rws match for TeakV so that plot works
			typ = case (sampleComp, portPatterns) of
				(TeakF _, [_, Many outs]) -> TeakF (replicate (length outs) 0) 
				(TeakS {}, [_, Many outs]) -> TeakS (0+:1) (replicate (length outs) ([Imp 0 0], 0))
				(TeakV {}, [wgs, wds, rgs, rds]) -> let
					flatWgs = flattenSome wgs
					flatWds = flattenSome wds
					flatRgs = flattenSome rgs
					flatRds = flattenSome rds
					writeCount = max (length flatWgs) (length flatWds)
					readCount = max (length flatRgs) (length flatRds)
					in TeakV name 1 [] (replicate writeCount 0) (replicate readCount 0)
				(sample, _) -> sample

	loutSection :: String -> String -> ShowS -> ShowS
	loutSection sectionWord name body = -- loutEscape body
		showString "@" . showString sectionWord . showString " @Title {" . showString name . showString "} @Begin\n" .
		loutEscape body . showString "\n@End @" . showString sectionWord . showString "\n"

	loutPara :: ShowS -> ShowS
	loutPara body = showString "\n@LP\n" . body

	loutEscape :: ShowS -> ShowS
	loutEscape body tailStr = esc $ body tailStr
		where
			esc [] = []
			esc ('/':str) = "\"/\"" ++ esc str
			esc (chr:str) = chr : esc str

