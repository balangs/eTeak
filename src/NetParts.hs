{-
	Teak synthesiser for the Balsa language
	Copyright (C) 2007-2010 The University of Manchester

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,

	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
-}

module NetParts (
	Access (..),
	AccessBody (..),
	AccessRef (..),
	NetworkComp (..),
	NetworkCompRef (..),
	NetworkLink (..),
	NetworkLinkConn (..),
	NetworkLinkRef (..),
	NetworkLinkUsage (..),
	NetworkMonad (..),
	NetworkPort (..),
	NetworkProperty (..),
	NetworkIF (..),
	NetworkPortInfo (..),
	Latching (..),
	TeakCompInfo (..),
	Part (..),
	RW (..),
	TeakCompType (..),
	TeakOSlice,
	TeakOTerm (..),
	TeakOp (..),
	TeakParam (..),
	directionToSense,
	trimPart,
	exprToTeakParam,
	funcActualToTeakParam,
	teakParamToFuncActual,
	isFork,
	isTeakI,
	isInstanceComp,
	isTeakR,
	isTeakA,
	isTeakJ,
	isLinkComp,
	isMerge,
	isTeakO,
	isSteer,
	isMux,
	isTeakV,
	teakOOpNames,
	oTermExtractSlices,
	oTermInsertSlices,
	makeLinkUsage,
	addUsagePair,
	addUsagePairUnsafe,
	removeUsagePair,
	runWhyTPart,
	runWhyTPart_,
	runPart,
	runPart_,
	tryPart,
	networkPortToPortInfo,
	nwFuncToNetworkMonad,
	nwAccessLinkUsage,
	nwAccessLinks,
	nwAddLinkRef,
	nwAddPortAccess,
	nwAddProperty,
	nwBreakLink,
	nwCombineAccesses,
	nwCompLinkUsage,
	nwCompPortNames,
	nwCompPortSenses,
	nwCompPortCardinalities,
	nwCompShortName,
	nwCompShortNameToTest,
	nwCompShortNameToSampleTeakType,
	nwConnectedComps,
	nwFindPart,
	nwFindPartIndex,
	nwFoldComps,
	nwFoldCompsIf,
	nwGetAccess,
	nwFindAccess,
	nwFindPortByRef,
	nwFindPortIndexByRef,
	nwFoldLinks,
	nwGetAttributeVal,
	nwGetLinkName,
	nwGetLinkNames,
	nwGetLinkWidth,
	nwGetLinkLatching,
	nwGetPortAccess,
	nwGetPosArray,
	nwLinkIsPort,
	nwLinkIsUnused,
	nwLinkToComp,
	nwLinkToCompIf,
	nwMapComps,
	nwMapCompsIf,
	nwMapCompsIf_,
	nwMapLinks,
	nwMapLinks_,
	nwMatchAccess,
	nwNewLink,
	nwNewLinkRef,
	nwNewTeakComp,
	nwNoLinkUsage,
	nwRemapLink,
	nwRemoveComp,
	nwRemovePortAccess,
	nwRemoveUnusedLinks,
	nwReplaceLinkAtConn,
	nwReplaceLinkInAccess,
	nwReplaceLinkInComp,
	nwSetLinkName,
	nwTeakCompInfo,
	oSliceIncrementTermNo,
	oSliceIndex,
	oSliceWidth,
	oTermIncrementTermNo,
	oTermResultWidth,
	oTermVisitSlices,
	oTermsLastIndex,
	prettyPrintOTerms,
	prettyPrintOTermPrec,
	refComp,
	refLink,
	runNetwork,
	runNetwork0,
	runNetwork_,
	tryNetwork,
	showPart,
	showPosForNetwork,
	showPrettyNetwork,
	readNetworkFile,
	uniquifyPart,
	checkPart,
	writeNetworkFile
	) where

	import Misc
	import Context
	import ParseTree
	import Report
	import Show
	import Type
	import Bits
	import State
	import Print

	import System.IO
	import Data.Maybe
	-- import Control.Monad
	import Data.List
	import Data.Char (toLower, isDigit)
	import qualified Data.Map as Map
	import Data.Array
	import Control.Monad.State

	data RW = Read | Write
		deriving (Show, Read, Eq)

	newtype NetworkLinkRef = Link { nwLink :: Int }
		deriving (Eq, Ord, Ix)

	newtype NetworkCompRef = Comp { nwComp :: Int }
		deriving (Eq, Ord, Ix)

	newtype Latching = HalfBuffer { latchingDepth :: Int }
		deriving Eq

	data NetworkLink = NetworkLink { nwLinkIndex :: ! Int, nwLinkWidth :: ! Int, nwLinkLatching :: Latching }
		deriving (Eq)

	refLink :: NetworkLink -> NetworkLinkRef
	refLink link = Link $ nwLinkIndex link

	refComp :: NetworkComp -> NetworkCompRef
	refComp comp = Comp $ nwCompIndex comp

	data NetworkLinkConn = NoConn
		| LinkComp ! NetworkCompRef ! [Int]
		| LinkAccess ! AccessRef ! [Int]
		deriving (Eq, Show, Read)

	instance ShowTab NetworkLinkConn

	nwNoLinkUsage :: NetworkLinkUsage
	nwNoLinkUsage = NetworkLinkUsage NoConn NoConn

	data NetworkLinkUsage = NetworkLinkUsage {
		nwPassiveLinkUsage :: ! NetworkLinkConn, nwActiveLinkUsage :: ! NetworkLinkConn }
		deriving (Eq)

	type TeakOSlice = (Int, Slice Int) -- index, slice

	data TeakParam = TeakParamInt Integer
		| TeakParamString String
		| TeakParamType Type
		deriving (Eq, Ord, Show, Read)

	data TeakOp =
		  TeakOpAdd | TeakOpSub -- add, subtract equal width
		| TeakOpAnd | TeakOpOr | TeakOpNot | TeakOpXor -- logical equal width
		| TeakOpSignedGT | TeakOpSignedGE -- signed comparison, 1 bit result
		| TeakOpUnsignedGT | TeakOpUnsignedGE -- unsigned comparison, 1 bit result
		| TeakOpEqual | TeakOpNotEqual -- comparison, 1 bit result
		deriving (Eq, Ord, Read, Show)

	data TeakOTerm =
		  TeakOConstant { teakOWidth :: Int, teakOValue :: Integer }
		| TeakOAppend { teakOCount :: Int, teakOSlices :: [TeakOSlice] }
		| TeakOBuiltin { teakOBuiltin :: String, teakOWidth :: Int,
			teakOParams :: [TeakParam], teakOSlices :: [TeakOSlice] }
		| TeakOp { teakOOp :: TeakOp, teakOSlices :: [TeakOSlice] }
		| TeakOMux { teakOSpec :: [[Implicant]], teakOSlices :: [TeakOSlice] }
		deriving (Eq, Ord)

	oSliceWidth :: TeakOSlice -> Int
	oSliceWidth (_, slice) = sliceWidth slice

	oSliceIndex :: TeakOSlice -> Int
	oSliceIndex (i, _) = i

	oTermExtractSlices :: TeakOTerm -> [TeakOSlice]
	oTermExtractSlices (TeakOConstant {}) = []
	oTermExtractSlices term = teakOSlices term

	oTermInsertSlices :: TeakOTerm -> [TeakOSlice] -> TeakOTerm
	oTermInsertSlices term@(TeakOConstant {}) _ = term
	oTermInsertSlices term slices = term { teakOSlices = slices }

	oTermResultWidth :: TeakOTerm -> Int
	oTermResultWidth (TeakOConstant width _) = width
	oTermResultWidth (TeakOAppend count slices) = count * (sum (map oSliceWidth slices))
	oTermResultWidth (TeakOBuiltin _ width _ _) = width
	oTermResultWidth (TeakOp op slices)
		| isEqualWidthOp op = oSliceWidth (head slices)
		| otherwise = 1
	oTermResultWidth (TeakOMux _ (_:slice:_)) = oSliceWidth slice
	oTermResultWidth term = error $ "oTermResultWidth: unrecognised term `" ++ show term ++ "'"

	isEqualWidthOp :: TeakOp -> Bool
	isEqualWidthOp TeakOpAdd = True
	isEqualWidthOp TeakOpSub = True
	isEqualWidthOp TeakOpAnd = True
	isEqualWidthOp TeakOpOr = True
	isEqualWidthOp TeakOpXor = True
	isEqualWidthOp TeakOpNot = True
	isEqualWidthOp _ = False

	-- teakOOpNames : (justAlphabetic, infix/pretty) names for TeakOps
	teakOOpNames :: TeakOp -> (String, String)
	teakOOpNames TeakOpAdd = ("add", "+")
	teakOOpNames TeakOpAnd = ("and", "and")
	teakOOpNames TeakOpEqual = ("eq", "=")
	teakOOpNames TeakOpSignedGE = ("sge", "s>=")
	teakOOpNames TeakOpSignedGT = ("sgt", "s>")
	teakOOpNames TeakOpUnsignedGT = ("ugt", ">")
	teakOOpNames TeakOpUnsignedGE = ("uge", ">=")
	teakOOpNames TeakOpNotEqual = ("ne", "/=")
	teakOOpNames TeakOpNot = ("not", "not")
	teakOOpNames TeakOpOr = ("or", "or")
	teakOOpNames TeakOpSub = ("sub", "-")
	teakOOpNames TeakOpXor = ("xor", "xor")

	oSliceIncrementTermNo :: Int -> TeakOSlice -> TeakOSlice
	oSliceIncrementTermNo incr (term, slice) = (term + incr, slice)

	oTermsLastIndex :: [(Int, TeakOTerm)] -> Int
	oTermsLastIndex [] = 0
	oTermsLastIndex terms = fst $ last terms

	oTermIncrementTermNo :: Int -> (Int, TeakOTerm) -> (Int, TeakOTerm)
	oTermIncrementTermNo incr (i, term) = (i + incr, oTermVisitSlices (oSliceIncrementTermNo incr) term)

	oTermVisitSlices :: (TeakOSlice -> TeakOSlice) -> TeakOTerm -> TeakOTerm
	oTermVisitSlices f term = body term
		where
			slices = teakOSlices term
			term' = term { teakOSlices = map f slices }

			body (TeakOAppend {}) = term'
			body (TeakOBuiltin {}) = term'
			body (TeakOp {}) = term'
			body (TeakOMux {}) = term'
			body term = term

	data TeakCompType =
		  TeakA
		| TeakF [Int] -- [2, 3], low indices of outputs in order.  Widths determined by links.
		| TeakJ
		| TeakM
		| TeakO [(Int, TeakOTerm)]
		| TeakS (Slice Int) [([Implicant], Int)] -- condition/low index for each output
		| TeakX [[Implicant]] -- condition for each input
		| TeakV String Int [Int] [Int] [Int] -- name, overall width, builtin offsets, write offsets, read offsets
		-- Newer, optimisation parts
		| TeakI
		| TeakR
		deriving (Eq, Ord, Read)

	-- FIXME, write a Read for TeakCompType

	data NetworkComp =
		TeakComp {
			nwCompIndex :: ! Int,
			nwTeakType :: ! TeakCompType,
			nwCompLinks :: ! [Some NetworkLinkRef],
			nwCompPos :: ! Pos }
		| InstanceComp {
			nwCompIndex :: ! Int,
			nwPartName :: ! String,
			nwCompPorts :: ! [NetworkPort],
			nwCompLinks :: ! [Some NetworkLinkRef],
			nwCompPos :: ! Pos }

	instance Eq NetworkComp where
		c1 == c2 = nwCompIndex c1 == nwCompIndex c2

	instance Ord NetworkComp where
		compare c1 c2 = compare (nwCompIndex c1) (nwCompIndex c2)

	data NetworkPort = NetworkPort { nwPortName :: ! String,
		nwPortDirection :: ! Direction, nwPortWidth :: ! Int,
		nwPortRef :: ! (Maybe AccessRef) }
		deriving Eq

	networkPortToPortInfo :: NetworkPort -> NetworkPortInfo
	networkPortToPortInfo port = NetworkPortInfo (nwPortName port)
		(directionToSense $ nwPortDirection port) False

	data NetworkProperty = NetworkComment String
		| NetworkPosArray PosArray
		| NetworkAttrs [(String, TeakParam)]
		| NetworkLinkNames (Map.Map NetworkLinkRef String)
		deriving (Read, Eq, Ord)

	data AccessBody =
		  ChanInputAccess { chanAccessGo :: NetworkLinkRef, chanAccessDone :: NetworkLinkRef }
		| ChanBareInputAccess { chanAccessData :: NetworkLinkRef }
		| ChanOutputAccess { chanAccessGo :: NetworkLinkRef, chanAccessDone :: NetworkLinkRef }
		| ChanBareOutputAccess { chanAccessData :: NetworkLinkRef }
		| VarAccess { accessRW :: RW, accessCtrl :: NetworkLinkRef,
			accessData :: NetworkLinkRef, accessRange :: Slice Int }
		| SharedCallAccess { accessGo :: NetworkLinkRef, accessDone :: NetworkLinkRef }
		| PortLinkAccess { accessSense :: Sense, accessLink :: NetworkLinkRef }
		deriving (Eq)

	data AccessRef =
		  GoAccess
		| DoneAccess
		| InstanceAccess Ref
		deriving (Show, Read, Eq, Ord)

	data Access = Access { accessRef :: AccessRef, accessBodys :: ! [AccessBody] }
		deriving (Eq)

	nwCombineAccesses :: [Access] -> [Access] -> [Access]
	nwCombineAccesses accesses1 accesses2 = mergeByWith by with accesses1 accesses2
		where
			by (Access ref1 _) (Access ref2 _) = ref1 `compare` ref2
			with (Access ref bodysL) (Access _ bodysR) = Access ref (bodysL ++ bodysR)

	instance Ord Access where
		compare (Access refL _) (Access refR _) = compare refL refR

	nwAccessLinks :: Access -> [NetworkLinkRef]
	nwAccessLinks (Access _ body) = concatMap inAccessBody body
		where
			inAccessBody (ChanInputAccess go done) = [go, done]
			inAccessBody (ChanOutputAccess go done) = [go, done]
			inAccessBody (ChanBareInputAccess link) = [link]
			inAccessBody (ChanBareOutputAccess dat) = [dat]
			inAccessBody (VarAccess _ ctrlLink dataLink _) = [ctrlLink, dataLink]
			inAccessBody (SharedCallAccess go done) = [go, done]
			inAccessBody (PortLinkAccess _ link) = [link]

	data NetworkPortInfo = NetworkPortInfo {
		networkPortName :: String,
		networkPortSense :: Sense,
		networkPortIsArrayed :: Bool }
		deriving Show

	data TeakCompInfo = TeakCompInfo {
		teakCompShortName :: String,
		teakCompTest :: NetworkComp -> Bool,
		teakCompPortInfo :: [NetworkPortInfo] }

	nwTeakCompInfo :: TeakCompType -> TeakCompInfo
	nwTeakCompInfo typ = body typ
		where
			port = NetworkPortInfo

			body (TeakJ {}) = TeakCompInfo "j" isTeakJ [port "i" Passive True, port "o" Active False]
			body (TeakM {}) = TeakCompInfo "m" isMerge [port "i" Passive True, port "o" Active False]
			body (TeakF {}) = TeakCompInfo "f" isFork [port "i" Passive False, port "o" Active True]
			body (TeakS {}) = TeakCompInfo "s" isSteer [port "i" Passive False, port "o" Active True]
			body (TeakX {}) = TeakCompInfo "x" isMux [port "i" Passive True, port "s" Passive False,
				port "o" Active False]
			body (TeakO {}) = TeakCompInfo "o" isTeakO [port "i" Passive False, port "o" Active False]
			body (TeakV {}) = TeakCompInfo "v" isTeakV
				[port "wg" Passive True, port "wd" Active True, port "rg" Passive True, port "rd" Active True]
			body (TeakA {}) = TeakCompInfo "a" isTeakA [port "i" Passive True, port "o" Active False]
			body (TeakI {}) = TeakCompInfo "i" isTeakI [port "i" Passive False, port "o" Active False]
			body (TeakR {}) = TeakCompInfo "r" isTeakR [port "o" Active False]

	nwCompShortNameToSampleTeakType :: String -> Maybe TeakCompType
	nwCompShortNameToSampleTeakType name = body (map toLower name)
		where
			body "j" = Just $ TeakJ
			body "m" = Just $ TeakM
			body "f" = Just $ TeakF []
			body "s" = Just $ TeakS emptySlice []
			body "x" = Just $ TeakX []
			body "o" = Just $ TeakO []
			body "v" = Just $ TeakV "" 0 [] [] []
			body "a" = Just $ TeakA
			body "i" = Just $ TeakI
			body "r" = Just $ TeakR
			body _ = Nothing

	nwCompShortName :: NetworkComp -> String
	nwCompShortName (TeakComp { nwTeakType = typ }) = teakCompShortName $ nwTeakCompInfo typ
	nwCompShortName _ = ""

	nwCompShortNameToTest :: String -> Maybe (NetworkComp -> Bool)
	nwCompShortNameToTest name = do
		sample <- nwCompShortNameToSampleTeakType name
		return $ teakCompTest $ nwTeakCompInfo sample

	nwCompPortNames :: NetworkComp -> [String]
	nwCompPortNames comp = body comp
		where
			body (TeakComp {}) = map networkPortName $ teakCompPortInfo $ nwTeakCompInfo $ nwTeakType comp
			body (InstanceComp { nwCompPorts = ports }) = map nwPortName ports
			-- body comp = map show [0..length (nwCompPortSenses comp) - 1]

	directionToSense :: Direction -> Sense
	directionToSense Input = Passive
	directionToSense Output = Active

	nwCompPortSenses :: NetworkComp -> [Sense]
	nwCompPortSenses comp = body comp
		where
			body (TeakComp {}) = map networkPortSense $ teakCompPortInfo $ nwTeakCompInfo $ nwTeakType comp
			body (InstanceComp { nwCompPorts = ports }) = map (directionToSense . nwPortDirection) ports
			-- body _ = []

	nwCompPortCardinalities :: NetworkComp -> [Some ()]
	nwCompPortCardinalities comp = body comp
		where
			isMany True = Many [()]
			isMany False = One ()

			body (TeakComp {}) = map (isMany . networkPortIsArrayed) $
				teakCompPortInfo $ nwTeakCompInfo $ nwTeakType comp
			body (InstanceComp { nwCompPorts = ports }) = replicate (length ports) (One ())
			-- body _ = []

	isFork :: NetworkComp -> Bool
	isFork (TeakComp { nwTeakType = (TeakF {}) }) = True
	isFork _ = False

	isSteer :: NetworkComp -> Bool
	isSteer (TeakComp { nwTeakType = (TeakS {}) }) = True
	isSteer _ = False

	isMux :: NetworkComp -> Bool
	isMux (TeakComp { nwTeakType = (TeakX {}) }) = True
	isMux _ = False

	isTeakJ :: NetworkComp -> Bool
	isTeakJ (TeakComp { nwTeakType = (TeakJ {}) }) = True
	isTeakJ _ = False

	isMerge :: NetworkComp -> Bool
	isMerge (TeakComp { nwTeakType = (TeakM {}) }) = True
	isMerge _ = False

	isTeakA :: NetworkComp -> Bool
	isTeakA (TeakComp { nwTeakType = (TeakA {}) }) = True
	isTeakA _ = False

	isTeakV :: NetworkComp -> Bool
	isTeakV (TeakComp { nwTeakType = (TeakV {}) }) = True
	isTeakV _ = False

	isTeakO :: NetworkComp -> Bool
	isTeakO (TeakComp { nwTeakType = (TeakO {}) }) = True
	isTeakO _ = False

	isTeakI :: NetworkComp -> Bool
	isTeakI (TeakComp { nwTeakType = (TeakI {}) }) = True
	isTeakI _ = False

	isTeakR :: NetworkComp -> Bool
	isTeakR (TeakComp { nwTeakType = (TeakR {}) }) = True
	isTeakR _ = False

	isInstanceComp :: NetworkComp -> Bool
	isInstanceComp (InstanceComp {}) = True
	isInstanceComp _ = False

	-- nwConnectedComps : returns all the components reachable from the given
	--	one to the requested depth
	nwConnectedComps :: NetworkIF network => Int -> NetworkCompRef -> NetworkMonad network [NetworkCompRef]
	nwConnectedComps depth comp = liftM Map.keys $ body depth Map.empty comp
		where
			body depth visited comp
				| depth <= 0 || alreadyVisited comp visited depth = return visited
				| otherwise = do
					newComps <- connectedComps1 comp
					foldM (body (depth - 1)) (Map.insert comp depth visited) newComps

			alreadyVisited thisComp visited depth = fromMaybe False $ do
				lastDepth <- Map.lookup thisComp visited
				return $ lastDepth >= depth

			connectedComps1 compRef = do
				comp <- nwGetComp compRef
				if isJust comp
					then do
						let links = nwCompLinks (fromJust comp)
						liftM concat $ mapM linkComps $ concatMap flattenSome links
					else return []

			linkComps link = do
				pas <- nwLinkToComp Passive link
				act <- nwLinkToComp Active link
				return $ map (refComp . fst) $ catMaybes [pas, act]
	
	makeUsage :: Sense -> (Int, NetworkLinkConn) -> (Int, NetworkLinkUsage)
	makeUsage Passive (i, conn) = (i, NetworkLinkUsage conn NoConn)
	makeUsage Active (i, conn) = (i, NetworkLinkUsage NoConn conn)

	nwCompLinkUsage :: NetworkComp -> [(Int, NetworkLinkUsage)]
	nwCompLinkUsage comp = concatMap makePortUsage $ zip3 [0..] (nwCompPortSenses comp) (nwCompLinks comp)
		where
			compNo = refComp comp

			makePortUsage (linkPos, sense, One (Link linkNo)) = [makeUsage sense (linkNo, LinkComp compNo [linkPos])]
			makePortUsage (linkPos, sense, Many links) = map (makeUsage sense . makeMany) $ zip [0..] links
				where makeMany (linkPos2, (Link linkNo)) = (linkNo, LinkComp compNo [linkPos, linkPos2])
			makePortUsage _ = error "nwCompLinkUsage makePortUsage: can't happen"

	nwReplaceLinkInComp :: NetworkComp -> [Int] -> NetworkLinkRef -> NetworkComp
	nwReplaceLinkInComp comp addr link = comp { nwCompLinks = links' }
		where Some links' = replaceElemInSome addr link (Some (nwCompLinks comp))

	nwAccessLinkUsage :: Int -> Access -> [(Int, NetworkLinkUsage)]
	nwAccessLinkUsage bodyOffset (Access ref bodys) = concatMap makeAccessUsage $ zip [bodyOffset..] bodys
		where
			makeAccessUsage (bodyNo, access) = body access
				where
					mkAccess link sense addr = makeUsage sense (link, LinkAccess ref (bodyNo:addr))

					-- body returns usage for passive/active connections of the
					--	relative to the unplaced components which form the access implementation.
					--	For example, ChanInput has a passive go (sourced from the circuit to the unplaced
					--	portion) and a data-carrying active done
					body (ChanInputAccess (Link go) (Link done)) = [
						mkAccess go Passive [0],
						mkAccess done Active [1] ]
					body (ChanOutputAccess (Link go) (Link done)) = [
						mkAccess go Passive [0],
						mkAccess done Active [1] ]
					body (ChanBareInputAccess (Link dataLink)) = [mkAccess dataLink Active [0]]
					body (ChanBareOutputAccess (Link dat)) = [mkAccess dat Passive [0]]
					-- variable accesses
					body (VarAccess Read (Link ctrlLink) (Link dataLink) _) = [
						mkAccess dataLink Active [1],
						mkAccess ctrlLink Passive [0] ]
					body (VarAccess Write (Link ctrlLink) (Link dataLink) _) = [
						mkAccess ctrlLink Active [0],
						mkAccess dataLink Passive [1] ]
					body (SharedCallAccess (Link go) (Link done)) = [
						mkAccess go Passive [0],
						mkAccess done Active [1] ]
					-- ports are viewed from the environment, hence the sense inversion
					body (PortLinkAccess sense (Link link)) = [mkAccess link (invertSense sense) [0]]

	nwReplaceLinkInAccess :: Access -> [Int] -> NetworkLinkRef -> Access
	nwReplaceLinkInAccess (Access ref bodys) (bodyNo:addr) link = Access ref bodys'
		where
			bodys' = replaceAt bodys bodyNo $ replace (bodys !! bodyNo) addr

			replace (ChanInputAccess _ done) [0] = ChanInputAccess link done
			replace (ChanInputAccess go _) [1] = ChanInputAccess go link
			replace (ChanBareInputAccess _) [0] = ChanBareInputAccess link
			replace (ChanOutputAccess _ done) [0] = ChanOutputAccess link done
			replace (ChanOutputAccess go _) [1] = ChanOutputAccess go link
			replace (ChanBareOutputAccess _) [0] = ChanBareOutputAccess link
			replace (VarAccess Read _ dataLink range) [0] = VarAccess Read link dataLink range
			replace (VarAccess Read ctrlLink _ range) [1] = VarAccess Read ctrlLink link range
			replace (SharedCallAccess _ done) [0] = SharedCallAccess link done
			replace (SharedCallAccess go _) [1] = SharedCallAccess go link
			replace (PortLinkAccess sense _) [0] = PortLinkAccess sense link
			replace body addr = error $ "nwReplaceLinkInAccess: can't replace " ++ show body ++ " " ++ show addr
	nwReplaceLinkInAccess _ _ _ = error "nwReplaceLinkInAccess: need port number"

	escDoubleQuote :: String -> String
	escDoubleQuote str = concatMap escChar str
		where
			escChar '"' = ['\\', '"']
			escChar chr = [chr]

	prettyPrintOSlice :: Int -> [(Int, TeakOTerm)] -> (Int -> ShowS) -> TeakOSlice -> String
	prettyPrintOSlice prec terms inputName (index,slice) =
		showParen (prec > thisPrec) (showString $ prefix
		++ (if wholeSlice then "" else verilogShowSlice slice "")
		) ""
		where
			thisPrec
				| wholeSlice = prec
				| otherwise = 10
			prefix
				| index == 0 = inputName (thisPrec + 1) ""
				| isNothing term = "?" ++ show index
				| otherwise = "t" ++ show index
			term = lookup index terms
			wholeSlice = index /= 0 && sliceOffset slice == 0 &&
				isJust term && sliceWidth slice == oTermResultWidth (fromJust term)

	prettyPrintSubterm :: Int -> [(Int, TeakOTerm)] -> (Int -> ShowS) -> [Int] -> Bool -> TeakOSlice -> String
	prettyPrintSubterm prec terms inputName _ _ slice@(0,_) = prettyPrintOSlice prec terms inputName slice
	prettyPrintSubterm prec terms inputName noDescendSlices inPrint (index,slice)
		| isNothing maybeTerm = "?"
		| termWidth == sliceWidth slice && sliceOffset slice == 0 =
			prettyPrintOTerm prec terms inputName noDescendSlices inPrint (index, term)
		| otherwise = showParen (prec > thisPrec) (showString $
			prettyPrintOTerm (thisPrec + 1) terms inputName noDescendSlices False (index, term) ++
			verilogShowSlice slice ""
			) ""
		where
			thisPrec = 10
			maybeTerm = lookup index terms
			Just term = maybeTerm
			termWidth = oTermResultWidth term

	prettyPrintOTermPrec :: TeakOTerm -> Int
	prettyPrintOTermPrec (TeakOp op _) = prettyPrintTeakOpPrec op
	prettyPrintOTermPrec (TeakOMux {}) = 0
	prettyPrintOTermPrec _ = 10

	prettyPrintTeakOpPrec :: TeakOp -> Int
	prettyPrintTeakOpPrec TeakOpAdd = 6
	prettyPrintTeakOpPrec TeakOpSub = 6
	prettyPrintTeakOpPrec TeakOpAnd = 3
	prettyPrintTeakOpPrec TeakOpOr = 2
	prettyPrintTeakOpPrec TeakOpXor = 2
	prettyPrintTeakOpPrec TeakOpNot = 10
	prettyPrintTeakOpPrec TeakOpSignedGT = 4
	prettyPrintTeakOpPrec TeakOpSignedGE = 4
	prettyPrintTeakOpPrec TeakOpUnsignedGT = 4
	prettyPrintTeakOpPrec TeakOpUnsignedGE = 4
	prettyPrintTeakOpPrec TeakOpEqual = 4
	prettyPrintTeakOpPrec TeakOpNotEqual = 4

	prettyPrintOTerm :: Int -> [(Int, TeakOTerm)] -> (Int -> ShowS) -> [Int] -> Bool -> (Int, TeakOTerm) -> String
	prettyPrintOTerm prec terms inputName noDescendSlices inPrint (index, term) = paren $ case term of
		TeakOConstant width constant
			| intWidth constant == width -> show constant
			| otherwise -> show width ++ "'d" ++ show constant
		TeakOAppend count slices
			| count == 1 && length slices == 1 -> subTerm 0 (head slices)
 			| otherwise -> (if count == 1 then "" else show count) ++
				"{" ++ joinWith "," (map (subTerm 0) (reverse slices)) ++ "}"
		TeakOMux spec (choiceSlice:slices) -> "case " ++ subTerm 0 choiceSlice ++ " of " ++
			joinWith " | " (map makeTerm $ zip spec slices)
			where makeTerm (imps, slice) = joinWith "," (map (showImp True True 4) imps) ++ " -> " ++ subTerm 0 slice
		TeakOp TeakOpNot [slice] -> "not " ++ subTerm (thisPrec + 1) slice
		TeakOp binOp [sliceL, sliceR] -> subTerm (thisPrec + 1) sliceL ++ " " ++ snd (teakOOpNames binOp) ++ " " ++
			subTerm (thisPrec + 1) sliceR
		TeakOBuiltin "StringAppend" _ _ [str1, str2]
			| inPrint -> subTerm 0 str1 ++ ", " ++ subTerm 0 str2
		TeakOBuiltin "ToString" _ [TeakParamType typ] [slice] -> "$#(" ++ showTypeName [] typ ++ ")(" ++
			subTerm 0 slice ++ ")"
		TeakOBuiltin "String" _ [TeakParamString str] _ -> "\"" ++ escDoubleQuote str ++ "\""
		TeakOBuiltin "tWriteMessage" _ _ [slice] -> "print " ++ subTermInPrint 0 slice
		TeakOBuiltin name _ [] slices -> builtinName name ++ " (" ++ joinWith "," (map (subTerm 0) slices) ++ ")"
		TeakOBuiltin name _ params slices -> builtinName name ++
			" #(" ++ joinWith "," (map showParam params) ++ ")" ++
			" (" ++ joinWith "," (map (subTerm 0) slices) ++ ")"
		_ -> error $ "prettyPrintOTerm: unhandled term `" ++ show term ++ "'"
		where
			thisPrec = prettyPrintOTermPrec term
			paren str = showParen (prec > thisPrec) (showString str) ""

			subTerm prec slice@(subIndex, _)
				| subIndex > index = "?" ++ show slice
				| subIndex `elem` noDescendSlices = prettyPrintOSlice prec terms inputName slice
				| otherwise = prettyPrintSubterm prec terms inputName noDescendSlices inPrint slice
			subTermInPrint prec slice@(subIndex, _)
				| subIndex > index = "?" ++ show slice
				| subIndex `elem` noDescendSlices = prettyPrintOSlice prec terms inputName slice
				| otherwise = prettyPrintSubterm prec terms inputName noDescendSlices True slice

			showParam (TeakParamType typ) = "type " ++ showTypeName [] typ
			showParam (TeakParamInt int) = show int
			showParam (TeakParamString str) = "\"" ++ escDoubleQuote str ++ "\""

			builtinName = id

	prettyPrintOTerms :: Int -> (Int -> ShowS) -> [(Int, TeakOTerm)] -> [String]
	prettyPrintOTerms _ _ [] = []
	prettyPrintOTerms prec inputName terms = mapMaybe showTerm sharedSlices ++
		[prettyPrintOTerm prec terms inputName sharedSlices False (last terms)]
		where
			sharedSlices = map snd $ filter ((> 1) . fst) $ frequencyBy (==) $
				map oSliceIndex $ concatMap (oTermExtractSlices . snd) terms

			showTerm 0 = Nothing
			showTerm i
				| isNothing term = Just $ "t" ++ show i ++ " = bad slice ref"
				| otherwise = Just $ "t" ++ show i ++ " = " ++
					prettyPrintOTerm 0 terms inputName sharedSlices False (i, fromJust term)
				where term = lookup i terms

	instance Show NetworkLinkRef where
		showsPrec _ (Link link) = showString "L" . shows link

	instance Show NetworkCompRef where
		showsPrec _ (Comp comp) = showString "C" . shows comp

	instance Read NetworkLinkRef where
		readsPrec _ = readParen False readNetworkLinkRef

	instance Read NetworkLinkUsage where
		readsPrec _ = readParen False readNetworkLinkUsage

	instance Read NetworkCompRef where
		readsPrec _ = readParen False readNetworkCompRef

	instance Show NetworkLink where
		showsPrec prec (NetworkLink index width latching) = showParen (prec > applyPrecedence) $
			showString "NetworkLink " . shows (Link index) . space . shows width . space . showsPrec 11 latching

	instance Show Latching where
		showsPrec prec (HalfBuffer depth) = showParen (prec > applyPrecedence) $
			showString "HalfBuffer " . shows depth

	instance Read NetworkLink where
		readsPrec _ = readParen False readNetworkLink

	instance Read Latching where
		readsPrec _ = readParen False readLatching

	instance Show NetworkComp where
		showsPrec prec comp = showsPrecTab prec 0 comp

	instance Show TeakCompType where
		showsPrec prec comp = showsPrecTab prec 0 comp

	instance ShowTab TeakOTerm where
		showListTab = showListWithComment showDropListWith False (shows . (+1))

	instance Show NetworkLinkUsage where
		showsPrec prec (NetworkLinkUsage passive active) = showParen (prec > applyPrecedence) $
			showString "NetworkLinkUsage " . showsPrec 11 passive . space . showsPrec 11 active

	instance Show TeakOTerm where
		showsPrec prec term = showParen (prec > applyPrecedence) $ body term
			where
				body (TeakOConstant width value) = showString "TeakOConstant " . shows width . space . shows value
				body (TeakOAppend count slices) = showString "TeakOAppend " . shows count . space . shows slices
				body (TeakOBuiltin builtin width params slices) = showString "TeakOBuiltin " . shows builtin .
					space . shows width . space . shows params . space . shows slices
				body (TeakOp op slices) = showString "TeakOp " . shows op . space . shows slices
				body (TeakOMux spec slices) = showString "TeakOMux " . shows spec . space . shows slices

	instance ShowTab NetworkComp where
		showsPrecTab prec tabs (TeakComp i typ links pos) = showParen (prec > applyPrecedence) $
			showString "TeakComp " . shows (Comp i) . space . showsPrecTab 11 (tabs + 1) typ . space . shows links .
				space . showsPrec 11 pos
		showsPrecTab prec _ (InstanceComp i name ports links pos) = showParen (prec > applyPrecedence) $
			showString "InstanceComp " . shows (Comp i) . space . shows name . space . shows ports . space .
				shows links .  space . showsPrec 11 pos

	instance ShowTab TeakCompType where
		showListTab = showListWithComment showBlockListWith False shows
		showsPrecTab prec tabs typ = showParen (prec > applyPrecedence) $ body typ
			where
				body TeakA = showString "TeakA"
				body (TeakF offsets) = showString "TeakF " . shows offsets
				body TeakJ = showString "TeakJ"
				body TeakM = showString "TeakM"
				body (TeakO terms) = showString "TeakO " . showsPrecTab 11 tabs terms
				body (TeakS slice matches) = showString "TeakS " . showsPrec 11 slice . space . shows matches
				body (TeakX matches) = showString "TeakX " . shows matches
				body (TeakV name width bOffsets wOffsets rOffsets) = showString "TeakV " .
					shows name . space . shows width . space .
					shows bOffsets . space . shows wOffsets . space . shows rOffsets
				body TeakI = showString "TeakI"
				body TeakR = showString "TeakR"

	instance Read NetworkComp where
		readsPrec _ = readParen False readNetworkComp

	instance Show NetworkPort where
		showsPrec prec (NetworkPort name direction width ref) = showParen (prec > applyPrecedence) $
			showString "NetworkPort " . shows name . space .
				shows direction . space . shows width . space . shows ref
			where
				shows :: Show a => a -> ShowS
				shows = showsPrec (applyPrecedence + 1)

	instance Read NetworkPort where
		readsPrec _ = readParen False readNetworkPort

	instance ShowTab NetworkLink where
		showListTab = showBlockList

	instance ShowTab NetworkLinkUsage where
		showListTab = showBlockList

	instance ShowTab NetworkLinkRef where
		showListTab = showBlockList

	instance Show Access where
		showsPrec prec (Access ref bodys) = showParen (prec > applyPrecedence) $
			showString "Access " . showsPrec 11 ref . space . showList bodys
	instance ShowTab Access where
		showListTab = showBlockList

	instance Read Access where
		readsPrec _ = readParen False readAccess

	instance Read AccessBody where
		readsPrec _ = readParen False readAccessBody

	readAccess :: String -> [(Access, String)]
	readAccess str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"Access" -> readFields fields (Access GoAccess []) rest
				where fields = [
					ReadField "accessRef" readsC (\o f -> o { accessRef = f }),
					ReadField "accessBodys" readListC (\o f -> o { accessBodys = f }) ]
			_ -> Nothing

	initLink :: NetworkLinkRef
	initLink = Link 0

	readAccessBody :: String -> [(AccessBody, String)]
	readAccessBody str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"ChanInputAccess" -> readFields fields (ChanInputAccess initLink initLink) rest
				where fields = [
					ReadField "chanAccessGo" readsC (\o f -> o { chanAccessGo = f }),
					ReadField "chanAccessDone" readsC (\o f -> o { chanAccessDone = f }) ]
			"ChanOutputAccess" -> readFields fields (ChanOutputAccess initLink initLink) rest
				where fields = [
					ReadField "chanAccessGo" readsC (\o f -> o { chanAccessGo = f }),
					ReadField "chanAccessDone" readsC (\o f -> o { chanAccessDone = f }) ]
			"ChanBareOutputAccess" -> readFields fields (ChanBareOutputAccess initLink) rest
				where fields = [
					ReadField "chanAccessData" readsC (\o f -> o { chanAccessData = f }) ]
			"ChanBareInputAccess" -> readFields fields (ChanBareInputAccess initLink) rest
				where fields = [
					ReadField "chanAccessData" readsC (\o f -> o { chanAccessData = f }) ]
			"VarAccess" -> readFields fields (VarAccess Read initLink initLink emptySlice) rest
				where fields = [
					ReadField "accessRW" readsC (\o f -> o { accessRW = f }),
					ReadField "accessCtrl" readsC (\o f -> o { accessCtrl = f }),
					ReadField "accessData" readsC (\o f -> o { accessData = f }),
					ReadField "accessRange" readsC (\o f -> o { accessRange = f }) ]
			"SharedCallAccess" -> readFields fields (SharedCallAccess initLink initLink) rest
				where fields = [
					ReadField "accessGo" readsC (\o f -> o { accessGo = f }),
					ReadField "accessDone" readsC (\o f -> o { accessDone = f }) ]
			"PortLinkAccess" -> readFields fields (PortLinkAccess Passive initLink) rest
				where fields = [
					ReadField "accessSense" readsC (\o f -> o { accessSense = f }),
					ReadField "accessLink" readsC (\o f -> o { accessLink = f }) ]
			_ -> Nothing

	instance Show AccessBody where
		showsPrec prec accessBody = showParen (prec > applyPrecedence) (body accessBody)
			where
				shows e = space . showsPrec 11 e

				body (ChanInputAccess go done) = showString "ChanInputAccess" . shows go . shows done
				body (ChanOutputAccess go done) = showString "ChanOutputAccess" . shows go . shows done
				body (ChanBareOutputAccess dat) = showString "ChanBareOutputAccess" . shows dat
				body (ChanBareInputAccess dat) = showString "ChanBareInputAccess" . shows dat
				body (VarAccess rw ctrlLink dataLink range) =
					showString "VarAccess" . shows rw . shows ctrlLink . shows dataLink . shows range
				body (SharedCallAccess go done) = showString "SharedCallAccess" . shows go . shows done
				body (PortLinkAccess sense link) = showString "PortLinkAccess" . shows sense . shows link

	instance ShowTab NetworkPort where
		showListTab = showBlockList

	instance ShowTab TeakParam

	instance Show NetworkProperty where
		showsPrec prec comp = showsPrecTab prec 0 comp

	instance ShowTab NetworkProperty where
		showsPrecTab prec _ (NetworkComment str) = showParen (prec > applyPrecedence) $
			showString "NetworkComment " . shows str
		showsPrecTab prec tabs (NetworkPosArray poss) = showParen (prec > applyPrecedence)
			$ showString "NetworkPosArray " . showsPrecTab 11 tabs poss
		showsPrecTab prec _ (NetworkLinkNames names) = showParen (prec > applyPrecedence)
			$ showString "NetworkLinkNames (" . shows names . showString ")"
		showsPrecTab prec tabs (NetworkAttrs attrs) = showParen (prec > applyPrecedence)
			$ showString "NetworkAttrs " . showDropListWith showAttr tabs attrs
			where
				showAttr tabs (name, value) = showString "(" . shows name .
					showString ", " . showsPrecTab 0 tabs value . showString ")"

	readNetworkLinkRef :: String -> [(NetworkLinkRef, String)]
	readNetworkLinkRef str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"Link" -> readFields fields (Link 0) rest
				where fields = [
					ReadField "nwLink" readsC (\o f -> o { nwLink = f }) ]
			'L':num | all isDigit num -> return (Link $ read num, rest)
			_ -> Nothing

	readNetworkCompRef :: String -> [(NetworkCompRef, String)]
	readNetworkCompRef str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"Comp" -> readFields fields (Comp 0) rest
				where fields = [
					ReadField "nwComp" readsC (\o f -> o { nwComp = f }) ]
			'C':num | all isDigit num -> return (Comp $ read num, rest)
			_ -> Nothing

	readNetworkLink :: String -> [(NetworkLink, String)]
	readNetworkLink str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"NetworkLink" -> readFields fields (NetworkLink 0 0 (HalfBuffer 0)) rest
				where fields = [
					ReadField "nwLinkIndex" readsC (\o f -> o { nwLinkIndex = nwLink f }),
					ReadField "nwLinkWidth" readsC (\o f -> o { nwLinkWidth = f }),
					ReadField "nwLinkLatching" readsC (\o f -> o { nwLinkLatching = f }) ]
			_ -> Nothing

	readLatching :: String -> [(Latching, String)]
	readLatching str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"HalfBuffer" -> readFields fields (HalfBuffer 0) rest
				where fields = [
					ReadField "latchingDepth" readsC (\o f -> o { latchingDepth = f }) ]
			_ -> Nothing

	readNetworkLinkUsage :: String -> [(NetworkLinkUsage, String)]
	readNetworkLinkUsage str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"NetworkLinkUsage" -> readFields fields nwNoLinkUsage rest
				where fields = [
					ReadField "nwPassiveLinkUsage" readsC (\o f -> o { nwPassiveLinkUsage = f }),
					ReadField "nwActiveLinkUsage" readsC (\o f -> o { nwActiveLinkUsage = f }) ]
			_ -> Nothing

	readNetworkComp :: String -> [(NetworkComp, String)]
	readNetworkComp str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"TeakComp" -> readFields fields (TeakComp 0 TeakA [] NoPos) rest
				where fields = [
					ReadField "nwCompIndex" readsC (\o f -> o { nwCompIndex = nwComp f }),
					ReadField "nwTeakType" readsC (\o f -> o { nwTeakType = f }),
					ReadField "nwCompLinks" readListC (\o f -> o { nwCompLinks = f }),
					ReadField "nwCompPos" readsC (\o f -> o { nwCompPos = f }) ]
			"InstanceComp" -> readFields fields (InstanceComp 0 "" [] [] NoPos) rest
				where fields = [
					ReadField "nwCompIndex" readsC (\o f -> o { nwCompIndex = nwComp f }),
					ReadField "nwPartName" readsC (\o f -> o { nwPartName = f }),
					ReadField "nwCompPorts" readListC (\o f -> o { nwCompPorts = f }),
					ReadField "nwCompLinks" readListC (\o f -> o { nwCompLinks = f }),
					ReadField "nwCompPos" readsC (\o f -> o { nwCompPos = f }) ]
			_ -> Nothing

	readNetworkPort :: String -> [(NetworkPort, String)]
	readNetworkPort str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"NetworkPort" -> readFields fields (NetworkPort "" Input 0 Nothing) rest
				where fields = [
					ReadField "nwPortName" readsC (\o f -> o { nwPortName = f }),
					ReadField "nwPortDirection" readsC (\o f -> o { nwPortDirection = f }),
					ReadField "nwPortWidth" readsC (\o f -> o { nwPortWidth = f }),
					ReadField "nwPortRef" readsC (\o f -> o { nwPortRef = f }) ]
			_ -> Nothing

	instance Read TeakOTerm where
		readList = readListC
		readsPrec _ = readParen False readTeakOTerm

	readTeakOTerm :: String -> [(TeakOTerm, String)]
	readTeakOTerm str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"TeakOConstant" -> readFields fields (TeakOConstant 1 0) rest
				where fields = [
					ReadField "teakOWidth" readsC (\o f -> o { teakOWidth = f }),
					ReadField "teakOValue" readsC (\o f -> o { teakOValue = f }) ]
			"TeakOAppend" -> readFields fields (TeakOAppend 1 []) rest
				where fields = [
					ReadField "teakOCount" readsC (\o f -> o { teakOCount = f }),
					ReadField "teakOSlices" readListC (\o f -> o { teakOSlices = f }) ]
			"TeakOBuiltin" -> readFields fields (TeakOBuiltin "" 0 [] []) rest
				where fields = [
					ReadField "teakOBuiltin" readsC (\o f -> o { teakOBuiltin = f }),
					ReadField "teakOWidth" readsC (\o f -> o { teakOWidth = f }),
					ReadField "teakOParams" readListC (\o f -> o { teakOParams = f }),
					ReadField "teakOSlices" readListC (\o f -> o { teakOSlices = f }) ]
			"TeakOp" -> readFields fields (TeakOp TeakOpAdd []) rest
				where fields = [
					ReadField "teakOOp" readsC (\o f -> o { teakOOp = f }),
					ReadField "teakOSlices" readListC (\o f -> o { teakOSlices = f }) ]
			"TeakOMux" -> readFields fields (TeakOMux [] []) rest
				where fields = [
					ReadField "teakOSpec" readsC (\o f -> o { teakOSpec = f }),
					ReadField "teakOSlices" readListC (\o f -> o { teakOSlices = f }) ]
			_ -> Nothing

	newtype {- NetworkIF network => -}
		NetworkMonad network a = NetworkMonad { networkMonadM :: State network a }

	instance NetworkIF network => Monad (NetworkMonad network) where
		l >>= k = NetworkMonad $ (networkMonadM l) >>= k'
			where k' v = networkMonadM $ k v
		return r = NetworkMonad $ return r
		fail str = NetworkMonad $ fail str

	class ShowTab network => NetworkIF network where
		nwAddAccess :: Access -> NetworkMonad network ()
		nwAddComp :: NetworkComp -> NetworkMonad network NetworkComp
		nwAddLink :: NetworkLink -> NetworkMonad network NetworkLink
		nwGetAccesses :: NetworkMonad network [Access]
		nwGetComp :: NetworkCompRef -> NetworkMonad network (Maybe NetworkComp)
		nwGetCompBounds :: NetworkMonad network (Int,Int)
		nwGetLink :: NetworkLinkRef -> NetworkMonad network NetworkLink
		nwGetLinkBounds :: NetworkMonad network (Int,Int)
		nwSetProperties :: [NetworkProperty] -> NetworkMonad network ()
		nwGetProperties :: NetworkMonad network [NetworkProperty]
		nwGetLinkUsage :: Sense -> NetworkLinkRef -> NetworkMonad network (Maybe NetworkLinkConn)
		nwLinkIsValid :: NetworkLinkRef -> NetworkMonad network Bool
		nwNewNetwork :: network
		nwRemoveAccess :: AccessRef -> NetworkMonad network (Maybe Access)
		nwRemoveCompRef :: NetworkCompRef -> NetworkMonad network ()
		nwRemoveLinkRef :: NetworkLinkRef -> NetworkMonad network (Maybe NetworkLink)
		nwUpdateComp :: NetworkComp -> NetworkMonad network ()
		nwUpdateLink :: NetworkLink -> NetworkMonad network ()

	data Part network = Part {
		networkName :: String,
		networkPorts :: [NetworkPort],
		networkBody :: network }

	nwAddProperty :: NetworkIF network => NetworkProperty -> NetworkMonad network ()
	nwAddProperty prop = do
		props <- nwGetProperties
		nwSetProperties (prop:props)

	nwFindPartIndex :: [Part network] -> String -> Maybe Int
	nwFindPartIndex parts name = findIndex ((== name) . networkName) parts

	nwFindPart :: [Part network] -> String -> Maybe (Part network)
	nwFindPart parts name = do
		i <- nwFindPartIndex parts name
		return $ parts !! i

	nwAddLinkRef :: NetworkIF network => NetworkLink -> NetworkMonad network NetworkLinkRef
	nwAddLinkRef link = liftM refLink $ nwAddLink link

	-- nwNewLink : create and add new push link of the given width
	nwNewLink :: NetworkIF network => Int -> NetworkMonad network NetworkLink
	nwNewLink width = nwAddLink $ NetworkLink 0 width $ HalfBuffer 0

	nwNewLinkRef :: NetworkIF network => Int -> NetworkMonad network NetworkLinkRef
	nwNewLinkRef width = liftM refLink $ nwNewLink width

	nwAddPortAccess :: NetworkIF network => Sense -> AccessRef -> NetworkLinkRef -> NetworkMonad network ()
	nwAddPortAccess sense ref link = nwAddAccess $ Access ref [PortLinkAccess sense link]

	nwGetLinkWidth :: NetworkIF network => NetworkLinkRef -> NetworkMonad network Int
	nwGetLinkWidth ref = liftM nwLinkWidth $ nwGetLink ref

	nwGetLinkLatching :: NetworkIF network => NetworkLinkRef -> NetworkMonad network Latching
	nwGetLinkLatching ref = liftM nwLinkLatching $ nwGetLink ref

	portAccessLinks :: Access -> [NetworkLinkRef]
	portAccessLinks access = map accessLink (accessBodys access)

	nwMapComps :: NetworkIF network => (NetworkComp -> NetworkMonad network r) -> NetworkMonad network [r]
	nwMapComps f = liftM reverse $ nwFoldComps f' []
		where f' acc comp = do
			r <- f comp
			return (r:acc)

	nwMapLinks :: NetworkIF network => (NetworkLink -> NetworkMonad network r) -> NetworkMonad network [r]
	nwMapLinks f = do
		linkBounds <- nwGetLinkBounds
		liftM catMaybes $ mapM f' (range linkBounds)
		where
			f' i = do
				unused <- nwLinkIsUnused (Link i)
				if unused
					then return Nothing
					else do
						decl <- nwGetLink (Link i)
						liftM Just $ f decl

	nwMapLinks_ :: NetworkIF network => (NetworkLink -> NetworkMonad network r) -> NetworkMonad network ()
	nwMapLinks_ f = nwMapLinks f >> return ()

	nwFoldLinks :: NetworkIF network => (a -> NetworkLink -> NetworkMonad network a) -> a -> NetworkMonad network a
	nwFoldLinks f a = do
		linkBounds <- nwGetLinkBounds
		foldM f' a (range linkBounds)
		where
			f' a i = do
				unused <- nwLinkIsUnused (Link i)
				if unused
					then return a
					else do
						decl <- nwGetLink (Link i)
						f a decl

	nwRemoveComp :: NetworkIF network => NetworkComp -> NetworkMonad network ()
	nwRemoveComp = nwRemoveCompRef . refComp

	nwMapCompsIf_ :: NetworkIF network =>
		(NetworkComp -> NetworkMonad network Bool) ->
		(NetworkComp -> NetworkMonad network r) -> NetworkMonad network ()
	nwMapCompsIf_ p f = do
		nwMapCompsIf p f
		return ()

	nwMapCompsIf :: NetworkIF network =>
		(NetworkComp -> NetworkMonad network Bool) ->
		(NetworkComp -> NetworkMonad network r) -> NetworkMonad network [r]
	nwMapCompsIf p f = liftM reverse $ nwFoldComps f' []
		where
			f' ret comp = do
				predResult <- p comp
				if predResult
					then do
						r <- f comp
						return (r:ret)
					else return ret

	nwFoldCompsIf :: NetworkIF network =>
		(NetworkComp -> NetworkMonad network Bool) ->
		(a -> NetworkComp -> NetworkMonad network a) -> a -> NetworkMonad network a
	nwFoldCompsIf p f a = nwFoldComps f' a
		where
			f' a comp = do
				predResult <- p comp
				if predResult
					then f a comp
					else return a

	nwGetPortAccess :: NetworkIF network => AccessRef -> NetworkMonad network [NetworkLinkRef]
	nwGetPortAccess ref = do
		access <- nwGetAccess ref
		return (maybe [] portAccessLinks access)

	nwRemovePortAccess :: NetworkIF network => AccessRef -> NetworkMonad network [NetworkLinkRef]
	nwRemovePortAccess ref = do
		access <- nwRemoveAccess ref
		return (maybe [] portAccessLinks access)

	nwLinkIsPort :: NetworkIF network => NetworkLinkRef -> NetworkMonad network Bool
	nwLinkIsPort link = do
		p <- linkIsPort Passive
		a <- linkIsPort Active
		return $ p || a
		where
			linkIsPort sense = do
				usage <- nwGetLinkUsage sense link
				if isJust usage
					then do
						let maybeRef = isAccessLink (fromJust usage)
						if isJust maybeRef
							then do
								access <- nwGetAccess (fromJust maybeRef)
								return $ isPortLinkAccess $ fromJust access
							else return False
					else return False

			isAccessLink (LinkAccess ref _) = Just ref
			isAccessLink _ = Nothing

			isPortLinkAccess (Access _ [PortLinkAccess {}]) = True
			isPortLinkAccess _ = False

	isLinkComp :: Maybe NetworkLinkConn -> Bool
	isLinkComp (Just (LinkComp {})) = True
	isLinkComp _ = False

	nwLinkToComp :: NetworkIF network => Sense -> NetworkLinkRef ->
		NetworkMonad network (Maybe (NetworkComp, [Int]))
	nwLinkToComp sense link = do
		conn <- nwGetLinkUsage sense link
		if isLinkComp conn
			then do
				let Just (LinkComp compNo addr) = conn
				comp <- nwGetComp compNo
				when (isNothing comp) $
					error $ "nwLinkToCompUsage: no component at compNo " ++ show compNo
				return $ Just (fromJust comp, addr)
			else return Nothing

	nwLinkToCompIf :: NetworkIF network => Sense ->
		(NetworkComp -> NetworkMonad network Bool) ->
		NetworkLinkRef -> NetworkMonad network (Maybe (NetworkComp, [Int]))
	nwLinkToCompIf sense pred link = do
		usage <- nwLinkToComp sense link
		if isJust usage
			then do
				let Just (comp, _) = usage
				predResult <- pred comp
				if predResult
					then return usage
					else return Nothing
			else return Nothing

	-- nwReplaceLinkAtConn : replace the link referenced by conn with the new given link
	nwReplaceLinkAtConn :: NetworkIF network => NetworkLinkConn -> NetworkLinkRef ->
		NetworkMonad network ()
	nwReplaceLinkAtConn conn link = do
		case conn of
			LinkComp compNo addr -> do
				Just comp <- nwGetComp compNo
				nwUpdateComp (nwReplaceLinkInComp comp addr link)
			LinkAccess ref addr -> do
				Just access <- nwGetAccess ref
				nwRemoveAccess ref
				nwAddAccess (nwReplaceLinkInAccess access addr link)
			NoConn -> error "nwReplaceLinkAtConn: no conn"
		return ()

	-- nwBreakLink : break the given link into 2 halves.  The active component end will be
	--	the original link, and passive component end will be a new link.  Returns (active,passive)
	--	link pair with their passive and active ends. respectively, unconnected.  The passive end
	--	component will also be replaced with a new component
	-- NB. At the moment this only works where both ends are connected to components
	nwBreakLink :: NetworkIF network => NetworkLinkRef -> NetworkMonad network (NetworkLinkRef, NetworkLinkRef)
	nwBreakLink link = do
		width <- nwGetLinkWidth link
		newLink <- nwNewLinkRef width

		passiveUsage <- nwGetLinkUsage Passive link
		case passiveUsage of
			Just conn -> nwReplaceLinkAtConn conn newLink
			Nothing -> error $ "nwBreakLink: passive end not connected " ++ show link
		return (link, newLink)

	nwNewTeakComp :: NetworkIF network => TeakCompType -> [Some NetworkLinkRef] -> Pos ->
		NetworkMonad network NetworkComp
	nwNewTeakComp typ links pos = nwAddComp $ TeakComp 0 typ links pos

	nwGetPosArray :: NetworkIF network => NetworkMonad network (Maybe PosArray)
	nwGetPosArray = do
		properties <- nwGetProperties
		let npa = find isPosArray properties
		if isJust npa
			then do
				let Just (NetworkPosArray pa) = npa
				return $ Just pa
			else return Nothing
		where
			isPosArray (NetworkPosArray _) = True
			isPosArray _ = False

	nwGetLinkNames :: NetworkIF network => NetworkMonad network (Maybe (Map.Map NetworkLinkRef String))
	nwGetLinkNames = do
		properties <- nwGetProperties
		return $ do
			NetworkLinkNames names <- find isLinkNames properties
			return names

	isLinkNames :: NetworkProperty -> Bool
	isLinkNames (NetworkLinkNames {}) = True
	isLinkNames _ = False

	nwGetLinkName :: NetworkIF network => NetworkLinkRef -> NetworkMonad network (Maybe String)
	nwGetLinkName link = runMaybeT $ do
		linkNames <- MaybeT $ nwGetLinkNames
		MaybeT $ return $ Map.lookup link linkNames

	nwSetLinkName :: NetworkIF network => NetworkLinkRef -> String -> NetworkMonad network ()
	nwSetLinkName link name = do
		properties <- nwGetProperties
		let
			NetworkLinkNames linkNames = fromMaybe (NetworkLinkNames Map.empty) $ find isLinkNames properties
			linkNames' = Map.insert link name linkNames

		nwSetProperties $ NetworkLinkNames linkNames' : filter (not . isLinkNames) properties

	nwGetAttributeVal :: NetworkIF network => String -> NetworkMonad network (Maybe TeakParam)
	nwGetAttributeVal attrName = do
		properties <- nwGetProperties
		return $ do
			NetworkAttrs attrList <- find isAttrList properties
			(_, val) <- find ((== attrName) . fst) attrList
			return val
		where
			isAttrList (NetworkAttrs _) = True
			isAttrList _ = False

	nwRemoveUnusedLinks :: NetworkIF network => NetworkMonad network ()
	nwRemoveUnusedLinks = do
		linkBounds <- nwGetLinkBounds
		mapM_ removeUnusedLink (range linkBounds)
		where
			removeUnusedLink i = do
				unused <- nwLinkIsUnused (Link i)
				when unused $ do	
					nwRemoveLinkRef (Link i)
					return ()

	nwRemapLink :: NetworkIF network => NetworkLinkRef -> NetworkLinkRef -> NetworkMonad network ()
	nwRemapLink from to = do
		activeFrom <- nwGetLinkUsage Active from
		passiveFrom <- nwGetLinkUsage Passive from
		when (isJust activeFrom) $ nwReplaceLinkAtConn (fromJust activeFrom) to
		when (isJust passiveFrom) $ nwReplaceLinkAtConn (fromJust passiveFrom) to
		nwRemoveLinkRef from
		return ()

	nwFoldComps :: NetworkIF network => (a -> NetworkComp -> NetworkMonad network a)
		-> a -> NetworkMonad network a
	nwFoldComps f acc = do
		compBounds <- nwGetCompBounds
		foldM visitComp acc (range compBounds)
		where
			visitComp a i = do
				comp <- nwGetComp $ Comp i
				if isJust comp
					then do
						a' <- f a (fromJust comp)
						return a'
					else return a

	nwLinkIsUnused :: NetworkIF network => NetworkLinkRef -> NetworkMonad network Bool
	nwLinkIsUnused link = do
		pas <- nwGetLinkUsage Passive link
		act <- nwGetLinkUsage Active link
		return $ isNothing pas && isNothing act

	nwGetAccess :: NetworkIF network => AccessRef -> NetworkMonad network (Maybe Access)
	nwGetAccess ref = do
		accesses <- nwGetAccesses
		return $ nwFindAccess accesses ref

	nwFindAccess :: [Access] -> AccessRef -> Maybe Access
	nwFindAccess accesses ref = find (nwMatchAccess ref) accesses

	nwFindPortIndexByRef :: [NetworkPort] -> AccessRef -> Maybe Int
	nwFindPortIndexByRef ports ref = findIndex ((== Just ref) . nwPortRef) ports

	nwFindPortByRef :: [NetworkPort] -> AccessRef -> Maybe NetworkPort
	nwFindPortByRef ports ref = nwFindPortIndexByRef ports ref >>= return . (ports !!)

	nwMatchAccess :: AccessRef -> Access -> Bool
	nwMatchAccess ref1 (Access ref2 _) = ref1 == ref2

	showPart :: (Int -> Int -> network -> ShowS) ->
		Int -> Int -> Part network -> ShowS
	showPart showNetwork prec tabs network = showParen (prec > applyPrecedence) (showBody network)
		where
			showBody (Part name ports body) = showString "Part " .
				tabbedNL tabs . showChar '{' .
				tabbedNL tabs' . showString "networkName = " . shows name . showChar ',' .
				tabbedNL tabs' . showString "networkPorts = " . showListTab tabs' ports . showChar ',' .
				tabbedNL tabs' . showString "networkBody = " . showNetwork prec' tabs' body .
				tabbedNL tabs . showChar '}'

			tabs' = tabs + 1
			prec' = applyPrecedence + 1

	readPart :: Read network => ReadS (Part network)
	readPart str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"Part" -> readFields fields (Part "" [] undefined) rest
				where fields = [
					ReadField "networkName" readsC (\o f -> o { networkName = f }),
					ReadField "networkPorts" readListC (\o f -> o { networkPorts = f }),
					ReadField "networkBody" readsC (\o f -> o { networkBody = f }) ]
			_ -> Nothing

	instance (Read network) => Read (Part network) where
		readsPrec _ = readParen False readPart

	instance ShowTab network => ShowTab (Part network) where
		showsPrecTab = showPart showsPrecTab
		showListTab = showBlockList

	instance ShowTab network => Show (Part network) where
		showsPrec prec = showsPrecTab prec 0

	runNetwork :: NetworkIF network => network -> NetworkMonad network a -> (a, network)
	runNetwork nw nm = runState (networkMonadM nm) nw

	runNetwork_ :: NetworkIF network => network -> NetworkMonad network a -> network
	runNetwork_ nw nm = snd $ runNetwork nw nm

	runNetwork0 :: NetworkIF network => NetworkMonad network a -> (a, network)
	runNetwork0 nm = runNetwork nwNewNetwork nm

	tryNetwork :: NetworkIF network => network -> NetworkMonad network a -> a
	tryNetwork nw nm = fst $ runNetwork nw nm

	runPart :: NetworkIF network =>	
		Part network -> NetworkMonad network r -> (r, Part network)
	runPart (Part name ports body) f = (r, Part name ports body')
		where (r, body') = runNetwork body f

	runPart_ :: NetworkIF network =>
		Part network -> NetworkMonad network r -> Part network
	runPart_ part f = snd $ runPart part f

	tryPart :: NetworkIF network =>
		Part network -> NetworkMonad network r -> r
	tryPart part f = fst $ runPart part f

	nwFuncToNetworkMonad :: NetworkIF network => (network -> a) -> NetworkMonad network a
	nwFuncToNetworkMonad f = NetworkMonad $ do
		nw <- get
		return $ f nw

	addConn :: String -> NetworkLinkConn -> NetworkLinkConn -> NetworkLinkConn
	addConn _ NoConn new = new
	addConn _ old NoConn = old
	addConn msg old new
		| old /= new = error $ "addConn: " ++ msg ++ " " ++ show old ++ " " ++ show new
		| otherwise = new

	addConnUnsafe :: NetworkLinkConn -> NetworkLinkConn -> NetworkLinkConn
	addConnUnsafe NoConn new = new
	addConnUnsafe old NoConn = old
	addConnUnsafe old _ = old

	addUsagePair :: String -> NetworkLinkUsage -> NetworkLinkUsage -> NetworkLinkUsage
	addUsagePair msg (NetworkLinkUsage pas1 act1) (NetworkLinkUsage pas2 act2) =
		NetworkLinkUsage (addConn (msg ++ "(P)") pas1 pas2) (addConn (msg ++ "(A)") act1 act2)

	addUsagePairUnsafe :: NetworkLinkUsage -> NetworkLinkUsage -> NetworkLinkUsage
	addUsagePairUnsafe (NetworkLinkUsage pas1 act1) (NetworkLinkUsage pas2 act2) =
		NetworkLinkUsage (addConnUnsafe pas1 pas2) (addConnUnsafe act1 act2)

	removeConn :: NetworkLinkConn -> NetworkLinkConn -> NetworkLinkConn
	removeConn old NoConn = old
	removeConn old rm
		| old /= rm = error $ "removeConn: " ++ show old ++ " " ++ show rm
		| otherwise = NoConn

	removeUsagePair :: NetworkLinkUsage -> NetworkLinkUsage -> NetworkLinkUsage
	removeUsagePair (NetworkLinkUsage pas1 act1) (NetworkLinkUsage pas2 act2) =
		NetworkLinkUsage (removeConn pas1 pas2) (removeConn act1 act2)

	addUsages :: [(Int, NetworkLinkUsage)] -> (Int, NetworkLinkUsage)
	addUsages usages@((i, _):_) = (i, foldl' (addUsagePair "") nwNoLinkUsage $ map snd usages)
	addUsages [] = error "addUsages: no usages"

	makeLinkUsage :: [NetworkComp] -> [Access] -> [(Int, NetworkLinkUsage)]
	makeLinkUsage comps accesses = mixedUsages
		where
			rawCompUsages = concatMap nwCompLinkUsage comps
			rawAccessUsages = concatMap (nwAccessLinkUsage 0) accesses
			mixedUsages = map addUsages $ groupBy eqFst $ sortBy compareFst $ rawAccessUsages ++ rawCompUsages

	showPosForNetwork :: Maybe PosArray -> Pos -> String
	showPosForNetwork (Just posArray) = showPos (Just posArray)
	showPosForNetwork Nothing = showPos noPosContext

	showPrettyCompRef :: NetworkCompRef -> ShowS
	showPrettyCompRef (Comp compNo) = showChar 'C' . shows compNo -- . showChar 'o'

	showPrettyLinkRef :: NetworkLinkRef -> ShowS
	showPrettyLinkRef (Link linkNo) = showChar 'L' . shows linkNo -- . showChar 'o'

	embrace :: String -> String -> ShowS -> ShowS
	embrace before after localShows = showString before . localShows . showString after

	comment :: ShowS -> ShowS
	comment = embrace "(-- " " --)"

	quote :: String -> ShowS
	quote string = shows string

	paren :: ShowS -> ShowS
	paren = embrace " (" ")"

	space :: ShowS
	space = showChar ' '

	dot :: ShowS
	dot = showChar '.'

	showPrettyPort :: NetworkIF network => NetworkPort -> NetworkMonad network ShowS
	showPrettyPort (NetworkPort name dir width maybeAccess) = do
		accesses <- accessShows
		return $ showDirection dir . space . quote name . showString " : " .
			shows width .  showString " bits" . accesses
		where
			accessShows
				| isJust maybeAccess = do
					access <- nwGetAccess $ fromJust maybeAccess
					case access of
						Just (Access ref [PortLinkAccess _ link]) ->
							return $ (paren $ showString "link " . showPrettyLinkRef link) . case ref of
								GoAccess -> showString " -- go"
								DoneAccess -> showString " -- done"
								_ -> id
						_ -> return id
				| otherwise = return id

	showPrettyLink :: NetworkIF network => [NetworkPort] -> NetworkLink -> NetworkMonad network ShowS
	showPrettyLink ports link@(NetworkLink i width _) = do
		pas <- liftM (fromMaybe NoConn) $ nwGetLinkUsage Passive $ refLink link
		act <- liftM (fromMaybe NoConn) $ nwGetLinkUsage Active $ refLink link
		let hasConn = NetworkLinkUsage pas act /= nwNoLinkUsage

		pasShows <- showPrettyLinkConn ports pas
		actShows <- showPrettyLinkConn ports act

		return $ showString "link " . showPrettyLinkRef (Link i) . showString " : " .
			shows width . showString " bits" . (if hasConn
				then paren $ showString "path " . actShows . showString " -> " . pasShows
				else id)

	showPrettyLinkConn :: NetworkIF network => [NetworkPort] -> NetworkLinkConn ->
		NetworkMonad network ShowS
	showPrettyLinkConn _ NoConn = return $ showString "unconnected"
	showPrettyLinkConn _ (LinkComp (Comp _) []) = return $ showString "no port given"
	showPrettyLinkConn _ (LinkComp compRef@(Comp compNo) (portNo:portIndices)) = do
		maybeComp <- nwGetComp compRef
		case maybeComp of
			Just comp -> do
				let portName = nwCompPortNames comp !! portNo
				return $ showPrettyCompRef compRef . dot . showString portName . (if null portIndices then
					id else shows portIndices)
			_ -> return $ comment $ showString "comp not found: " . shows compNo . shows (portNo:portIndices)
		where
	showPrettyLinkConn ports (LinkAccess ref _) = return $ fromMaybe (shows ref) $ do
		NetworkPort { nwPortName = name } <- nwFindPortByRef ports ref
		return $ showString "\"" . showString name . showString "\""

	showPrettyTeakCompParameters :: NetworkIF network => [Some NetworkLinkRef] -> TeakCompType ->
		NetworkMonad network ShowS
	showPrettyTeakCompParameters conns typ = body typ
		where
			paramParen params = showString " #(" . showString (joinWith ", " params) . showString ")"

			slice :: Slice Int -> String
			slice slice = show slice

			list ls = "[" ++ joinWith "," ls ++ "]"

			body TeakJ = do
				inpWidths <- mapM nwGetLinkWidth inps
				outWidth <- nwGetLinkWidth out
				return $ paramParen [show outWidth, list (map show inpWidths)]
				where [Many inps, One out] = conns
			body TeakM = do
				inpWidths <- mapM nwGetLinkWidth inps
				outWidth <- nwGetLinkWidth out
				return $ paramParen [show outWidth, list (map show inpWidths)]
				where [Many inps, One out] = conns
			body (TeakF offsets) = do
				inpWidth <- nwGetLinkWidth inp
				outWidths <- mapM nwGetLinkWidth outs
				let outSlices = zipWith (+:) offsets outWidths
				return $ paramParen [show inpWidth, list (map slice outSlices)]
				where [One inp, Many outs] = conns
			body (TeakV name width builtinOffsets writeOffsets readOffsets) = do
				wgWidths <- mapM nwGetLinkWidth wgs
				rdWidths <- mapM nwGetLinkWidth rds
				let
					writeSlices = map slice $ zipWith (+:) writeOffsets wgWidths
					readSlices = map slice $ zipWith (+:) readOffsets rdWidths
				return $ paramParen [quote name "",
					show width, list (map show builtinOffsets), list writeSlices, list readSlices]
				where [Many wgs, _, _, Many rds] = conns
			body (TeakS matchSlice matches) = do
				inpWidth <- nwGetLinkWidth inp
				outWidths <- mapM nwGetLinkWidth outs
				let
					showMatch (_, outWidth, (imps, offset)) = "([" ++
						joinWith "," (map (showImp True True 4) imps) ++
						"]," ++ slice (offset +: outWidth) ++ ")"
				return $ paramParen [show inpWidth,
					slice matchSlice, list (map showMatch $ zip3 outs outWidths matches)]
				where [One inp, Many outs] = conns
			body (TeakX matches) = do
				width <- nwGetLinkWidth out
				let
					showMatch (_, imps) = "[" ++
						joinWith "," (map (showImp True True 4) imps) ++
						"]"
				return $ paramParen [show width,
					list (map showMatch $ zip inps matches)]
				where [Many inps, One _sel, One out] = conns
			body (TeakO {}) = return $ paramParen ["(-- FIXME --)"]
			body _ = return $ id -- A I R

	showPrettyComp :: NetworkIF network => NetworkComp -> NetworkMonad network ShowS
	showPrettyComp comp = body comp
		where
			body (TeakComp compNo typ conns _) = do
				paramShows <- showPrettyTeakCompParameters conns typ
				return $ showString "teak." . showString (nwCompShortName comp) . space .
					showPrettyCompRef (Comp compNo) . paramShows . showLinks (nwCompPortNames comp) conns
			body (InstanceComp compNo part compPorts compLinks _) = do
				return $ showString "part " . quote part .
					showPrettyCompRef (Comp compNo) .
					showLinks (map nwPortName compPorts) compLinks

			showLink link = showPrettyLinkRef link ""

			showLinks portNames compLinks = paren $
				showListWithSep (showString ", ") showPort (zip portNames compLinks)
				where
					showPort (name, someLinks) =
						showChar '.' . showString name . embrace "(" ")" (
						showString (showSomeHaskell showLink someLinks))

	showPrettyNetwork :: NetworkIF network => Bool -> Int -> Int -> Part network -> ShowS
	showPrettyNetwork _ prec tabs part@(Part partName ports _) = tryPart part $ do
		portShows <- mapM showPrettyPort ports		
		linkShows <- nwMapLinks (showPrettyLink ports)
		compShows <- nwMapComps showPrettyComp
		let
			sPNBody = showString "part " . quote partName . showString " (" .
				tabbedNL tabs' .
				showListWithSep (tabbedNL tabs') id portShows .
				tabbedNL tabs . showString ") is" .
				tabbedNL tabs' .
				showListWithSep (tabbedNL tabs') id linkShows .
				tabbedNL tabs . showString "begin" .
				tabbedNL tabs' .
				showListWithSep (tabbedNL tabs') id compShows .
				{-
				showChar ',' .
				tabbedNL tabs' . showString "networkAccesses = " .
				showListTab tabs' accesses .
				showChar ',' .
				tabbedNL tabs' . showString "networkProperties = " .
				showListTab tabs' properties .
				-}
				tabbedNL tabs . showString "end"

		return $ showParen (prec > applyPrecedence) sPNBody
		where
			tabs' = tabs + 1

	-- trimPart : make a new part which only contains the components referenced from comps
	trimPart :: NetworkIF network => Part network -> [NetworkCompRef] -> Part network
	trimPart (Part name ports nw) comps = uncurry ($) $ runNetwork nw $ do
		nwMapComps removeComp
		ports' <- liftM catMaybes $ mapM removePort ports
		return $ Part name ports'
		where
			removeComp comp = when (refComp comp `notElem` comps) $ nwRemoveComp comp

			removePort port@(NetworkPort { nwPortRef = Just ref }) = runMaybeT $ do
				access <- MaybeT $ nwGetAccess ref
				let Access _ [PortLinkAccess _ link] = access
				keepPort <- lift $ connectedToComp link
				if keepPort
					then return port
					else do
						lift $ nwRemoveAccess ref
						fail ""
			removePort _ = return Nothing

			connectedToComp link = do
				act <- nwGetLinkUsage Active link
				pas <- nwGetLinkUsage Passive link
				return $ isLinkComp act || isLinkComp pas

	writeNetworkFile :: NetworkIF network => Bool -> String -> String -> [Part network] -> IO ()
	writeNetworkFile pretty flatArgs teakFilename parts = do
		teakFile <- openFile teakFilename WriteMode
		if pretty
			then do
				hPutStrLn teakFile "-- Teak output file"
				when (flatArgs /= "") $ do
					hPutStrLn teakFile $ "-- from command: teak " ++ flatArgs ++ "\n"
				mapM_ (\nw -> do
					hPutStr teakFile $ showPrettyNetwork True 0 0 nw ""
					hPutStr teakFile "\n\n") parts
			else do
				hPutStrLn teakFile "-- Teak output file"
				when (flatArgs /= "") $ do
					hPutStrLn teakFile $ "-- from command: teak " ++ flatArgs ++ "\n"
				mapM_ (\part -> hPutStrLn teakFile $ showsPrecTab 0 0 part "") parts
		hClose teakFile

	readNetworkFile :: (Read network, NetworkIF network) => String -> WhyT IO [Part network]
	readNetworkFile teakFilename = do
		contents <- whyTReadFile NoPos teakFilename 
		let (parts, trailing) = readBareListC contents -- :: ([Part network], String)
		if skipComments trailing == ""
			then return parts
			else failPos PosTopLevel $ "can't parse Teak network file: `" ++ teakFilename
				++ "', remaining: `" ++ take 100 trailing ++ "'"

	-- uniquifyPart : starting from part `topLevel', make each InstanceComp instantiate a unique
	--	part (copied from an element in `parts').  Returns the newly created parts.
	uniquifyPart :: NetworkIF network => [Part network] -> String -> [Part network]
	uniquifyPart originalParts topLevelName = snd $ uniquifyPartBody [] topLevel topLevelName
		where
			Just topLevel = nwFindPart originalParts topLevelName

			uniquifyPartBody counts part newName = (counts', newParts ++ [renamedPartCopy])
				where
					((counts', newParts), renamedPartCopy) = runPart (part { networkName = newName }) uniquifyInstances
					uniquifyInstances = nwFoldCompsIf (return . isInstanceComp) uniquifyInstance (counts, [])

			uniquifyInstance (counts, accumNewParts) comp = do
				nwUpdateComp $ comp { nwPartName = newPartName }
				let (counts'', newParts) = uniquifyPartBody counts' part newPartName
				return (counts'', accumNewParts ++ newParts)
				where
					partName = nwPartName comp
					Just part = nwFindPart originalParts partName
					count = if null oldCount
						then (1 :: Int)
						else (snd (head oldCount)) + 1
					(oldCount, otherCount) = partition ((== partName) . fst) counts
					counts' = (partName, count) : otherCount
					newPartName = partName ++ if count == 1 then "" else "_v" ++ show count

	nwCheckComp :: NetworkIF network => NetworkComp -> WhyT (NetworkMonad network) ()
	nwCheckComp comp = decorateErrorsT pos' $ do
		let links = nwCompLinks comp
		gatherFailMap checkLink $ concatMap flattenSome links
		gatherFailMap checkPortStruct links
		checkComp comp
		return ()
		where
			pos' = PosLabel NoPos $ "C" ++ show (nwCompIndex comp)

			checkPortStruct (One {}) = return ()
			checkPortStruct (Many []) = fail "arrayed port with no connections"
			checkPortStruct (Many (_:_)) = return ()
			checkPortStruct _ = fail badPortStructure

			checkLink link = do
				valid <- lift $ nwLinkIsValid link
				when (not valid) $ fail $ "invalid link" ++ show link

			checkSlice maxWidth slice
				| offset < 0 = fail "slice offset < 0"
				| offset + width > maxWidth = fail "slice top exceeds bounding width"
				| width == 0 && offset /= 0 = fail "token slice not at offset 0"
				| otherwise = return ()
				where
					offset = sliceOffset slice
					width = sliceWidth slice

			checkComp (InstanceComp {}) = return () -- FIXME
			checkComp (TeakComp { nwTeakType = TeakA, nwCompLinks = [Many inps, One out] })
				| length inps /= 2 = fail "wrong number of inputs, must be exactly 2"
				| otherwise = do
					inpWidths <- lift $ mapM nwGetLinkWidth inps
					outWidth <- lift $ nwGetLinkWidth out
					when (not (all (== outWidth) inpWidths)) $ fail "input and output widths must match"
			checkComp (TeakComp { nwTeakType = TeakF offsets, nwCompLinks = [One inp, Many outs] })
				| length offsets /= length outs = fail "offset list and outputs must be the same length"
				| otherwise = do
					inpWidth <- lift $ nwGetLinkWidth inp
					outWidths <- lift $ mapM nwGetLinkWidth outs
					let	
						checkFOffset (i, offset, width) = decorateErrorsT pos $ checkSlice inpWidth (offset +: width)
							where pos = PosLabel NoPos $ "out[" ++ show i ++ "]"
					gatherFailMap checkFOffset $ zip3 [(0::Int)..] offsets outWidths
					return ()
			checkComp (TeakComp { nwTeakType = TeakJ, nwCompLinks = [Many inps, One out] }) = do
				outWidth <- lift $ nwGetLinkWidth out
				inpWidths <- lift $ mapM nwGetLinkWidth inps
				when (outWidth /= sum inpWidths) $ fail "output width must equal sum of input widths"
				return ()
			checkComp (TeakComp { nwTeakType = TeakM, nwCompLinks = [Many inps, One out] }) = do
				inpWidths <- lift $ mapM nwGetLinkWidth inps
				outWidth <- lift $ nwGetLinkWidth out
				when (not (all (== outWidth) inpWidths)) $ fail "input and output widths must match"
			checkComp (TeakComp { nwTeakType = TeakO terms, nwCompLinks = [One _inp, One _out] })
				| length terms == 0 = fail "must have at least one term"
				| otherwise = return () -- FIXME
			checkComp (TeakComp { nwTeakType = TeakS selSlice terms, nwCompLinks = [One inp, Many outs] })
				| length terms /= length outs = fail "offset/imps list and outputs must be the same length"
				| otherwise = do
					inpWidth <- lift $ nwGetLinkWidth inp
					outWidths <- lift $ mapM nwGetLinkWidth outs
					let
						checkTerm (i, (_imps, offset), width) = decorateErrorsT pos $
							checkSlice inpWidth (offset +: width)
							where pos = PosLabel NoPos $ "out[" ++ show i ++ "]"
					gatherFailMap id [
						decorateErrorsT (PosLabel NoPos $ "select slice") $ checkSlice inpWidth selSlice,
						(gatherFailMap checkTerm $ zip3 [(0::Int)..] terms outWidths) >> return ()
						]
					return ()
					-- FIXME, check slice and coverage?
			checkComp (TeakComp { nwTeakType = TeakX terms, nwCompLinks = [Many inps, One _, One out] })
				| length terms /= length inps = fail "imps list and inputs must be the same length"
				| otherwise = do
					outWidth <- lift $ nwGetLinkWidth out
					inpWidths <- lift $ mapM nwGetLinkWidth inps
					when (not (all (== outWidth) inpWidths)) $ fail "input and output widths must match"
					return ()
					-- FIXME, check slice and coverage?
			checkComp (TeakComp { nwTeakType = TeakV _ varWidth _bOffsets wOffsets rOffsets,
				nwCompLinks = [Many wgs, Many wds, Many rgs, Many rds] })
				| length wOffsets /= length wgs || length wOffsets /= length wds =
					fail "write offset/write port length mismatch"
				| length rOffsets /= length rgs || length rOffsets /= length rds =
					fail "read offset/write port length mismatch"
				| otherwise = do
					-- FIXME, check bOffsets
					wgWidths <- lift $ mapM nwGetLinkWidth wgs
					wdWidths <- lift $ mapM nwGetLinkWidth wds
					rgWidths <- lift $ mapM nwGetLinkWidth rgs
					rdWidths <- lift $ mapM nwGetLinkWidth rds

					let
						checkWrite (i, offset, width) = decorateErrorsT pos $ checkSlice varWidth (offset +: width)
							where pos = PosLabel NoPos $ "write[" ++ show i ++ "]"

						checkRead (i, offset, width) = decorateErrorsT pos $ checkSlice varWidth (offset +: width)
							where pos = PosLabel NoPos $ "read[" ++ show i ++ "]"

					gatherFailMap id [
						when (not (all (== 0) wdWidths)) $ fail "some write dones not token links",
						when (not (all (== 0) rgWidths)) $ fail "some read gos not token links",
						when (not (all (/= 0) wgWidths)) $ fail "some write gos are token links",
						when (not (all (/= 0) rdWidths)) $ fail "some read dones are token links",
						gatherFailMap checkWrite (zip3 [(0::Int)..] wOffsets wgWidths) >> return (),
						gatherFailMap checkRead (zip3 [(0::Int)..] rOffsets rdWidths) >> return ()
						]
					return ()
			checkComp (TeakComp { nwTeakType = TeakI, nwCompLinks = [One inp, One out] }) = do
				inpWidth <- lift $ nwGetLinkWidth inp
				outWidth <- lift $ nwGetLinkWidth out
				gatherFailMap id [
					when (inpWidth /= 0) $ fail "input must be a token link",
					when (outWidth /= 0) $ fail "output must be a token link"
					]
				return ()
			checkComp (TeakComp { nwTeakType = TeakR, nwCompLinks = [One out] }) = do
				outWidth <- lift $ nwGetLinkWidth out
				when (outWidth /= 0) $ fail "output must be a token link"
			checkComp (TeakComp {}) = fail badPortStructure

			badPortStructure = "bad port structure"

	checkPart :: NetworkIF network => Part network -> Completeness
	checkPart part = comp
		where
			Why comp _ = decorateErrors pos $ gatherFail $ tryPart part $ nwMapComps (runWhyT . nwCheckComp)

			pos = PosLabel PosTopLevel $ networkName part

	runWhyTPart :: NetworkIF network => Part network -> WhyT (NetworkMonad network) r -> Why (r, Part network)
	runWhyTPart part nm = Why comp (ret, part')
		where (Why comp ret, part') = runPart part $ runWhyT nm

	runWhyTPart_ :: NetworkIF network => Part network -> WhyT (NetworkMonad network) r -> Why (Part network)
	runWhyTPart_ part nm = Why comp part'
		where (Why comp _, part') = runPart part $ runWhyT nm

	funcActualToTeakParam :: FuncActual -> TeakParam
	funcActualToTeakParam (TypeFuncActual typ) = TeakParamType typ
	funcActualToTeakParam (ExprFuncActual _ expr) = exprToTeakParam expr

	exprToTeakParam :: Expr -> TeakParam
	exprToTeakParam (ValueExpr _ _ (StringValue str)) = TeakParamString str
	exprToTeakParam (ValueExpr _ _ (IntValue val)) = TeakParamInt val
	exprToTeakParam expr = error $ "exprToTeakParam: bad expr " ++ show expr

	teakParamToFuncActual :: TeakParam -> FuncActual
	teakParamToFuncActual (TeakParamInt int) = ExprFuncActual False
		(ValueExpr undefined undefined (IntValue int))
	teakParamToFuncActual (TeakParamString str) = ExprFuncActual False
		(ValueExpr undefined undefined (StringValue str))
	teakParamToFuncActual (TeakParamType typ) = TypeFuncActual typ
