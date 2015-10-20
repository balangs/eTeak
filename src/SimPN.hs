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

module SimPN (
	makePartPN,
	makeTopLevelPartPN,
	runPN,
	runMkPNMonad0,
	stepPN,
	teakR,
	showUntypedSimValue,
	PN (..),
	Place (..),
	pnToDot,
	pnTestCard,
	dotGraphGroupNodesByLabel,
	writeHLNet,
	showMarkingElem,
	showPNExpr,
	PNExpr (..),
	MkPNMonad,
	MkPN (..),
	ReqAckPair,
	Chan,
	Marking,
	PlaceNo,
	TransNo,
	Trans (..),
	PlaceType (..),
	testPNs
	) where

	import Misc
	import SimTypes
	import SimBuiltin
	import ParseTree hiding (Chan)
	import NetParts
	import Bits
	import Dot
	import Show
	import State
	import GuiSupport (colours, black)

	import qualified Data.Map as DM
	import Data.List
	import Data.Bits
	import Control.Monad.State
	import Data.Maybe
	import Data.Char
	import System.IO

	type PlaceNo = Int
	type TransNo = Int

	data Place = Place {
		placeLabel :: [String],
		placeType :: PlaceType }
	data Trans m value = Trans {
		transLabel :: [String],
		transInPlaces :: [PlaceNo],
		transOutPlaces :: [PlaceNo],
		transCondition :: Maybe PNExpr,
		transExprs :: [PNExpr],
		transReport :: ([value] -> m ()) }

	data PlaceType = BlackPlaceType | NumericPlaceType Int
		deriving (Show, Eq)

	instance Show Place where
		showsPrec prec (Place label typ) =
			showParen (prec > 10) $ showString "Place " . shows label . space . shows typ

	instance Show (Trans m value) where
		showsPrec prec (Trans label inPlaces outPlace cond exprs _) =
			showParen (prec > 10) $ showString "Trans " . shows label . space .
				shows inPlaces . space . shows outPlace . space . shows cond . shows exprs

	type Marking value = DM.Map PlaceNo [value]

	joinLabel :: [String] -> String
	joinLabel = joinWith "." . reverse

	showMarkingElem :: PN SimFlow SimValue -> (PlaceNo, [SimValue]) -> ShowS
	showMarkingElem pn (placeNo, tokens) = showString "Place " . shows placeNo . space . showString label .
		showString ": " . shows (length tokens) . (if length tokens > 1 then showString "!!!" else id) .
		space . showString (plural "token" tokens) . showString " [" .
		showListWithSep (showString ", ") (showString . showUntypedSimValue) tokens .
		showString "]"
		where label = joinLabel $ placeLabel $ (pnPlaces pn) DM.! placeNo

	data {- Monad m => -} PN m value = PN {
		pnPlaces :: DM.Map PlaceNo Place,
		pnTrans :: DM.Map TransNo (Trans m value),
		pnInitialMarking :: Marking value }
		deriving Show

	data {- Monad m => -} MkPN m value = MkPN {
		mkpnLabelStack :: [String],
		mkpnPlaceCount :: Int,
		mkpnTransCount :: Int,
		mkpnPlaces :: DM.Map PlaceNo Place,
		mkpnTrans :: DM.Map TransNo (Trans m value),
		mkpnMarking :: DM.Map PlaceNo [value] }

	type MkPNMonad m value a = State (MkPN m value) a

	data PNExpr =
		  PNLiteral SimValue
		| PNBool Bool
		| PNInput Int
		| PNBitfield (Slice Int) PNExpr
		| PNInsertBitfield (Slice Int) PNExpr PNExpr
		| PNOp [(Int, TeakOTerm)] PNExpr
		| PNAppend [Int] [PNExpr]
		| PNMatch (Slice Int) [Implicant] PNExpr
		deriving Show

	space :: ShowS
	space = showChar ' '

	pnExprPrec :: PNExpr -> Int
	pnExprPrec (PNOp terms _) = prettyPrintOTermPrec $ snd $ last terms
	pnExprPrec (PNLiteral {}) = 11
	pnExprPrec (PNBool {}) = 11
	pnExprPrec (PNInput {}) = 11
	pnExprPrec _ = 10

	showPNExpr :: Int -> PNExpr -> ShowS
	showPNExpr prec expr = showParen (prec > thisPrec) $ body expr
		where
			body (PNLiteral NoSimValue) = showString "token"
			body (PNLiteral (SimValue int [])) = shows int
			body (PNBool bool) = shows bool
			body (PNInput inp) = showString "i" . shows inp
			body (PNBitfield slice expr) = showPNExpr (thisPrec + 1) expr . verilogShowSlice slice
			body (PNInsertBitfield slice expr1 expr2) = showString "insert " . paren (verilogShowSlice slice .
				showString "," . showPNExpr 0 expr1 . showString "," . showPNExpr 0 expr2)
			body (PNOp terms expr) = if listAtLeastLength 2 termStrs	
				then showString "let " .  showListWithSep (showString ";") showString (init termStrs) .
					showString " in " . showString (last termStrs)
				else showString (last termStrs)
				where
					termStrs = prettyPrintOTerms 0 (\prec -> showPNExpr prec expr) terms
			body (PNAppend widths exprs) = case exprs' of
				[] -> showString "token"
				[expr] -> showPNExpr prec expr
				_ -> showChar '{' . showListWithSep (showString ", ") (showPNExpr 0) exprs' .
					showChar '}'
				where
					exprs' = map fst $ filter nonZeroWidth $ zip exprs widths

					nonZeroWidth (_, 0) = False
					nonZeroWidth _ = True

			body (PNMatch slice imps expr) = showString "match " . paren (verilogShowSlice slice . showString "," .
				showString "[" .
				showString (joinWith "," $ map (showImp True True 4) imps) .
				showString "]," .
				showPNExpr 0 expr)
			body expr = showString "?" . shows expr

			thisPrec = pnExprPrec expr
			paren = showParen True

	hlShowOTerms :: [(Int, TeakOTerm)] -> PNExpr -> ShowS
	hlShowOTerms terms expr = snd $ last termShows
		where
			getTerm :: Int -> TeakOTerm
			getTerm termNo = fromJust $ lookup termNo terms

			termShows :: [(Int, ShowS)]
			termShows = (0, hlShowPNExpr expr) : mapSnd showTerm terms

			subTerm :: TeakOSlice -> ShowS
			subTerm (termNo, slice)
				| termNo /= 0 && isConstant rawTerm = shows $
					extractBitfield slice constValue
				| termNo /= 0 && sliceWidth slice == resultWidth = hlShr term $ sliceOffset slice
				| otherwise = hlBitfield slice term
				where
					isConstant (TeakOConstant {}) = True
					isConstant _ = False

					TeakOConstant _ constValue = rawTerm

					term = fromJust $ lookup termNo termShows
					rawTerm = getTerm termNo
					resultWidth = oTermResultWidth rawTerm

			showTerm :: TeakOTerm -> ShowS
			showTerm (TeakOConstant _ int) = shows int
			showTerm (TeakOAppend count slices) = hlAppend
				(concat $ replicate count $ map oSliceWidth slices)
				(concat $ replicate count $ map subTerm slices)
			showTerm term@(TeakOBuiltin {}) -- FIXME
				| oTermResultWidth term == 0 = showString "dot"
				| otherwise = shows (0 :: Integer)
			showTerm (TeakOp op slices) = case (op, slices) of
				(TeakOpAdd, [l, r]) -> clip "+" l r
				(TeakOpSub, [l, r]) -> clip "-" l r
				(TeakOpAnd, [l, r]) -> logical "&" l r
				(TeakOpOr, [l, r]) -> logical "|" l r
				(TeakOpXor, [l, r]) -> logical "#" l r
				(TeakOpNot, [r]) -> hlInfixOp "-" (shows ((bit (oSliceWidth r) :: Integer) - 1)) (subTerm r)
				(TeakOpSignedGT, [l, r]) -> signedInEq ">" l r
				(TeakOpSignedGE, [l, r]) -> signedInEq ">=" l r
				(TeakOpUnsignedGT, [l, r]) -> hlBoolToBit $ bin ">" l r
				(TeakOpUnsignedGE, [l, r]) -> hlBoolToBit $ bin ">=" l r
				(TeakOpEqual, [l, r]) -> bin "=" l r
				(TeakOpNotEqual, [l, r]) -> bin "#" l r
				_ -> error $ "hlShowOTerms.showTerm: bad TeakOp `" ++ show op ++ "' with slices `" ++ show slices ++ "'"
				where
					width = oSliceWidth $ head slices
					signedInEq op l r = hlBoolToBit $ hlInfixOp op (padSigned l) (padSigned r)
					bin op l r = hlInfixOp op (subTerm l) (subTerm r)
					clip op l r = hlInfixOp "%" (hlInfixOp op
						(hlInfixOp "+" (showsBit (oSliceWidth l)) (subTerm l))
						(subTerm r)) (showsBit $ oSliceWidth l)

					logical op l r = foldl1' (hlInfixOp "+") $ map bitOp [0..width-1]
						where
							lStr = subTerm l
							rStr = subTerm r
							bitOp i = hlIf (hlInfixOp op (bitBool lStr i) (bitBool rStr i))
								(showsBit i) (shows (0::Int))

					-- padSigned : add 2^width to positive numbers to keep numbers is order and
					--	nice and positive for signed comparisons
					padSigned slice = hlInfixOp "+" (hlIf (bitBool str (width - 1))
						(shows (0::Int)) (showsBit width)) str
						where str = subTerm slice

					bitBool expr i = hlInfixOp ">="
						(hlInfixOp "%" expr (showsBit (i + 1))) (showsBit i)
			showTerm (TeakOMux spec (choiceSlice:slices)) =
				foldr ($) (shows (0::Int)) $ zipWith makeMatch spec slices
				where
					makeMatch imps inSlice = hlIf (hlImpsMatch choiceWidth imps choiceStr) (subTerm inSlice)
					choiceWidth = oSliceWidth choiceSlice
					choiceStr = subTerm choiceSlice
			showTerm term = error $ "hlShowOTerms.showTerm: bad TeakOTerm `" ++ show term ++ "'"

	hlShowPNExpr :: PNExpr -> ShowS
	hlShowPNExpr expr = body expr
		where
			body (PNLiteral NoSimValue) = showString "dot"
			body (PNLiteral (SimValue int [])) = shows int
			body (PNBool True) = showString "tt"
			body (PNBool False) = showString "ff"
			body (PNInput inp) = showChar 'i' . shows inp
			body (PNBitfield slice expr) = hlBitfield slice (subExpr expr)
			body (PNInsertBitfield slice expr1 expr2) = foldl1' (hlInfixOp "+") $ catMaybes [
				if offset == 0 then Nothing else Just $ hlBitfield (0 +: offset) shows1,
				Just $ hlInfixOp "-" shows1 $ hlInfixOp "%" shows1 $ showsBit $ sliceHigh slice + 1,
				Just $ hlShl shows2 offset ]
				where	
					shows1 = subExpr expr1
					shows2 = subExpr expr2
					offset = sliceOffset slice
			body (PNAppend widths exprs) = hlAppend widths $ map subExpr exprs
			body (PNMatch slice imps expr) = hlImpsMatch (sliceWidth slice) imps $ hlBitfield slice $ subExpr expr
			body (PNOp terms expr) = hlShowOTerms terms expr
			body expr = error $ "body: can't show PNExpr `" ++ show expr ++ "'"

			subExpr expr = hlShowPNExpr expr

	hlImpsMatch :: Width -> [Implicant] -> ShowS -> ShowS
	hlImpsMatch width imps exprStr = foldl1' (hlInfixOp "|") $ concatMap makeMatch imps
		where
			makeMatch (Imp v 0) = [hlInfixOp "=" exprStr (shows v)]
			makeMatch (Imp v dc) = map (testSubSlice v) $ bitmaskToIntervals $ bitNot width dc

			testSubSlice v careSlice = hlInfixOp "=" (hlBitfield careSlice exprStr)
				(shows (extractBitfield careSlice v))

	showsBit :: Int -> ShowS
	showsBit dist = shows (bit dist :: Integer)

	hlInfixOp :: String -> ShowS -> ShowS -> ShowS
	hlInfixOp op l r = showParen True $ l . space . showString op . space . r

	hlBoolToBit :: ShowS -> ShowS
	hlBoolToBit cond = hlIf cond (shows (1::Int)) (shows (0::Int))

	hlIf :: ShowS -> ShowS -> ShowS -> ShowS
	hlIf cond then_ else_ = showString "if(" . cond . showChar ',' . then_ . showChar ',' . else_ . showChar ')'

	hlAppend :: [Int] -> [ShowS] -> ShowS
	hlAppend _ [] = shows (0::Int)
	hlAppend widths exprs
		| null exprStrs = showString "dot"
		| otherwise = foldl1' (hlInfixOp "+") exprStrs
		where
			offsets = scanl (+) 0 widths
			exprStrs = mapMaybe maybeInsert $ zip3 exprs offsets widths

			maybeInsert (_, _, 0) = Nothing
			maybeInsert (expr, offset, _) = Just $ hlShl expr offset

	hlShl :: ShowS -> Int -> ShowS
	hlShl l 0 = l
	hlShl l dist = hlInfixOp "*" l (showsBit dist)

	hlShr :: ShowS -> Int -> ShowS
	hlShr l 0 = l
	hlShr l dist = hlInfixOp "/" l (showsBit dist)

	hlBitfield :: Slice Int -> ShowS -> ShowS
	hlBitfield slice inp = hlInfixOp "%"
		(hlShr inp (sliceOffset slice))
		(shows (bit (sliceWidth slice) :: Integer))

	evalPNExpr :: [SimValue] -> PNExpr -> SimFlow SimValue
	evalPNExpr inputs expr = body expr
		where
			body (PNLiteral value) = return value
			body (PNBool True) = return $ SimValue 1 []
			body (PNBool False) = return $ SimValue 0 []
			body (PNInput i) = return $ inputs !! i
			body (PNBitfield slice expr) = subExpr expr >>= return . getBitfield slice
			body (PNInsertBitfield slice expr1 expr2) = do
				val1 <- subExpr expr1
				val2 <- subExpr expr2
				return $ simInsertBitfield slice val1 val2
			body (PNOp terms expr) = do
				v <- subExpr expr
				results <- evalOTerms terms v
				let (_, result) = last results
				return result
			body (PNAppend widths exprs) = do
				vs <- mapM subExpr exprs
				return $ simAppendValues widths vs
			body (PNMatch slice imps expr) = do
				v <- subExpr expr
				case v of
					SimValue {} -> do
						let SimValue guard _ = simExtractBitfield slice v
						return $ SimValue (
							if any (impIsCoveredByImp (sliceWidth slice) (Imp guard 0)) imps
								then 1
								else 0) []
					_ -> return NoSimValue

			subExpr expr = evalPNExpr inputs expr

	runMkPNMonad0 :: Monad m => MkPNMonad m value a -> (a, PN m value)
	runMkPNMonad0 m = (ret, PN places trans marking)
		where
			trans = mkpnTrans pn'
			places = mkpnPlaces pn'
			marking = mkpnMarking pn'
			pn0 = MkPN [] 0 0 DM.empty DM.empty DM.empty
			(ret, pn') = runState m pn0

	newPlace :: Monad m => PlaceType -> [value] -> MkPNMonad m value PlaceNo
	newPlace typ initValues = do
		label <- getLabel
		let op mkpn = (placeNo, mkpn')
			where
				placeNo = mkpnPlaceCount mkpn + 1
				mkpn' = mkpn { mkpnPlaceCount = placeNo,
					mkpnPlaces = DM.insert placeNo (Place label typ) (mkpnPlaces mkpn),
					mkpnMarking = DM.insert placeNo initValues (mkpnMarking mkpn)
					}
		state op

	newBlackPlace :: Monad m => [value] -> MkPNMonad m value PlaceNo
	newBlackPlace = newPlace BlackPlaceType

	newNumericPlace :: Monad m => Width -> [value] -> MkPNMonad m value PlaceNo
	newNumericPlace 0 = newBlackPlace
	newNumericPlace width = newPlace (NumericPlaceType width)

	addTrans :: Monad m => Trans m value -> MkPNMonad m value TransNo
	addTrans trans
		| length (transExprs trans) /= length (transOutPlaces trans) = error $
			"addTrans: must be one expr per out place `" ++ show (transExprs trans) ++ "', for "
			++ show (length (transOutPlaces trans)) ++ " out places"
		| otherwise = state op
		where op mkpn = (transNo, mkpn')
			where
				transNo = mkpnTransCount mkpn + 1
				mkpn' = mkpn { mkpnTransCount = transNo, mkpnTrans = DM.insert transNo trans (mkpnTrans mkpn) }

	newTrans :: Monad m => [PlaceNo] -> [PlaceNo] -> [PNExpr] ->
		MkPNMonad m value TransNo
	newTrans inPlaces outPlaces exprs = do
		label <- getLabel
		addTrans $ Trans label inPlaces outPlaces Nothing exprs $ const $ return ()

	newReportTrans :: Monad m => [PlaceNo] -> [PlaceNo] -> [PNExpr] ->
		([value] -> m ()) -> MkPNMonad m value TransNo
	newReportTrans inPlaces outPlaces exprs report = do
		label <- getLabel
		addTrans $ Trans label inPlaces outPlaces Nothing exprs report

	newCondTrans :: Monad m => PNExpr -> [PlaceNo] -> [PlaceNo] -> [PNExpr] ->
		MkPNMonad m value TransNo
	newCondTrans cond inPlaces outPlaces exprs = do
		label <- getLabel
		addTrans $ Trans label inPlaces outPlaces (Just cond) exprs $ const $ return ()

	popPlaces :: Marking value -> [PlaceNo] -> (Marking value, [value])
	popPlaces marking placeNos = mapAccumL adjust marking placeNos
		where adjust marking placeNo = (DM.insert placeNo values marking, value)
			where value:values = marking DM.! placeNo

	pushPlaces :: Marking value -> [(PlaceNo, value)] -> Marking value
	pushPlaces marking placeNoValuePairs = foldl' adjust marking placeNoValuePairs
		where adjust marking (placeNo, value) = DM.adjust (++ [value]) placeNo marking

	pushLabel :: Monad m => String -> MkPNMonad m value () 
	pushLabel label = state op
		where op mkpn = ((), mkpn')
			where mkpn' = mkpn { mkpnLabelStack = label : mkpnLabelStack mkpn } 

	popLabel :: Monad m => MkPNMonad m value () 
	popLabel = state op
		where op mkpn = ((), mkpn')
			where mkpn' = mkpn { mkpnLabelStack = tail $ mkpnLabelStack mkpn } 

	getLabel :: Monad m => MkPNMonad m value [String]
	getLabel = state op
		where op mkpn = (mkpnLabelStack mkpn, mkpn)

	withLabel :: String -> MkPNMonad SimFlow SimValue a -> MkPNMonad SimFlow SimValue a
	withLabel label m = do
		pushLabel label
		ret <- m
		popLabel
		return ret

	type ReqAckPair = (PlaceNo, PlaceNo)
	type Chan = (ReqAckPair, ReqAckPair)

	-- FIXME Needs fairness for choice places
	-- stepPN :: (Show value, Monad m) => PN m value -> Marking value -> m (Bool, Marking value)
	stepPN :: PN SimFlow SimValue -> Marking SimValue -> SimFlow (Bool, Marking SimValue)
	stepPN (PN _ trans _) marking = do
		(progress, marking') <- foldM tryTrans (False, marking) $ DM.elems trans
		let unsafePlaces = filter (listAtLeastLength 2) $ DM.elems marking'
		when (not (null unsafePlaces)) $ do
			fail $ "Not safe " ++ show unsafePlaces
		return (progress, marking')
		where
			tryTrans (prevProgress, updatedMarking) trans
				| all inpCanFire inPlaces = do
					fire <- case transCondition trans of
						Nothing -> return True
						Just cond -> do
							v <- evalPNExpr inValues cond
							let ret = case v of
								SimValue 1 [] -> True
								_ -> False
							return ret
					if fire
						then do
							-- outValues <- transAction trans trans inValues
							outValues <- mapM (evalPNExpr inValues) $ transExprs trans
							transReport trans inValues
							when (length outValues /= length outPlaces) $ error $
								"stepPN: transition `" ++ joinLabel (transLabel trans) ++ "' out token length mismatch"
							return (True, pushPlaces poppedPlaces $ zip outPlaces outValues)
						else return (prevProgress, updatedMarking)
				| otherwise = return (prevProgress, updatedMarking)
				where
					inPlaces = transInPlaces trans
					outPlaces = transOutPlaces trans

					(poppedPlaces, inValues) = popPlaces updatedMarking inPlaces
					-- can I take a token now (to cover choice places), and before this step (to cover places
					--	which had a token added earlier this step)
					inpCanFire placeNo = listAtLeastLength 1 (marking DM.! placeNo) &&
						listAtLeastLength 1 (updatedMarking DM.! placeNo)
					-- Use this for faster, not cycle number correct results
					-- inpCanFire placeNo = listAtLeastLength 1 (updatedMarking DM.! placeNo)

	type Width = Int

	token :: SimValue
	token = NoSimValue -- SimValue 0 []

	-- chan :: Monad m => Int -> value -> MkPNMonad m value Chan
	chan :: Width -> Int -> MkPNMonad SimFlow SimValue Chan
	chan width depth = do
		inp <- pair
		out <- if depth == 0
			then return inp
			else foldM makeLatch inp [1..depth]
		return (out, inp)
		where
			makeLatch inp i = withLabel (show i) $ do
				out <- pair
				latch inp out
				return out

			pair = do
				req <- withLabel "r" $ newNumericPlace width []
				ack <- withLabel "a" $ newBlackPlace []
				return (req, ack)

	teakLink :: String -> NetworkLink -> MkPNMonad SimFlow SimValue Chan
	teakLink partName link
		| depth == 0 = withLabel linkName $ do
			inp <- withLabel "a" $ pair
			out <- withLabel "p" $ pair
			withLabel "L" $ linkTrans "L" inp out
			return (out, inp)
		| otherwise = withLabel linkName $ do
			inp <- withLabel "a" $ pair
			out <- withLabel "p" $ pair
			internalLinks <- mapM (\i -> withLabel ("i" ++ show i) $ pair) [0..depth]
			withLabel "A" $ linkTrans "A" inp (head internalLinks)
			withLabel "P" $ linkTrans "P" (last internalLinks) out
			mapM_ (\(i, inp, out) -> withLabel (show i) $ latch inp out) $ zip3
				[(1::Int)..] (init internalLinks) (tail internalLinks)
			return (out, inp)
		where
			HalfBuffer depth = nwLinkLatching link
			ref = refLink link
			width = nwLinkWidth link
			linkName = show ref

			linkTrans end (inpR, inpA) (outR, outA) = do
				reqPlace <- withLabel "r" $ newBlackPlace [token]
				ackPlace <- withLabel "a" $ newBlackPlace []
				withLabel "r" $ trans "R" [inpR, reqPlace] [outR, ackPlace]
				withLabel "a" $ trans "SPACER" [outA, ackPlace] [inpA, reqPlace]
				where
					trans event inp out = newReportTrans inp out
						[PNInput 0, PNLiteral token]
						$ \[v, _] -> do
							TimeState time <- simFlowGet TimeRef
							FileState handle <- simFlowGet LogFileRef
							lift $ hPutStrLn handle $ "#" ++ show time ++ " " ++ partName ++ "."
								++ linkName ++ "." ++ end ++ " " ++ event
								++ case v of
									SimValue int [] -> " " ++ show int
									_ -> ""
							return ()

			pair = do
				req <- withLabel "r" $ newNumericPlace width []
				ack <- withLabel "a" $ newBlackPlace []
				return (req, ack)

	latch :: ReqAckPair -> ReqAckPair -> MkPNMonad SimFlow SimValue ()
	latch (inpR, inpA) (outR, outA) = withLabel "L" $ do
		ackPlace <- withLabel "a" $ newBlackPlace [token]
		withLabel "r" $ newTrans [inpR, ackPlace] [outR, inpA] [PNInput 0, PNLiteral token]
		withLabel "a" $ newTrans [outA] [ackPlace] [PNLiteral token]
		return ()

	-- type MkPNMonad m value a = State (MkPN m value) a

	showUntypedSimValue :: SimValue -> String
	showUntypedSimValue NoSimValue = "token"
	showUntypedSimValue (SimValue int specials) = show int ++ if null specials
		then "" else " " ++ (joinWith " " $ map showSpecial specials)
		where
			showSpecial (i, StringSimValue str) = "\"" ++ str ++ "\"@" ++ show i
			showSpecial _ = "???"

	getBitfield :: Slice Int -> SimValue -> SimValue
	getBitfield _ NoSimValue = NoSimValue
	getBitfield slice value
		| isEmptySlice slice = token
		| otherwise = simExtractBitfield slice value

	pnBitfield :: Slice Int -> PNExpr -> PNExpr
	pnBitfield slice expr
		| isEmptySlice slice = PNLiteral NoSimValue
		| otherwise = PNBitfield slice expr

	teakR :: Chan -> MkPNMonad SimFlow SimValue ()
	teakR (_, (outR, _outA)) = withLabel "R" $ do
		initPlace <- withLabel "init" $ newBlackPlace [token]
		withLabel "or" $ newTrans [initPlace] [outR] [PNLiteral token]
		return ()

	teakI :: Chan -> Chan -> MkPNMonad SimFlow SimValue ()
	teakI ((inpR, inpA), _) (_, (outR, outA)) = withLabel "I" $ do
		reqPlace <- withLabel "r" $ newBlackPlace [token]
		withLabel "ir" $ newTrans [inpR, outA] [reqPlace, inpA] [PNInput 0, PNLiteral token]
		withLabel "or" $ newTrans [reqPlace] [outR] [PNLiteral token]
		return ()

	teakM :: Width -> [Chan] -> Chan -> MkPNMonad SimFlow SimValue ()
	teakM width inp (_, (outR, outA)) = withLabel "M" $ do
		reqPlace <- withLabel "r" $ newNumericPlace width []
		ackPlace <- withLabel "a" $ newBlackPlace []
		choicePlace <- withLabel "choice" $ newBlackPlace [token]
		withLabel "or" $ newTrans [reqPlace] [outR] [PNInput 0]
		withLabel "oa" $ newTrans [outA] [ackPlace] [PNLiteral token]
		let
			makeInp i ((inpR, inpA), _) = withLabel ("i" ++ show i) $ do
				inpAPlace <- withLabel "a" $ newBlackPlace []
				withLabel "r" $ newTrans [inpR, choicePlace] [inpAPlace, reqPlace] [PNLiteral token, PNInput 0]
				withLabel "a" $ newTrans [inpAPlace, ackPlace] [inpA, choicePlace] [PNLiteral token, PNLiteral token]
		zipWithM_ makeInp [(0::Int)..] inp

	teakS :: Width -> Slice Int -> [Slice Int] -> [[Implicant]] -> Chan -> [Chan] ->
		MkPNMonad SimFlow SimValue ()
	teakS inpWidth guardSlice outSlices impss ((inpR, inpA),_) out = withLabel "S" $ do
		reqPlace <- withLabel "r" $ newNumericPlace inpWidth []
		ackPlace <- withLabel "a" $ newBlackPlace []
		withLabel "ir" $ newTrans [inpR] [reqPlace] [PNInput 0]
		withLabel "ia" $ newTrans [ackPlace] [inpA] [PNLiteral token]
		let
			makeOut (_, (outR, outA)) (i, outSlice, imps) = withLabel ("o" ++ show i) $ do	
				withLabel "r" $ newCondTrans canFire [reqPlace] [outR] [pnBitfield outSlice (PNInput 0)]
				withLabel "a" $ newTrans [outA] [ackPlace] [PNLiteral token]
				where
					-- canFire inp = any (impIsCoveredByImp (sliceWidth guardSlice) (Imp guard 0)) imps
					canFire = PNMatch guardSlice imps $ PNInput 0

		zipWithM makeOut out $ zip3 [(0::Int)..] outSlices impss
		return ()

	teakF :: [Slice Int] -> Chan -> [Chan] -> MkPNMonad SimFlow SimValue ()
	teakF outSlices ((inpR, inpA), _) out = withLabel "F" $ do
		withLabel "r" $ newTrans [inpR] outR (map (\slice -> pnBitfield slice (PNInput 0)) outSlices)
		withLabel "a" $ newTrans outA [inpA] [PNLiteral token]
		return ()
		where
			(_, outActive) = unzip out
			(outR, outA) = unzip outActive

	teakJ :: [Width] -> [Chan] -> Chan -> MkPNMonad SimFlow SimValue ()
	teakJ inpWidths inp (_, (outR, outA)) = withLabel "J" $ do
		withLabel "r" $ newTrans inpR [outR] [PNAppend inpWidths (map PNInput [0..inpCount - 1])]
		withLabel "a" $ newTrans [outA] inpA (replicate (length inpA) (PNLiteral token))
		return ()
		where
			inpCount = length inp

			(inpPassive, _) = unzip inp
			(inpR, inpA) = unzip inpPassive

	teakA :: Width -> [Chan] -> Chan -> MkPNMonad SimFlow SimValue ()
	teakA width inp (_, (outR, outA)) = withLabel "A" $ do
		reqPlace <- withLabel "r" $ newNumericPlace width []
		ackPlace <- withLabel "a" $ newBlackPlace []
		choicePlace <- withLabel "choice" $ newBlackPlace [token]
		withLabel "or" $ newTrans [reqPlace] [outR] [PNInput 0]
		withLabel "oa" $ newTrans [outA] [ackPlace] [PNLiteral token]
		let
			makeInp i ((inpR, inpA), _) = withLabel ("i" ++ show i) $ do
				inpAPlace <- newBlackPlace []
				withLabel "r" $ newTrans [inpR, choicePlace] [inpAPlace, reqPlace] [PNLiteral token, PNInput 0]
				withLabel "a" $ newTrans [inpAPlace, ackPlace] [inpA, choicePlace] [PNLiteral token, PNLiteral token]
		zipWithM makeInp [(0::Int)..] inp
		return ()

	teakO :: [(Int, TeakOTerm)] -> Chan -> Chan -> MkPNMonad SimFlow SimValue ()
	teakO terms ((inpR, inpA), _) (_, (outR, outA)) = withLabel "O" $ do
		when (not isStop) $ do
			withLabel "r" $ newTrans [inpR] [outR] [PNOp terms (PNInput 0)]
			return ()
		withLabel "a" $ newTrans [outA] [inpA] [PNLiteral token]
		return ()
		where
			isStop = case last terms of
				(_, TeakOBuiltin { teakOBuiltin = "BalsaSimulationStop" }) -> True
				_ -> False

	teakV :: Width -> [Slice Int] -> [Slice Int] -> [Chan] -> [Chan] -> [Chan] -> [Chan] ->
		MkPNMonad SimFlow SimValue ()
	teakV width wSlices rSlices wg wd rg rd = withLabel "V" $ do
		idlePlaces <- mapM read $ zip4 [(0::Int)..] rSlices rg rd
		mapM_ (write idlePlaces) $ zip4 [(0::Int)..] wSlices wg wd
		where
			write idlePlaces (i, slice, ((goR, goA), _), (_, (doneR, doneA))) = withLabel ("w" ++ show i) $ do
				withLabel "r" $ newTrans (goR : idlePlaces) (doneR : idlePlaces)
					((PNLiteral token) : replicate readCount writeExpr)
				withLabel "a" $ newTrans [doneA] [goA] [PNLiteral token]
				where
					writeExpr
						| slice == 0 +: width = PNInput 0
						| otherwise = PNInsertBitfield slice (PNInput 1) (PNInput 0)

			read (i, slice, ((goR, goA), _), (_, (doneR, doneA))) = withLabel ("r" ++ show i) $ do
				idlePlace <- withLabel "idle" $ newNumericPlace width [initValue]
				busyPlace <- withLabel "busy" $ newNumericPlace width []
				withLabel "r" $ newTrans [goR, idlePlace] [doneR, busyPlace] [pnBitfield slice (PNInput 1), PNInput 1]
				withLabel "a" $ newTrans [doneA, busyPlace] [goA, idlePlace] [PNLiteral token, PNInput 1]
				return idlePlace

			readCount = length rg
			initValue = SimValue 0 []
			
	evalOTerms :: [(Int, TeakOTerm)] -> SimValue -> SimFlow [(Int, SimValue)]
	evalOTerms terms inp = do
		values <- foldM evalTerm [(0, inp)] terms
		return $ reverse values
		where
			getOSliceValue values (i, slice) = getBitfield slice $ fromJust $ lookup i values

			evalTerm :: [(Int, SimValue)] -> (Int, TeakOTerm) -> SimFlow [(Int, SimValue)]
			evalTerm values (i, term) = case term of
				TeakOBuiltin name _ params slices -> do
					value <- simBuiltinCall (map (getOSliceValue values) slices) (BuiltinCallExpr undefined name
						(map teakParamToFuncActual params) undefined undefined)
					return $ (i, value):values
				_ -> return $ (i, evalPureTerm values term) : values

			evalPureTerm _ (TeakOConstant _ value) = SimValue value []
			evalPureTerm values (TeakOAppend count slices) = simAppendValues
				(concat (replicate count widths)) (concat (replicate count inValues))
				where
					widths = map oSliceWidth slices
					inValues = map (getOSliceValue values) slices
			evalPureTerm values (TeakOp op slices) = opFunc sliceValues
				where
					binOp op [SimValue l _, SimValue r _] = SimValue (cropInt width (l `op` r)) []
					binOp _ _ = error "binOp: bad args"
					unsignedInEq op [SimValue l _, SimValue r _] = SimValue (boolToBit $ l `op` r) []
					unsignedInEq _ _ = error "unsignedInEq: bad args"
					signedInEq op [SimValue l _, SimValue r _] = SimValue
						(boolToBit $ (recoverSign width l) `op` (recoverSign width r)) []
					signedInEq _ _ = error "signedInEq: bad args"

					slice:_ = slices
					width = oSliceWidth slice
					sliceValues = map (getOSliceValue values) slices

					opFunc = case op of
						TeakOpAdd -> binOp (+)
						TeakOpSub -> binOp (-)
						TeakOpAnd -> binOp (.&.)
						TeakOpOr -> binOp (.|.)
						TeakOpNot -> \[SimValue l _] -> SimValue (bitNot width l) []
						TeakOpXor -> binOp xor
						TeakOpUnsignedGT -> unsignedInEq (>)
						TeakOpUnsignedGE -> unsignedInEq (>=)
						TeakOpEqual -> unsignedInEq (==)
						TeakOpNotEqual -> unsignedInEq (/=)
						TeakOpSignedGT -> signedInEq (>)
						TeakOpSignedGE -> signedInEq (>=)
			evalPureTerm values (TeakOMux impss slices@(choiceSlice:_)) = fromMaybe (SimValue 0 []) $
				liftM snd $ find match $ zip impss sliceValues
				where
					match (imps, _) = any (impIsCoveredByImp choiceWidth (Imp choiceValue 0)) imps
					(SimValue choiceValue _):sliceValues = map (getOSliceValue values) slices
					choiceWidth = oSliceWidth choiceSlice
			evalPureTerm _ term = error $ "putEvalTerm: unrecognised term `" ++ show term ++ "'"

	makeComp :: NetworkIF network => [Part network] -> Part network -> DM.Map NetworkLinkRef Chan ->
		NetworkComp -> MkPNMonad SimFlow SimValue ()
	makeComp _ part chans (TeakComp compNo teakTyp links _) = do
		let
			chanLinks = map (fmap (chans DM.!)) links
			linkWidths = tryPart part $ mapM' (mapM' nwGetLinkWidth . flattenSome) links
			partName = networkName part

		withLabel partName $ withLabel ("C" ++ show compNo) $ do
			case (teakTyp, chanLinks, linkWidths) of
				(TeakR, [One out], _) -> teakR out
				(TeakI, [One inp, One out], _) -> teakI inp out
				(TeakM, [Many inp, One out], [_, [width]]) -> teakM width inp out
				(TeakS guardSlice impsXOffsets, [One inp, Many out], [[inpWidth], outWidths]) -> let
					(impss, offsets) = unzip impsXOffsets
					outSlices = zipWith (+:) offsets outWidths
					in teakS inpWidth guardSlice outSlices impss inp out
				(TeakF offsets, [One inp, Many out], [_, outWidths]) ->
					teakF (zipWith (+:) offsets outWidths) inp out
				(TeakJ, [Many inp, One out], [inpWidths, _]) -> teakJ inpWidths inp out
				(TeakA, [Many inp, One out], [_, [width]]) -> teakA width inp out
				(TeakO terms, [One inp, One out], _) -> teakO terms inp out
				(TeakV _ width _ wOffsets rOffsets, [Many wg, Many wd, Many rg, Many rd],
					[wWidths, _, _, rWidths]) -> teakV width
						(zipWith (+:) wOffsets wWidths)
						(zipWith (+:) rOffsets rWidths) wg wd rg rd
				(_, _, _) -> error $ "makeComp: unrecognised component " ++ show teakTyp
	makeComp parts _ chans (InstanceComp _compNo partName ports links _) = do
		let
			parentLinks :: [Chan]
			parentLinks = flattenSome $ Some $ map (fmap (chans DM.!)) links
			Just part = nwFindPart parts partName

			connectLink port parentLink instanceLink = withLabel (nwPortName port) $ body
				(directionToSense $ nwPortDirection port) parentLink instanceLink
				where
					body Passive ((chanLinkPR, chanLinkPA), _) (_, (portLinkAR, portLinkAA)) = do
						withLabel "r" $ newTrans [chanLinkPR] [portLinkAR] [PNInput 0]
						withLabel "a" $ newTrans [portLinkAA] [chanLinkPA] [PNLiteral token]
					body Active (_, (chanLinkAR, chanLinkAA)) ((portLinkPR, portLinkPA), _) = do
						withLabel "r" $ newTrans [portLinkPR] [chanLinkAR] [PNInput 0]
						withLabel "a" $ newTrans [chanLinkAA] [portLinkPA] [PNLiteral token]
		instanceLinks <- makePartPN parts part
		withLabel (networkName part) $ withLabel "port" $ do
			mapM_ (uncurry3 connectLink) $ zip3 ports parentLinks instanceLinks

	makePartPN :: NetworkIF network => [Part network] -> Part network -> MkPNMonad SimFlow SimValue [Chan]
	makePartPN parts part = do
		let partName = networkName part
		linkChans <- withLabel partName $ liftM DM.fromList $ sequence $ tryPart part $ do
			nwMapLinks $ \link -> return $ do
				linkChan <- teakLink partName link
				return (refLink link, linkChan)

		let
			portChans = tryPart part $ do
			portLinks <- liftM concat $ mapM' (nwGetPortAccess . fromJust . nwPortRef) $ networkPorts part
			return $ map (linkChans DM.!) portLinks

		let comps = tryPart part $ nwMapComps return
		mapM' (makeComp parts part linkChans) comps
		return portChans

	-- makeTopLevelPartPN : make a PN for a Part and connect the go port upto an R component
	makeTopLevelPartPN :: NetworkIF network => [Part network] -> Part network -> PN SimFlow SimValue
	makeTopLevelPartPN parts part = pn
		where
			(_, pn) = runMkPNMonad0 $ do
				portChans <- makePartPN parts part
				case (goPort, donePort) of
					(Just go, Nothing) -> teakR (portChans !! go)
					(Just go, Just _done) -> teakR (portChans !! go)
					_ -> return ()

			ports = networkPorts part
			goPort = findIndex (fromMaybe False . liftM (== GoAccess) . nwPortRef) ports
			donePort = findIndex (fromMaybe False . liftM (== DoneAccess) . nwPortRef) ports

	-- runPN :: (Show value, Monad m) => (a -> m ()) -> PN m value -> Marking value -> [a] -> m (Marking value)
	runPN :: (a -> SimFlow ()) -> PN SimFlow SimValue -> Marking SimValue -> [a] -> SimFlow (Marking SimValue)
	runPN setTime pn marking times = run pn marking times
		where
			step pn marking time = do
				setTime time
				-- simFlowSet TimeRef $ TimeState time
				(progress, marking') <- stepPN pn marking
				return (not progress, marking')

			run _ marking [] = return marking
			run pn marking (time:times) = do
				(stop, marking') <- step pn marking time
				if stop
					then return marking'
					else run pn marking' times

	dotGraphGroupNodesByLabel :: Maybe Int -> [String] -> DotGraph -> DotGraph
	dotGraphGroupNodesByLabel maybeMaxDepth prefix graph@(DotGraph nvs name nodes edges subGraphs)
		| isNothing maybeMaxDepth || maxDepth > 0 = DotGraph nvs name nonSubNodes edges subGraphs'
		| otherwise = graph
		where
			Just maxDepth = maybeMaxDepth
			nextDepth = maybeMaxDepth >>= return . (+ (-1))

			level = length prefix

			subGraphs' = subGraphs ++ map (uncurry (dotGraphGroupNodesByLabel nextDepth)) newSubgraphs

			newSubgraphs = mapMaybe makeSubgraph groupedNodes

			nonSubNodes = fromMaybe [] $ lookup "" groupedNodes

			makeSubgraph ("", _) = Nothing
			makeSubgraph (label, nodes) = Just (prefix', DotGraph ("cluster_" ++ escGraphName prefixLabel)
				[("label", prefixLabel)] nodes [] [])
				where
					prefix' = prefix ++ [label]
					prefixLabel = joinWith "." prefix'

			groupedNodes = groupByName (nthNameElem level . getLabel) nodes

			groupByName f nodes = map cleanUp $ sortAndGroupByElem fst prefices
				where
					cleanUp (name, elems) = (name, map snd elems)
					prefices = map (\thing -> (f thing, thing)) nodes

			getLabel node = fromMaybe "" $ liftM snd $ find ((== "label") . fst) $ dotNodeNVs node

	nthNameElem :: Int -> String -> String
	nthNameElem n name
		| n >= (nameLength - 1) = "" -- last or further component
		| otherwise = nameParts !! n
		where
			nameParts = splitWith "." name
			nameLength = length nameParts

	escGraphName :: String -> String
	escGraphName name = concatMap escChar name
		where
			escChar c | isAlphaNum c = [c]
			escChar _ = "_" -- X" ++ show (toEnum c :: Int)

	pnToDot :: Monad m => Maybe (Marking value) -> String -> PN m value -> DotGraph
	pnToDot maybeMarking name pn = DotGraph name
		[("size", "7x10"), {- ("ratio", "0.7"), -} ("pagedir", "TL"), ("fontname", "Helvetica")] ([
			DotNode "node" [("fontname", "Helvetica")],
			DotNode "edge" [("fontname", "Helvetica"), ("fontsize", "8")]] ++
			mapMaybe (uncurry makePlaceNode) (DM.assocs allPlaces) ++
			map (uncurry makeTransNode) (DM.assocs allTrans)
			)
		edges []
		where
			marking = if isJust maybeMarking
				then fromJust maybeMarking
				else pnInitialMarking pn

			showPlace placeNo = "P" ++ show placeNo
			showTrans transNo = "T" ++ show transNo

			isReducablePlace placeNo = fromMaybe False $ do
				(ins, outs) <- DM.lookup placeNo placesToTrans
				return $ case (ins, outs) of
					([_], [_]) -> null $ marking DM.! placeNo
					_ -> False

			makePlaceNode placeNo place
				| isReducablePlace placeNo = Nothing
				| otherwise = Just $ DotNode (showPlace placeNo) [
					("label", joinLabel (placeLabel place) ++ (case initialMarking of
						0 -> ""
						1 -> ""
						_ -> " " ++ show initialMarking)),
					("style", if initialMarking > 0
						then "solid"
						else case DM.lookup placeNo placesToTrans of
							Just ([], _) -> "dotted"
							Just (_, []) -> "dotted"
							Nothing -> "dotted"
							Just _ -> "solid"),
					("shape", "ellipse"),
					("penwidth", case initialMarking of
						1 -> "3"
						_ -> "1")]
					 where
						initialMarking = length $ marking DM.! placeNo

			escapeLabel = escapeString "|{}<>"

			makeTransNode transNo trans = DotNode (showTrans transNo)
				[("label", escapeLabel label), ("shape", "rectangle")]
				where
					label
						| null exprLabels = joinLabel $ transLabel trans
						| otherwise = joinLabel (transLabel trans) ++ ": " ++ exprLabels

					exprLabels = (case transCondition trans of
						Just cond -> "if " ++ showPNExpr 0 cond "" ++
							(if null assignments then "" else " then ")
						_ -> "")
						++ showListWithSep (showString ",") id assignments ""

					assignments = mapMaybe showExpr $ zip [(0::Int)..] $ transExprs trans

					showExpr (_, PNLiteral NoSimValue) = Nothing
					showExpr (i, expr) = Just $ showChar 'o' . shows i . showString "=" . showPNExpr 0 expr

			allPlaces = pnPlaces pn
			allTrans = pnTrans pn

			placesToTrans = pnPlacesToTrans pn

			edges = concatMap makeEdge $ DM.assocs placesToTrans
				where
					getPlaceArcIndex getPlaces placeNo transNo = fromJust $ findIndex (== placeNo) $
						getPlaces (allTrans DM.! transNo)
					inLabel placeNo transNo = "i" ++ show (getPlaceArcIndex transInPlaces placeNo transNo)
					outLabel placeNo transNo = "o" ++ show (getPlaceArcIndex transOutPlaces placeNo transNo)

					colour placeNo = case placeType (allPlaces DM.! placeNo) of
						BlackPlaceType -> colourToRGB black
						NumericPlaceType width -> colourToRGB $ colours !! (min (intWidth width) 9)

					colourToRGB (r, g, b) = "#" ++ component r ++ component g ++ component b
						where component c = numberToString (Bits 8) (floor (c * 0xFF)) 16 0 True

					makeEdge (placeNo, (inTrans, outTrans))
						| isReducablePlace placeNo = [DotEdge
							(showTrans (head inTrans)) (showTrans (head outTrans))
							[("label", joinLabel (placeLabel (allPlaces DM.! placeNo))),
							 ("headlabel", inLabel placeNo (head outTrans)),
							 ("taillabel", outLabel placeNo (head inTrans)),
							 ("color", colour placeNo)]]
						| otherwise =
							(map (\from -> DotEdge (showTrans from) (showPlace placeNo)
								[("taillabel", outLabel placeNo from), ("color", colour placeNo)])
								inTrans) ++
							(map (\to -> DotEdge (showPlace placeNo) (showTrans to)
								[("headlabel", inLabel placeNo to), ("color", colour placeNo)])
								outTrans)

	pnPlacesToTrans :: Monad m => PN m value -> DM.Map PlaceNo ([TransNo], [TransNo])
	pnPlacesToTrans pn = DM.fromList $ map combine $ sortAndGroupByElem fst $
		concatMap transToIOPlaces $ DM.assocs $ pnTrans pn
		where
			combine (placeNo, trans) = (placeNo, (concat ins, concat outs))
				where (ins, outs) = unzip $ map snd trans

			transToIOPlaces (transNo, trans) =
				(map (\placeNo -> (placeNo, ([], [transNo]))) $ transInPlaces trans) ++
				(map (\placeNo -> (placeNo, ([transNo], []))) $ transOutPlaces trans)

	pnTestCard :: FilePath -> IO ()
	pnTestCard file = writeGraphsToFile file $ map (dotGraphGroupNodesByLabel Nothing [] . uncurry (pnToDot Nothing))
		testPNs

	testPNs :: [(String, PN SimFlow SimValue)]
	testPNs = map runGraph graphs
		where
			runGraph (name, mk) = (name, snd $ runMkPNMonad0 mk)
			slice08 = 0 +: 8

			graphs = [
				("A2", do
					i0 <- withLabel "inp0" $ chan 8 0
					i1 <- withLabel "inp1" $ chan 8 0
					o <- withLabel "out" $ chan 8 0
					teakA 8 [i0, i1] o),
				("M2", do
					i0 <- withLabel "inp0" $ chan 8 0
					i1 <- withLabel "inp1" $ chan 8 0
					o <- withLabel "out" $ chan 8 0
					teakM 8 [i0, i1] o),
				("J2", do
					i0 <- withLabel "inp0" $ chan 8 0
					i1 <- withLabel "inp1" $ chan 8 0
					o <- withLabel "out" $ chan 16 0
					teakJ [8, 8] [i0, i1] o),
				("V23", do
					wg0 <- withLabel "wg0" $ chan 8 0
					wd0 <- withLabel "wd0" $ chan 0 0
					wg1 <- withLabel "wg1" $ chan 8 0
					wd1 <- withLabel "wd1" $ chan 0 0
					rg0 <- withLabel "rg0" $ chan 0 0
					rd0 <- withLabel "rd0" $ chan 8 0
					rg1 <- withLabel "rg1" $ chan 0 0
					rd1 <- withLabel "rd1" $ chan 8 0
					rg2 <- withLabel "rg2" $ chan 0 0
					rd2 <- withLabel "rd2" $ chan 8 0
					teakV 8 [slice08, slice08] [slice08, slice08, slice08]
						[wg0,wg1] [wd0,wd1] [rg0,rg1,rg2] [rd0,rd1,rd2]),
				("V11", do
					wg0 <- withLabel "wg0" $ chan 8 0
					wd0 <- withLabel "wd0" $ chan 0 0
					rg0 <- withLabel "rg0" $ chan 0 0
					rd0 <- withLabel "rd0" $ chan 8 0
					teakV 8 [slice08] [slice08] [wg0] [wd0] [rg0] [rd0]),
				("S2", do
					i <- withLabel "inp" $ chan 8 0
					o0 <- withLabel "out0" $ chan 8 0
					o1 <- withLabel "out1" $ chan 8 0
					teakS 8 slice08 [slice08, slice08] [[Imp 0 0], [Imp 0 0]] i [o0, o1]),
				("F2", do
					i <- withLabel "inp" $ chan 8 0
					o0 <- withLabel "out0" $ chan 8 0
					o1 <- withLabel "out1" $ chan 8 0
					teakF [slice08, slice08] i [o0, o1]),
				("R", do
					c <- withLabel "out" $ chan 0 0
					teakR c),
				("I", do
					inp <- withLabel "inp" $ chan 0 0
					out <- withLabel "out" $ chan 0 0
					teakI inp out),
				("O", do
					inp <- withLabel "inp" $ chan 0 0
					out <- withLabel "out" $ chan 8 0
					teakO [(1, TeakOConstant 8 0)] inp out),
				("C", do
					withLabel "C0" $ teakLink "C" $ NetworkLink 0 8 $ HalfBuffer 0
					withLabel "C1" $ teakLink "C" $ NetworkLink 0 8 $ HalfBuffer 1
					withLabel "C2" $ teakLink "C" $ NetworkLink 0 8 $ HalfBuffer 2
					return ())
				]

	writeHLNet :: Monad m => FilePath -> String -> PN m SimValue -> IO ()
	writeHLNet file _partName pn = do
		handle <- openFile file WriteMode
		hPutStrLn handle "PEP"
		hPutStrLn handle "HLNet"
		hPutStrLn handle "FORMAT_N"
		hPutStrLn handle "PL"
		mapM_ (hPutStrLn handle . (flip showPlace) "") $ DM.assocs $ pnPlaces pn
		hPutStrLn handle "TR"
		mapM_ (hPutStrLn handle . (flip showTrans) "") $ DM.assocs $ pnTrans pn
		hPutStrLn handle "TP"
		mapM_ (\(transNo, trans) -> mapM' (\(pos, placeNo) ->
			hPutStrLn handle $ showTtoP pos transNo placeNo "") $ zip [0..] $ transOutPlaces trans
			) $ DM.assocs $ pnTrans pn
		hPutStrLn handle "PT"
		mapM_ (\(transNo, trans) -> mapM' (\(pos, placeNo) ->
			hPutStrLn handle $ showPtoT pos placeNo transNo "") $ zip [0..] $ transInPlaces trans
			) $ DM.assocs $ pnTrans pn
		hClose handle
		where
			showPlace (placeNo, place) = shows placeNo . shows (joinLabel (placeLabel place)) .
				showString "0@0" .
				showString "Z\"" .
				(case placeType place of
					BlackPlaceType -> showString "{dot}"
					NumericPlaceType width -> showString "{0.." . shows ((bit width :: Integer) - 1) . showChar '}'
					) .
				showString "\"" .
				initMarking
				where
					initMarking = case marking DM.! placeNo of
						[] -> id
						[value] -> let
							markingShows = showChar '"' . showChar '{' . showSimValue value . showChar '}' .
								showChar '"'
							in showChar 'z' . markingShows . showChar 'y' . markingShows
						_ -> error "writeHLNet: can't handle initial marking with >1 token"

			showTrans (transNo, trans) = shows transNo . shows (joinLabel (transLabel trans)) .
				showString "0@0" .
				showString "g\"" . -- tt & " .
				(case transCondition trans of
					Just cond -> hlShowPNExpr cond . showString " & "
					_ -> id) .
				(showListWithSep (showString " & ") showExpr $ zip [(0::Int)..] (transExprs trans)) .
				showString "\""
				where showExpr (i, expr) = showChar 'o' . shows i . showString " = " . hlShowPNExpr expr

			showTtoP pos transNo placeNo = shows transNo . showChar '<' . shows placeNo .
				showString "p\"o" . shows (pos :: Int) . showString "\""

			showPtoT pos placeNo transNo = shows placeNo . showChar '>' . shows transNo .
				showString "p\"i" . shows (pos :: Int) . showString "\""

			showSimValue NoSimValue = showString "dot"
			showSimValue (SimValue int []) = shows int
			showSimValue simValue = error $ "writeHLNet: can't print SimValue `" ++ show simValue ++ "'"

			marking = pnInitialMarking pn
			-- placesToTrans = pnPlacesToTrans pn
