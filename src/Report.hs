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

module Report (
	andCompleteness,
	Completeness (..),
	Why (..),
	WhyT (..),
	whyTReadFile,
	decorateErrors,
	decorateErrorsT,
	defaultWhy,
	defaultWhyT,
	defaultConnectWhy,
	defaultConnectWhyT,
	MonadPos (..),
	MonadGatherFail (..),
	gatherCompleteness,
	ImportPath (..),
	incomplete,
	isIncomplete,
	firstFailConnectWhy,
	firstFailConnectWhyT,
	maybeToWhy,
	noPosContext,
	pairToWhy,
	whyToPair,
	whyToPairDefault,
	Pos (..),
	PosArray (..),
	PosContext (..),
	posFillBetween,
	posGetColumn,
	posGetLine,
	posGetRefs,
	posGetRoots,
	posLookup,
	posMoveByChar,
	posMoveToRight,
	posStartOfLine,
	printCompleteness,
	Report (..),
	showPos,
	showPosAdjustFilename,
	showPosLC,
	showReport,
	tryWhyT
	) where

	import Data.Maybe
	import Data.Array
	import Control.Monad.State
	import Data.List

	import Show
	import Misc

	data Pos = NoPos
		| PosTopLevel
		| PosFile String ImportPath
		| PosLC { posFrom :: Pos, posLine :: Int, posColumn :: Int }
		| Pos { posFromRef :: Int, posLine :: Int, posColumn :: Int }
		| PosLabel { posFrom :: Pos, posLabel :: String }
		| PosList { posList :: [Pos] }
		| PosRef Int
			deriving (Eq)

	newtype PosArray = PosArray (Array Int Pos)
		deriving (Eq, Ord, Show, Read)

	data ImportPath =
		  ImportPath [String]
		| ImportFile String
		deriving (Show, Eq, Ord, Read)

	data Report = Report Pos String
		deriving (Show, Read, Eq, Ord)

	data Completeness = Wrong [Report] | Incomplete | Complete
		deriving (Show, Read, Eq, Ord)

	data Why a = Why Completeness a
		deriving Show

	newtype {- Monad m => -} WhyT m a = WhyT { runWhyT :: m (Why a) }

	tabDistance :: Int
	tabDistance = 4

	posGetRefs :: Pos -> [Int]
	posGetRefs (PosRef i) = [i]
	posGetRefs (Pos i _ _) = [i]
	posGetRefs (PosList ps) = concatMap posGetRefs ps
	posGetRefs _ = []

	posGetRoots :: PosContext c => (Maybe c) -> Pos -> [Pos]
	posGetRoots _ PosTopLevel = [PosTopLevel]
	posGetRoots _ NoPos = [NoPos]
	posGetRoots _ pos@(PosFile {}) = [pos]
	posGetRoots c (PosLC parent _ _) = posGetRoots c parent
	posGetRoots c (Pos parentRef _ _) = posGetRoots c parent
		where parent = posLookup c (PosRef parentRef)
	posGetRoots c (PosLabel from _) = posGetRoots c from
	posGetRoots c (PosList poss) = concatMap (posGetRoots c) poss
	posGetRoots c (PosRef ref) = posGetRoots c pos
		where pos = posLookup c (PosRef ref)

	posStartOfLine :: Pos -> Pos
	posStartOfLine (PosLC from line _) = PosLC from line 1
	posStartOfLine (Pos from line _) = Pos from line 1
	posStartOfLine pos = pos

	posNextLine :: Pos -> Pos
	posNextLine (PosLC from line _) = PosLC from (line+1) 1
	posNextLine (Pos from line _) = Pos from (line+1) 1
	posNextLine pos = pos

	posMoveToRight :: Int -> Pos -> Pos
	posMoveToRight distance (PosLC from line column) = PosLC from line (column+distance)
	posMoveToRight distance (Pos from line column) = Pos from line (column+distance)
	posMoveToRight _ pos = pos

	posGetLine :: Pos -> Int
	posGetLine (PosLC _ line _) = line
	posGetLine (Pos _ line _) = line
	posGetLine _ = 1

	posGetColumn :: Pos -> Int
	posGetColumn (PosLC _ _ column) = column
	posGetColumn (Pos _ _ column) = column
	posGetColumn _ = 1

	posMoveByTab :: Pos -> Pos
	posMoveByTab (PosLC from line column) =
		PosLC from line $ column + tabDistance - (column - 1) `mod` tabDistance
	posMoveByTab (Pos from line column) =
		Pos from line $ column + tabDistance - (column - 1) `mod` tabDistance
	posMoveByTab pos = pos

	posNextChar :: Pos -> Pos
	posNextChar = posMoveToRight 1

	posMoveByChar :: Char -> Pos -> Pos
	posMoveByChar '\t' = posMoveByTab
	posMoveByChar '\n' = posNextLine
	posMoveByChar _ = posNextChar

	class PosContext a where
		posIndex :: a -> Int -> Maybe Pos

	posLookup :: PosContext c => (Maybe c) -> Pos -> Pos
	posLookup Nothing (PosRef _) = NoPos
	posLookup (Just c) (PosRef ref)
		| isJust pos = fromJust pos
		| otherwise = NoPos
		where pos = posIndex c ref
	posLookup _ pos = pos

	instance PosContext PosArray where
		posIndex (PosArray arr) i
			| indexOK = Just (arr ! i)
			| otherwise = Nothing
			where indexOK = inRange (bounds arr) i

	noPosContext :: Maybe PosArray
	noPosContext = Nothing

	instance Show Pos where
		showsPrec _ NoPos = showString "NoPos"
		showsPrec _ PosTopLevel = showString "PosTopLevel"
		showsPrec prec (PosLC from line column) = showParen (prec > applyPrecedence) $ showString "PosLC " .
			showsPrec 11 from . showChar ' ' . shows line . showChar ' ' . shows column
		showsPrec prec (Pos from line column) = showParen (prec > applyPrecedence) $ showString "Pos " .
			showsPrec 11 from . showChar ' ' . shows line . showChar ' ' . shows column
		showsPrec prec (PosFile file path) = showParen (prec > applyPrecedence) $
			showString "PosFile " . showsPrec 11 file . showChar ' ' . showsPrec 11 path
		showsPrec prec (PosRef ref) = showParen (prec > applyPrecedence) $ showString "PosRef " . shows ref
		showsPrec prec (PosLabel from name) = showParen (prec > applyPrecedence) $ showString "PosLabel "
			. showsPrec 11 from . showChar ' ' . shows name
		showsPrec prec (PosList poss) = showParen (prec > applyPrecedence) $ showString "PosList "
			. showList poss

	instance Ord Pos where
		compare (PosList poss1) (PosList poss2) = compare poss1 poss2
		compare (PosList {}) _ = GT
		compare (PosLabel pos1 label1) (PosLabel pos2 label2) = compare (compare pos1 pos2) (compare label1 label2)
		compare (PosLabel {}) _ = GT
		compare (PosLC from1 line1 column1) (PosLC from2 line2 column2) =
			compare (from1, line1, column1) (from2, line2, column2)
		compare (PosLC {}) _ = GT
		compare (Pos from1 line1 column1) (Pos from2 line2 column2) =
			compare (from1, line1, column1) (from2, line2, column2)
		compare (Pos {}) _ = GT
		compare (PosFile f1 ip1) (PosFile f2 ip2) = compare f1 f2 `compare` compare ip1 ip2
		compare (PosFile {}) _ = GT
		compare (PosRef ref1) (PosRef ref2) = compare ref1 ref2
		compare (PosRef {}) _ = GT
		compare PosTopLevel PosTopLevel = EQ
		compare PosTopLevel _ = GT
		compare NoPos NoPos = EQ
		compare NoPos _ = GT
		-- compare _ _ = LT

	posFillBetween :: Pos -> Pos -> String
	posFillBetween (PosLC _ line1 column1) (PosLC _ line2 column2)
		| line1 == line2 = columnFill
		| line2 > line1 = take (line2 - line1) (repeat '\n') ++ take (column2 - 1) (repeat ' ')
		| otherwise = ""
			where columnFill = take (column2 - column1) (repeat ' ')
	posFillBetween (Pos _ line1 column1) (Pos _ line2 column2)
		| line1 == line2 = columnFill
		| line2 > line1 = take (line2 - line1) (repeat '\n') ++ take (column2 - 1) (repeat ' ')
		| otherwise = ""
			where columnFill = take (column2 - column1) (repeat ' ')
	posFillBetween _ _ = " "

	showPosLC :: Pos -> String
	showPosLC (PosLC _ line column) = show line ++ ":" ++ show column
	showPosLC (Pos _ line column) = show line ++ ":" ++ show column
	showPosLC _ = "?"

	showPosAdjustFilename :: PosContext c => (String -> String) -> Maybe c -> Pos -> String
	showPosAdjustFilename processFilename posContext pos = body posContext pos
		where
			body _ (PosLC from _ _) = body posContext from ++ ":" ++ showPosLC pos
			body _ (Pos from _ _) = body posContext (PosRef from) ++ ":" ++ showPosLC pos
			body _ (PosFile file _) = processFilename file
			body Nothing (PosRef ref) = "#" ++ show ref
			body (Just c) (PosRef ref)
				| isJust subPos = body (Just c) (fromJust subPos)
				| otherwise = "#" ++ show ref
				where subPos = posIndex c ref
			body _ NoPos = "?"
			body _ PosTopLevel = "top level"
			body c (PosLabel {}) = body c restOfPos ++ ":" ++ joinWith "." labels
				where (labels, restOfPos) = showPosLabelString c pos []
			body c (PosList poss) = joinWith ", " $ map (body c) poss

	showPos :: PosContext c => Maybe c -> Pos -> String
	showPos = showPosAdjustFilename id

	posInsertParent :: Pos -> Pos -> Pos
	posInsertParent parent (PosLC oldParent line column) = PosLC (posInsertParent parent oldParent) line column
	posInsertParent parent (PosLabel oldParent label) = PosLabel (posInsertParent parent oldParent) label
	posInsertParent parent (PosList poss) = PosList (map (posInsertParent parent) poss)
	-- doesn't handle PosRefs
	posInsertParent parent _ = parent

	showPosLabelString :: PosContext c => (Maybe c) -> Pos -> [String] -> ([String], Pos)
	showPosLabelString c (PosLabel from name) labels = showPosLabelString c (posLookup c from) (name:labels)
	showPosLabelString c (PosLC from _ _) labels = showPosLabelString c (posLookup c from) labels
	showPosLabelString c (Pos from _ _) labels = showPosLabelString c (posLookup c (PosRef from)) labels
	showPosLabelString _ pos labels = (labels, pos)

	instance Read Pos where
		readsPrec _ = readParen False readPos

	readPos :: String -> [(Pos, String)]
	readPos str = maybeToList $ do
		(headToken, rest) <- maybeLex str
		case headToken of
			"NoPos" -> return (NoPos, rest)
			"PosFile" -> readFields fields (PosFile "" (ImportFile "")) rest
				where fields = [
					ReadField "posFile" readsC (\(PosFile _ ip) f -> PosFile f ip),
					ReadField "posImportPath" readsC (\(PosFile file _) f -> PosFile file f)]
			"PosLC" -> readFields fields (PosLC NoPos 1 1) rest
				where fields = [
					ReadField "posFrom" readsC (\o f -> o { posFrom = f }),
					ReadField "posLine" readsC (\o f -> o { posLine = f }),
					ReadField "posColumn" readsC (\o f -> o { posColumn = f })]
			"Pos" -> readFields fields (Pos 0 1 1) rest
				where fields = [
					ReadField "posFromRef" readsC (\o f -> o { posFromRef = f }),
					ReadField "posLine" readsC (\o f -> o { posLine = f }),
					ReadField "posColumn" readsC (\o f -> o { posColumn = f })]
			"PosLabel" -> readFields fields (PosLabel NoPos "") rest
				where fields = [
					ReadField "posFrom" readsC (\o f -> o { posFrom = f }),
					ReadField "posLabel" readsC (\o f -> o { posLabel = f })]
			"PosList" -> readFields fields (PosList []) rest
				where fields = [ReadField "posList" readsC (\o f -> o { posList = f })]
			"PosRef" -> readFields fields (PosRef 0) rest
				where fields = [ReadField "posRef" readsC (\_ f -> PosRef f)]
			_ -> Nothing

	instance ShowTab PosArray where
		showsPrecTab prec tabs (PosArray poss) = showParen (prec > applyPrecedence)
			$ showString "PosArray " . showParen True (showArrayTab tabs poss)

	instance ShowTab Pos

	andCompleteness :: Completeness -> Completeness -> Completeness
	andCompleteness (Wrong m1) (Wrong m2) = Wrong (m1 ++ m2)
	andCompleteness _ (Wrong m) = Wrong m
	andCompleteness (Wrong m) _ = Wrong m
	andCompleteness Complete Complete = Complete
	andCompleteness Incomplete _ = Incomplete
	andCompleteness _ Incomplete = Incomplete

	gatherCompleteness :: [Completeness] -> Completeness
	gatherCompleteness rs = foldl' andCompleteness Complete rs

	class Monad m => MonadPos m where
		failPos :: Pos -> String -> m a
		failPos pos msg = failPosDefault undefined pos msg

		failPosDefault :: a -> Pos -> String -> m a
		failPosDefault _ pos msg = fail $ showPos noPosContext pos ++ ": " ++ msg

	instance Monad Why where
		(Why Complete l) >>= k = k l
		(Why comp _) >>= _ = Why comp undefined
		fail = failPos NoPos
		return v = Why Complete v

	instance Monad m => Monad (WhyT m) where
		l >>= k = defaultConnectWhyT undefined l k
		return r = WhyT $ return $ Why Complete r
		fail msg = WhyT $ return $ Why (Wrong [Report NoPos msg]) undefined

	instance MonadTrans WhyT where
		lift = WhyT . liftM (Why Complete)

	class MonadGatherFail m where
		gatherFail :: [m a] -> m [a]

		gatherFailMap :: (a -> m b) -> [a] -> m [b]
		gatherFailMap f l = gatherFail $ map f l

	instance MonadGatherFail Why where
		gatherFail ms = Why (gatherCompleteness comps) rets
			where (comps, rets) = unzip $ map whyToPair ms

	instance Monad m => MonadGatherFail (WhyT m) where
		gatherFail ms = WhyT $ do
			cms <- mapM runWhyT ms
			return $ gatherFail cms

	instance Monad m => MonadPos (WhyT m) where
		failPosDefault _ pos str = WhyT $ return $ failPos pos str

	instance MonadPos Why where
		failPosDefault def pos str = Why (Wrong [Report pos str]) def

	isIncomplete :: Why a -> Bool
	isIncomplete (Why Incomplete _) = True
	isIncomplete _ = False

	incomplete :: Why a
	incomplete = Why Incomplete undefined

	maybeToWhy :: Why a -> Maybe a -> Why a
	maybeToWhy failM Nothing = failM
	maybeToWhy _ (Just v) = Why Complete v

	whyToPair :: Why a -> (Completeness, a)
	whyToPair (Why comp ret) = (comp, ret)

	whyToPairDefault :: a -> Why a -> (Completeness, a)
	whyToPairDefault _ (Why Complete ret) = (Complete, ret)
	whyToPairDefault def (Why comp _) = (comp, def)

	pairToWhy :: (Completeness, a) -> Why a
	pairToWhy (comp, ret) = Why comp ret

	defaultConnectWhy :: b -> Why a -> (a -> Why b) -> Why b
	defaultConnectWhy _ (Why Complete lv) f = f lv
	defaultConnectWhy def (Why lComp _) _ = Why lComp def

	defaultWhy :: a -> Why a -> Why a
	defaultWhy _ m@(Why Complete _) = m 
	defaultWhy def (Why r _) = Why r def

	-- firstFailConnectWhy : connect Whys where, if the left fails, its value is
	--	returned, otherwise the right function is executed
	firstFailConnectWhy :: Why a -> (a -> Why a) -> Why a
	firstFailConnectWhy (Why Complete lv) f = f lv
	firstFailConnectWhy l _ = l

	decorateErrors :: Pos -> Why a -> Why a
	decorateErrors parent (Why (Wrong reports) ret) =
		Why (Wrong (map decorateReport reports)) ret
		where decorateReport (Report pos str) = Report (posInsertParent parent pos) str
	decorateErrors _ m = m

	decorateErrorsT :: Monad m => Pos -> WhyT m a -> WhyT m a
	decorateErrorsT parent m = WhyT $ do
		rm@(Why cm _) <- runWhyT m
		case cm of
			Wrong {} -> return $ decorateErrors parent rm
			_ -> return rm

	-- defaultConnectWhyT : connectCompleteness across another Monad
	defaultConnectWhyT :: Monad m => b -> WhyT m a -> (a -> WhyT m b) -> WhyT m b
	defaultConnectWhyT def l r = WhyT $ do
		Why cl vl <- runWhyT l
		case cl of
			Complete -> runWhyT $ r vl
			_ -> return $ Why cl def

	-- defaultWhyT : default the return value of a failing WhyT
	defaultWhyT :: Monad m => a -> WhyT m a -> WhyT m a
	defaultWhyT def m = WhyT $ do
		rm@(Why cm _) <- runWhyT m
		case cm of
			Complete -> return $ rm
			_ -> return $ Why cm def

	firstFailConnectWhyT :: Monad m => WhyT m a -> (a -> WhyT m a) -> WhyT m a
	firstFailConnectWhyT l r = WhyT $ do
		Why cl vl <- runWhyT l
		case cl of
			Complete -> runWhyT $ r vl
			_ -> return $ Why cl vl

	showReport :: PosContext context => Maybe context -> Report -> String
	showReport _ (Report NoPos message) = message
	showReport c (Report position message) = showPos c position ++ ": " ++ message

	printCompleteness :: PosContext context => Maybe context -> Completeness -> IO Bool
	printCompleteness _ Complete = return False
	printCompleteness _ Incomplete = do
		putStrLn "*** incomplete"
		return True
	printCompleteness c (Wrong reports) = do
		mapM printReport reports
		return True
		where printReport = putStrLn . showReport c

	whyTReadFile :: Pos -> FilePath -> WhyT IO String
	whyTReadFile pos filename = do
		fileIsReadable <- lift $ canReadFile filename
		if fileIsReadable
			then lift $ readFile filename
			else failPos pos $ "can't read file `" ++ filename ++ "'"

	-- tryWhyT : run a WhyT but display errors rather than returning them
	tryWhyT :: WhyT IO a -> IO a
	tryWhyT m = do
		Why comp ret <- runWhyT m
		printCompleteness noPosContext comp
		return ret
