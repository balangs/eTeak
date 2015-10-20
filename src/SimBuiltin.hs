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

module SimBuiltin (
	simBuiltinCall
	) where

	import ParseTree
	import Bits
	import SimTypes
	import Type
	import Show hiding (skipComments)

	import Data.Char (isSpace, isPrint, toUpper, toLower)
	import System.IO
	import Data.Ix
	import Data.Maybe
	import Control.Monad
	import Data.List

	simBuiltinCall :: [SimValue] -> Expr -> SimFlow SimValue
	simBuiltinCall argSvs (BuiltinCallExpr _ func params _ _) = do
		let done = return
		case func of
			"TypeName" -> do
				let [TypeFuncActual t] = params
				done $ stringToSv $ showTypeName [] t
			"StringAppend" -> do
				let [l, r] = argSvs
				done $ stringToSv (svToString l ++ svToString r)
			"StringDowncase" -> do
				let [s] = argSvs
				done $ stringToSv (map toLower $ svToString s)
			"StringUpcase" -> do
				let [s] = argSvs
				done $ stringToSv (map toUpper $ svToString s)
			"FitStringToWidth" -> do
				let [str, width, just] = argSvs
				done $ stringToSv $ fitStringToWidth (svToString str)
					(fromEnum $ svToInt width) (intToStringJustification $ svToInt just)
			"BalsaSimulationTime" -> do
				TimeState time <- simFlowGet TimeRef
				done $ stringToSv $ show time
			"ToString" -> do
				let
					[TypeFuncActual typ] = params
					[val] = argSvs
				done $ stringToSv $ showSimValue False typ val ""
			"String" -> do
				let
					[ExprFuncActual _ (ValueExpr _ _ (StringValue string))] = params
				done $ stringToSv string
			"SubString" -> do
				let
					[strSv, indexSv, lengthSv] = argSvs
					str = svToString strSv
					index = fromEnum $ svToInt indexSv
					length = fromEnum $ svToInt lengthSv
				done $ stringToSv $ take length $ drop index str
			"StringEqual" -> do
				let
					[str1Sv, str2Sv] = argSvs
				done $ intToSv $ boolToBit $ (svToString str1Sv) == (svToString str2Sv)
			"NumberToString" -> do
				let
					[TypeFuncActual typ] = params
					[valSv, radixSv, underscoreSpacingSv, showLeadingZeroesSv] = argSvs
					val = svToInt valSv
					underscoreSpacing = svToInt underscoreSpacingSv
					showLeadingZeroes = (svToInt showLeadingZeroesSv) /= 0
				radix <- simFlowRadixInRange "NumberToString" $ svToInt radixSv
				done $ stringToSv $ numberToString typ val radix underscoreSpacing showLeadingZeroes
			"ForwardValuePrintMessage" -> do
				let
					[ExprFuncActual _ (ValueExpr _ _ (StringValue portName))] = params
					[value] = argSvs
				done $ stringToSv $ portName ++ ": " ++ svToString value
			"BalsaMemoryNew" -> do
				-- FIXME Process d/a width
				uid <- simFlowNewUid (MemoryState [])
				done $ specialToSv $ UidSimValue uid
			"BalsaGetDefinesArg" -> do
				let
					[nameSv] = argSvs
					name = svToString nameSv
				DefinesState defines <- simFlowGet DefinesRef
				let maybeValue = lookup name defines
				if isJust maybeValue
					then done $ stringToSv $ fromJust maybeValue
					else error $ "*** Couldn't find command line definition for name: " ++ name
			"BalsaMemoryRead" -> do
				let [memorySv, addr] = argSvs
				MemoryState memory <- simFlowGet $ svToUid memorySv
				let
					addrInt = svToInt addr
					value = lookup addrInt memory
				if isJust value
					then done $ fromJust value
					else do
						simFlowIo $ putStrLn $ "*** Memory: invalid read from address 0x" ++
							numberToString (Bits 32) addrInt 16 0 True
						done $ intToSv 0xDEADBEEF
			"BalsaMemoryWrite" -> do
				let
					[memorySv, addr, value] = argSvs
					uid = svToUid memorySv
				MemoryState memory <- simFlowGet uid
				let
					addrInt = svToInt addr
					notIndex (addr2, _) = addr2 /= addrInt
					memory' = (addrInt, value):(filter notIndex memory)
				simFlowSet uid (MemoryState memory')
				done memorySv
			"Chr" -> do
				let [ascii] = argSvs
				done $ stringToSv [toEnum $ fromEnum $ svToInt ascii]
			"Ord" -> do
				let
					[strSv] = argSvs
					str = svToString strSv
				int <- if (length str) == 1
					then return $ fromEnum (head str)
					else do
						simFlowIo $ putStrLn $ "*** Ord: string must have exactly one character: " ++ str
						return 0
				done $ intToSv $ cropInt 8 $ toInteger int
			"BalsaSimulationStop" -> do
				simFlowSet RunRef (RunState False)
				-- redo $ intToSv 1 -- Redo here forces a yield to the top level
				done $ intToSv 1 -- Redo here forces a yield to the top level
			"FileOpen" -> do
				-- FIXME, assume read
				let
					[filenameSv, _mode] = argSvs
					filename = svToString filenameSv
				handle <- simFlowIo $ openFile filename ReadMode
				uid <- simFlowNewUid $ FileState handle
				done $ uidToSv uid
			"FileEOF" -> do
				let [file] = argSvs
				FileState handle <- simFlowGet $ svToUid file
				eof <- simFlowIo $ hIsEOF handle
				done $ intToSv $ boolToBit $ eof
			"FileReadLine" -> do
				-- FIXME, assume read
				let [file] = argSvs
				FileState handle <- simFlowGet $ svToUid file
				line <- simFlowIo $ hGetLine handle
				done $ stringToSv line
			"TokenFromString" -> do
				let
					[strSv, _] = argSvs
					str = svToString strSv
				simFlowIo $ do
					putStrLn "*** TokenFromString: writeback to `remainder' argument not supported in teak"
					putStrLn "*** TokenFromString: use StringGetToken instead"
				done $ stringToSv $ head $ words $ str
			"NumberFromString" -> do
				let
					[TypeFuncActual typ] = params
					[strSv, radixSv] = argSvs
					str = svToString strSv
				radix <- simFlowRadixInRange "NumberFromString" $ svToInt radixSv
				done $ intToSv $ fst $ numberFromString typ str $ fromEnum radix
			"FromString" -> do
				let
					[TypeFuncActual typ] = params
					[strSv] = argSvs
					str = svToString strSv
				done $ fromString typ str
			"StringGetToken" -> do
				let
					[strSv] = argSvs
					str = svToString strSv

					takeToken [] = ""
					takeToken (word:_) = word

					str' = skipWS str
					token = takeToken $ words str'
					tail = skipWS $ drop (length token) str'
				done $ SimValue 0 [(0, StringSimValue token), (builtinTypeWidth, StringSimValue tail)]
			"StringToPrintable" -> do
				let
					[strSv] = argSvs
					str = svToString strSv
				done $ stringToSv $ stringToPrintable str
			"WriteMessage" -> do
				let
					[strSv] = argSvs
					str = svToString strSv
				simFlowIo $ do
					putStrLn str
				done $ intToSv 1
			"tWriteMessage" -> do
				let
					[strSv] = argSvs
					str = svToString strSv
				simFlowIo $ do
					putStrLn str
				done $ intToSv 1
			"StringLength" -> do
				let
					[strSv] = argSvs
					str = svToString strSv
				done $ intToSv $ toInteger $ length str
			"RepeatString" -> do
				let
					[strSv, nSv] = argSvs
					str = svToString strSv
					n = svToInt nSv
				done $ stringToSv $ concat $ replicate (fromEnum n) str
			_ -> error $ "Don't know builtin function: " ++ func
	simBuiltinCall _ _ = error "simBuiltinCall: not a builtin call"

	data StringJustification = StringJustification_left | StringJustification_right
		deriving (Show, Eq, Enum)

	intToStringJustification :: Integral i => i -> StringJustification
	intToStringJustification 0 = StringJustification_left
	intToStringJustification _ = StringJustification_right

	fitStringToWidth :: String -> Int -> StringJustification -> String
	fitStringToWidth str toWidth just
		| toWidth < fromWidth = take toWidth str
		| just == StringJustification_left = str ++ padding
		| otherwise = padding ++ str
		where
			fromWidth = length str
			padding = replicate (toWidth - fromWidth) ' '

	-- simFlowRadixInRange : print a warning if the given radix is outside the range [2,36].
	--	Returns 10 on error, or radix
	simFlowRadixInRange :: String -> Integer -> SimFlow Integer
	simFlowRadixInRange funcName radix = do
		if inRange (2, 36) radix
			then return radix
			else do
				simFlowIo $ putStrLn $ "*** " ++ funcName ++ ": radix " ++ show radix ++ " is out of range" ++
					" (falling back to decimal)"
				return 10

	charToDigit :: Char -> Maybe Int
	charToDigit chr
		| '0' <= chr && chr <= '9' = Just $ fromEnum chr - fromEnum '0'
		| 'a' <= chr && chr <= 'z' = Just $ 10 + fromEnum chr - fromEnum 'a'
		| 'A' <= chr && chr <= 'Z' = Just $ 10 + fromEnum chr - fromEnum 'A'
		| otherwise = Nothing

	numberFromString :: Type -> String -> Int -> (Integer, String)
	numberFromString typ ('0':'x':str) _ = numberFromString typ str 16
	numberFromString typ ('0':'X':str) _ = numberFromString typ str 16
	numberFromString typ str radix = body str'
		where
			str' = skipComments str
			width = widthOfType [] typ
			crop int = cropInt width int
			radixInteger = toInteger radix

			parse ('_':str) ret = parse str ret
			parse (chr:str) ret
				| isJust digit && (fromJust digit) < radix = parse str $
					(ret * radixInteger) + (toInteger (fromJust digit))
				| otherwise = (ret, chr:str)
				where digit = charToDigit chr
			parse [] ret = (ret, [])

			body ('-':str) = (crop (- int), rest)
				where (int, rest) = parse str 0
			body str = (crop int, rest)
				where (int, rest) = parse str 0

	stringToPrintable :: String -> String
	stringToPrintable [] = ""
	stringToPrintable (chr:str)
		| isPrint chr = chr : tail
		| otherwise = "<" ++ numberToString (Bits 8) (toInteger $ fromEnum chr) 16 0 True ++ ">" ++ tail
		where tail = stringToPrintable str

	skipWS :: String -> String
	skipWS = dropWhile isSpace

	skipEOL :: String -> String
	skipEOL ('\n':rest) = rest
	skipEOL l = l

	skipToEOL :: String -> String
	skipToEOL = skipEOL . dropWhile (/= '\n')

	skipComments :: String -> String
	skipComments str = body (skipWS str)
		where
			body ('(':'-':'-':rest) = skipComments (skipBlockComment 1 rest)
			body ('-':'-':rest) = skipComments (skipToEOL rest)
			body rest = rest

	skipBlockComment :: Int -> String -> String
	skipBlockComment depth str = body (dropWhile (/= '-') str)
		where
			body ('-':'-':')':rest)
				| depth == 1 = rest
				| otherwise = skipBlockComment (depth - 1) rest
			body rest = skipBlockComment depth rest

	fromString :: Type -> String -> SimValue
	fromString typ str
		| isNothing ret = error $ "fromString: can't parse `" ++ str ++ "' for type `" ++ show typ ++ "'"
		| otherwise = value
		where
			ret = fromStringTail typ str
			Just (value, _, _) = ret

	fromStringTail :: Type -> String -> Maybe (SimValue, Int, String)
	fromStringTail typ@(Bits width) str = Just (intToSv num, width, rest)
		where (num, rest) = numberFromString typ str 10
	fromStringTail typ@(SignedBits width) str = Just (intToSv num, width, rest)
		where (num, rest) = numberFromString typ str 10
	fromStringTail (StructRecordType _ elems _) str = do
		(elemValues, width, rest) <- readStruct 0 str types
		return (flattenOffsetSimValues elemValues, width, rest)
		where
			types = map recordElemType elems
			recordElemType (RecordElem _ _ typ) = typ
	fromStringTail (StructArrayType (Interval range _) typ) str = do
		(elemValues, width, rest) <- readStruct 0 str (replicate (rangeSize range) typ)
		return (flattenOffsetSimValues elemValues, width, rest)
	fromStringTail (StructEnumType typeName bindings overType) str = do
		(elemName, rest) <- maybeLex str
		let maybeElem = find ((== elemName) . simpleBindingName) bindings
		when (isNothing maybeElem) $
			error $ "fromString: no element `" ++ elemName ++ "' in enumeration type `" ++ typeName ++ "'"
		elem <- maybeElem
		return (intToSv (simpleBindingValue elem), widthOfType [] overType, rest)
	fromStringTail typ str = error $ "fromString: can't handle type `" ++ show typ ++ "' for string `" ++ str ++ "'"

	flattenOffsetSimValues :: [(Int, SimValue)] -> SimValue
	flattenOffsetSimValues [] = intToSv 0
	flattenOffsetSimValues values = SimValue (sum values') (sort (concat specialss'))
		where
			(values', specialss') = unzip $ map pairSimValue $ map (uncurry offsetSimValue) values
			pairSimValue (SimValue value specials) = (value, specials)
			pairSimValue _ = error "flattenOffsetSimValues: arg must not be NoSimValue"

	readStruct :: Int -> String -> [Type] -> Maybe ([(Int, SimValue)], Int, String)
	readStruct _ _ [] = error "readStruct: can't read 0 element structure"
	readStruct offset str types = do
		rest <- liftM skipComments $ maybeKeyword "{" str
		if isPrefixOf "}" rest
			then fail ""
			else body offset [] types $ skipComments rest
		where
			body _ _ [] rest = error $ "readStruct: too many elements, trailing string `" ++ rest ++ "'"
			body offset ret (typ:types) rest = do
				(value, width, rest2) <- fromStringTail typ $ skipComments rest
				(sep, rest3) <- maybeLex rest2
				let ret' = (offset, value):ret
				case sep of
					"}"
						| null types -> return (reverse ret', offset + width, rest3)
						| otherwise -> error $ "readStruct: too few elements, expecting "
							++ show (length types) ++ "more"
					"," -> body (offset + width) ret' types rest3
					_ -> error $ "readStruct: bad separator `" ++ sep ++ "'"

