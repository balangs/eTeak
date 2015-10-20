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

module TeakScript (
	parseWithTSLexer,
	tsEvalExpr,
	tsUpdatesParser,
	tsExprParser,
	tsFuncBindingParser,
	TSExpr (..),
	TSInfixOp (..),
	TSPair,
	TSToken (..),
	TSValue (..),
	TSVRecordClass (..),
	TSUpdates,
	TSParser,
	TSNoRecord (..),
	intToTSVInt,
	testTS,
	showTestTS,
	tsKeyword,
	tsNumber,
	tsLabel,
	tsName,
	tsString,
	tsSemiList,
	fromTSVBool,
	fromTSVList,
	fromTSVImp,
	isTSVBool,
	isTSVImp,
	isTSVInt,
	isTSVListOf,
	tsMiscVals,
	tsPassError,
	tsBadArg,
	tsApply,
	tsApply2,
	tsvToWhy,
	fromTSVInt
	) where

	import qualified Lexer
	import Parser
	import Report
	import Misc
	import Show
	import Bits

	import Data.Maybe
	import Control.Monad
	import Data.List

	data TSToken =
		  TSNameToken { tokenPos :: Pos, tokenRaw :: String }
		| TSLabelToken { tokenPos :: Pos, tokenRaw :: String, tokenLabel :: String }
		| TSKeywordToken { tokenPos :: Pos, tokenRaw :: String, isLayoutKeyword :: Bool }
		| TSStringToken { tokenPos :: Pos, tokenRaw :: String, tokenString :: String }
		| TSNumberToken { tokenPos :: Pos, tokenRaw :: String, tokenNumber :: Integer }
		| TSBadToken { tokenPos :: Pos, tokenRaw :: String }
		deriving Show

	type TSParser = Parser (LA1Tokens TSToken) Report

	data TSInfixOp =
		  TSOpAdd | TSOpSub | TSOpMul
		| TSOpAppend
		| TSOpEQ | TSOpNE | TSOpLT | TSOpLE | TSOpGT | TSOpGE
		| TSOpAnd | TSOpOr
		deriving Show

	type TSUpdates record = [(String, TSExpr record)]

	data {- TSVRecordClass record => -} TSExpr record =
		  TSName String
		| TSInfixOp TSInfixOp (TSExpr record) (TSExpr record)
		| TSExprMod (TSExpr record) (TSUpdates record)
		| TSIf (TSExpr record) (TSExpr record) (TSExpr record)
		| TSValue (TSValue record)
		| TSElem (TSExpr record) String
		| TSList [TSExpr record]
		| TSCall (TSExpr record) (TSExpr record)
		| TSLambda String (TSExpr record)
		| TSLet (TSUpdates record) (TSExpr record)
		deriving Show

	data {- TSVRecordClass record => -} TSValue record =
		  TSVInt Integer
		| TSVBool Bool
		| TSVString String
		| TSVImp Implicant
		| TSVList [TSValue record]
		| TSVFunc ([(String, TSValue record)] -> TSValue record -> TSValue record)
		| TSVRecord record
		| TSVError String

	class TSVRecordClass record where
		tsExtractElem :: [(String, TSValue record)] -> record -> String -> Maybe (TSValue record)
		tsExtractElem _ _ _ = Nothing

		tsExtractElems :: [(String, TSValue record)] -> record -> [(String, TSValue record)]
		tsExtractElems _ _ = []

		{-
		tsElemConcat :: [(String, TSValue record)] -> record -> String -> ([TSValue record] -> TSValue record)
		tsElemConcat _ _ _ = TSVList
		-}

		tsInsertElem :: [(String, TSValue record)] -> record -> String -> TSValue record -> Maybe record
		tsInsertElem _ r _ _ = Just r

		tsShowRecord :: record -> ShowS
		tsShowRecord _ = showString "?tsRecord?"

		tsRecordEq :: record -> record -> Bool
		tsRecordEq l r = tsRecordCompare l r == EQ

		tsRecordCompare :: record -> record -> Ordering
		tsRecordCompare _ _ = LT

		tsRecordSameClass :: record -> record -> Bool
		tsRecordSameClass _ _ = False

	data TSNoRecord = TSNoRecord

	instance TSVRecordClass TSNoRecord

	instance Lexer.LayoutTokenClass TSToken where
		layoutIsLayoutKeyword token@(TSKeywordToken {}) = isLayoutKeyword token
		layoutIsLayoutKeyword _ = False

		layoutIsOpen (TSKeywordToken _ "{" _) = True
		layoutIsOpen _ = False
		layoutIsClose (TSKeywordToken _ "}" _) = True
		layoutIsClose _ = False

		layoutMakeOpen pos = TSKeywordToken pos "{" False
		layoutMakeClose pos = TSKeywordToken pos "}" False
		layoutMakeSemi pos = TSKeywordToken pos ";" False

		layoutGetPos = tokenPos
		layoutTokenShow = tokenRaw

	tsLexer :: [String] -> [String] -> [String] -> Pos -> String -> [TSToken]
	tsLexer extraKeywords extraLayoutKeywords extraSymbols pos text = {- case status of
		ParseMatch tokens -> -}
		-- (ParseMatch (Lexer.insertLayoutTokens tokens), trailing) _ -> (status, trailing)
		Lexer.insertLayoutTokens tokens
		where
			-- (status, trailing) = Lexer.lexer (const (TSBadToken NoPos "")) $ token pos text
			tokens = Lexer.lexer (const (TSBadToken NoPos "")) token pos text
		
			token = anyOneOfStr "token" [
				Lexer.ws >> return Nothing,
				do
					Lexer.comment (Lexer.string "--") (Lexer.string "(--") (Lexer.string "--)")
					return Nothing,
				number,
				name,
				symbol,
				tokenString,
				do
					(pos, c) <- Lexer.anyChar
					return $ Just $ TSBadToken pos [c] ]

			char = Lexer.char
			repChar = Lexer.repChar

			symbolMatches = makeMatchTree $ nub $ extraSymbols ++ ["(", ",", ")", "[", "...", "]",
				"_", ".", ",", "=", "{", "}", ";",
				"*", "+", "-", "++", "&&", "||", "/=", "<", "<=", ">", ">=", "==", "\\", "->"]
			keywordMatches = makeMatchTree $ nub $ extraKeywords ++ layoutKeywords ++ ["if", "then", "else"]
			layoutKeywords = nub $ extraLayoutKeywords ++ ["where", "let"]
			layoutKeywordMatches = makeMatchTree layoutKeywords

			number = do
				(pos, d) <- Lexer.decDigitChar
				(_, s) <- repChar Lexer.decDigitChar
				return $ Just $ TSNumberToken { tokenPos = pos, tokenRaw = d:s, tokenNumber = read (d:s) }
			name = do
				(pos, c) <- Lexer.alphaChar
				(_, s) <- repChar (anyOneOfStr "tsName char" [
					Lexer.alphaChar, Lexer.decDigitChar, char '_', char '\'' ])
				let str = c:s
				isLabel <- optional False (char ':' >> return True)
				case (matchInMatchTree keywordMatches str, isLabel) of
					(True, True) -> parserFail $ Report pos "label must not be a keyword"
					(True, False) -> return $ Just $ TSKeywordToken pos str $
						matchInMatchTree layoutKeywordMatches str
					(False, True) -> return $ Just $
						TSLabelToken { tokenPos = pos, tokenRaw = str ++ ":", tokenLabel = str }
					(False, False) -> return $ Just $ TSNameToken pos str
			symbol = do
				(pos:_, s) <- liftM unzip $ parseMatchTree (expecting "symbol") (mapM char) symbolMatches
				return $ Just $ TSKeywordToken pos s False

			anyButQuote = Lexer.makeCharParser "`\"'" (/= '"')

			tokenString = do
				(pos, _) <- char '"'
				(_, s) <- repChar anyButQuote
				char '"'
				return $ Just $ TSStringToken { tokenPos = pos, tokenRaw = "\"" ++ escapeString "" s ++ "\"",
					tokenString = s }

	tsKeyword :: String -> TSParser ()
	tsKeyword word = matchToken eofReport match where
		match (LA1Tokens [TSKeywordToken _ word2 _]) | word == word2 = return ()
		match _ = parserFail $ expecting $ "`" ++ word ++ "'"

	tsClose :: TSParser ()
	tsClose = matchToken eofReport match where
		match (LA1Tokens [TSKeywordToken _ "}" _]) = return ()
		match _ = parserFail $ expecting "`}' or newline to same indent level"

	tsName :: TSParser String
	tsName = matchToken eofReport match where
		match (LA1Tokens [TSNameToken _ str]) = return str
		match _ = parserFail $ expecting "an identifier "

	tsLabel :: TSParser String
	tsLabel = matchToken eofReport match where
		match (LA1Tokens [TSLabelToken _ _ str]) = return str
		match _ = parserFail $ expecting "a label"

	tsNumber :: TSParser Integer
	tsNumber = matchToken eofReport match where
		match (LA1Tokens [TSNumberToken _ _ int]) = return int
		match _ = parserFail $ expecting "a number"

	tsString :: TSParser String
	tsString = matchToken eofReport match where
		match (LA1Tokens [TSStringToken _ _ str]) = return str
		match _ = parserFail $ expecting "a string"

	instance TSVRecordClass record => Eq (TSValue record) where
		(TSVInt l) == (TSVInt r) = l == r
		(TSVBool l) == (TSVBool r) = l == r
		(TSVString l) == (TSVString r) = l == r
		(TSVImp l) == (TSVImp r) = l == r
		(TSVList l) == (TSVList r) = length l == length r && and (zipWith (==) l r)
		(TSVFunc {}) == (TSVFunc {}) = False
		(TSVRecord l) == (TSVRecord r) = tsRecordEq l r
		_ == _ = False

	instance TSVRecordClass record => Ord (TSValue record) where
		(TSVBool l) `compare` (TSVBool r) = l `compare` r
		(TSVInt l) `compare` (TSVInt r) = l `compare` r
		(TSVString l) `compare` (TSVString r) = l `compare` r
		(TSVImp l) `compare` (TSVImp r) = l `compare` r
		(TSVList l) `compare` (TSVList r) = l `compare` r
		(TSVRecord l) `compare` (TSVRecord r) = tsRecordCompare l r
		l `compare` r = error $ "compare: undefined (TSValue record) ordering `" ++ show l ++ "' `" ++ show r ++ "'"

	tsvSameClass :: TSVRecordClass record => TSValue record -> TSValue record -> Bool
	tsvSameClass (TSVBool _) (TSVBool _) = True
	tsvSameClass (TSVInt _) (TSVInt _) = True
	tsvSameClass (TSVString _) (TSVString _) = True
	tsvSameClass (TSVImp _) (TSVImp _) = True
	tsvSameClass (TSVList _) (TSVList _) = True
	tsvSameClass (TSVFunc {}) (TSVFunc {}) = True
	tsvSameClass (TSVRecord r1) (TSVRecord r2) = tsRecordSameClass r1 r2
	tsvSameClass _ _ = False

	instance TSVRecordClass record => Show (TSValue record) where
		showsPrec _ (TSVInt int) = shows int
		showsPrec _ (TSVBool bool) = shows bool
		showsPrec _ (TSVString str) = shows str
		showsPrec _ (TSVImp imp) = shows imp
		showsPrec _ (TSVFunc {}) = showString "!tsvFunc!"
		showsPrec _ (TSVList ls) = showChar '[' . showListWithSep (showChar ',') shows ls . showChar ']'
		showsPrec _ (TSVRecord r) = tsShowRecord r
		showsPrec _ (TSVError msg) = showString "*** " . showString msg

	isTSVImp :: TSVRecordClass record => TSValue record -> Bool
	isTSVImp (TSVImp _) = True
	isTSVImp _ = False

	isTSVInt :: TSVRecordClass record => TSValue record -> Bool
	isTSVInt (TSVInt _) = True
	isTSVInt _ = False

	intToTSVInt :: TSVRecordClass record => Int -> TSValue record
	intToTSVInt i = TSVInt $ toInteger i

	isTSVList :: TSVRecordClass record => TSValue record -> Bool
	isTSVList (TSVList _) = True
	isTSVList _ = False

	isTSVListOf :: TSVRecordClass record => (TSValue record -> Bool) -> TSValue record -> Bool
	isTSVListOf f (TSVList ls) = all f ls
	isTSVListOf _ _ = False

	isTSVString :: TSVRecordClass record => TSValue record -> Bool
	isTSVString (TSVString _) = True
	isTSVString _ = False

	isTSVBool :: TSVRecordClass record => TSValue record -> Bool
	isTSVBool (TSVBool _) = True
	isTSVBool _ = False

	isTSVRecord :: TSVRecordClass record => TSValue record -> Bool
	isTSVRecord (TSVRecord _) = True
	isTSVRecord _ = False

	fromTSVInt :: (Integral ret, TSVRecordClass record) => TSValue record -> ret
	fromTSVInt (TSVInt i) = fromInteger i
	fromTSVInt _ = error "fromTSVInt: not a TSVInt"

	fromTSVImp :: TSVRecordClass record => TSValue record -> Implicant
	fromTSVImp (TSVImp i) = i
	fromTSVImp _ = error "fromTSVImp: not a TSVImp"

	fromTSVBool :: TSVRecordClass record => TSValue record -> Bool
	fromTSVBool (TSVBool bool) = bool
	fromTSVBool _ = error "fromTSVBool: not a TSVBool"

	fromTSVList :: TSVRecordClass record => TSValue record -> [TSValue record]
	fromTSVList (TSVList ls) = ls
	fromTSVList _ = error "fromTSVList: not a TSVList"

	fromTSVRecord :: TSVRecordClass record => TSValue record -> record
	fromTSVRecord (TSVRecord r) = r
	fromTSVRecord _ = error "fromTSVRecord: not a TSVRecord"

	tsInfixOp :: TSVRecordClass record => String -> TSInfixOp -> TSParser (TSExpr record) ->
		TSParser (TSExpr record -> TSExpr record)
	tsInfixOp kw op rhs = do
		tsKeyword kw
		r <- rhs
		return $ \l -> TSInfixOp op l r

	{-
		tsExpr ::= tsExprOr

		tsExprOr ::=
			  tsExprAnd "||" tsExprAnd
			| tsExprAnd

		tsExprAnd ::=
			  tsExprIneq "&&" tsExprIneq
			| tsExprIneq

		tsExprIneq ::=
			  tsExprAdd "==" tsExprAdd
			| tsExprAdd "/=" tsExprAdd
			| tsExprAdd "<" tsExprAdd
			| tsExprAdd "<=" tsExprAdd
			| tsExprAdd ">" tsExprAdd
			| tsExprAdd ">=" tsExprAdd
			| tsExprAdd

		tsExprAdd ::=
			  tsExprMul "+" tsExprMul
			| tsExprMul "-" tsExprMul
			| tsExprMul "++" tsExprMul
			| tsExprMul

		tsExprMul ::=
			  tsExprKeyworded "*" tsExprKeyworded
			| tsExprKeyworded

		tsExprKeyworded ::=
			  "\" tsName ( tsName )* "->" tsExpr
			| "if" tsExpr "then" tsExpr "else" tsExpr
			| tsExprCall

		tsExprCall ::= tsExprElem ( tsExprElem )*

		tsExprBinding ::= tsName "=" tsExpr

		tsFuncBinding name ::= ( tsName tsName )* "=" tsExpr

		tsUpdates ::= "{" ( tsExprBinding ( "," tsExprBinding )* )? "}"

		tsExprElem ::=
			  tsExprElem . tsName
			| tsExprElem tsUpdates
			| tsExprParen

		tsExprParen ::=
			  "(" tsExpr ")'
			| "[" ( tsExpr ( "," tsExpr )* )? "]"
			| tsTerminals

		tsTerminals ::= tsNumber | tsString | tsName
	-}

	tsExprParser :: TSVRecordClass record => TSParser (TSExpr record)
	tsExprParser = tsExprOr
		where
			tsExprOr = leftAssocInfix tsExprAnd [
				tsInfixOp "||" TSOpOr tsExprAnd ]

			tsExprAnd = leftAssocInfix tsExprIneq [
				tsInfixOp "&&" TSOpAnd tsExprIneq ]

			tsExprIneq = leftAssocInfix tsExprAdd [
				tsInfixOp "==" TSOpEQ tsExprAdd,
				tsInfixOp "/=" TSOpNE tsExprAdd,
				tsInfixOp "<" TSOpLT tsExprAdd,
				tsInfixOp "<=" TSOpLE tsExprAdd,
				tsInfixOp ">" TSOpGT tsExprAdd,
				tsInfixOp ">=" TSOpGE tsExprAdd ]

			tsExprAdd = leftAssocInfix tsExprMul [
				tsInfixOp "+" TSOpAdd tsExprMul,
				tsInfixOp "-" TSOpSub tsExprMul,
				tsInfixOp "++" TSOpAppend tsExprMul ]

			tsExprMul = leftAssocInfix tsExprKeyworded [
				tsInfixOp "*" TSOpMul tsExprKeyworded ]

			tsExprKeyworded = anyOneOfStr "keyworded expression" [
				do
					tsKeyword "\\"
					arg1 <- tsName
					args <- rep tsName
					tsKeyword "->"
					e <- tsExprParser
					return $ foldr TSLambda e (arg1:args),
				do
					tsKeyword "if"
					c <- tsExprParser
					tsKeyword "then"
					t <- tsExprParser
					tsKeyword "else"
					e <- tsExprParser
					return $ TSIf c t e,
				tsExprCall
				]

			tsExprCall = do
				expr <- tsExprElem
				args <- rep tsExprElem
				return $ if null args
					then expr
					else foldl' TSCall expr args

			tsExprElem = leftAssocInfix tsExprParen [
				do
					tsKeyword "."
					elem <- tsName
					return $ \l -> TSElem l elem,
				do
					updates <- tsUpdatesParser
					return $ \l -> TSExprMod l updates ]

			tsExprParen = anyOneOfStr "terminals or parentheses" [
				do
					tsKeyword "("
					e <- tsExprParser
					tsKeyword ")"
					return e,
				do
					tsKeyword "["
					es <- optional [] $ sepList (tsKeyword ",") tsExprParser
					tsKeyword "]"
					return $ TSList es,
				tsTerminals ]

			tsTerminals = anyOneOfStr "terminals" [
				do
					n <- tsNumber
					return $ TSValue $ TSVInt n,
				do
					s <- tsString
					return $ TSValue $ TSVString s,
				tsName >>= return . TSName ]

	tsUpdatesParser :: TSVRecordClass record => TSParser [(String, TSExpr record)]
	tsUpdatesParser = do
		tsKeyword "{"
		elems <- optional [] $ sepList (tsKeyword ",") tsExprBindingParser
		tsKeyword "}"
		return elems

	tsExprBindingParser :: TSVRecordClass record => TSParser (String, TSExpr record)
	tsExprBindingParser = do
		name <- tsName
		tsKeyword "="
		expr <- tsExprParser
		return (name, expr)

	tsFuncBindingParser :: TSVRecordClass record => String -> TSParser (String, TSExpr record)
	tsFuncBindingParser name = do
		args <- rep tsName
		tsKeyword "="
		expr <- tsExprParser
		bindings <- optional [] $ do
			tsKeyword "where"
			tsSemiList $ tsName >>= tsFuncBindingParser
		let expr' = if null bindings
			then expr
			else TSLet bindings expr
		if null args
			then return (name, expr')
			else return (name, foldr TSLambda expr' args)

	parseWithTSLexer :: [String] -> [String] -> [String] -> Pos -> a -> TSParser a -> String -> Why a
	parseWithTSLexer extraKeywords extraLayoutKeywords extraSymbols pos nullRet parser text =
		lexAndParse (tsLexer extraKeywords extraLayoutKeywords extraSymbols)
			(parseLA1 parser) tokenPos tokenRaw pos nullRet text

	tsSemiList :: TSParser a -> TSParser [a]
	tsSemiList parser = do
		tsKeyword "{"
		ret <- optional [] $ sepList (tsKeyword ";") parser
		tsClose
		return ret

	-- TSPair :: example TSVRecordClass
	data TSPair = TSPair (TSValue TSPair) (TSValue TSPair)

	instance TSVRecordClass TSPair where
		tsExtractElem _ (TSPair l _) "fst" = Just l
		tsExtractElem _ (TSPair _ r) "snd" = Just r
		tsExtractElem _ _ _ = Nothing

		tsExtractElems _ (TSPair l r) = [("fst", l), ("snd", r)]

		tsInsertElem _ (TSPair _ r) "fst" l = Just $ TSPair l r
		tsInsertElem _ (TSPair l _) "snd" r = Just $ TSPair l r
		tsInsertElem _ _ _ _ = Nothing

		tsShowRecord (TSPair l r) = showChar '(' . shows l . showChar ',' . shows r . showChar ')'

		tsRecordCompare (TSPair l1 r1) (TSPair l2 r2) = (l1 `compare` l2) `compare` (r1 `compare` r2)

		tsRecordSameClass _ _ = True

	testTS :: String -> Why [(String, TSValue TSPair)]
	testTS text = do
		bindings <- parseWithTSLexer [] [] [] (PosLC PosTopLevel 1 1) [] bindingsParser text
		let vals = mapSnd (tsEvalExpr [] (vals ++ tsMiscVals ++ otherVals)) bindings
		return vals
		where
			otherVals = [("pair", TSVRecord (TSPair (TSVInt 0) (TSVInt 0)))]
			bindingsParser = tsSemiList (tsName >>= tsFuncBindingParser)

	showTestTS :: String -> IO Completeness
	showTestTS text = do
		when (comp == Complete) $ mapM_ putStrLn bindingLines
		return comp
		where
			Why comp bindings = testTS text
			bindingLines = columnFormat [names, map show values] ["", " = "] ["", "   "]
			(names, values) = unzip bindings

	tsEvalExpr :: TSVRecordClass record =>
		[(String, TSValue record)] -> [(String, TSValue record)] -> TSExpr record -> TSValue record
	tsEvalExpr dynVals vals expr = body expr
		where
			eval = tsEvalExpr dynVals vals

			body (TSValue val) = val
			body (TSName name)
				| isJust valsV = fromJust valsV
				| otherwise = TSVError $ "can't find bound name `" ++ name ++ "'"
				where valsV = lookup name vals
			body (TSList ls) = tsPassError tsvReverse $ foldl' (\ret l -> tsPassError (cons l) ret) (TSVList []) ls
				where
					cons l (TSVList ret) = tsPassError (\lv -> TSVList (lv:ret)) $ eval l
					cons _ _ = error "tsEvalExpr: can't happen"
					tsvReverse (TSVList rs) = TSVList (reverse rs)
					tsvReverse _ = error "tsEvalExpr tsvReverse: can't happen"
			body (TSInfixOp op l r) = tsPassError handleOp $ eval l
				where
					handleOp lv = case (op, lv, eval r) of
						(TSOpAdd, _, rv) -> binOp isTSVInt "+" "an integer"
							(\(TSVInt li) (TSVInt ri) -> TSVInt $ li + ri) lv rv
						(TSOpSub, _, rv) -> binOp isTSVInt "-" "an integer"
							(\(TSVInt li) (TSVInt ri) -> TSVInt $ li - ri) lv rv
						(TSOpMul, _, rv) -> binOp isTSVInt "*" "an integer"
							(\(TSVInt li) (TSVInt ri) -> TSVInt $ li * ri) lv rv
						(TSOpAppend, TSVList _, rv) -> binOp isTSVList "++" "a list"
							(\(TSVList ls) (TSVList rs) -> TSVList (ls ++ rs)) lv rv
						(TSOpAppend, TSVString _, rv) -> binOp isTSVString "++" "a string"
							(\(TSVString ls) (TSVString rs) -> TSVString (ls ++ rs)) lv rv
						(TSOpEQ, _, rv) -> ineq (==) lv rv
						(TSOpNE, _, rv) -> ineq (/=) lv rv
						(TSOpLT, _, rv) -> ineq (<) lv rv
						(TSOpLE, _, rv) -> ineq (<=) lv rv
						(TSOpGT, _, rv) -> ineq (>) lv rv
						(TSOpGE, _, rv) -> ineq (>=) lv rv
						(TSOpAnd, TSVBool lb, rv)
							| lb -> tsPassError (logicalRhs "&&") rv
							| otherwise -> TSVBool False
						(TSOpOr, TSVBool lb, rv)
							| lb -> TSVBool True
							| otherwise -> tsPassError (logicalRhs "||") rv
						(op, l, r) -> TSVError $ "bad arguments for operator `" ++ show op
							++ "', lhs: `" ++ show l ++ "', rhs: `" ++ show r ++ "'"

					logicalRhs _ rv@(TSVBool {}) = rv
					logicalRhs op rv = TSVError $ op ++ " rhs must be boolean, got `" ++ show rv ++ "'"

					binOp test opName mustBe op lv rv
						| test lv = tsPassError binBody rv
						| otherwise = TSVError $ "`" ++ opName ++ "' left hand side `" ++ show lv
							++ "' must be " ++ mustBe
						where
							binBody _
								| test rv = lv `op` rv
								| otherwise = TSVError $ "`" ++ opName ++ "' right hand side `" ++ show rv
									++ "' must be " ++ mustBe

					ineq op lv = tsPassError ineqBody
						where
							ineqBody rv | tsvSameClass lv rv = TSVBool $ lv `op` rv
							ineqBody rv = TSVError $ "not same class " ++ show lv ++ " (from "
								++ show l ++ "), " ++ show rv ++ " (from " ++ show r ++ ")"

			body (TSElem l elemName) = case eval l of
				TSVRecord r | isJust elem -> fromJust elem
					where elem = tsExtractElem dynVals r elemName
				TSVList rs
					| all isTSVRecord rs && length rs == length elems -> TSVList elems
					where elems = mapMaybe (\r -> tsExtractElem dynVals (fromTSVRecord r) elemName) rs
				err@(TSVError {}) -> err
				_ -> TSVError $ "can't extract element `" ++ elemName ++ "' from `" ++ show l ++ "'"

			body (TSExprMod l updates) = case eval l of
				err@(TSVError {}) -> err
				TSVList vs -> TSVList $ map applyUpdates vs
				lv -> applyUpdates lv
				where
					applyUpdates (TSVRecord r) = foldl'
						(\acc (n, e) -> tsPassError2 (insertElem n) e acc) (TSVRecord r) updateVs
						where
							insertElem name val (TSVRecord record)
								| isNothing record' = TSVError $ "can't insert element `" ++ name
									++ "' (value `" ++ show val ++ "') into `" ++ tsShowRecord record "'"
								| otherwise = TSVRecord $ fromJust record'
								where record' = tsInsertElem dynVals record name val
							insertElem name val notRecord = TSVError $ "can't insert element `" ++ name
							 	++ "' (value `" ++ show val ++ "') into non-record `" ++ show notRecord ++ "'"

							extractVals = tsExtractElems dynVals r ++ vals
							updateVs = mapSnd (tsEvalExpr dynVals extractVals) updates
					applyUpdates val = TSVError $ "can't update non-record `" ++ show val ++ "' with '"
						++ show updates ++ "'"
			body (TSCall f arg) = case eval f of
				err@(TSVError {}) -> err
				TSVFunc f' -> tsPassError (f' dynVals) (eval arg)
				_ -> TSVError $ "can't call non-function `" ++ show f ++ "' with argument `" ++ show arg ++ "'"
			body (TSLambda argName expr) = TSVFunc (\passedDynVals val ->
				tsEvalExpr passedDynVals ((argName, val):vals) expr)
			body (TSIf cond thenExpr elseExpr) = case eval cond of
				err@(TSVError {}) -> err
				TSVBool True -> eval thenExpr
				TSVBool False -> eval elseExpr
				_ -> TSVError $ "if guard must be boolean `" ++ show cond ++ "'"
			body (TSLet bindings expr) = evalLocal expr
				where
					evalLocal = tsEvalExpr dynVals $ localVals ++ vals
					localVals = mapSnd evalLocal bindings

	tsPassError :: TSVRecordClass record => (TSValue record -> TSValue record) -> TSValue record -> TSValue record
	tsPassError _ err@(TSVError {}) = err
	tsPassError f arg = f arg

	tsPassError2 :: TSVRecordClass record => (TSValue record -> TSValue record -> TSValue record) ->
		TSValue record -> TSValue record -> TSValue record
	tsPassError2 f arg1 arg2 = tsPassError (const (tsPassError (const (f arg1 arg2)) arg2)) arg1

	tsBadArg :: TSVRecordClass record => String -> String -> TSValue record -> TSValue record
	tsBadArg name expecting arg = TSVError $ "bad argument `" ++ show arg ++ "' to function `" ++ name
		++ "' expecting " ++ expecting

	tsApply :: TSVRecordClass record =>
		[(String, TSValue record)] -> TSValue record -> TSValue record -> TSValue record
	tsApply _ err@(TSVError {}) _ = err
	tsApply dyn (TSVFunc f) arg = tsPassError (f dyn) arg
	tsApply _ f _ = TSVError $ "tsApply: argument not a function `" ++ show f ++ "'"

	tsApply2 :: TSVRecordClass record =>
		[(String, TSValue record)] -> TSValue record -> TSValue record -> TSValue record -> TSValue record
	tsApply2 dyn f arg1 arg2 = tsApply dyn (tsApply dyn f arg1) arg2

	tsMiscVals :: TSVRecordClass record => [(String, TSValue record)]
	tsMiscVals = [
		("last", TSVFunc lastF),
		("map", TSVFunc mapF),
		("length", TSVFunc lengthF),
		("foldl", TSVFunc foldlF),
		("error", TSVFunc errorF),
		("show", TSVFunc showF),
		("isList", TSVFunc isListF),
		("not", TSVFunc notF),
		("and", TSVFunc andF),
		("or", TSVFunc orF),
		("false", TSVBool False),
		("true", TSVBool True)
		]
		where
			lastF _ (TSVList ls) = last ls
			lastF _ arg = tsBadArg "last" "a list" arg

			lengthF _ (TSVList ls) = TSVInt $ toInteger $ length ls
			lengthF _ arg = tsBadArg "length" "a list" arg

			mapF _ (TSVFunc f) = TSVFunc $ mapF1 f
			mapF _ arg = tsBadArg "map" "a function" arg

			mapF1 f dyn (TSVList ls) = TSVList $ map (f dyn) ls
			mapF1 _ _ arg = tsBadArg "map _" "a list" arg

			andF _ (TSVList ls)
				| all isTSVBool ls = TSVBool $ all fromTSVBool ls
			andF _ arg = tsBadArg "and" "a list of booleans" arg

			orF _ (TSVList ls)
				| all isTSVBool ls = TSVBool $ all fromTSVBool ls
			orF _ arg = tsBadArg "or" "a list of booleans" arg

			foldlF _ f@(TSVFunc {}) = TSVFunc $ foldlF1 f
			foldlF _ arg = tsBadArg "foldl" "a function" arg

			foldlF1 f _ acc = TSVFunc $ foldlF2 f acc

			foldlF2 f acc dyn (TSVList ls) = foldl' (tsApply2 dyn f) acc ls
			foldlF2 _ _ _ arg = tsBadArg "foldl _ _" "a list" arg

			errorF _ (TSVString str) = TSVError str
			errorF _ arg = tsBadArg "error" "a string" arg

			showF _ value = TSVString $ show value

			isListF _ = TSVBool . isTSVList

			notF _ (TSVBool b) = TSVBool $ not b
			notF _ arg = tsBadArg "not" "a boolean" arg

	tsvToWhy :: TSValue record -> Why (TSValue record)
	tsvToWhy (TSVError msg) = fail msg
	tsvToWhy v = return v

