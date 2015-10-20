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

module Parser (
	ParseStatus (..),
	ParseArgs,
	ParseResult,
	Parser (..),
	ParserTokens (..),
	matchToken,
	sepList,
	eitherOneOf,
	anyOneOfDefault,
	anyOneOf,
	eofReport,
	expecting,
	anyOneOfStr,
	rep,
	openRep,
	optional,
	obligatory,
	parserFail,
	MatchTree (..),
	makeMatchTree,
	matchInMatchTree,
	parseMatchTree,
	LA1Tokens (..),
	lexAndParse,
	leftAssocInfix,
	parse,
	parseLA1
	) where

	import Data.List

	import Misc
	import Report

	data ParseStatus reason syn = ParseMatch syn | ParseNoMatch | ParseFail (Maybe reason)
		deriving (Eq, Show)

	type ParseArgs tokens = (Bool, tokens)
	type ParseResult tokens syn reason = (ParseStatus reason syn, tokens)
	newtype Parser tokens reason syn = Parser (ParseArgs tokens -> ParseResult tokens syn reason)

	class ParserTokens tokens where
		nextTryArgs :: tokens -> ParseArgs tokens
		noMoreTokens :: tokens -> Bool
		tokenStep :: tokens -> (tokens, tokens)

	newtype LA1Tokens token = LA1Tokens [token]
		deriving Show

	instance ParserTokens (LA1Tokens token) where
		nextTryArgs tokens = (False, tokens)
		noMoreTokens (LA1Tokens tokens) = null tokens
		tokenStep (LA1Tokens (t:ts)) = (LA1Tokens [t], LA1Tokens ts)
		tokenStep (LA1Tokens []) = error "LA1Tokens tokenStep: no more tokens"

	instance ParserTokens tokens => Monad (Parser tokens reason) where
		parser >>= k = follow parser k
		return v = Parser parseMatch where
			parseMatch (_, tokens) = (ParseMatch v, tokens)
		fail _ = makeFail Nothing

	parse :: Parser tokens reason syn -> ParseArgs tokens -> ParseResult tokens syn reason
	parse (Parser parser) = parser

	parseLA1 :: Parser (LA1Tokens token) reason syn -> [token] -> ParseResult [token] syn reason
	parseLA1 parser tokens = unWrap $ parse parser (False, LA1Tokens tokens)
		where unWrap (result, LA1Tokens remainingTokens) = (result, remainingTokens)

	follow :: ParserTokens tokens =>
		Parser tokens reason syn -> (syn -> Parser tokens reason syn') -> Parser tokens reason syn'
	follow parser1 f = Parser (onMatch . (parse parser1))
		where
			onMatch (ParseMatch syn, rest) = parse (f syn) (nextTryArgs rest)
			onMatch (ParseNoMatch, rest) = (ParseNoMatch, rest)
			onMatch (ParseFail result, rest) = (ParseFail result, rest)

	obligatory :: Parser tokens reason syn -> Parser tokens reason syn
	obligatory parser = Parser parseFunc
		where parseFunc (_, tokens) = parse parser (False, tokens)

	makeFail :: (Maybe reason) -> Parser tokens reason syn
	makeFail reason = Parser parseFunc
		where parseFunc (match, tokens) = (if match then ParseNoMatch else ParseFail reason, tokens)

	parserFail :: reason -> Parser tokens reason syn
	parserFail reason = makeFail $ Just reason

	-- Note that matchToken actually passes its arg as a list of tokens, not a token
	matchToken :: ParserTokens tokens => reason -> (tokens -> Parser tokens reason syn) -> Parser tokens reason syn
	matchToken eofReason f = Parser try where
		try args@(_, tokens)
			| noMoreTokens tokens = parse (parserFail eofReason) args
			| otherwise = step (parse (f token) args) where
				step (ParseMatch result, _) = (ParseMatch result, rest)
				step (result, _) = (result, tokens)
				(token, rest) = tokenStep tokens

	eitherOneOf :: Parser tokens reason syn -> Parser tokens reason syn -> Parser tokens reason syn
	eitherOneOf parser1 parser2 = Parser parseFunc where
		parseFunc args@(_, tokens) = onNoMatch (parse parser1 (True, tokens))
			where
				onNoMatch result1@(ParseMatch _, _) = result1
				onNoMatch (ParseNoMatch, _) = parse parser2 args
				onNoMatch (ParseFail result, rest) = (ParseFail result, rest)

	anyOneOfDefault :: (Parser tokens reason syn) -> [Parser tokens reason syn] -> Parser tokens reason syn
	anyOneOfDefault def [] = def
	anyOneOfDefault def (parser:parsers) = eitherOneOf parser (anyOneOfDefault def parsers)

	anyOneOf :: reason -> [Parser tokens reason syn] -> Parser tokens reason syn
	anyOneOf reason = anyOneOfDefault (parserFail reason)

	optional :: ParserTokens tokens => syn -> Parser tokens reason syn -> Parser tokens reason syn
	optional nullRet parser = eitherOneOf parser (return nullRet)

	rep :: ParserTokens tokens => Parser tokens reason syn -> Parser tokens reason [syn]
	rep parser = body []
		where
			body acc = eitherOneOf (do
				left <- parser
				body (left:acc))
				(return $ reverse acc)

	-- openRep : repeat returning a list which can be partially evaluated.  Each element is either
	--	Left (Maybe mesg), or Right syn.  Don't use at levels where NoMatch is possible (i.e. use
	--	some kind of `no-match' syn).
	openRep :: ParserTokens tokens => Parser tokens reason syn -> ParseArgs tokens -> [Either (Maybe reason) syn]
	openRep parser state0 = body state0
		where
			body state = case parse parser state of
				(ParseMatch ret, state') -> Right ret : body (nextTryArgs state')
				(ParseNoMatch, _) -> []
				(ParseFail msg, _) -> [Left msg]

	sepList :: ParserTokens tokens =>
		Parser tokens reason sepSyn -> Parser tokens reason syn -> Parser tokens reason [syn]
	sepList sepParser parser = do
		left <- parser
		right <- rep (sepParser >> parser)
		return (left:right)

	data MatchTree elem = Options [(elem, MatchTree elem)] Bool
		deriving (Show)

	emptyMatchTree :: MatchTree elem
	emptyMatchTree = Options [] False

	insertMatch :: Eq elem => MatchTree elem -> [elem] -> MatchTree elem
	insertMatch tree word = walk tree word where
		walk (Options matches _) [] = Options matches True
		walk (Options matches winner) (c:cs) = Options ((c, walk thisMatch cs):otherMatches) winner
			where
				thisMatch = findMatch $ lookup c matches
					where
						findMatch Nothing = emptyMatchTree
						findMatch (Just existing) = existing
				otherMatches = filter (\(e2, _) -> c /= e2) matches

	makeMatchTree :: [String] -> MatchTree String
	makeMatchTree keywords = compactTree $ foldl' insertMatch emptyMatchTree keywords

	compactTree :: MatchTree Char -> MatchTree String
	compactTree (Options matches0 winner0) = Options (map (compact "") matches0) winner0
		where
			compact prefix (char, Options [] winner) = (prefix ++ [char], Options [] winner)
			compact prefix (char, Options [match] False) = compact (prefix ++ [char]) match
			compact prefix (char, Options matches winner) = (prefix ++ [char],
				Options (map (compact "") matches) winner)

	matchInMatchTree :: Eq elem => MatchTree [elem] -> [elem] -> Bool
	matchInMatchTree tree word = walk tree word where
		walk (Options _ winner) [] = winner
		walk (Options matches _) str = tryMatch thisMatch
			where
				thisMatch = lookupPrefix str matches
				tryMatch Nothing = False
				tryMatch (Just (l2, v2)) = walk v2 $ drop (length l2) str

	lookupPrefix :: Eq a => [a] -> [([a], t)] -> Maybe ([a], t)
	lookupPrefix _ [] = Nothing
	lookupPrefix l1 ((l2, v2):lvs)
		| isPrefixOf l2 l1 = Just (l2, v2)
		| otherwise = lookupPrefix l1 lvs

	parseMatchTree :: ParserTokens tokens => reason -> (elem -> Parser tokens reason [a]) ->
		MatchTree elem -> Parser tokens reason [a]
	parseMatchTree reason parsePrefix (Options options0 winner0) = anyOneOfFail [] winner0 options0
		where
			anyOneOfFail ret winner options = do
				ret' <- anyOneOfDefault (if winner
					then return ret
					else makeFail $ Just reason) $ map (matchOption ret) options
				return ret'

			matchOption ret (prefix, Options options winner) = do
				ret2 <- parsePrefix prefix
				let ret' = ret ++ ret2
				anyOneOfFail ret' winner options

	lexAndParse :: (Pos -> text -> tokens) -> (tokens -> (ParseStatus Report ret, [token])) ->
		(token -> Pos) -> (token -> [Char]) ->
		Pos -> ret -> text -> Why ret
	lexAndParse lexer parser tokenPos tokenShow pos noRet text = firstFailConnectWhy (return noRet) $ const $
		case parseResult of
			ParseMatch ret
				| null remainingTokens -> return ret
				| otherwise -> parseError
					(Just (Report (tokenPos (head remainingTokens)) "unrecognised trailing content:"))
			ParseNoMatch -> failPosDefault noRet pos "curious, no match shouldn't be possible"
			ParseFail report -> parseError report
		where
			(parseResult, remainingTokens) = parser tokens
			tokens = lexer pos text

			parseError report = failPosDefault noRet (position report) msg
				where
					msg = errorMsg ++ remainingTokensString
						
					remainingTokensString
						| null remainingTokens = ""
						| otherwise = " (at `" ++ escapeString "`'\t" (tokensToString tokenPos tokenShow
							(tokenPos (head tokensToShow)) tokensToShow) ++ "'...)"
						where
							tokensToShow = take 4 remainingTokens

					position (Just (Report reportPos _)) | reportPos /= NoPos = reportPos
					position _
						| null remainingTokens = NoPos -- FIXME, have a PosEndOfFile?
						| otherwise = tokenPos $ head remainingTokens

					errorMsg = case report of
						Just (Report _ str) -> str
						_ -> ""

	tokensToString :: (token -> Pos) -> (token -> String) -> Pos -> [token] -> String
	tokensToString tokenPos tokenShow startingPos tokens = ret
		where
			(_, ret) = foldl' doToken (startingPos, "") tokens
			doToken (position, returnString) token =
				(finalPos, returnString ++ posFillBetween position nextPos ++ string)
				where
					string = tokenShow token
					nextPos = tokenPos token
					finalPos = posMoveToRight (length string) nextPos

	eofReport :: Report
	eofReport = Report NoPos "unexpected end of file"

	expecting :: String -> Report
	expecting str = Report NoPos ("expecting " ++ str)

	anyOneOfStr :: String -> [Parser tokens Report a] -> Parser tokens Report a
	anyOneOfStr str = anyOneOf $ expecting str

	leftAssocInfix :: ParserTokens token => Parser token Report a -> [Parser token Report (a -> a)] ->
		Parser token Report a
	leftAssocInfix lhs rhss = lhs >>= tryRhs where
		tryRhs left = optional left $ do
			cons <- anyOneOfStr "a left associative infix operator" rhss
			tryRhs $ cons left
