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

module Lexer (
	Lexer,
	LexerTokens (..),
	PosChar,
	makeCharParser,
	alphaChar,
	decDigitChar,
	firstId,
	nameChar,
	anyButEOL,
	anyChar,
	char,
	ichar,
	ws,
	string,
	skip,
	lineComment,
	blockComment,
	comment,
	lexer,
	LayoutTokenClass (..),
	insertLayoutTokens,
	repChar
	) where

	import Data.Char (isDigit, isAlpha, isSpace, toLower)
	import Data.List hiding (insert)

	import Parser
	import Report

	data LexerTokens = LexerTokens Pos String
		deriving (Show)

	type PosChar = (Pos, Char)

	type Lexer = Parser LexerTokens Report

	instance ParserTokens LexerTokens where
		nextTryArgs tokens = (True, tokens)
		noMoreTokens (LexerTokens _ ts) = null ts
		tokenStep (LexerTokens pos (t:ts)) = (LexerTokens pos [t], LexerTokens (posMoveByChar t pos) ts)
		tokenStep (LexerTokens _ []) = error "LexerTokens tokenStep: no tokens"

	makeCharParser :: String -> (Char -> Bool) -> Lexer (Pos, Char)
	makeCharParser notFoundStr test = matchToken eofReport match where
		match (LexerTokens pos [chr]) | test chr = return (pos, chr)
		match _ = parserFail $ expecting notFoundStr

	firstId :: Lexer PosChar
	firstId = makeCharParser "identifier first character" match where
		match chr = isAlpha chr || chr == '_'

	alphaChar :: Lexer (Pos, Char)
	alphaChar = makeCharParser "letter" isAlpha

	decDigitChar :: Lexer (Pos, Char)
	decDigitChar = makeCharParser "decimal digit" isDigit

	nameChar :: Lexer (Pos, Char)
	nameChar = anyOneOfStr "identifier body character" [ firstId, decDigitChar ]

	anyButEOL :: Lexer (Pos, Char)
	anyButEOL = makeCharParser "any character but newline" (/= '\n')

	anyChar :: Lexer PosChar
	anyChar = makeCharParser "any character" (const True)

	char :: Char -> Lexer (Pos, Char)
	char chr = makeCharParser ("`" ++ [chr] ++ "'") (== chr)

	ichar :: Char -> Lexer (Pos, Char)
	ichar chr = makeCharParser ("`" ++ [chr] ++ "' in either case") $ (== chr) . toLower

	ws :: Lexer (Pos, Char)
	ws = makeCharParser "whitespace" isSpace

	-- string str = mapM char str

	string :: String -> Lexer ()
	string str1 = Parser parseFunc where
		parseFunc (_, LexerTokens pos str2)
			| isPrefixOf str1 str2 = (ParseMatch (), LexerTokens (posMoveToRight strLen pos) (drop strLen str2))
			| otherwise = (ParseNoMatch, LexerTokens pos str2)
				where strLen = length str1

	skip :: Lexer a -> Lexer ()
	skip parser = rep parser >> return ()

	lineComment :: Lexer () -> Lexer ()
	lineComment start = do
		start
		skip anyButEOL
		return ()

	blockComment :: Lexer () -> Lexer () -> Lexer ()
	blockComment open close = do
		open
		let inBlockComment = anyOneOfStr "comment body" [
			close,
			do
				blockComment open close
				inBlockComment,
			do
				anyChar
				inBlockComment ]
		inBlockComment
		return ()

	comment :: Lexer () -> Lexer () -> Lexer () -> Lexer ()
	comment start open close = anyOneOfStr "comment" [ lineComment start , blockComment open close ]

	lexer :: (Maybe reason -> token) -> Parser LexerTokens reason (Maybe token) ->
		Pos -> String -> [token]
	lexer errorToken getToken pos str = catTokens $ openRep getToken $ (True, LexerTokens pos str)
		where
			catTokens ((Left msg):ts) = errorToken msg : catTokens ts
			catTokens ((Right (Just t)):ts) = t : catTokens ts
			catTokens ((Right Nothing):ts) = catTokens ts
			catTokens [] = []

	repChar :: Lexer PosChar -> Lexer (Pos, String)
	repChar parser = eitherOneOf
		(do
			(pos, firstChar) <- parser
			str <- rep (parser >>= (return . snd))
			return (pos, firstChar:str))
		(return (NoPos, []))

	data {- LayoutTokenClass token => -} LayoutWrapper token =
		  LayoutToken Pos token
		| LayoutHaveIndent Pos Int
		| LayoutNeedIndent Pos Int

	instance LayoutTokenClass token => Show (LayoutWrapper token) where
		showsPrec _ (LayoutToken _ token) = showString (layoutTokenShow token)
		showsPrec _ (LayoutHaveIndent _ column) = showChar '<' . shows column . showChar '>'
		showsPrec _ (LayoutNeedIndent _ column) = showChar '{' . shows column . showChar '}'

	class LayoutTokenClass token where
		layoutIsLayoutKeyword :: token -> Bool

		layoutIsOpen :: token -> Bool
		layoutIsClose :: token -> Bool
		layoutMakeOpen :: Pos -> token
		layoutMakeClose :: Pos -> token
		layoutMakeSemi :: Pos -> token

		layoutGetPos :: token -> Pos
		layoutTokenShow :: token -> String

	insertLayoutWrappers :: (Show token, LayoutTokenClass token) => [token] -> [LayoutWrapper token]
	insertLayoutWrappers [] = []
	insertLayoutWrappers (firstToken:restTokens)
		| layoutIsOpen firstToken = wrap (firstToken:restTokens)
		| otherwise = need firstToken (layoutGetPos firstToken) : wrap (firstToken:restTokens)
		where
			need token pos = LayoutNeedIndent pos (posGetColumn (layoutGetPos token))
			have token pos = LayoutHaveIndent pos (posGetColumn (layoutGetPos token))

			insert token = LayoutToken (layoutGetPos token) token

			wrap [] = []
			wrap [token]
				| layoutIsLayoutKeyword token = [insert token, need token (layoutGetPos token)]
				| otherwise = [insert token]
			wrap (token1:token2:tokens)
				| layoutIsLayoutKeyword token1 = if layoutIsOpen token2
					then insert token1 : rest
					else insert token1 : need token2 pos1 : rest
				| lineBreak = insert token1 : have token2 pos1 : rest
				| otherwise = insert token1 : rest
				where
					rest = wrap (token2:tokens)
					pos1 = layoutGetPos token1
					pos2 = layoutGetPos token2
					lineBreak = posGetLine pos1 /= posGetLine pos2

	insertLayoutTokens :: (Show token, LayoutTokenClass token) => [token] -> [token]
	insertLayoutTokens tokens = indent (insertLayoutWrappers tokens) []
		where
			indent (LayoutHaveIndent pos n : ts) (m:ms)
				| m == n = layoutMakeSemi pos : indent ts (m:ms)
				| n < m = layoutMakeClose pos : indent (LayoutHaveIndent pos n : ts) ms
			indent (LayoutHaveIndent {} : ts) ms = indent ts ms
			indent (LayoutNeedIndent pos n : ts) (m:ms)
				| n > m = layoutMakeOpen pos : indent ts (n:m:ms)
			indent (LayoutNeedIndent pos n : ts) []
				| n > 0 = layoutMakeOpen pos : indent ts [n]
			indent (LayoutNeedIndent pos n : ts) ms = layoutMakeOpen pos : layoutMakeClose pos
				: indent (LayoutHaveIndent pos n : ts) ms
			indent (LayoutToken _ t : ts) (0:ms) | layoutIsClose t = t : indent ts ms
			-- indent (LayoutToken pos t : ts) ms | layoutIsClose t = error "insertLayoutTokens: parse error"
			indent (LayoutToken _ t : ts) ms | layoutIsOpen t = t : indent ts (0:ms)
			{-
			indent (t:ts) (m:ms)
				| m /= 0 = layoutMakeClose (layoutPos t) : indent (t:ts) ms
				-}
			indent (LayoutToken _ t : ts) ms = t : indent ts ms
			indent [] [] = []
			indent [] (m:ms)
				| m /= 0 = layoutMakeClose NoPos : indent [] ms
			indent a b = error $ "insertLayoutTokens: parse error " ++ show a ++ " -- " ++ show b
