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

module BalsaLexer (
    BalsaToken (..),
    tokensToString,
    balsaLexer
    ) where

    import Parser
    import Report
    import Misc
    import Lexer

    import Data.Char (isDigit, toLower, isHexDigit, isOctDigit, digitToInt)
    import Data.List
    import Control.Monad
    import Data.Bits

    data BalsaToken =
          BalsaName { tokenPos :: Pos, tokenRaw :: String }
        | BalsaKeyword { tokenPos :: Pos, tokenRaw :: String }
        | BalsaLabel { tokenPos :: Pos, tokenLabel :: String, tokenRaw :: String }
        | BalsaInt { tokenPos :: Pos, tokenRaw :: String, tokenValInt :: Integer }
        | BalsaImplicant { tokenPos :: Pos, tokenRaw :: String,
            tokenValInt :: Integer, tokenDCInt :: Integer }
        | BalsaString { tokenPos :: Pos, tokenRaw :: String, tokenValStr :: String }
        | BalsaErrorToken { tokenPos :: Pos, tokenRaw :: String,
            tokenReport :: Maybe Report }
            deriving (Show)

    balsaSymbols :: [String]
    balsaSymbols = ["^", "<", "=", ">", "|", "-", ",", ";", ":", "?", "/", ".", "(", ")",
        "[", "]", "{", "}", "@", "*", "#", "%", "+", "'", "!", "\"", "<=", "<-", ">=",
        "||", "->", ":=", "/=", "..", "(*", "*)"]

    balsaKeywords :: [String]
    balsaKeywords = ["and", "arbitrate", "array", "as",
        "begin", "bits", "builtin",
        "case", "channel", "constant", "continue",
        "else", "end", "enumeration",
        "for", "function",
        "if", "in", "import", "input", "is",
        "local", "log", "loop",
        "not",
        "of", "or", "output", "over",
        "parameter", "print", "procedure",
        "record",
        "select", "shared", "signed", "sink", "sizeof",
        "then", "type",
        "variable",
        "while",
        "xor"]

    keywordMatches :: MatchTree String
    keywordMatches = makeMatchTree balsaKeywords

    symbolMatches :: MatchTree String
    symbolMatches = makeMatchTree balsaSymbols

    anyButQuote :: Lexer (Pos, Char)
    anyButQuote = makeCharParser "`\"'" (/= '"')

    tokenString :: Lexer (Maybe BalsaToken)
    tokenString = do
        (pos, _) <- char '"'
        (_, s) <- repChar anyButQuote
        char '"'
        return $ Just $ BalsaString pos ("\"" ++ escapeString "" s ++ "\"") s

    tokenName :: Lexer (Maybe BalsaToken)
    tokenName = do
        (pos, c) <- firstId
        (_, s) <- repChar nameChar
        let str = c:s
        isLabel <- optional False (char ':' >> return True)
        case (matchInMatchTree keywordMatches str, isLabel) of
            (True, True) -> parserFail $ Report pos "label must not be a keyword"
            (True, False) -> return $ Just $ BalsaKeyword pos str
            (False, True) -> return $ Just $ BalsaLabel pos str (str ++ ":")
            (False, False) -> return $ Just $ BalsaName pos str

    isDontCare :: Char -> Bool
    isDontCare = (`elem` "xX?")

    isDigitSep :: Char -> Bool
    isDigitSep = (== '_')

    decDigit :: Lexer (Pos, Char)
    decDigit = makeCharParser "decimal digit" (\c -> isDigit c || isDigitSep c)

    binDigit :: Lexer (Pos, Char)
    binDigit = makeCharParser "binary digit" (\c -> c `elem` "01" || isDontCare c || isDigitSep c)

    hexDigit :: Lexer (Pos, Char)
    hexDigit = makeCharParser "hexadecimal digit" (\c -> isHexDigit c || isDontCare c || isDigitSep c)

    octDigit :: Lexer (Pos, Char)
    octDigit = makeCharParser "octal digit" (\c -> isOctDigit c || isDontCare c || isDigitSep c)

    oneToNine :: Lexer (Pos, Char)
    oneToNine = makeCharParser "non-zero decimal digit" (\c -> c >= '1' && c <= '9')

    digitValue :: Int -> Char -> (Integer, Integer)
    digitValue digitBits digit
        | isHexDigit digit = (toInteger (digitToInt digit), 0)
        | isDontCare digit = (0, bit digitBits - 1)
        | otherwise = (0, 0)

    parseNumber :: String -> Pos -> Int -> String -> BalsaToken
    parseNumber prefix pos radix str =
        if dc == 0 then BalsaInt pos rawToken value else BalsaImplicant pos rawToken value dc
        where
            (value, dc) = foldl' step (0, 0) str

            step v '_' = v
            step v chr = addDigits (digitValue digitBits (toLower chr)) v

            digitBits = case radix of { 2 -> 1; 8 -> 3; _ -> 4 }
            addDigits (v1, m1) (v2, m2) = (v2 * toInteger radix + v1, m2 * toInteger radix + m1)
            rawToken = prefix ++ str

    tokenInt :: Lexer (Maybe BalsaToken)
    tokenInt = anyOneOfStr "integer" [
        do
            (pos, _) <- char '0'
            anyOneOfStr "radix indicator or decimal digit" [
                do
                    (_, c) <- ichar 'x'
                    (_, d) <- obligatory hexDigit
                    (_, ds) <- repChar hexDigit
                    return $ Just $ parseNumber ['0', c] pos 16 (d:ds),
                do
                    (_, c) <- ichar 'b'
                    (_, d) <- obligatory binDigit
                    (_, ds) <- repChar binDigit
                    return $ Just $ parseNumber ['0', c] pos 2 (d:ds),
                do
                    (_, c) <- ichar 'o'
                    (_, d) <- obligatory octDigit
                    (_, ds) <- repChar octDigit
                    return $ Just $ parseNumber ['0', c] pos 8 (d:ds),
                do
                    (_, ds) <- repChar decDigit
                    return $ Just $ parseNumber "0" pos 10 ds ],
        do
            (pos, d) <- oneToNine
            (_, ds) <- repChar decDigit
            return $ Just $ parseNumber "" pos 10 (d:ds) ]

    tokenSymbol :: Lexer (Maybe BalsaToken)
    tokenSymbol = do
        (pos:_, s) <- liftM unzip $ parseMatchTree (expecting "symbol") (mapM char) symbolMatches
        return $ Just $ BalsaKeyword pos s

    getToken :: Lexer (Maybe BalsaToken)
    getToken = anyOneOfStr "a valid token" [
        do
            ws
            return Nothing,
        tokenString,
        tokenName,
        tokenInt,
        do
            comment (string "--") (string "(--") (string "--)")
            return Nothing,
        tokenSymbol, -- Must be after comment to avoid "(" match
        do
            (pos, chr) <- anyChar
            return $ Just $ BalsaErrorToken pos [chr] (Just (Report pos ("bad character `" ++ [chr] ++ "'")))]

    balsaLexer :: Pos -> String -> [BalsaToken]
    balsaLexer = lexer (BalsaErrorToken NoPos "") getToken

    tokensToString :: Pos -> [BalsaToken] -> String
    tokensToString startingPos tokens =
        ret
        where
            (_, ret) = foldl' doToken (startingPos, "") tokens
            doToken (position, returnString) token =
                (finalPos, returnString ++ posFillBetween position nextPos ++ raw)
                where
                    raw = tokenRaw token
                    nextPos = tokenPos token
                    finalPos = posMoveToRight (length raw) nextPos
