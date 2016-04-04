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

module Options (
    CommandLineOption (..),
    CommandLineOptions (..),
    parseOptions,
    parseNameValuePairs,
    showOptionUsage,
    SubOption (..),
    SubOptionUsage (..),
    SubOptionUsages (..),
    SubOptionError,
    boolSubOption,
    parseSubOptionsString,
    subOptionsUsage,
    addSubOption,
    removeSubOption,
    replaceSubOption,
    findSubOption,
    getSubOption,
    findBoolSubOption,
    appendSubOptionUsages
    ) where

    import Data.List
    import Data.Maybe
    import Data.Char (isAlphaNum)
    import Control.Monad

    import Misc

    data CommandLineOption state =
          CommandLineOption {
            optionLongName :: String,
            optionShortName :: Char,
            optionArgNames :: [String],
            optionSummary :: String,
            optionOperation :: state -> [String] -> IO state }
        | CommandLineSeparator String

    shortNameGiven :: CommandLineOption state -> Bool
    shortNameGiven option = isAlphaNum $ optionShortName option

    longNameGiven :: CommandLineOption state -> Bool
    longNameGiven option = optionLongName option /= ""

    data CommandLineOptions state = CommandLineOptions {
        optionUsage :: String -> IO (),
        optionOptions :: [CommandLineOption state] }

    showOptionUsage :: String -> String -> CommandLineOptions state -> String
    showOptionUsage indent1 _indent2 (CommandLineOptions _ options) = joinWith "\n" usage ++ "\n"
        where
            argsStrs = map optionPattern options
            summaryStrs = map optionSummary' options

            optionSummary' option@(CommandLineOption {}) = optionSummary option
            optionSummary' _ = "\n" -- Treat as an `other' row.  no -- between option and summary

            optionPattern option@(CommandLineOption {})
                | shortNameGiven option = "-" ++ [shortName] ++ args ++
                    if longNameGiven option then " (or --" ++ longName ++ ")" else ""
                | longNameGiven option = "--" ++ longName ++ args
                | otherwise = error "showOptionUsage: option with long or short name"
                where
                    shortName = optionShortName option
                    longName = optionLongName option

                    args = concat $ map argStr $ optionArgNames option
                    argStr name = " <" ++ name ++ ">"
            optionPattern (CommandLineSeparator section) = section

            usage = columnFormat [argsStrs, summaryStrs] [indent1, " -- "] [indent1, "    "]

    parseOptions :: CommandLineOptions state -> state -> [String] -> IO (state, [String])
    parseOptions options state args = body args
        where
            optionList = optionOptions options

            body [] = return (state, [])
            body ("--":args) = return (state, args)
            body (name@('-':'-':longArg):args) = takeOption name args foundArg
                where foundArg = find (matchLong longArg) optionList
            body (name@['-', shortArg]:args) = takeOption name args foundArg
                where foundArg = find (matchShort shortArg) optionList
            body (name@('-':_:_):args) = takeOption name args Nothing
            body args = return (state, args)

            matchLong arg option@(CommandLineOption {}) = longNameGiven option && optionLongName option == arg
            matchLong _ _ = False

            matchShort arg option@(CommandLineOption {}) = shortNameGiven option && optionShortName option == arg
            matchShort _ _ = False

            takeOption name args Nothing = do
                optionUsage options $ "unrecognised command line option `" ++ name ++ "'"
                body args
            takeOption name args (Just option)
                | optionCount > argCount = do
                    optionUsage options $ "too few arguments for option `" ++ name ++ "', "
                        ++ show optionCount ++ " needed, only " ++ show argCount ++ " given"
                    body []
                | otherwise = do
                    state' <- optionOperation option state theseArgs
                    parseOptions options state' restArgs
                where
                    argCount = length args
                    (theseArgs, restArgs) = splitAt optionCount args
                    optionCount = length $ optionArgNames option

    parseNameValuePairs :: String -> Maybe [(String, Maybe String)]
    parseNameValuePairs opts = mapM splitPair $ splitWith ":" opts
        where
            splitPair pair = checkPair $ splitWith "=" pair
                where
                    checkPair [name, value] = return (name, Just value)
                    checkPair [name] = return (name, Nothing)
                    checkPair _ = fail ""

    class SubOption subOption where
        matchSubOption :: subOption -> subOption -> Bool

    data SubOptionUsage subOption = SubOptionUsage {
        subOptionIsBool :: Bool,
        subOptionArgName :: String,
        subOptionDescription :: String,
        subOptionSampleValue :: String,
        subOptionCanParseValue :: String -> Bool,
        subOptionParseValue :: String -> ([subOption], [subOption]) } -- (add, remove)

    data SubOptionUsages subOption = SubOptionUsages {
        subOptionClassName :: String,
        subOptionShowValue :: subOption -> String,
        subOptionNoneValue :: Maybe [subOption],
        subOptionDefaultValue :: Maybe [subOption],
        subOptionUsages :: [(String, SubOptionUsage subOption)] }

    boolSubOption :: String -> subOption -> SubOptionUsage subOption
    boolSubOption desc opt = SubOptionUsage True "" desc "" (const True) (const ([opt], []))

    addSubOption :: SubOption subOption => [subOption] -> subOption -> [subOption]
    addSubOption opts opt = opt : removeSubOption opts opt

    removeSubOption :: SubOption subOption => [subOption] -> subOption -> [subOption]
    removeSubOption opts opt = filter (not . matchSubOption opt) opts

    replaceSubOption :: SubOption subOption => [subOption] -> subOption -> [subOption]
    replaceSubOption opts opt
        | isJust i = replaceAt opts (fromJust i) opt
        | otherwise = addSubOption opts opt
        where i = findIndex (matchSubOption opt) opts

    -- findSubOption : find a sub-option `matchSubOption'ing the given one (if any)
    findSubOption :: SubOption subOption => [subOption] -> subOption -> Maybe subOption
    findSubOption opts opt = find (matchSubOption opt) opts

    -- getSubOption : like findSubOption but use the given option if none is found in the list
    getSubOption :: SubOption subOption => [subOption] -> subOption -> subOption
    getSubOption opts opt = fromMaybe opt $ find (matchSubOption opt) opts

    findBoolSubOption :: SubOption subOption => [subOption] -> subOption -> Bool
    findBoolSubOption opts opt = isJust $ findSubOption opts opt

    type SubOptionError subOption = [subOption] -> String -> IO ()

    parseSubOption :: (Monad m, SubOption subOption) => SubOptionUsages subOption ->
        ([subOption] -> String -> m ()) -> [subOption] -> (String, Maybe String) -> m [subOption]
    parseSubOption usage _ _ ("none", Nothing)
        | isJust $ subOptionNoneValue usage = return $ fromJust $ subOptionNoneValue usage
    parseSubOption _ localError opts ("none", Just _) = do
        localError opts "can't pass a value to `none' option"
        return opts
    parseSubOption usage _ _ ("default", Nothing)
        | isJust $ subOptionDefaultValue usage = return $ fromJust $ subOptionDefaultValue usage
    parseSubOption _ localError opts ("default", Just _) = do
        localError opts "can't pass a value to `none' option"
        return opts
    parseSubOption _ localError opts ("no-none", _) = do
        localError opts "can't use `no-' prefix with `none' option"
        return opts
    parseSubOption _ localError opts ("no-default", _) = do
        localError opts "can't use `no-' prefix with `default' option"
        return opts
    parseSubOption usage localError opts (name, maybeValue)
        | isNothing opt = do
            localError opts $ "unrecognised " ++ className ++ " option `" ++ bareName ++ "'"
            return opts
        | not isBool && isNothing maybeValue = do
            localError opts $ "must pass a value to option `" ++ bareName ++ "'"
            return opts
        | isBool && isJust maybeValue = do
            localError opts $ "can't pass a value to boolean option `" ++ bareName ++ "'"
            return opts
        | not isBool && not (subOptionCanParseValue (fromJust opt) value) = do
            localError opts $ "bad value `" ++ value ++ "' for " ++ className ++ " option `" ++ name ++ "'"
            return opts
        | no = return $ foldl' addSubOption (foldl' removeSubOption opts subOptionsToAdd) subOptionsToRemove
        | otherwise = return $ foldl' addSubOption (foldl' removeSubOption opts subOptionsToRemove) subOptionsToAdd
        where
            value = fromMaybe "true" $ maybeValue
            isBool = subOptionIsBool $ fromJust opt
            className = subOptionClassName usage
            opt = lookup bareName $ subOptionUsages usage
            -- Just (_, _, _, _, testArg, f) = opt
            (subOptionsToAdd, subOptionsToRemove) = subOptionParseValue (fromJust opt) value

            bareName = if no then drop 3 name else name
            no = isPrefixOf "no-" name

    parseSubOptionsString :: (Monad m, SubOption subOption) => SubOptionUsages subOption ->
        ([subOption] -> String -> m ()) -> [subOption] -> String -> m [subOption]
    parseSubOptionsString usage localError opts optString = do
        let pairs = parseNameValuePairs optString
        when (isNothing pairs) $ localError opts $ "bad " ++ subOptionClassName usage ++ " options"
        foldM (parseSubOption usage localError) opts $ fromJust pairs

    prettyPrintSubOptions :: SubOption subOption => SubOptionUsages subOption ->
        String -> [subOption] -> IO ()
    prettyPrintSubOptions usage prefix opts = mapM_ putStrLn $ columnFormat [optOns, optNames, optValues, optDescs]
        [prefix,"",""," -- "] [prefix,"","","    "]
        where
            optionOnDef (name, optUsage) =
                (on, name, value, subOptionDescription optUsage)
                where
                    on = if allOptsFound then "   " else "no-"

                    value
                        | allOptsFound && showValue = " = " ++ optValue
                        | otherwise = ""

                    (sampleAdded, sampleRemoved) = subOptionParseValue optUsage $ subOptionSampleValue optUsage
                    -- This option is on if all its added options are and none of its removed options
                    allOptsFound = all (isJust . findSubOption opts) sampleAdded &&
                        all (isNothing . findSubOption opts) sampleRemoved
                    (showValue, optValue) = case (sampleAdded, sampleRemoved) of
                        ([sample], []) | not (subOptionIsBool optUsage) -> (True, subOptionShowValue usage $
                            fromMaybe sample $ findSubOption opts sample)
                        _ -> (False, "")

            (optOns, optNames, optValues, optDescs) = unzip4 $ map optionOnDef $ subOptionUsages usage

    subOptionsUsage :: SubOption subOption => SubOptionUsages subOption -> [subOption] -> String -> String -> IO ()
    subOptionsUsage usage opts commandLineOption message = do
        putStrLn $ "  " ++ capitalise optionClass ++ " sub-options status and usage"
        putStrLn ""
        putStrLn $
            "  usage: " ++ commandLineOption ++ " (<option>:)*[<option>] where <option> ::= [no-]<name>[=<value>]"
        putStrLn "    `no-' prefix indicates that the option is off for the set of arguments given"
        when (isJust $ subOptionNoneValue usage) $
            putStrLn $ "    `none' can be given as <name> to remove existing options"
        when (isJust $ subOptionDefaultValue usage) $
            putStrLn $ "    `default' can be given as <name> to return options to defaults"
        putStrLn ""
        prettyPrintSubOptions usage "  " opts
        putStrLn ""
        when (message /= "") $ putStrLn $ "*** " ++ message
        where optionClass = subOptionClassName usage

    appendSubOptionUsages :: SubOption subOption =>
        SubOptionUsages subOption -> SubOptionUsages subOption -> SubOptionUsages subOption
    appendSubOptionUsages l r = l { subOptionUsages = subOptionUsages l ++ subOptionUsages r }
