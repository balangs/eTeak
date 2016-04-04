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

module Balsa (
    parseBalsaFile,
    compileBalsaFile,
    parseFile,
    printFile
    )
    where

    import Data.List
    import Data.Maybe
    import System.Directory
    import Control.Monad.Trans

    import Misc
    import BalsaParser
    import Report
    import ParseTree
    import Parser
    import BalsaLexer
    import Print (showTree)
    import Context
    import Bind
    import Eval
    import Finish (finish)

    findImportFilename :: [String] -> (Pos, ImportPath) -> WhyT IO String
    findImportFilename _ (pos, ImportFile file) = do
        let fileDotBalsa = file ++ ".balsa"
        fileExists <- lift $ doesFileExist file
        dotBalsaFileExists <- lift $ doesFileExist fileDotBalsa
        if fileExists
            then return file
            else if dotBalsaFileExists
                then return fileDotBalsa
                else failPos pos ("can't open file `" ++ file ++ "' or `" ++ fileDotBalsa ++ "'")
    findImportFilename searchPath (pos, ImportPath is) = findFilename searchPath
        where
            baseName = joinWith "/" is ++ ".balsa"

            findFilename [] = failPos pos ("file not found for module [" ++ joinWith "." is ++ "]"
                ++ " (search path: " ++ joinWith ":" searchPath ++ ")")
            findFilename (path:paths) = do
                fileExists <- lift $ doesFileExist normalName
                if fileExists then return normalName else findFilename paths
                where
                    normalName = if "./" `isPrefixOf` filename || "/" `isPrefixOf` filename
                        then filename else "./" ++ filename
                    filename = path ++ "/" ++ baseName

    addImport :: [(String, ImportPath)] -> String -> (Pos, ImportPath) -> [(String, ImportPath)]
    addImport imported filename (_, path) = imported ++ [(filename, path)]

    handleImport :: [String] -> [(String, ImportPath)] -> [Binding Decl] -> [(Pos, ImportPath)] ->
        WhyT IO ([(String, ImportPath)], [Binding Decl])
    handleImport _ imported bindings [] = return (imported, bindings)
    handleImport searchPath imported bindings (i:is) = do
        let connect = defaultConnectWhyT (imported, bindings)
        findImportFilename searchPath i `connect` \filename -> do
            let
                imported' = addImport imported filename i
                connect' = defaultConnectWhyT (imported', bindings)
            if isJust (lookup filename imported)
                then handleImport searchPath imported bindings is
                else do
                    let fileNo = length imported + 1
                    parseFile filename fileNo `connect'` \(thisImports, thisBindings) -> do
                        handleImport searchPath imported' bindings thisImports `firstFailConnectWhyT`
                            \(thisImported, thisImportBindings) -> do
                                handleImport searchPath thisImported (thisImportBindings ++ thisBindings) is

    parseFile :: String -> Int -> WhyT IO ([(Pos, ImportPath)], [Binding Decl])
    parseFile filename fileNo = defaultWhyT ([], []) $ do
        contents <- whyTReadFile NoPos filename
        (imports, context) <- WhyT $ return $ lexAndParse balsaLexer balsaParser tokenPos tokenRaw
            (Pos fileNo 1 1) ([], emptyContext) contents
        return (imports, contextBindingsList context)

    defaultImports :: [ImportPath]
    defaultImports = [ImportPath ["teak", "builtin"]]

    topBindings :: [Binding Decl]
    topBindings = []

    parseBalsaFile :: [String] -> String -> WhyT IO (Context Decl)
    parseBalsaFile searchPath filename = WhyT $ do
        Why r (imported, bindings) <- runWhyT $ handleImport searchPath [] topBindings
            (map (\i -> (PosTopLevel, i)) (defaultImports ++ [ImportFile filename]))
        let
            fileBindings = map filenameToFileBinding $ zip [1..] imported
            (posBindings, bindings') = gatherPositions (length fileBindings + 1) bindings

            filenameToFileBinding (i, (file, path)) = Binding 0 ("@" ++ show (i :: Int)) PosNamespace Complete
                (PosDecl (PosFile file path))
        return $ Why r $ bindingsToContext1 (fileBindings ++ posBindings ++ bindings')

    compileBalsaFile :: [String] -> String -> WhyT IO (Context Decl)
    compileBalsaFile searchPath filename = do
        let connect = firstFailConnectWhy
        firstFailConnectWhyT
            (parseBalsaFile searchPath filename)
            (\parsedContext -> WhyT $ return $ (return parsedContext) `connect`
                bind `connect` evalContext `connect` (return . finish))


    printFile :: String -> IO ()
    printFile f = do
      Why _ trees <- runWhyT $ parseFile f 0
      mapM_ (putStrLn . showTree) trees
