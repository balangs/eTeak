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

    ToolOptions : command line and GUI options not related just to command-line-invocation flow
-}

module ToolOptions (
    defaultToolOptions,
    ToolOptions (..),
    teakLibraryPath,
    teakOptimPath,
    setTeakHome,
    teakFindTechs,
    teakFindTechMapping,
    teakFindTechFile
    ) where

    import Latch
    import NetParts
    import Rule
    import Optim
    import Teak
    import Plot
    import Gen
    import Gates
    import GuiSupport
    import Report
    import Misc
    import Monitor
    import Sim

    import Data.List
    import System.Directory
    import Control.Monad.Trans
    import System.FilePath
    import Data.Maybe

    import Paths_eTeak

    data {- NetworkIF network => -} ToolOptions network = ToolOptions {
        -- General options
        optBalsaSearchPath :: [FilePath],
        optVerbose :: Bool,
        -- Compilation options
        optDumpParseTree :: Bool,
        optTeak :: [TeakOption],
        -- Optimisation options
        optOptim :: [OptimOption network],
        optAllOptims :: [Optim network],
        optEnabledOptims :: [Optim network],
        optRules :: RuleSet network,
        -- Latch insertion options
        optDoLatch :: Bool,
        optLatch :: [LatchOption],
        -- Plot options
        optPlot :: [PlotOption],
        -- Gen options
        optGenPartToGate :: [GenPartToGateOption],
        optTech :: String,
        -- Simulate options
        optSimDefines :: [(String, String)],
        optSim :: [SimOption],
        -- Gui options
        optGui :: [GuiOption],
        optPartMonitorEvents :: PartMonitorEvents,
        optBaseName :: Maybe String
        {-
        optOptimPasses :: Int,
        optTopLevel :: Maybe String,
        optPrettyNetwork :: Bool,
        optRuleFiles :: [String],
        -}
        }

    defaultToolOptions :: NetworkIF network => ToolOptions network
    defaultToolOptions = ToolOptions {
        optBalsaSearchPath = ["."],
        optVerbose = False,
        optDumpParseTree = False,
        optTeak = defaultTeakOptions,
        optOptim = defaultOptimOptions,
        optAllOptims = [],
        optEnabledOptims = [],
        optRules = emptyRuleSet,
        optDoLatch = False,
        optLatch = defaultLatchOptions,
        optGenPartToGate = [],
        optTech = "example",
        optPlot = defaultPlotOptions,
        optSimDefines = [],
        optSim = defaultSimOptions,
        optGui = defaultGuiOptions,
        optPartMonitorEvents = [],
        optBaseName = Nothing
        }


    teakLibraryPath:: IO FilePath
    teakLibraryPath = getDataFileName "library"

    teakOptimPath :: IO FilePath
    teakOptimPath = getDataFileName "optim"

    teakTechPath :: IO FilePath
    teakTechPath = getDataFileName "tech"

    setTeakHome :: NetworkIF network => FilePath -> ToolOptions network -> ToolOptions network
    setTeakHome librarypath opts = opts'
        where opts' = opts {
            optBalsaSearchPath = [".", librarypath] }

    techMappingFileSuffix :: String
    techMappingFileSuffix = "-mapping" <.> "v"

    teakFindTechs :: NetworkIF network => ToolOptions network -> IO [String]
    teakFindTechs toolOpts = do
        techPath <- teakTechPath
        canRead <- canReadDirectory techPath
        if canRead
            then do
                dirContents <- getDirectoryContents techPath
                return $ filter (/= "empty") $ map (dropSuffix techMappingFileSuffix) $
                    filter (isSuffixOf techMappingFileSuffix) dirContents
            else return []

    teakFindTechFile :: NetworkIF network => ToolOptions network -> String -> WhyT IO [GateNetlist]
    teakFindTechFile toolOpts techName = do
        techPath <- lift $ teakTechPath
        let mappingFile = if isSuffixOf ".v" techName
            then techName
            else techPath </> (techName ++ techMappingFileSuffix)

        exists <- lift $ doesFileExist mappingFile
        if exists
            then parseVerilogFile mappingFile
            else failPos PosTopLevel $ "tech. mapping file `" ++ mappingFile ++ "' doesn't exist"

    teakFindTechMapping :: NetworkIF network => ToolOptions network -> String -> WhyT IO TechMapping
    teakFindTechMapping toolOpts techName = do
        mappingNetlists <- teakFindTechFile toolOpts techName
        return $ mappingNetlistToTechMapping mappingNetlists

