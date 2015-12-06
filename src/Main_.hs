{-

	eTeak synthesiser a GALS back-end for the Balsa language
	Copyright (C) 2012- The University of Manchester

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

	Mahdi Jelodari <m.j.1989@ieee.org> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main (
	main
	) where

		
	import Bind
	import Context
	import Eval
	import Finish
	import Gen
	import Misc
	import NetParts
	import Gui
	import Options
	import ParseTree
	import Report
	import Print
	import Sim
	import SimTypes
	import Teak
	import Config
	import Latch
	import Plot
	import Layout
	import Balsa
	import Rule
	import Optim
	import Monitor
	import ToolOptions
	import SimPN
	import State
	import Dot
	import Scp
	import Network
	import Gates

	import VeriOpt
	import DeElastisise
	import WriteRTL

	import System.Environment
	import System.Exit
	import System.Time
	import System.FilePath
	import Data.List hiding (group)
	import System.Directory
	import Control.Monad
	import System.IO
	import Data.Maybe
	import Control.Monad.Trans
	import qualified Data.Map as DM
	import Debug.Trace as DT
	import System.IO.Unsafe

	type TeakNetwork = Network.Network

	main :: IO ()
	main = do
	
        let
                readNetwork :: String -> WhyT IO [Part Network]
                readNetwork = readNetworkFile
		inpDirectory = "./"
		outDirectory = "./"
		
	filename <- getArgs
	Why comp parts <- runWhyT $ readNetwork (inpDirectory ++ (head filename) ++ ".teak")

	let

		part = head parts 	
		list' = cycleMeasure part	
		part' = foldl runPart_ part list'
		backLinks = map (\(x,_) -> x) $ tryPart part $ findBackLinks 1
	print backLinks
--	writeNetworkFile False "" (outDirectory ++ (head filename) ++ "_SM.teak") [part']


