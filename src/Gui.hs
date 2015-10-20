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

{-# LANGUAGE RankNTypes #-}

module Gui (
	gui,
	GuiOption (..),
	GuiDisplayMode (..),
	guiOptionUsage
	)
	where

	import System.Exit
	import System.FilePath
	import Control.Monad
	import Control.Monad.Trans
	import Data.Maybe
	import Data.List
	import System.Posix
	import Data.IORef
	import Prelude hiding (catch)
	import Control.Exception
	import qualified Data.IntMap as IM
	import Data.Array
	import Data.Char

	import Misc
	import NetParts
	import GuiSupport
	import Layout
	import Optim
	import Plot
	import Rule
	import Report
	import Dot
	import Gen
	import Context
	import ParseTree
	import ToolOptions
	import Monitor
	import Latch
	import Graph
	import Options

	import Balsa
	import Teak

	-- import Graphics.UI.Gtk hiding ({- fill, -} toLayout)
	import qualified Graphics.UI.Gtk as Gtk
	import qualified Graphics.UI.Gtk.Gdk.Events as Events

	gui :: (Read network, NetworkIF network) => ToolOptions network -> [Part network] -> IO [Part network]

	data LeadView network = LeadView {
		leadViewIndex :: Int,
		leadViewSummary :: Maybe String,
		leadViewCompType :: String,
		leadViewLead :: Lead network }

	data {- NetworkIF network => -} FlagView network = FlagView {
		flagViewEnabled :: Bool,
		flagViewOpt :: Maybe (Optim network),
		flagViewName :: String,
		flagViewEffect :: String }

	data {- NetworkIF network => -} GuiInfo network = GuiInfo {
		guiHistory :: IORef (GuiHistory network),
		guiPartLayout :: IORef (GuiLayout network PartGraphical),
		guiFromLayout :: IORef (GuiLayout network PartGraphical),
		guiToLayout :: IORef (GuiLayout network PartGraphical),
		guiOLayout :: IORef (GuiLayout network OGraphical),
		guiParts :: [Part network],
		guiLeads :: [Lead network],
		guiUpdateLeads :: IO (),
		guiGetLeadView :: Int -> Maybe (LeadView network),
		guiPartList :: Gtk.TreeView,
		guiLeadsList :: Gtk.TreeView,
		guiSetFlagList :: IO (),
		guiPartListModel :: Gtk.ListStore String,
		guiDiffWindow :: Gtk.Window,
		guiTimeWindow :: Gtk.Window,
		guiOWindow :: Gtk.Window,
		guiTimeLimits :: (Integer, Integer),
		guiTime :: Integer,
		guiStateColours :: Array MonitorState Colour,
		guiSetPartSelectedSensitive :: Bool -> IO (),
		guiSetTimeLimits :: (Integer, Integer) -> IO (),
		guiSetBaseName :: Maybe String -> IO (),
		guiLastBalsaFile :: Maybe FilePath,
		guiLastNetworkFile :: Maybe FilePath,
		guiLastOptimRulesFile :: Maybe FilePath,
		guiToolOptions :: ToolOptions network }

	data {- NetworkIF network => -} GuiHistory network = GuiHistory {
		guiHistoryVerbose :: Bool,
		guiHistoryPosition :: Int, -- index of 'current' state.  Undo will come from +1, redo from -1
		guiHistoryMaxLength :: Int,
		guiHistoryCheckpoints :: [GuiCheckpoint network],
		guiHistorySetSensitive :: IORef (GuiHistory network) -> IO () }

	data {- NetworkIF network => -} GuiCheckpoint network = GuiCheckpoint {
		guiCheckpointInfo :: GuiInfo network,
		guiCheckpointPartLayout :: GuiLayout network PartGraphical,
		guiCheckpointFromLayout :: GuiLayout network PartGraphical,
		guiCheckpointToLayout :: GuiLayout network PartGraphical,
		guiCheckpointOLayout :: GuiLayout network OGraphical }

	historySetSensitive :: NetworkIF network => Gtk.Action -> Gtk.Action -> IORef (GuiHistory network) -> IO ()
	historySetSensitive undoAction redoAction historyRef = do
		history <- readIORef historyRef
		let
			position = guiHistoryPosition history
			checkpoints = guiHistoryCheckpoints history
			currentLength = length checkpoints
		Gtk.actionSetSensitive undoAction (position + 1 < currentLength)
		Gtk.actionSetSensitive redoAction (position - 1 >= 0)

	loseHistory :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	loseHistory infoRef = do
		info <- readIORef infoRef
		let historyRef = guiHistory info
		history <- readIORef historyRef
		writeIORef historyRef $ history { guiHistoryPosition = 0, guiHistoryCheckpoints = [] }

	pushHistory :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	pushHistory infoRef = do
		info <- readIORef infoRef
		partLayout <- readIORef $ guiPartLayout info
		fromLayout <- readIORef $ guiFromLayout info
		toLayout <- readIORef $ guiToLayout info
		oLayout <- readIORef $ guiOLayout info
		let historyRef = guiHistory info
		history <- readIORef historyRef
		let
			position = guiHistoryPosition history
			maxLength = guiHistoryMaxLength history
			checkpoints = guiHistoryCheckpoints history

			checkpoint = GuiCheckpoint info partLayout fromLayout toLayout oLayout
			currentLength = length checkpoints

			checkpoints'
				| position > 0 = checkpoint : drop position checkpoints
				| currentLength >= maxLength = checkpoint : take (maxLength - 1) checkpoints
				| otherwise = checkpoint : checkpoints

		when (guiHistoryVerbose history) $ do
			putStrLn $ "PUSH"
			putStrLn $ "position: " ++ show position
			putStrLn $ "currentLength: " ++ show currentLength

		writeIORef historyRef $ history { guiHistoryPosition = 0, guiHistoryCheckpoints = checkpoints' }
		guiHistorySetSensitive history historyRef

	restoreFromCheckpoint :: NetworkIF network => IORef (GuiInfo network) -> GuiCheckpoint network -> IO ()
	restoreFromCheckpoint infoRef checkpoint = do
		let info' = guiCheckpointInfo checkpoint
		writeIORef infoRef info'
		writeIORef (guiPartLayout info') $ guiCheckpointPartLayout checkpoint
		writeIORef (guiFromLayout info') $ guiCheckpointFromLayout checkpoint
		writeIORef (guiToLayout info') $ guiCheckpointToLayout checkpoint
		writeIORef (guiOLayout info') $ guiCheckpointOLayout checkpoint

	redrawAfterUndo :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	redrawAfterUndo infoRef = do
		info <- readIORef infoRef
		exposeLayout $ guiFromLayout info
		exposeLayout $ guiToLayout info
		exposeLayout $ guiOLayout info
		partName <- liftM (maybe "" networkName) $ guiPart infoRef
		updatePartSelection infoRef partName
		updateLeads infoRef
		exposeLayout $ guiPartLayout info

	undo :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	undo infoRef = do
		info <- readIORef infoRef
		let historyRef = guiHistory info
		history <- readIORef historyRef
		let
			position = guiHistoryPosition history
			checkpoints = guiHistoryCheckpoints history

			currentLength = length checkpoints
			position' = position + 1
			checkpoint = checkpoints !! position'

		when (guiHistoryVerbose history) $ do
			putStrLn $ "UNDO"
			putStrLn $ "position: " ++ show position
			putStrLn $ "currentLength: " ++ show currentLength

		if position' >= currentLength
			then print "*** can't undo any further"
			else do
				restoreFromCheckpoint infoRef checkpoint
				writeIORef historyRef $ history { guiHistoryPosition = position' }
				redrawAfterUndo infoRef
		guiHistorySetSensitive history historyRef

	redo :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	redo infoRef = do
		info <- readIORef infoRef
		let historyRef = guiHistory info
		history <- readIORef historyRef
		let
			position = guiHistoryPosition history
			checkpoints = guiHistoryCheckpoints history

			currentLength = length checkpoints
			position' = position - 1
			checkpoint = checkpoints !! position'

		when (guiHistoryVerbose history) $ do
			putStrLn $ "REDO"
			putStrLn $ "position: " ++ show position
			putStrLn $ "currentLength: " ++ show currentLength

		if position' < 0
			then print "*** can't redo any further"
			else do
				restoreFromCheckpoint infoRef checkpoint
				writeIORef historyRef $ history { guiHistoryPosition = position' }
				redrawAfterUndo infoRef
		guiHistorySetSensitive history historyRef

	initLeadsList :: NetworkIF network => IORef (GuiInfo network) -> Gtk.TreeView -> IO ()
	initLeadsList infoRef tree = do
		model <- Gtk.listStoreNew []

		renderer <- Gtk.cellRendererTextNew

		compNoColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle compNoColumn "No."
		Gtk.treeViewColumnSetResizable compNoColumn True
		Gtk.treeViewColumnPackStart compNoColumn renderer True

		compTypeColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle compTypeColumn ""
		Gtk.treeViewColumnSetResizable compTypeColumn True
		Gtk.treeViewColumnPackStart compTypeColumn renderer True

		nameColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle nameColumn "Optim."
		Gtk.treeViewColumnSetResizable nameColumn True
		Gtk.treeViewColumnPackStart nameColumn renderer True

		summaryColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle summaryColumn "Summary"
		Gtk.treeViewColumnSetResizable summaryColumn True
		Gtk.treeViewColumnPackStart summaryColumn renderer True

		let
			showComp leadView = [ Gtk.cellText Gtk.:= show (nwComp $ leadCompNo $ leadViewLead leadView) ]
			showCompType leadView = [ Gtk.cellText Gtk.:= leadViewCompType leadView ]
			showName leadView = [ Gtk.cellText Gtk.:= optimName (leadOptim (leadViewLead leadView)) ]
			showSummary (LeadView { leadViewSummary = Nothing }) = [ Gtk.cellText Gtk.:= "" ]
			showSummary (LeadView { leadViewSummary = Just summary }) = [ Gtk.cellText Gtk.:= ("x " ++ summary) ]

		Gtk.cellLayoutSetAttributes compNoColumn renderer model showComp
		Gtk.cellLayoutSetAttributes compTypeColumn renderer model showCompType
		Gtk.cellLayoutSetAttributes nameColumn renderer model showName
		Gtk.cellLayoutSetAttributes summaryColumn renderer model showSummary

		Gtk.treeViewAppendColumn tree compNoColumn
		Gtk.treeViewAppendColumn tree compTypeColumn
		Gtk.treeViewAppendColumn tree nameColumn
		Gtk.treeViewAppendColumn tree summaryColumn

		Gtk.treeViewSetModel tree model

		let
			updateLeads = do
				info <- readIORef infoRef
				maybePart <- guiPart infoRef
				let
					toolOpts = guiToolOptions info
					Just part = maybePart
					leads = guiLeads info
					-- leadsModel = guiLeadsModel info
					context = OptimContext { optimContextParts = guiParts info }

					summaryMaxLength = 40 :: Int

					formatSummary Nothing = Nothing
					formatSummary (Just str)
						| listAtLeastLength summaryMaxLength str' = Just (take summaryMaxLength str' ++ " ...")
						| otherwise = Just str'
						where
							str' = concatMap escChar str
							escChar '\n' = " "
							escChar c = [c]

					showOnlyPassing = findBoolSubOption (optGui toolOpts) GuiOnlyPassingLeads

					addLead (i, lead) = do
						let
							compXsummary = do
								-- Remove leads with no component, where optimTestEnabled is false,
								--	and for which optimTestComp no longer holds
								leadComp <- tryPart part $ nwGetComp $ leadCompNo lead
								-- liftIf $ optimTestEnabled (leadOptim lead) flags
								when (not (optimTestComp (leadOptim lead) leadComp)) $ fail ""
								let leadSummary = formatSummary $ optimPassesTest context part lead
								return (leadComp, leadSummary)

							Just (comp, summary) = compXsummary

						if isJust compXsummary && (not showOnlyPassing || isJust summary)
							then do
								let view = LeadView i summary (nwCompShortName comp) lead
								Gtk.listStoreAppend model view
								return $ Just view
							else return Nothing

				Gtk.listStoreClear model
				getLead <- if isJust maybePart
					then do
						displayLeadViews <- liftM catMaybes $ mapM addLead $ zip [0..] leads
						return $ Just . (displayLeadViews !!)
					else return $ \_ -> Nothing
				modifyIORef infoRef $ \info -> info { guiGetLeadView = getLead }

		modifyIORef infoRef $ \info -> info { guiUpdateLeads = updateLeads }
		return ()

	makePartList :: NetworkIF network => Gtk.TreeView -> IORef (GuiInfo network) -> IO (Gtk.ListStore String)
	makePartList tree infoRef = do
		model <- Gtk.listStoreNew []

		renderer <- Gtk.cellRendererTextNew

		partColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle partColumn "Part"
		Gtk.treeViewColumnSetResizable partColumn True
		Gtk.treeViewColumnPackStart partColumn renderer True

		Gtk.cellLayoutSetAttributes partColumn renderer model $ \row -> [ Gtk.cellText Gtk.:= row ]
		Gtk.treeViewAppendColumn tree partColumn

		selection <- Gtk.treeViewGetSelection tree
		Gtk.onSelectionChanged selection $ do
			rows <- Gtk.treeSelectionGetSelectedRows selection
			case rows of
				[[rowNo]] -> do
					-- print $ "Row select " ++ show rowNo
					info <- readIORef infoRef
					let layout = guiPartLayout info
					let part = guiParts info !! rowNo
					setPart infoRef $ Just part
					setLeads infoRef []
					updatePartGraphical infoRef
					fitLayout layout
					exposeLayout layout
					partLeads infoRef
					updateLeads infoRef
					setLayoutButtonsSensitive (guiPartLayout info) True
					guiSetPartSelectedSensitive info True
				_ -> do
					-- print "Other row select"
					info <- readIORef infoRef
					let layout = guiPartLayout info
					oldPart <- guiPart infoRef
					setPart infoRef Nothing
					setLeads infoRef []
					-- You get this signal really early on, before the drawing area is set
					--	check we're unselecting a part, rather than initialising
					when (isJust oldPart) $ do
						updateLeads infoRef
						clearLayout layout
					setLayoutButtonsSensitive (guiPartLayout info) False
					guiSetPartSelectedSensitive info False
			loseHistory infoRef
			pushHistory infoRef

		Gtk.treeViewSetModel tree model

		return model

	setParts :: NetworkIF network => IORef (GuiInfo network) -> [Part network] -> IO ()
	setParts infoRef parts = modifyIORef infoRef $ \info -> info { guiParts = parts }

	setPart :: NetworkIF network => IORef (GuiInfo network) -> Maybe (Part network) -> IO ()
	setPart infoRef part = do
		info <- readIORef infoRef
		setLayoutPart (guiPartLayout info) part

	setLeads :: NetworkIF network => IORef (GuiInfo network) -> [Lead network] -> IO ()
	setLeads infoRef leads = modifyIORef infoRef $ \info -> info { guiLeads = leads }

	updateLeads :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	updateLeads infoRef = readIORef infoRef >>= guiUpdateLeads

	updatePartList :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	updatePartList infoRef = do
		info <- readIORef infoRef
		let partsListModel = guiPartListModel info

		Gtk.listStoreClear partsListModel
		mapM_ (Gtk.listStoreAppend partsListModel . networkName) $ guiParts info

	guiPart :: NetworkIF network => IORef (GuiInfo network) -> IO (Maybe (Part network))
	guiPart infoRef = do
		info <- readIORef infoRef
		liftM layoutPart $ readIORef $ guiPartLayout info

	makePartGraphical :: NetworkIF network =>
		[GuiOption] -> [PlotOption] -> Part network -> IO PartGraphical
	makePartGraphical guiOpts plotOpts part = do
		graphical <- case displayMode of
			GuiSpring -> partGraphicalSpringInfo part'
			GuiDot -> partGraphicalInfo False plotOpts name part'
		return $ graphical { partShowLinkColours = showLinkWidthColours }
		where
			part' = trimPartForPartialPlot plotOpts part
			name = networkName part
			showLinkWidthColours = findBoolSubOption guiOpts GuiShowLinkWidthColours
			GuiDisplayMode displayMode = getSubOption guiOpts $ GuiDisplayMode GuiDot

	updatePartGraphical :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	updatePartGraphical infoRef = do
		info <- readIORef infoRef
		maybePart <- guiPart infoRef
		let
			Just part = maybePart
			plotOpts = optPlot $ guiToolOptions info
		when (isJust maybePart) $ do
			graphical <- makePartGraphical
				(optGui $ guiToolOptions info)
				-- (optPlot $ guiToolOptions info)
				([{- PlotRatio 1, -} PlotSize (250, 250), PlotOLength 40]
					++ mapMaybe (findSubOption plotOpts) [PlotBreakVariables, PlotPartial undefined,
						PlotShowUnconnected])
				part
			let
				showLinkWidthColours = findBoolSubOption (optGui $ guiToolOptions info) GuiShowLinkWidthColours
				graphical' = graphical { partShowLinkColours = showLinkWidthColours }
			setLayoutMoving (guiPartLayout info) Nothing
			setLayoutGraphical (guiPartLayout info) $ Just graphical'
			readIORef (guiPartLayout info) >>= layoutUpdateScrollbars

	redrawPart :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	redrawPart infoRef = do
		maybePart <- guiPart infoRef
		when (isJust maybePart) $ do
			do
				info <- readIORef infoRef
				let layout = guiPartLayout info
				propViewOrigin <- getLayoutProportionalOrigin layout
				updatePartGraphical infoRef
				setLayoutProportionalOrigin layout propViewOrigin
			info <- readIORef infoRef
			let layout = guiPartLayout info
			exposeLayout layout

	redrawPartAfterOptionChange :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	redrawPartAfterOptionChange infoRef = do
		redrawPart infoRef

	updatePartSelection :: NetworkIF network => IORef (GuiInfo network) -> String -> IO ()
	updatePartSelection infoRef name = do
		info <- readIORef infoRef
		let
			parts = guiParts info
			maybeI = nwFindPartIndex parts name
			Just i = maybeI
		selection <- Gtk.treeViewGetSelection $ guiPartList info
		Gtk.treeSelectionSelectPath selection $ if isJust maybeI then [i] else []

	partLeads :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	partLeads infoRef = do
		info <- readIORef infoRef
		maybePart <- guiPart infoRef
		let Just part = maybePart
		when (isJust maybePart) $ do
			setLeads infoRef $ makePartLeads part $ optEnabledOptims $ guiToolOptions info

	-- skipToPassingLead : skip leads in the given list until one passes.  Returns (skipped leads, tail of leads list
	-- starting with first passing lead)
	skipToPassingLead :: NetworkIF network => OptimContext network -> Part network -> [Lead network] ->
		([Lead network], [Lead network])
	skipToPassingLead context part leads = body [] leads
		where
			body skipped [] = (reverse skipped, [])
			body skipped (lead:leads)
				| isJust (optimPassesTest context part lead) = (reverse skipped, lead:leads)
				| otherwise = body (lead:skipped) leads

	modifyToolOptions :: NetworkIF network => IORef (GuiInfo network) ->
		(ToolOptions network -> ToolOptions network) -> IO ()
	modifyToolOptions infoRef f = modifyIORef infoRef $ \info -> info { guiToolOptions = f (guiToolOptions info) }

	compileBalsaToParts :: NetworkIF network => IORef (GuiInfo network) -> String ->
		IO (Context Decl, [Part network], [Report])
	compileBalsaToParts infoRef filename = do
		balsaSearchPath <- liftM (optBalsaSearchPath . guiToolOptions) $ readIORef infoRef
		let
			splitFilename = splitWith "/" filename
			dirName = joinWith "/" $ init splitFilename
			baseName = dirName ++ "/" ++ (joinWith "." $ init $ splitWith "." $ last splitFilename)
			otherPaths
				| dirName == "" = []
				| otherwise = [dirName]
		parseTree <- runWhyT $ compileBalsaFile (otherPaths ++ balsaSearchPath) filename
		let
			connect = defaultConnectWhy []

			unalias partsIn = map ((flip runPart_) networkRemoveAliases) partsIn

			Why r parts = parseTree `connect`
				(\context -> do
					parts <- teak defaultTeakOptions context
					return $ unalias parts)

			Why _ context = parseTree

		case r of
			Incomplete -> return (context, [], [Report (PosFile filename (ImportFile filename)) "incomplete"])
			Complete -> do
				info <- readIORef infoRef
				guiSetBaseName info $ Just baseName
				return (context, parts, [])
			Wrong reports -> return (context, [], reports)

	updateAll :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	updateAll infoRef = do
		updatePartList infoRef
		maybePart <- guiPart infoRef
		info <- readIORef infoRef
		if isJust maybePart
			then do
				partLeads infoRef
				redrawPart infoRef
			else do
				let layout = guiPartLayout info
				clearLayout layout
		updateLeads infoRef

	catchAndRestore :: NetworkIF network => IORef (GuiInfo network) -> IO () -> IO ()
	catchAndRestore infoRef m = do
		info <- readIORef infoRef
		part <- guiPart infoRef
		catch m $ \(ErrorCall str) -> do
			writeIORef infoRef info
			setPart infoRef part
			putStrLn $ "ERROR: " ++ str
			updateAll infoRef
			when (isJust part) $ updatePartSelection infoRef $ networkName $ fromJust part

	newParts :: NetworkIF network => IORef (GuiInfo network) -> [Part network] -> IO ()
	newParts _ [] = return ()
	newParts infoRef parts = do
		info <- readIORef infoRef
		setParts infoRef parts
		setPart infoRef Nothing
		setLeads infoRef []
		setLayoutButtonsSensitive (guiPartLayout info) False
		guiSetPartSelectedSensitive info False
		updateAll infoRef

	replacePart :: NetworkIF network => IORef (GuiInfo network) -> Part network -> IO ()
	replacePart infoRef newPart = do
		info <- readIORef infoRef
		oldPart <- guiPart infoRef
		let
			parts = guiParts info
			Just i = nwFindPartIndex parts $ networkName newPart
			parts' = replaceAt parts i newPart
		setParts infoRef parts'
		setPart infoRef $ Just newPart
		-- redraw if we were previously looking at this part
		when (isJust oldPart && networkName (fromJust oldPart) == networkName newPart) $ redrawPart infoRef
		return ()

	showNetworkDiff :: NetworkIF network => IORef (GuiInfo network) -> Part network -> Part network -> IO ()
	showNetworkDiff infoRef fromPart toPart = do
		showDiff <- isSetGuiOption infoRef GuiDiffWhenStepping
		when showDiff $ do
			info <- readIORef infoRef
			setLayoutPart (guiFromLayout info) $ Just fromPart
			setLayoutPart (guiToLayout info) $ Just toPart

			redrawNetworkDiff infoRef

	type GetSet whole elem = (whole -> elem, whole -> elem -> whole) 

	guiGetSet :: NetworkIF network => GetSet (ToolOptions network) [GuiOption]
	guiGetSet = (optGui, \toolOpts opts -> toolOpts { optGui = opts })

	plotGetSet :: NetworkIF network => GetSet (ToolOptions network) [PlotOption]
	plotGetSet = (optPlot, \toolOpts opts -> toolOpts { optPlot = opts })

	type ModifyM network subOption = IORef (GuiInfo network) -> subOption ->
		(Maybe subOption -> [subOption] -> [subOption]) -> IO ()
	type SetM network subOption = IORef (GuiInfo network) -> subOption -> IO ()
	type FindM network subOption = IORef (GuiInfo network) -> subOption -> IO (Maybe subOption)
	type GetM network subOption = IORef (GuiInfo network) -> subOption -> IO subOption
	type ClearM network subOption = IORef (GuiInfo network) -> subOption -> IO ()
	type IsSetM network subOption = IORef (GuiInfo network) -> subOption -> IO Bool

	-- modifySubOptionM : modify the sub-option of ToolOptions matching the prototype option.  Apply the
	--	function f to `Maybe' the option (if found in the given list) and the
	--	original option list w/o the named option to give a new option list.  That is, to *keep* an option
	--	you need to reinsert it in the list.
	--	`get' and `set' are functions to extract and insert the sub-options into ToolOptions
	modifySubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> ModifyM network subOption
	modifySubOptionM (get, set) infoRef prototypeOpt f = modifyToolOptions infoRef $ \toolOpts ->
		let
			opts = get toolOpts
			opt = findSubOption opts prototypeOpt
			optsWOOpt = removeSubOption opts prototypeOpt
			opts' = f opt optsWOOpt
		in
			set toolOpts opts'

	setSubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> SetM network subOption
	setSubOptionM getSet infoRef opt = modifySubOptionM getSet infoRef opt (const (opt:))

	clearSubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> ClearM network subOption
	clearSubOptionM getSet infoRef opt = modifySubOptionM getSet infoRef opt (const id)

	findSubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> FindM network subOption
	findSubOptionM (get, _) infoRef opt = do
		info <- readIORef infoRef
		return $ findSubOption (get $ guiToolOptions info) opt

	getSubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> GetM network subOption
	getSubOptionM (get, _) infoRef opt = do
		info <- readIORef infoRef
		return $ getSubOption (get $ guiToolOptions info) opt

	isSetSubOptionM :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> IsSetM network subOption
	isSetSubOptionM getSet infoRef opt = do
		option <- findSubOptionM getSet infoRef opt
		return $ isJust option

	modifyGuiOption :: NetworkIF network => ModifyM network GuiOption
	modifyGuiOption = modifySubOptionM guiGetSet

	-- findGuiOption :: NetworkIF network => FindM network GuiOption
	-- findGuiOption = findSubOptionM guiGetSet

	getGuiOption :: NetworkIF network => GetM network GuiOption
	getGuiOption = getSubOptionM guiGetSet

	setGuiOption :: NetworkIF network => SetM network GuiOption
	setGuiOption = setSubOptionM guiGetSet

	-- clearGuiOption :: NetworkIF network => ClearM network GuiOption
	-- clearGuiOption = clearSubOptionM guiGetSet

	isSetGuiOption :: NetworkIF network => IsSetM network GuiOption
	isSetGuiOption = isSetSubOptionM guiGetSet

	tableBoolOption :: (SubOption subOption, NetworkIF network) =>
		GetSet (ToolOptions network) [subOption] -> IO () ->
		IORef (GuiInfo network) -> String -> subOption -> TableOption
	tableBoolOption getSet afterSetAction infoRef desc opt =
		TableOption desc TableOptionTypeBool Nothing getOpt setOpt
		where
			getOpt = do
				value <- isSetSubOptionM getSet infoRef opt
				return $ TableOptionValueBool value

			setOpt (TableOptionValueBool value) = do
				(if value then setSubOptionM getSet else clearSubOptionM getSet) infoRef opt
				afterSetAction
			setOpt _ = error "tableBoolOption: not a boolean value"

	redrawNetworkDiff :: NetworkIF network => IORef (GuiInfo network) -> IO ()
	redrawNetworkDiff infoRef = do
		info <- readIORef infoRef
		fromLayout <- readIORef $ guiFromLayout info
		toLayout <- readIORef $ guiToLayout info
		GuiDiffDepth depth <- getGuiOption infoRef (GuiDiffDepth 1)

		when (isJust (layoutPart fromLayout) && isJust (layoutPart toLayout)) $ do
			let
				Just fromPart = layoutPart fromLayout
				Just toPart = layoutPart toLayout

			(from, to) <- diffParts depth fromPart toPart

			setLayoutGraphical (guiFromLayout info) $ Just from
			setLayoutGraphical (guiToLayout info) $ Just to

			presentWindow $ guiDiffWindow info

			info2 <- readIORef infoRef
			fitLayout (guiFromLayout info2)
			fitLayout (guiToLayout info2)
			exposeLayout (guiFromLayout info2)
			exposeLayout (guiToLayout info2)
			return ()

	setRules :: NetworkIF network => IORef (GuiInfo network) -> RuleSet network -> IO ()
	setRules infoRef ruleSet = do
		modifyToolOptions infoRef $ \opts -> opts { optRules = ruleSet }
		info <- readIORef infoRef
		let
			toolOpts = guiToolOptions info
			prevEnabledOptimNames = map optimName $ optEnabledOptims toolOpts
			prevAllOptimNames = map optimName $ optAllOptims toolOpts
			optims = map ruleToOptim $ ruleSetRules ruleSet

			-- use the current on/off optim. status for existing (re-read?) optims.
			enableOptim optim = optimName optim `elem` prevEnabledOptimNames ||
				(optimName optim `notElem` prevAllOptimNames && optimOnByDefault optim)
		modifyToolOptions infoRef $ \opts -> opts {
			optAllOptims = optims, optEnabledOptims = filter enableOptim optims }
		setLeads infoRef []
		updateLeads infoRef
		guiSetFlagList info

	layoutClicked :: NetworkIF network => IORef (GuiInfo network) -> IORef (GuiLayout network PartGraphical) ->
		DPoint -> Events.Event -> IO ()
	layoutClicked infoRef layoutRef point event = do
		(maybePart, maybeGraphical, drawingArea) <- do
			layout <- readIORef layoutRef
			return (layoutPart layout, layoutGraphical layout, layoutDrawingArea layout)
		when (isJust maybeGraphical && isJust maybePart) $ do
			let
				Just graphical = maybeGraphical
				Just part = maybePart
				comps = map Comp $ pointInBoundingBoxs (map compNodeBoundingBox) (partCompNodes graphical) point
				getComp = fromJust . tryPart part . nwGetComp
				firstComp = getComp (head comps)

				compStr = tryPart part $ do
					compBodys <- liftM (map fromJust) $ mapM nwGetComp comps
					return $ joinWith "\n" $ map show compBodys

			case (Events.eventClick event, length comps, firstComp) of
				(Events.DoubleClick, 1, _) | isInstanceComp firstComp -> do
					updatePartSelection infoRef $ nwPartName firstComp
				(Events.DoubleClick, 0, _) -> do
					info <- readIORef infoRef
					let
						parts = guiParts info

						parentPartAndComp part2 = catMaybes $ tryPart part2 $ nwMapComps $ \comp ->
							if isInstanceComp comp && nwPartName comp == networkName part
								then return $ Just (part2, refComp comp)
								else return Nothing
						parent = concatMap parentPartAndComp parts

					case parent of
						[(parentPart, compInParent)] -> do
							updatePartSelection infoRef $ networkName parentPart
							partLayoutSetHighlightComps layoutRef [compInParent]

							let compNode = IM.lookup (nwComp compInParent) $ partCompNodes graphical

							when (isJust compNode) $ do
								let Just (node:_) = compNode
								(width, height) <- Gtk.widgetGetSize drawingArea
								let
									viewMinorDimension = fromIntegral $ min width height
									compMajorDimension = max (compNodeWidth node) (compNodeHeight node)
									scale = (viewMinorDimension / compMajorDimension) * 0.2

								-- FIXME, check this is working properly

								print node
								print scale

								setLayoutViewOrigin layoutRef $ compNodeOrigin node
								setLayoutViewScale layoutRef scale

							exposeLayout layoutRef
						_ -> putStrLn $ "Can't find parent for part " ++ networkName part
				(Events.SingleClick, _, _) -> do
					partLayoutSetHighlightComps layoutRef comps
					exposeLayout layoutRef

					when (not $ null comps) $ do
						info <- readIORef infoRef
						let
							compRef = head comps
							leads = guiLeads info
							leadNos = findIndices ((== compRef) . leadCompNo) leads
							comp = getComp compRef
						let oLayout = guiOLayout info

						{- when (null leadNos) $ -}
						putStrLn compStr

						let showPortLinkInfo = True

						when showPortLinkInfo $ do
							let
								portInfo
									| isInstanceComp comp = map networkPortToPortInfo $ nwCompPorts comp
									| otherwise = teakCompPortInfo $ nwTeakCompInfo $ nwTeakType comp
								portInfoXlinks = zip portInfo (map flattenSome $ nwCompLinks comp)
							forM_ portInfoXlinks $ \(info, links) -> do
								let
									sense = networkPortSense info
									portName = networkPortName info
									arrayed = networkPortIsArrayed info
								forM (zip [(0::Int)..] links) $ \(i, link) -> do
									let width = tryPart part $ nwGetLinkWidth link
									putStr $ portName ++ (if arrayed then "[" ++ show i ++ "]"
										else "") ++ " " ++ show link ++ " (width = " ++ show width ++ ") "
									case sense of
										Passive -> do
											let prevs = tryPart part $ prevLinks [] link
											putStrLn $ "prev: " ++ show prevs
										Active -> do
											let nexts = tryPart part $ nextLinks False [] link
											putStrLn $ "next (->): " ++ show nexts
											let nexts = tryPart part $ nextLinks True [] link
											putStrLn $ "next (-|->): " ++ show nexts

						if isTeakO comp
							then do
								let
									TeakO terms = nwTeakType comp
									[One inp, _] = nwCompLinks comp
									inputWidth = tryPart part $ nwGetLinkWidth inp
								setLayoutPart oLayout maybePart
								oGraphical <- oGraphicalInfo inputWidth terms
								setLayoutGraphical oLayout $ Just oGraphical
								presentWindow $ guiOWindow info
								fitLayout oLayout
								exposeLayout oLayout
								setLayoutButtonsSensitive oLayout True
								return ()
							else do
								-- Gtk.widgetHide $ guiOWindow info
								{-
								layout <- readIORef oLayout
								when (isJust $ layoutGraphical layout) $ do
									clearLayout oLayout
								setLayoutGraphical oLayout Nothing
								setLayoutButtonsSensitive oLayout False
								-}
								return ()

						-- FIXME, multiple selection
						leadSelection <- Gtk.treeViewGetSelection $ guiLeadsList info
						Gtk.treeSelectionSelectPath leadSelection $ take 1 leadNos

				_ -> return ()

	makeGates :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> IO ()
	makeGates parent infoRef = do
		info <- readIORef infoRef
		let
			baseName = optBaseName $ guiToolOptions info
			defaultFilename = fromMaybe "netlist.v" $ do
				base <- baseName
				return $ base ++ ".v"

		ok <- Gtk.buttonNewFromStock Gtk.stockOk
		close <- Gtk.buttonNewFromStock Gtk.stockClose
		entry <- Gtk.entryNew
		Gtk.entrySetText entry defaultFilename

		techCombo <- Gtk.comboBoxNewText
		mappingFileEntry <- Gtk.entryNew
		techsModel <- Gtk.comboBoxGetModelText techCombo

		table <- Gtk.tableNew 3 2 False
		tableAddNameValue table 0 0 "Netlist File" entry
		tableAddNameValue table 0 1 "Technology" techCombo
		mappingLabel <- tableAddNameValue table 0 2 "Mapping File" mappingFileEntry

		let
			setMappingEntrySensitive = do
				activeRow <- Gtk.comboBoxGetActive techCombo
				let usingMappingFile = activeRow == 0
				Gtk.widgetSetSensitivity mappingFileEntry usingMappingFile
				Gtk.widgetSetSensitivity mappingLabel usingMappingFile

			setEntryTech = do
				let toolOpts = guiToolOptions info
				techs <- teakFindTechs toolOpts
				Gtk.listStoreClear techsModel
				mapM_ (Gtk.listStoreAppend techsModel) ("From Mapping File":techs)
				info <- readIORef infoRef
				let
					tech = optTech $ guiToolOptions info
					index = maybe 0 (+1) $ findIndex (== tech) techs
					usingMappingFile = index == 0
				Gtk.comboBoxSetActive techCombo index
				Gtk.entrySetText mappingFileEntry $ if usingMappingFile then tech else ""
				setMappingEntrySensitive

			getEntryTech = do
				activeRow <- Gtk.comboBoxGetActive techCombo
				case activeRow of
					0 -> Gtk.entryGetText mappingFileEntry
					_ -> Gtk.comboBoxGetActiveText techCombo >>= return . (fromMaybe "")

		setEntryTech

		Gtk.on techCombo Gtk.changed setMappingEntrySensitive

		window <- makeDialogue parent "Make Gate-Level Netlist" [ok, close] table -- vBox
		Gtk.widgetSetSizeRequest window 350 200

		Gtk.onClicked close $ Gtk.widgetDestroy window
		Gtk.onClicked ok $ do
			info2 <- readIORef infoRef
			filename <- Gtk.entryGetText entry
			let toolOpts = guiToolOptions info
			tech <- getEntryTech
			putStrLn $ "Tech: " ++ tech
			Why comp techMapping <- runWhyT $ teakFindTechMapping toolOpts tech
			case comp of
				Complete -> genMakeGatesFile True [] -- FIXME
					"by teak gui" -- FIXME
					techMapping
					filename
					(guiParts info2)
				_ -> do
					printCompleteness noPosContext comp
					return ()
			Gtk.widgetDestroy window

		presentWindow window

	makeFlagsWindow :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> IO Gtk.Window
	makeFlagsWindow parent infoRef = do
		scrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
		flagList <- Gtk.treeViewNew
		Gtk.set flagList [ Gtk.treeViewHeadersVisible Gtk.:= False ]
		Gtk.containerAdd scrolledWindow flagList
		Gtk.set scrolledWindow [ Gtk.scrolledWindowHscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic,
			Gtk.scrolledWindowVscrollbarPolicy Gtk.:= Gtk.PolicyAlways ]
		close <- Gtk.buttonNewFromStock Gtk.stockClose
		window <- makeDialogue parent "Optimisation Flags" [close] scrolledWindow
		Gtk.widgetSetSizeRequest window 500 500
		Gtk.onClicked close $ Gtk.widgetHide window
		Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True

		model <- Gtk.listStoreNew []

		textRenderer <- Gtk.cellRendererTextNew
		toggleRenderer <- Gtk.cellRendererToggleNew

		enableColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle enableColumn ""
		Gtk.treeViewColumnSetResizable enableColumn True
		Gtk.treeViewColumnPackStart enableColumn toggleRenderer True

		nameColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle nameColumn "Name"
		Gtk.treeViewColumnSetResizable nameColumn True
		Gtk.treeViewColumnPackStart nameColumn textRenderer True

		effectColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle effectColumn "Effect"
		Gtk.treeViewColumnSetResizable effectColumn True
		Gtk.treeViewColumnPackStart effectColumn textRenderer True

		let
			showEnable row = [ Gtk.cellToggleActive Gtk.:= flagViewEnabled row, Gtk.cellToggleActivatable Gtk.:= True ]
			showName row = [ Gtk.cellText Gtk.:= flagViewName row ]
			showEffect row = [ Gtk.cellText Gtk.:= flagViewEffect row ]

			updateFlags = do
				flagViews <- Gtk.listStoreToList model
				modifyToolOptions infoRef $ \opts -> opts { optEnabledOptims =
					mapMaybe flagViewOpt $ filter flagViewEnabled flagViews }

			setRowEnable enableF rowNo = do
				row <- Gtk.listStoreGetValue model rowNo
				Gtk.listStoreSetValue model rowNo $ row { flagViewEnabled = enableF (flagViewEnabled row) }

		Gtk.cellLayoutSetAttributes enableColumn toggleRenderer model showEnable
		Gtk.cellLayoutSetAttributes nameColumn textRenderer model showName
		Gtk.cellLayoutSetAttributes effectColumn textRenderer model showEffect

		Gtk.treeViewAppendColumn flagList enableColumn
		Gtk.treeViewAppendColumn flagList nameColumn
		Gtk.treeViewAppendColumn flagList effectColumn

		Gtk.on toggleRenderer Gtk.cellToggled $ \s -> do
			let
				rowNo :: Int
				rowNo = read s
			case rowNo of
				0 -> do
					size <- Gtk.listStoreGetSize model
					allRow <- Gtk.listStoreGetValue model 0
					mapM_ (setRowEnable (const (not (flagViewEnabled allRow)))) [0..size-1]
				_ -> do
					setRowEnable not rowNo
					setRowEnable (const False) 0
			updateFlags
			updateLeads infoRef

		Gtk.treeViewSetModel flagList model

		let
			setFlagList = do
				info <- readIORef infoRef
				let optims = optEnabledOptims $ guiToolOptions info

				let
					setFlagViewElem optim =
						FlagView (isJust (find (isOptim name) optims)) (Just optim) name effect
						where
							name = optimName optim
							effect = optimDescription optim
					allFlagView = FlagView False Nothing "all" "set/reset all flags"

				Gtk.listStoreClear model
				mapM_ (Gtk.listStoreAppend model) $
					allFlagView : (map setFlagViewElem (optAllOptims $ guiToolOptions info))

		modifyIORef infoRef $ \info -> info { guiSetFlagList = setFlagList }
		setFlagList

		return window

	makePreferencesWindow :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> IO Gtk.Window
	makePreferencesWindow parent infoRef = do
		table <- Gtk.tableNew 2 2 False
		let
			redraw = redrawPartAfterOptionChange infoRef
			-- FIXME, improve redraw options
			guiBoolOpt desc opt = tableBoolOption guiGetSet redraw infoRef desc opt
			plotBoolOpt desc opt = tableBoolOption plotGetSet redraw infoRef desc opt

			stateColourOpt state desc = TableOption desc TableOptionTypeColour Nothing get set
				where
					get = do
						info <- readIORef infoRef
						return $ TableOptionValueColour $ guiStateColours info ! state

					set (TableOptionValueColour colour) = do
						modifyIORef infoRef $ \info -> info {
							guiStateColours = (guiStateColours info) // [(state, colour)] }
						info <- readIORef infoRef
						exposeLayout $ guiPartLayout info
					set _ = error "stateColourOpt: bad value"

			options = [
				TableOption "Layout mode"
					(TableOptionTypeEnum ["dot", "spring"])
					Nothing
					(do
						GuiDisplayMode displayMode <- getGuiOption infoRef (GuiDisplayMode GuiDot)
						return $ TableOptionValueEnum $ fromEnum displayMode) 
					(\(TableOptionValueEnum i) -> do
						setGuiOption infoRef $ GuiDisplayMode $ toEnum i
						redrawPartAfterOptionChange infoRef
						return ()
						),
				guiBoolOpt "Show link widths as colours" GuiShowLinkWidthColours,
				guiBoolOpt "Show only passing leads" GuiOnlyPassingLeads,
				guiBoolOpt "Show network diff. when stepping" GuiDiffWhenStepping,
				guiBoolOpt "Show monitor events on network" GuiShowMonitorEvents,
				plotBoolOpt "Break Vs into read/write portions" PlotBreakVariables,
				TableOptionSpacer "Handshake State Colours" 2,
				stateColourOpt HS_SPACER "spacer (SPACER)",
				stateColourOpt HS_r "req. rising, incomplete (r)",
				stateColourOpt HS_R "req. up, complete (R)",
				stateColourOpt HS_RA "req up, ack. up (RA)",
				stateColourOpt HS_rA "req falling, ack up (rA)",
				stateColourOpt HS_A "just ack up (A)"
				]

		mapM (\(row, opt) -> tableOptionMakeTableEntry table 0 row opt) $ zip [0..] options

		close <- Gtk.buttonNewFromStock Gtk.stockClose
		scrolledWindow <- makeAlignedScrolledWindow table
		window <- makeDialogue parent "Preferences" [close] scrolledWindow
		Gtk.widgetSetSizeRequest window 350 400
		Gtk.onClicked close $ Gtk.widgetHide window
		Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True

		return window

	plot :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> IO ()
	plot parent infoRef = do
		info <- readIORef infoRef
		let
			toolOpts = guiToolOptions info
			PlotLanguage language0 = getSubOption (optPlot toolOpts) (PlotLanguage PlotPS)
			baseName = optBaseName toolOpts
			defaultFilename = do
				fromMaybe "plot.ps" $ do
					base <- baseName
					return $ base ++ ".ps"

		table <- Gtk.tableNew 2 2 False
		plotAll <- newIORef True
		plotFilename <- newIORef defaultFilename
		let
			boolOpt desc opt = tableBoolOption plotGetSet (return ()) infoRef desc opt

			languages = [PlotPS, PlotPDF, PlotSVG]

			options = [
				TableOption "Output file" TableOptionTypeString Nothing
					(readIORef plotFilename >>= return . TableOptionValueString)
					(\(TableOptionValueString value) -> writeIORef plotFilename value),
				boolOpt "Show part titles" PlotShowTitle,
				TableOption "Parts to plot" (TableOptionTypeEnum ["All", "Selected"]) Nothing
					(readIORef plotAll >>= return . TableOptionValueEnum . (\v -> if v then 0 else 1))
					(\(TableOptionValueEnum value) -> writeIORef plotAll $ value == 0),
				TableOption "Language" (TableOptionTypeEnum (map (map toUpper . show) languages)) Nothing
					(do
						info <- readIORef infoRef
						let
							opts = optPlot $ guiToolOptions info
							PlotLanguage lang = getSubOption opts $ PlotLanguage language0
						return $ TableOptionValueEnum $ fromJust $ findIndex (== lang) languages
						)
					(\(TableOptionValueEnum value) -> do
						modifyToolOptions infoRef $ \opts -> opts { optPlot =
							replaceSubOption (optPlot opts) $ PlotLanguage $ languages !! value })
				]

		mapM (\(row, opt) -> tableOptionMakeTableEntry table 0 row opt) $ zip [0..] options

		close <- Gtk.buttonNewFromStock Gtk.stockClose
		ok <- Gtk.buttonNewFromStock Gtk.stockOk
		scrolledWindow <- makeAlignedScrolledWindow table
		window <- makeDialogue parent "Plot Parts" [ok, close] scrolledWindow
		Gtk.widgetSetSizeRequest window 350 400
		Gtk.onClicked close $ Gtk.widgetHide window
		Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True

		Gtk.onClicked ok $ do
			info <- readIORef infoRef
			all <- readIORef plotAll
			filename <- readIORef plotFilename
			parts <- if all
				then return $ guiParts info
				else liftM maybeToList $ guiPart infoRef

			let
				guiOpts = optGui $ guiToolOptions info
				opts = replaceSubOption (optPlot $ guiToolOptions info) $ PlotOnlyParts $ map networkName parts
				pre
					| findBoolSubOption guiOpts GuiShowMonitorEvents = Just $ \part graphical -> renderMonitorEvents
						(guiStateColours info) guiOpts (optPartMonitorEvents toolOpts)
						(guiTime info) part graphical
					| otherwise = Nothing

			plotParts filename False opts False (makePartGraphical guiOpts) pre $ guiParts info
			return ()

			Gtk.widgetDestroy window

		presentWindow window

	makeInsertLatchesWindow :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> IO Gtk.Window
	makeInsertLatchesWindow parent infoRef = do
		table <- Gtk.tableNew 2 10 False
		let
			eqStrategy strategy opt@(LatchOption {}) = strategy == teakLatchStrategy opt
			eqStrategy _ _ = False

			updateDepth strategy depth opts = fromMaybe (newOpt:opts) $ do
				i <- findIndex (eqStrategy strategy) opts
				return $ replaceAt opts i newOpt
				where newOpt = LatchOption strategy depth

			getDepth strategy = do
				info <- readIORef infoRef
				let
					depth = fromMaybe 0 $ do
						oldOpt <- find (eqStrategy strategy) $ optLatch $ guiToolOptions info
						return $ teakLatchDepth oldOpt
				return depth

			get strategy = do
				depth <- getDepth strategy
				return $ TableOptionValueInt depth

			set opt (TableOptionValueInt value) = do
				modifyToolOptions infoRef $ \opts -> opts { optLatch = updateDepth opt value (optLatch opts) }
			set _ _ = error "set: not an integer"

			latchOpt (_, subOption) = case sampleValue of
				-- Only add `simple' rules
				([LatchOption strategy _], []) -> do
					apply <- Gtk.buttonNewFromStock Gtk.stockApply
					Gtk.onClicked apply $ do
						TableOptionValueInt depth <- get strategy
						insertLatches [LatchOption strategy depth]
					return $ Just $ TableOption (capitalise desc) (TableOptionTypeIntSpin 0 20)
						(Just (Gtk.castToWidget apply)) (get strategy) (set strategy)
				_ -> return Nothing
				where
					desc = subOptionDescription subOption
					sampleValue = subOptionParseValue subOption $ subOptionSampleValue subOption

			insertLatches opts = do
				maybePart <- guiPart infoRef
				when (isJust maybePart) $ do
					let
						Just part = maybePart
						opts' = filter (\opt -> isLatchOption opt && teakLatchDepth opt > 0) opts
						Why comp part' = runWhyTPart_ part $ nwInsertLatches opts'
					bad <- printCompleteness noPosContext comp
					when (not (null opts') && not bad) $ do
						replacePart infoRef part'
						setLeads infoRef []
						updateLeads infoRef
						pushHistory infoRef
						readIORef infoRef >>= fitLayout . guiPartLayout

			doApplyAll = do
				info <- readIORef infoRef
				let opts = optLatch $ guiToolOptions info
				insertLatches opts

		options <- liftM catMaybes $ mapM latchOpt $ subOptionUsages latchOptionUsage
		mapM (\(row, opt) -> tableOptionMakeTableEntry table 0 row opt) $ zip [0..] options

		ok <- Gtk.buttonNewFromStock Gtk.stockOk
		apply <- Gtk.buttonNewFromStock Gtk.stockApply
		close <- Gtk.buttonNewFromStock Gtk.stockClose
		scrolledWindow <- makeAlignedScrolledWindow table
		window <- makeDialogue parent "Insert Latches" [apply, ok, close] scrolledWindow
		Gtk.widgetSetSizeRequest window 500 450
		Gtk.onClicked close $ Gtk.widgetHide window
		Gtk.onClicked ok $ doApplyAll >> Gtk.widgetHide window
		Gtk.onClicked apply doApplyAll
		Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True

		return window

	makeDiffWindow :: NetworkIF network => Gtk.Window -> IORef (GuiInfo network) -> Bool -> Int ->
		IO (Gtk.Window, IORef (GuiLayout network PartGraphical), IORef (GuiLayout network PartGraphical))
	makeDiffWindow parent infoRef showDepth diffDepth = do
		(fromLayoutRef, fromLayoutWidget) <- makeLayoutTable (Nothing :: Maybe PartGraphical) False False Nothing
		setLayoutSizeRequest fromLayoutRef 200 200

		(toLayoutRef, toLayoutWidget) <- makeLayoutTable (Nothing :: Maybe PartGraphical) False False Nothing
		setLayoutSizeRequest toLayoutRef 200 200

		diffPanes <- makeHPaned fromLayoutWidget toLayoutWidget
		close <- Gtk.buttonNewFromStock Gtk.stockClose

		diffWindow <- if showDepth
			then do
				diffVBox <- Gtk.vBoxNew False 2
				Gtk.boxPackStart diffVBox diffPanes Gtk.PackGrow 2
				spinHBox <- Gtk.hBoxNew False 2
				diffSpin <- Gtk.spinButtonNewWithRange 1 10 1
				Gtk.spinButtonSetValue diffSpin (fromIntegral diffDepth)
				Gtk.onValueSpinned diffSpin $ do
					-- FIXME, don't know why this is invoked more than once for each click
					depth <- liftM floor $ Gtk.spinButtonGetValue diffSpin
					modifyGuiOption infoRef (GuiDiffDepth 1) $ \_ opts -> GuiDiffDepth depth : opts
					redrawNetworkDiff infoRef
				spinLabel <- Gtk.labelNewWithMnemonic "Depth"
				Gtk.boxPackStart spinHBox spinLabel Gtk.PackNatural 2
				Gtk.boxPackStart spinHBox diffSpin Gtk.PackGrow 2
				Gtk.boxPackStart diffVBox spinHBox Gtk.PackNatural 2
				makeDialogue parent "Network difference" [close] diffVBox
			else makeDialogue parent "Network difference" [close] diffPanes

		Gtk.onClicked close $ Gtk.widgetHide diffWindow
		Gtk.on diffWindow Gtk.deleteEvent $ lift $ Gtk.widgetHide diffWindow >> return True

		return (diffWindow, fromLayoutRef, toLayoutRef)

	xmlTag :: String -> [(String, String)] -> Bool -> String
	xmlTag tag nvps close = "<" ++ tag ++ nvps' ++ (if close then "/" else "") ++ ">"
		where
			nvps'
				| null nvps = ""
				| otherwise = " " ++ (joinWith " " $ map (\(name, value) -> name ++ "=\"" ++ value ++ "\"") nvps)

	xmlCloseTag :: String -> String
	xmlCloseTag tag = "</" ++ tag ++ ">"

	xmlFormat :: String -> [(String, String)] -> [String] -> String
	xmlFormat tag nvps bodies = xmlTag tag nvps False ++ "\n" ++
		joinWith "\n" (map (indent tab) bodies) ++ "\n" ++ xmlCloseTag tag
		where tab = "\t"

	mainMenuUI :: String
	mainMenuUI = xmlFormat "ui" [] [
		xmlFormat "menubar" [("name", "mainMenubar")] menus,
		xmlFormat "toolbar" [("name", "leadsToolbar")] leadsToolitems,
		xmlFormat "toolbar" [("name", "timeToolbar")] timeToolitems]
		where
			menus = [
				menu "File" [
					menuitem "Compile Balsa...", menuitem "Open Network...",
					menuitem "Save Network", menuitem "Save Network As...",
					separator,
					menuitem "Make Gate-Level Netlist...",
					menuitem "Plot...",
					separator,
					menuitem "Read Optimisation Rules...",
					menuitem "Read Monitor Events...",
					separator,
					menuitem "Quit"],
				menu "Edit" [
					menuitem "Undo", menuitem "Redo",
					separator,
					menuitem "Optimisation Flags...",
					menuitem "Preferences..."],
				menu "Selected Part" [
					menuitem "Set As Top Level",
					menuitem "Insert Latches...",
					menuitem "Remove All Latches",
					menuitem "Experimental" ],
				menu "Windows" [
					menuitem "Time Window",
					menuitem "Network Diff. Window",
					menuitem "O Component Window"
				]]

			leadsToolitems = [
				toolitem "New Leads",
				toolitem "(Leads) Apply All",
				toolitem "(Leads) Step" ]

			timeToolitems = [
				toolitem "(Time) Play",
				toolitem "(Time) Stop",
				toolitem "(Time) Step Backward",
				toolitem "(Time) Step Forward" ]

			menu name items = xmlFormat "menu" [("action", name)] items
			menuitem action = xmlTag "menuitem" [("action", action)] True
			toolitem action = xmlTag "toolitem" [("action", action)] True
			separator = xmlTag "separator" [] True

	makeTimeWindow :: NetworkIF network => IORef (GuiInfo network) -> Gtk.Window -> Gtk.UIManager ->
		(Integer, Integer) -> Integer -> IO (Gtk.Window, (Integer, Integer) -> IO ())
	makeTimeWindow infoRef parent ui timeLimits0 stepSize0 = do
		-- Note that makeTimeWindow *must not* use infoRef in setting up the window, only once events start
		--	to be received
		close <- Gtk.buttonNewFromStock Gtk.stockClose
		let (low, high) = timeLimits0

		Just playAction <- Gtk.uiManagerGetAction ui "/ui/timeToolbar/(Time) Play"
		Just stopAction <- Gtk.uiManagerGetAction ui "/ui/timeToolbar/(Time) Stop"
		Just stepForwardAction <- Gtk.uiManagerGetAction ui "/ui/timeToolbar/(Time) Step Forward"
		Just stepBackwardAction <- Gtk.uiManagerGetAction ui "/ui/timeToolbar/(Time) Step Backward"

		toolbar <- liftM (Gtk.castToToolbar . fromJust) $ Gtk.uiManagerGetWidget ui "/ui/timeToolbar"
		toolbarSetup toolbar Gtk.OrientationHorizontal Gtk.ToolbarIcons

		vBox <- Gtk.vBoxNew False 2

		table <- Gtk.tableNew 3 2 False
		scrolledWindow <- makeAlignedScrolledWindow table

		Gtk.boxPackStart vBox toolbar Gtk.PackNatural 0
		Gtk.boxPackStart vBox scrolledWindow Gtk.PackGrow 0

		timeScale <- Gtk.hScaleNewWithRange (fromInteger low) (fromInteger high) 1
		Gtk.scaleSetDigits timeScale 0

		stepSpin <- Gtk.spinButtonNewWithRange 1 1000000 $ fromInteger stepSize0
		Gtk.spinButtonSetValue stepSpin $ fromInteger stepSize0
		tableAddNameValue table 0 1 "Time" timeScale
		tableAddNameValue table 0 2 "Play Step" stepSpin

		playHandler <- newIORef (Nothing :: Maybe Gtk.HandlerId)

		let
			setTimeLimits (low, high)
				| high <= low = setTimeLimits (low, low + 1)
				| otherwise = do
					info <- readIORef infoRef
					let
						time = guiTime info
						time'
							| low > time = low
							| high < time = high
							| otherwise = time
					Gtk.rangeSetRange timeScale (fromInteger low) (fromInteger high)
					setTime time'
					modifyIORef infoRef $ \info -> info { guiTimeLimits = (low, high) }

			setTime :: Integer -> IO ()
			setTime time = do
				Gtk.rangeSetValue timeScale (fromInteger time)
				modifyIORef infoRef $ \info -> info { guiTime = time }

			getStepSize = do
				GuiTimeStepSize stepSize <- getGuiOption infoRef (GuiTimeStepSize 100)
				return stepSize

			atEndOfTrace = do
				info <- readIORef infoRef
				let
					(_, high) = guiTimeLimits info
					time = guiTime info
				stepSize <- getStepSize
				return $ time + stepSize > high

			stopPlaying = do
				handler <- readIORef playHandler
				when (isJust handler) $ Gtk.timeoutRemove $ fromJust handler
				writeIORef playHandler Nothing
				return ()

			step stepMult = do
				atEnd <- atEndOfTrace
				when (not atEnd) $ do
					info <- readIORef infoRef
					stepSize <- getStepSize
					let time' = guiTime info + stepMult * stepSize
					setTime time'
				return atEnd

			playStep = do
				playing <- readIORef playHandler
				when (isJust playing) $ do
					atEnd <- step 1
					when atEnd stopPlaying
				liftM isJust $ readIORef playHandler

		Gtk.onValueSpinned stepSpin $ do
			stepSize <- Gtk.spinButtonGetValue stepSpin
			setGuiOption infoRef $ GuiTimeStepSize $ floor stepSize

		Gtk.onRangeValueChanged timeScale $ do
			newTime <- liftM floor $ Gtk.rangeGetValue timeScale
			modifyIORef infoRef $ \info -> info { guiTime = newTime }
			info <- readIORef infoRef
			exposeLayout $ guiPartLayout info

		Gtk.on playAction Gtk.actionActivated $ do
			playing <- readIORef playHandler
			when (isNothing playing) $ do
				atEnd <- atEndOfTrace
				when atEnd $ do
					(low, _) <- liftM guiTimeLimits $ readIORef infoRef
					setTime low
				handler <- Gtk.timeoutAddFull playStep Gtk.priorityLow 50
				writeIORef playHandler $ Just handler
			return ()

		Gtk.on stopAction Gtk.actionActivated stopPlaying
		Gtk.on stepForwardAction Gtk.actionActivated $ step 1 >> return ()
		Gtk.on stepBackwardAction Gtk.actionActivated $ step (-1) >> return ()

		window <- makeDialogue parent "Time" [close] vBox

		let closeWindow = stopPlaying >> Gtk.widgetHide window

		Gtk.onClicked close closeWindow
		Gtk.on window Gtk.deleteEvent $ do
			lift $ closeWindow
			return True
		Gtk.widgetSetSizeRequest window 350 200
		return (window, setTimeLimits)

	-- gui : start GUI
	-- gui :: (Read network, NetworkIF network) => ToolOptions network -> [Part network] -> IO [Part network]
	gui toolOpts0 parts0 = do
		Gtk.initGUI

		infoRef <- newIORef (undefined :: GuiInfo network)
		actions <- Gtk.actionGroupNew "Actions"

		-- Get some of the initial toolOpts/guiOpts that are needed to build windows etc.
		let
			GuiDisplayMode displayMode0 = getSubOption (optGui toolOpts0) $ GuiDisplayMode GuiDot
			GuiTimeStepSize timeStepSize0 = getSubOption (optGui toolOpts0) $ GuiTimeStepSize 100
			GuiDiffDepth diffDepth0 = getSubOption (optGui toolOpts0) $ GuiDiffDepth 1

			newAction name displayedName maybeTooltip maybeIcon maybeAccel = do
				action <- Gtk.actionNew name displayedName maybeTooltip maybeIcon
				Gtk.actionGroupAddActionWithAccel actions action maybeAccel
				return action

		newAction "File" "_File" Nothing Nothing Nothing
		newAction "Edit" "_Edit" Nothing Nothing Nothing
		partAction <- newAction "Selected Part" "Selected _Part" Nothing Nothing Nothing
		newAction "Windows" "_Windows" Nothing Nothing Nothing

		compileBalsaAction <- newAction "Compile Balsa..." "Compile Balsa..."
			Nothing (Just Gtk.stockOpen) (Just "<Control>b")
		openNetworkAction <- newAction "Open Network..." "Open Network..."
			Nothing (Just Gtk.stockOpen) (Just "<Control>o")
		saveNetworkAction <- newAction "Save Network" "Save Network" Nothing (Just Gtk.stockSave) (Just "<Control>s")
		saveAsNetworkAction <- newAction "Save Network As..." "Save Network As..."
			Nothing (Just Gtk.stockSaveAs) Nothing
		readRulesAction <- newAction "Read Optimisation Rules..." "Read Optimisation Rules..."
			Nothing (Just Gtk.stockOpen) (Just "")
		readMonitorEventsAction <- newAction "Read Monitor Events..." "Read Monitor Events..."
			Nothing (Just Gtk.stockOpen) (Just "")
		quitAction <- newAction "Quit" "Quit" Nothing (Just Gtk.stockQuit) (Just "<Control>q")

		undoAction <- newAction "Undo" "Undo" Nothing (Just Gtk.stockUndo) (Just "<Control>z")
		redoAction <- newAction "Redo" "Redo" Nothing (Just Gtk.stockRedo) (Just "<Control>y")
		flagsAction <- newAction "Optimisation Flags..." "Optimisation Flags..."
			Nothing (Just Gtk.stockPreferences) Nothing
		guiOptionsAction <- newAction "Preferences..." "Preferences..." Nothing (Just Gtk.stockPreferences) Nothing

		makeGatesAction <- newAction "Make Gate-Level Netlist..." "Make Gate-Level Netlist..."
			Nothing (Just Gtk.stockExecute) Nothing
		plotAction <- newAction "Plot..." "Plot..." Nothing (Just Gtk.stockPrint) Nothing
		topLevelAction <- newAction "Set As Top Level" "Set As Top Level" Nothing (Just Gtk.stockExecute) Nothing
		insertLatchesAction <- newAction "Insert Latches..." "Insert Latches..."
			Nothing (Just Gtk.stockExecute) Nothing
		removeLatchesAction <- newAction "Remove All Latches" "Remove All Latches"
			Nothing (Just Gtk.stockExecute) Nothing
		experimentalAction <- newAction "Experimental" "Experimental" Nothing (Just Gtk.stockExecute) Nothing
		showTimeWindowAction <- newAction "Time Window" "Time Window" Nothing Nothing Nothing
		showNetworkDiffWindowAction <- newAction "Network Diff. Window" "Network Diff. Window" Nothing Nothing Nothing
		showOComponentWindowAction <- newAction "O Component Window" "O Component Window" Nothing Nothing Nothing

		leadsAction <- newAction "New Leads" "New Leads" Nothing (Just Gtk.stockRefresh) Nothing
		optAction <- newAction "(Leads) Apply All" "Apply All" Nothing (Just Gtk.stockApply) Nothing
		stepAction <- newAction "(Leads) Step" "Step" Nothing (Just Gtk.stockGoForward) Nothing

		do
			newAction "(Time) Play" "Play" (Just "Play") (Just Gtk.stockMediaPlay) Nothing
			newAction "(Time) Stop" "Stop" (Just "Stop") (Just Gtk.stockMediaStop) Nothing
			newAction "(Time) Step Forward" "Step Forward" (Just "Step Forward") (Just Gtk.stockGoForward) Nothing
			newAction "(Time) Step Backward" "Step Backward" (Just "Step Backward") (Just Gtk.stockGoBack) Nothing

		ui <- Gtk.uiManagerNew
		Gtk.uiManagerAddUiFromString ui mainMenuUI
		Gtk.uiManagerInsertActionGroup ui actions 0
		Just mainMenubar <- Gtk.uiManagerGetWidget ui "/ui/mainMenubar"

		-- partToolbar <- liftM (Gtk.castToToolbar . fromJust) $ Gtk.uiManagerGetWidget ui "/ui/partToolbar"
		-- toolbarSetup partToolbar Gtk.OrientationHorizontal Gtk.ToolbarText

		leadsToolbar <- liftM (Gtk.castToToolbar . fromJust) $ Gtk.uiManagerGetWidget ui "/ui/leadsToolbar"
		toolbarSetup leadsToolbar Gtk.OrientationHorizontal Gtk.ToolbarBothHoriz

		(partLayoutRef, partWidget) <- makeLayoutTable (Nothing :: Maybe PartGraphical) True
			(displayMode0 == GuiSpring) (Just (layoutClicked infoRef))
		partFrame <- makeFrame "Selected Part" partWidget

		partsScrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
		partsList <- Gtk.treeViewNew
		Gtk.set partsList [ Gtk.treeViewHeadersVisible Gtk.:= False ]
		Gtk.containerAdd partsScrolledWindow partsList
		Gtk.set partsScrolledWindow [
			Gtk.scrolledWindowHscrollbarPolicy Gtk.:= Gtk.PolicyNever,
			Gtk.scrolledWindowVscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic ]

		let partsVBox = partsScrolledWindow

		partsFrame <- makeFrame "Parts" partsVBox

		leadsScrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
		leadsList <- Gtk.treeViewNew
		Gtk.containerAdd leadsScrolledWindow leadsList
		Gtk.set leadsScrolledWindow [ Gtk.scrolledWindowHscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic,
			Gtk.scrolledWindowVscrollbarPolicy Gtk.:= Gtk.PolicyAlways ]

		leadsVBox <- Gtk.vBoxNew False 2
		Gtk.boxPackStart leadsVBox leadsToolbar Gtk.PackNatural 2
		Gtk.boxPackStart leadsVBox leadsScrolledWindow Gtk.PackGrow 2

		leadsFrame <- makeFrame "Optimisation Leads" leadsVBox

		partsAndLeadsPaned <- makeVPaned partsFrame leadsFrame
		Gtk.set partsAndLeadsPaned [ Gtk.panedPosition Gtk.:= 200 ]
		mainHPaned <- makeHPaned partFrame partsAndLeadsPaned
		Gtk.set mainHPaned [ Gtk.panedPosition Gtk.:= 400 ]

		status <- Gtk.statusbarNew

		vBox <- Gtk.vBoxNew False 2
		Gtk.boxPackStart vBox mainMenubar Gtk.PackNatural 2
		Gtk.boxPackStart vBox mainHPaned Gtk.PackGrow 2
		Gtk.boxPackStart vBox status Gtk.PackNatural 2
		mainWindow <- Gtk.windowNew
		Gtk.set mainWindow [ Gtk.windowTitle Gtk.:= "Teak", Gtk.windowAllowShrink Gtk.:= True ]
		Gtk.containerAdd mainWindow vBox
		Gtk.widgetSetSizeRequest mainWindow 800 600

		(diffWindow, fromLayoutRef, toLayoutRef) <- makeDiffWindow mainWindow infoRef True diffDepth0

		(oLayoutRef, oLayoutWidget) <- makeLayoutTable (Nothing :: Maybe OGraphical) True False Nothing
		oWindow <- do
			close <- Gtk.buttonNewFromStock Gtk.stockClose
			window <- makeDialogue mainWindow "Operator" [close] oLayoutWidget
			setLayoutSizeRequest oLayoutRef 400 300
			Gtk.onClicked close $ Gtk.widgetHide window
			Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True
			return window

		(timeWindow, setTimeLimits) <- makeTimeWindow infoRef mainWindow ui (0, 1) timeStepSize0

		historyRef <- newIORef (GuiHistory False 0 10 [] (historySetSensitive undoAction redoAction))

		let
			partSensitiveActions = [optAction, leadsAction, stepAction, partAction, topLevelAction,
				insertLatchesAction, removeLatchesAction, experimentalAction]
			partSetSensitive s = forM_ partSensitiveActions $ \a -> Gtk.actionSetSensitive a s

			setBaseName baseName = do
				modifyToolOptions infoRef $ \opts -> opts { optBaseName = baseName }
				when (isJust baseName) $ do
					modifyIORef infoRef $ \info -> info { guiLastNetworkFile = Just $ case guiLastNetworkFile info of
						Nothing -> fromJust baseName <.> "teak"
						Just oldName -> replaceBaseName oldName (fromJust baseName) }
				Gtk.actionSetSensitive saveNetworkAction $ isJust baseName

			defaultRulesFile = teakOptimPath toolOpts0 </> "default" <.> "rules"

		writeIORef infoRef $ GuiInfo {
			guiHistory = historyRef,
			guiPartLayout = partLayoutRef,
			guiFromLayout = fromLayoutRef,
			guiToLayout = toLayoutRef,
			guiOLayout = oLayoutRef,
			guiParts = parts0,
			guiLeads = [],
			guiUpdateLeads = return (),
			guiGetLeadView = const Nothing,
			guiPartList = partsList,
			guiLeadsList = leadsList,
			guiSetFlagList = return (),
			guiPartListModel = undefined,
			guiDiffWindow = diffWindow,
			guiTimeWindow = timeWindow,
			guiOWindow = oWindow,
			guiTimeLimits = (0, 1),
			guiTime = 0,
			guiStateColours = defaultMonitorStateColours,
			guiSetPartSelectedSensitive = partSetSensitive,
			guiSetTimeLimits = setTimeLimits,
			guiSetBaseName = setBaseName,
			guiLastBalsaFile = Nothing,
			guiLastNetworkFile = Nothing,
			guiLastOptimRulesFile = Just defaultRulesFile,
			guiToolOptions = toolOpts0
			}

		setTimeLimits $ partMonitorEventsTimeLimits $ optPartMonitorEvents toolOpts0
		setBaseName $ optBaseName toolOpts0

		initLeadsList infoRef leadsList

		flagsWindow <- makeFlagsWindow mainWindow infoRef
		guiOptionsWindow <- makePreferencesWindow mainWindow infoRef
		insertLatchesWindow <- makeInsertLatchesWindow mainWindow infoRef

		let makeSaveAsNetworkFileChooser = do
			info <- readIORef infoRef
			makeFileChooser (Just mainWindow) "Save Network" (guiLastNetworkFile info) Gtk.FileChooserActionSave
				"gtk-save" $ \filename -> do
					modifyIORef infoRef $ \info -> info { guiLastNetworkFile = Just filename }
					info <- readIORef infoRef
					putStrLn $ "saving network to file `" ++ filename ++ "'"
					writeNetworkFile False "" filename $ guiParts info
					return True
			return ()

		Gtk.on compileBalsaAction Gtk.actionActivated $ do
			info <- readIORef infoRef
			chooser <- makeFileChooser (Just mainWindow) "Compile Balsa" (guiLastBalsaFile info)
				Gtk.FileChooserActionOpen
				"gtk-open" $ \filename -> do
					modifyIORef infoRef $ \info -> info { guiLastBalsaFile = Just filename }
					(context, parts, reports) <- compileBalsaToParts infoRef filename
					case (parts, reports) of
						([], []) -> do
							dialogue <- Gtk.messageDialogNew Nothing [Gtk.DialogModal] Gtk.MessageWarning
								Gtk.ButtonsYesNo $ "File `" ++ filename ++ "' contains no procedures\n\nLoad anyway?"
							response <- Gtk.dialogRun dialogue
							Gtk.widgetDestroy dialogue
							case response of
								Gtk.ResponseYes -> do
									newParts infoRef []
									return True
								_ -> return False
						(_, _:_) -> do
							dialogue <- Gtk.messageDialogNew Nothing [Gtk.DialogModal] Gtk.MessageError
								Gtk.ButtonsYesNo $ "File `" ++ filename ++ "' has errors\n\nView errors?"
							response <- Gtk.dialogRun dialogue
							Gtk.widgetDestroy dialogue
							case response of
								Gtk.ResponseYes -> do
									makeSourceErrorViewWindow mainWindow "Source Errors"
										(Just context) filename reports
									return False -- set to True if we want the file chooser to go away here
								_ -> return False
						(_:_, []) -> do
							newParts infoRef parts
							return True

			fileChooserAddFilters chooser [("*", "All files"), ("*.balsa", "Balsa files")]

		Gtk.on saveNetworkAction Gtk.actionActivated $ do
			info <- readIORef infoRef
			let baseName = optBaseName $ guiToolOptions info

			if isJust baseName
				then do
					let fileName = fromJust baseName ++ ".teak"
					writeNetworkFile False "" fileName $ guiParts info
				else makeSaveAsNetworkFileChooser

		Gtk.on saveAsNetworkAction Gtk.actionActivated $ makeSaveAsNetworkFileChooser

		Gtk.on openNetworkAction Gtk.actionActivated $ do
			info <- readIORef infoRef
			chooser <- makeFileChooser (Just mainWindow) "Open Network" (guiLastNetworkFile info)
				Gtk.FileChooserActionOpen
				"gtk-open" $ \filename -> do
					modifyIORef infoRef $ \info -> info { guiLastNetworkFile = Just filename }
					Why comp parts <- runWhyT $ readNetworkFile filename
					case comp of
						Complete -> do
							newParts infoRef parts
							pushHistory infoRef
							return True
						_ -> return False

			fileChooserAddFilters chooser [("*", "All files"), ("*.teak", "Teak network files")]
			return ()

		Gtk.on readRulesAction Gtk.actionActivated $ do
			info <- readIORef infoRef
			chooser <- makeFileChooser (Just mainWindow) "Read Optimisation Rules" (guiLastOptimRulesFile info)
				Gtk.FileChooserActionOpen
				"gtk-open" $ \filename -> do
					modifyIORef infoRef $ \info -> info { guiLastOptimRulesFile = Just filename }
					contents <- readFile filename
					let
						filePos = PosLC (PosFile filename (ImportFile filename)) 1 1
						Why rulesComp ruleSet = parseRules filePos contents
					case rulesComp of
						Complete -> do
							putStrLn $ "*** read " ++ summariseRuleSet ruleSet (" from file " ++ filename)
							setRules infoRef ruleSet
							pushHistory infoRef
							return True
						Wrong reports -> do
							dialogue <- Gtk.messageDialogNew Nothing [Gtk.DialogModal] Gtk.MessageError
								Gtk.ButtonsYesNo $ "File `" ++ filename ++ "' has errors\n\nView errors?"
							response <- Gtk.dialogRun dialogue
							Gtk.widgetDestroy dialogue
							case response of
								Gtk.ResponseYes -> do
									makeSourceErrorViewWindow mainWindow "Rule Errors"
										(Nothing :: Maybe (Context Decl)) filename reports
									return False -- set to True if we want the file chooser to go away here
								_ -> return False
						Incomplete -> return False

			fileChooserAddFilters chooser [("*", "All files"), ("*.rules", "Optim. rules files")]
			return ()

		Gtk.on readMonitorEventsAction Gtk.actionActivated $ do
			chooser <- makeFileChooser (Just mainWindow) "Read Monitor Events Rules" Nothing Gtk.FileChooserActionOpen
				"gtk-open" $ \filename -> do
					info <- readIORef infoRef
					Why comp monitorEvents <- runWhyT $ readMonitorFile (guiParts info) filename
					case comp of
						Complete -> do
							putStrLn $ "*** read events from file " ++ filename
							modifyToolOptions infoRef $ \opts -> opts { optPartMonitorEvents = monitorEvents }
							info <- readIORef infoRef
							guiSetTimeLimits info $ partMonitorEventsTimeLimits monitorEvents
							return True
						Wrong reports -> do
							dialogue <- Gtk.messageDialogNew Nothing [Gtk.DialogModal] Gtk.MessageError
								Gtk.ButtonsYesNo $ "File `" ++ filename ++ "' has errors\n\nView errors?"
							response <- Gtk.dialogRun dialogue
							Gtk.widgetDestroy dialogue
							case response of
								Gtk.ResponseYes -> do
									makeSourceErrorViewWindow mainWindow "Monitor Event Errors"
										(Nothing :: Maybe (Context Decl)) filename reports
									return False -- set to True if we want the file chooser to go away here
								_ -> return False
						Incomplete -> return False

			fileChooserAddFilters chooser [("*", "All files"), ("*.report", "Monitor state files")]
			return ()

		Gtk.on quitAction Gtk.actionActivated Gtk.mainQuit

		Gtk.onDestroy mainWindow Gtk.mainQuit

		{- Populate structures, choose inital part and setup TreeView widgets -}

		partsListModel <- makePartList partsList infoRef
		modifyIORef infoRef $ \info -> info { guiPartListModel = partsListModel }

		updateLeads infoRef

		Gtk.on topLevelAction Gtk.actionActivated $ do
			catchAndRestore infoRef $ do
				info <- readIORef infoRef
				Just part <- guiPart infoRef
				let
					parts = guiParts info
					topLevelName = networkName part
					parts' = uniquifyPart parts topLevelName
					parts'' = removeGo parts' topLevelName
				let Just part' = nwFindPart parts'' topLevelName
				setParts infoRef parts''
				setPart infoRef (Just part')
				updateAll infoRef
				updatePartSelection infoRef $ networkName part'
				
		Gtk.on insertLatchesAction Gtk.actionActivated $ Gtk.widgetShowAll insertLatchesWindow
		Gtk.on removeLatchesAction Gtk.actionActivated $ do
			maybePart <- guiPart infoRef
			when (isJust maybePart) $ do
				let part' = runPart_ (fromJust maybePart) nwRemoveLatches
				replacePart infoRef part'
				setLeads infoRef []
				updateLeads infoRef
				pushHistory infoRef
				readIORef infoRef >>= fitLayout . guiPartLayout

		Gtk.on experimentalAction Gtk.actionActivated $ do
			catchAndRestore infoRef $ do
				Just part <- guiPart infoRef
				putStrLn "*** findLinkDeps"
				let linkDeps = tryPart part findLinkDeps
				mapM_ print linkDeps
				putStrLn "*** rootComponents"
				let root = rootComponents linkDeps
				mapM_ print root
				putStrLn "*** makeEdges"
				let edges = makeEdges linkDeps
				mapM_ print edges
				putStrLn "*** loop"
				let loop = loopLinks edges root
				mapM_ print loop
				-- let backLinks = tryPart part $ findBackLinks 10
				-- print backLinks
				-- backLinks <- findBackLinks2 10
				-- print backLinks
				info <- readIORef infoRef
				let builtinInfo = findBuiltins $ guiParts info
				print $ fbsPartStates builtinInfo

		setLayoutPreRender partLayoutRef $ \layout -> do
			info <- readIORef infoRef
			let
				toolOpts = guiToolOptions info
				guiOpts = optGui toolOpts
			if findBoolSubOption guiOpts GuiShowMonitorEvents
				then return $ fromMaybe (const (return ())) $ do
					part <- layoutPart layout
					graphical <- layoutGraphical layout
					return $ renderMonitorEvents (guiStateColours info) guiOpts (optPartMonitorEvents toolOpts)
						(guiTime info) part graphical
				else return (const (return ()))

		-- Gtk.onClicked flagsButton $ Gtk.widgetShowAll flagsWindow
		Gtk.on flagsAction Gtk.actionActivated $ presentWindow flagsWindow
		Gtk.on guiOptionsAction Gtk.actionActivated $ presentWindow guiOptionsWindow

		Gtk.on leadsAction Gtk.actionActivated $ do
		-- Gtk.onToolButtonClicked leadsButton $ do
			pushHistory infoRef
			info <- readIORef infoRef
			maybePart <- guiPart infoRef
			when (isJust maybePart) $ do
				setLeads infoRef $ makePartLeads (fromJust maybePart) $ optEnabledOptims $ guiToolOptions info
				updateLeads infoRef

		Gtk.on optAction Gtk.actionActivated $ do
		-- Gtk.onToolButtonClicked optButton $ do
			info <- readIORef infoRef
			maybePart <- guiPart infoRef
			let Just part = maybePart
			when (isJust maybePart) $ do
				let
					parts = guiParts info
					Just i = nwFindPartIndex parts $ networkName part
					leads = guiLeads info
					context = OptimContext { optimContextParts = parts }
				Why comp part' <- runWhyT $ applyLeads context part noOptimLog 
					(optEnabledOptims $ guiToolOptions info) leads
				let parts' = replaceAt parts i part'

				stop <- printCompleteness noPosContext comp

				when (not stop) $ do
					showNetworkDiff infoRef part part'
					-- Check
					printCompleteness noPosContext $ gatherCompleteness $ map checkPart parts'
					setParts infoRef parts'
					setPart infoRef $ Just part'
					setLeads infoRef $ makePartLeads part' $ optEnabledOptims $ guiToolOptions info
					-- setLeads infoRef []
					redrawPart infoRef
					updateLeads infoRef
				pushHistory infoRef

		Gtk.on stepAction Gtk.actionActivated $ do
		-- Gtk.onToolButtonClicked stepButton $ do
			-- FIXME, need to remove leads (or indicate when stepping) whose components have disappeared
			info <- readIORef infoRef
			maybePart <- guiPart infoRef
			let Just part = maybePart
			when (isJust maybePart) $ do
				-- Step when a line is selected will apply that lead (or the next one that actually passes)
				selection <- Gtk.treeViewGetSelection leadsList
				selectedLeads <- liftM ((map leadViewIndex) . (mapMaybe (guiGetLeadView info . head))) $
					Gtk.treeSelectionGetSelectedRows selection
				let
					parts = guiParts info
					Just i = nwFindPartIndex parts $ networkName part
					context = OptimContext { optimContextParts = parts }
					allLeads = guiLeads info

					(skippedLeads, tryLeads) = case selectedLeads of
						[selectedLeadIndex] -> (firstLeads ++ nonPassingLeads, tryLeads1)
							where
								(firstLeads, tryLeads0) = splitAt selectedLeadIndex allLeads
								(nonPassingLeads, tryLeads1) = skipToPassingLead context part tryLeads0
						_ -> skipToPassingLead context part allLeads

				(newLeads, oldLeads, part') <- if null tryLeads
					then return ([], [], part)
					else do
						let
							apply2 (newLeads1, part1) = return
								(newLeadsToLeads (optEnabledOptims $ guiToolOptions info) newLeads1, part2)
								where part2 = runPart_ part1 nwRemoveUnusedLinks

						Why errors (newLeads, part') <- runWhyT $ defaultConnectWhyT ([], part)
							(applyLead context part noOptimLog (head tryLeads)) apply2

						-- writeNetworkFile False "" "afterStep" [part']
						stop <- printCompleteness noPosContext errors

						when (not stop) $ showNetworkDiff infoRef part part'
						return (newLeads, tail tryLeads, part')

				-- Unselect leads before messing with the lead list
				Gtk.treeSelectionUnselectAll selection
				let parts' = replaceAt parts i part'
				-- Check
				printCompleteness noPosContext $ gatherCompleteness $ map checkPart parts'
				setParts infoRef parts'
				setPart infoRef $ Just part'
				setLeads infoRef $ newLeads ++ skippedLeads ++ oldLeads
				updateLeads infoRef
				redrawPart infoRef
				pushHistory infoRef

		leadSelection <- Gtk.treeViewGetSelection leadsList
		Gtk.onSelectionChanged leadSelection $ do
			info <- readIORef infoRef
			rows <- Gtk.treeSelectionGetSelectedRows leadSelection
			-- print rows
			let
				selectedLeads = map leadViewLead $ mapMaybe (guiGetLeadView info) $ concat rows
				comps = map leadCompNo selectedLeads
			-- print compNos
			maybePart <- guiPart infoRef
			let Just part = maybePart
			when (isJust maybePart) $ do
				let layoutRef = guiPartLayout info
				partLayoutSetHighlightComps layoutRef comps
				exposeLayout layoutRef
				let context = OptimContext { optimContextParts = guiParts info }

				mapM_ (\lead -> do
					let summary = optimPassesTest context part lead
					when (isJust summary) $ putStrLn $ fromJust summary) selectedLeads

		Gtk.on undoAction Gtk.actionActivated $ undo infoRef
		Gtk.on redoAction Gtk.actionActivated $ redo infoRef

		Gtk.on makeGatesAction Gtk.actionActivated $ makeGates mainWindow infoRef
		Gtk.on plotAction Gtk.actionActivated $ plot mainWindow infoRef

		Gtk.on showTimeWindowAction Gtk.actionActivated $ presentWindow timeWindow
		Gtk.on showNetworkDiffWindowAction Gtk.actionActivated $ presentWindow diffWindow
		Gtk.on showOComponentWindowAction Gtk.actionActivated $ presentWindow oWindow

		{- Go live -}

		Gtk.widgetShowAll mainWindow
		updatePartList infoRef

		-- Just to get the rest of the windows realised
		{- FIXME, check to see that it's OK to not do this.  Seems to be a history problem -}
		{-
		Gtk.widgetShowAll oWindow
		Gtk.widgetHide oWindow
		Gtk.widgetShowAll diffWindow
		Gtk.widgetHide diffWindow
		-}

		when (findBoolSubOption (optGui toolOpts0) GuiShowTimeWindow) $ presentWindow timeWindow
		when (findBoolSubOption (optGui toolOpts0) GuiShowNetworkDiffWindow) $ presentWindow diffWindow
		when (findBoolSubOption (optGui toolOpts0) GuiShowOWindow) $ presentWindow oWindow

		installHandler sigINT (Catch $ exitWith (ExitFailure 1)) Nothing

		Gtk.mainGUI

		finalParts <- liftM guiParts $ readIORef infoRef
		return finalParts
