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

module GuiSupport (
	Corner (..),
	colours,
	black,
	brown,
	red,
	orange,
	yellow,
	green,
	blue,
	paleBlue,
	violet,
	grey,
	white,
	pink,
	cyan,
	line,
	vLine,
	box,
	strokeFill,
	arrowhead,
	curveLine,
	curveBoxProp,
	curveBox,
	trapezium,
	invTrapezium,
	nickedTrapezium,
	centredText,
	blockTexts,
	vboxText,
	presentWindow,
	tableAddNameValueExtra,
	tableAddNameValue,
	TableOption (..),
	TableOptionType (..),
	TableOptionValue (..),
	tableOptionMakeTableEntry,
	Colour,
	makeAlignedScrolledWindow,
	makeDialogue,
	makeFrame,
	makeHPaned,
	makeVPaned,
	makeFileChooser,
	makeToolButton,
	fileChooserAddFilters,
	toolbarSetup,
	SimpleShapeRender,
	makeSourceErrorViewWindow,
	GuiOption (..),
	GuiDisplayMode (..),
	guiOptionUsage,
	defaultGuiOptions
	)
	where

	import Options
	import Show

	import Data.Maybe
	import Control.Monad
	import Data.List

	import Misc
	import Dot
	import Report

	-- import Graphics.UI.Gtk hiding ({- fill, lineWidth, -} layoutHeight, layoutWidth)
	import qualified Graphics.UI.Gtk as Gtk
	import Graphics.Rendering.Cairo
	import Graphics.UI.Gtk.Gdk.EventM (eventCoordinates)
	import Control.Monad.Trans

	type Colour = (Double, Double, Double)

	data Corner = TL | TR | BL | BR
		deriving (Show, Eq)

	black :: Colour
	black = (0.00, 0.00, 0.00)

	brown :: Colour
	brown = (0.65, 0.16, 0.16)

	red :: Colour
	red = (1.00, 0.00, 0.00)

	orange :: Colour
	orange = (1.00, 0.65, 0.00)

	yellow :: Colour
	yellow = (0.85, 0.65, 0.12)

	green :: Colour
	green = (0.00, 1.00, 0.00)

	blue :: Colour
	blue = (0.00, 0.00, 1.00)

	violet :: Colour
	violet = (0.85, 0.51, 0.93)

	grey :: Colour
	grey = (0.75, 0.75, 0.75)

	white :: Colour
	white = (1.00, 1.00, 1.00)

	offWhite :: Colour
	offWhite = (0.90, 0.90, 0.90)

	pink :: Colour
	pink = (1.00, 0.75, 0.75)

	paleBlue :: Colour
	paleBlue = (0.75, 0.75, 1.00)

	cyan :: Colour
	cyan = (0.00, 1.00, 1.00)

	colours :: [Colour]
	colours = [black, brown, red, orange, yellow, green, blue, violet, grey, offWhite, pink]

	strokeFill :: (Double, Double, Double) -> Render ()
	strokeFill (r,g,b) = do
		save
		setSourceRGB r g b
		fillPreserve
		restore
		stroke

	type SimpleShapeRender = DPoint -> Double -> Double -> Render ()

	box :: SimpleShapeRender
	box (x, y) width height = rectangle (x - width / 2) (y - height / 2) width height

	vLine :: DPoint -> Double -> Render ()
	vLine (x, y) height = line [(x, y - halfHeight), (x, y + halfHeight)]
		where halfHeight = height / 2

	{-
	-- titleCurve : box with curved top and inversely curve bottom
	titleCurve c (x, y) width height = do
		moveTo (left + c) top
		arc (right - c) (top + c) c (3 * halfPi) 0
		arcNegative (right - c) (bottom + c) c 0 (3 * halfPi)
		arcNegative (left + c) (bottom + c) c (3 * halfPi) (2 * halfPi)
		arc (left + c) (top + c) c (2 * halfPi) halfPi
		closePath
		where
			BoundingBox top bottom left right = pointWHToBoundingBox (x, y) width height
			halfPi = pi / 2
			-}

	-- curveBox : show a filled box with curved corners in the places specified (...Prop -- curved corners
	--	have size in proportion to the box's shortest dimension
	curveBoxProp :: [Corner] -> Double -> SimpleShapeRender
	curveBoxProp corners curveProportion (x, y) width height = curveBox corners c (x, y) width height
		where c = (min width height) * curveProportion

	curveBox :: [Corner] -> Double -> SimpleShapeRender
	curveBox corners c (x, y) width height = do
		moveTo (left + c) top
		if TR `elem` corners
			then arc (right - c) (top + c) c north east
			else lineTo right top
		if BR `elem` corners
			then arc (right - c) (bottom - c) c east south
			else lineTo right bottom
		if BL `elem` corners
			then arc (left + c) (bottom - c) c south west
			else lineTo left bottom
		if TL `elem` corners
			then arc (left + c) (top + c) c west north
			else lineTo left top
		closePath
		where
			BoundingBox top left bottom right = pointWHToBoundingBox (x, y) width height
			halfPi = pi / 2
			(east, south, west, north) = (0, halfPi, 2 * halfPi, 3 * halfPi)

	trapezium :: SimpleShapeRender
	trapezium (x, y) width height = do
		moveTo (x - width / 2) (y - height / 2)
		relLineTo width 0
		relLineTo (- skew) height
		lineTo (x - width / 2 + skew) (y + height / 2)
		closePath
		where skew
			| height < width = height / 2
			| otherwise = width / 4

	nickedTrapezium :: SimpleShapeRender
	nickedTrapezium (x, y) width height = do
		moveTo (x - width / 2) (y - height / 2)
		lineTo (x - skew / nickDivider) (y - height / 2)
		relLineTo (skew / nickDivider) (height / nickDivider)
		relLineTo (skew / nickDivider) (- (height / nickDivider))
		lineTo (x + width / 2) (y - height / 2)
		relLineTo (- skew) height
		lineTo (x - width / 2 + skew) (y + height / 2)
		closePath
		where
			skew
				| height < width = height / 2
				| otherwise = width / 4
			nickDivider = 4

	invTrapezium :: SimpleShapeRender
	invTrapezium (x, y) width height = do
		moveTo (x - width / 2) (y + height / 2)
		relLineTo width 0
		relLineTo (- skew) (- height)
		lineTo (x - width / 2 + skew) (y - height / 2)
		closePath
		where skew
			| height < width = height / 2
			| otherwise = width / 4

	line :: [DPoint] -> Render ()
	line [] = return ()
	line ((x, y):rest) = do
		moveTo x y
		mapM_ (uncurry lineTo) rest
		stroke

	arrowhead :: Double -> Double -> DPoint -> DPoint -> Render ()
	arrowhead size widthFraction (xPrev, yPrev) (x, y) = do
		save
		translate x y
		rotate (- angle)
		moveTo (- halfWidth) (- size)
		lineTo halfWidth (- size)
		lineTo 0 0
		closePath
		fillPreserve
		stroke
		restore
		where
			halfWidth = (size * widthFraction) / 2
			angle = atan2 (x - xPrev) (y - yPrev)

	curveLine :: [DPoint] -> Render ()
	curveLine ((x, y):rest) = do
		moveTo x y
		sequence $ mapN 3 curveToL rest
		stroke
		where
			curveToL [(x1, y1), (x2, y2), (x3, y3)] = curveTo x1 y1 x2 y2 x3 y3
			curveToL _ = return ()
	curveLine _ = return ()

	centredText :: DPoint -> String -> Render ()
	centredText (x, y) string = do
		ext <- textExtents string
		let
			x' = x - textExtentsWidth ext / 2
			y' = y + textExtentsHeight ext / 2
		moveTo x' y'
		showText string

	blockTexts :: DPoint -> [String] -> Render ()
	blockTexts (x, y) strings = do
		exts <- mapM textExtents strings
		let
			maxX = maximum (map textExtentsWidth exts)
			maxY = maximum (map textExtentsHeight exts)
			x' = x - maxX / 2
			firstY = y + (maxY * fromIntegral (length strings)) / 2
		foldM (\lineY string -> do
			moveTo x' lineY
			showText string
			return $ lineY - maxY) firstY strings
		return ()

	vboxText :: Double -> (Double -> Double) -> Double -> [String] -> Render ()
	vboxText y calcX vDelta strs = do
		mapM_ showTextStep $ zip [0..] strs
		where
			showTextStep (i, string) = do
				ext <- textExtents string
				moveTo (calcX (textExtentsWidth ext)) (y + i * vDelta + textExtentsHeight ext / 2)
				showText string

	toolbarGetItems :: Gtk.ToolbarClass toolbar => toolbar -> IO [Gtk.ToolItem]
	toolbarGetItems toolbar = do
		itemCount <- Gtk.toolbarGetNItems toolbar
		liftM catMaybes $ mapM (Gtk.toolbarGetNthItem toolbar) [0..itemCount-1]

	-- toolbarSetup : set a toolbar style and also do some cleaning up to make elements homogeneous
	--	and actually make icons and text appear correctly
	toolbarSetup :: Gtk.ToolbarClass toolbar => toolbar -> Gtk.Orientation -> Gtk.ToolbarStyle -> IO ()
	toolbarSetup toolbar orient style = do
		Gtk.toolbarSetOrientation toolbar orient
		Gtk.toolbarSetStyle toolbar style
		Gtk.toolbarSetIconSize toolbar Gtk.IconSizeSmallToolbar
		Gtk.toolbarSetShowArrow toolbar True
		items <- toolbarGetItems toolbar
		forM_ items $ \item -> do
			Gtk.set item [ Gtk.toolItemIsImportant Gtk.:= True ]
			Gtk.set toolbar [ Gtk.toolbarChildHomogeneous item Gtk.:= False ]

	makeDialogue :: Gtk.WidgetClass widget => Gtk.Window -> String -> [Gtk.Button] -> widget -> IO Gtk.Window
	makeDialogue parent title buttons child = do
		ret <- Gtk.windowNew
		Gtk.windowSetTransientFor ret parent
		Gtk.set ret [ Gtk.windowTitle Gtk.:= title, Gtk.windowAllowShrink Gtk.:= True ]
		buttonBox <- Gtk.hButtonBoxNew
		Gtk.boxSetSpacing buttonBox 6
		Gtk.set buttonBox [ Gtk.buttonBoxLayoutStyle Gtk.:= Gtk.ButtonboxEnd ]
		alignment <- Gtk.alignmentNew 0.5 0.5 1.0 1.0
		Gtk.set alignment [ Gtk.alignmentTopPadding Gtk.:= 5,
			Gtk.alignmentBottomPadding Gtk.:= 5,
			Gtk.alignmentLeftPadding Gtk.:= 5,
			Gtk.alignmentRightPadding Gtk.:= 5 ]
		vBox <- Gtk.vBoxNew False 2
		Gtk.boxPackStart vBox child Gtk.PackGrow 2
		mapM_ (Gtk.containerAdd buttonBox) buttons
		Gtk.boxPackStart vBox buttonBox Gtk.PackNatural 2
		Gtk.containerAdd alignment vBox
		Gtk.containerAdd ret alignment
		return ret

	makeFrame :: Gtk.WidgetClass widget => String -> widget -> IO Gtk.Frame
	makeFrame title child = do
		frame <- Gtk.frameNew
		label <- Gtk.labelNew (Just ("<b>" ++ title ++ "</b>"))
		Gtk.set label [ Gtk.labelUseMarkup Gtk.:= True ]
		Gtk.frameSetLabelWidget frame label
		alignment <- Gtk.alignmentNew 0.5 0.5 1.0 1.0
		Gtk.set alignment [ Gtk.alignmentTopPadding Gtk.:= 4,
			Gtk.alignmentBottomPadding Gtk.:= 4,
			Gtk.alignmentLeftPadding Gtk.:= 4,
			Gtk.alignmentRightPadding Gtk.:= 4 ]
		Gtk.containerAdd alignment child
		Gtk.containerAdd frame alignment
		return frame

	makeHPaned :: (Gtk.WidgetClass left, Gtk.WidgetClass right) => left -> right -> IO Gtk.HPaned
	makeHPaned left right = do
		paned <- Gtk.hPanedNew
		Gtk.panedAdd1 paned left
		Gtk.panedAdd2 paned right
		return paned

	makeVPaned :: (Gtk.WidgetClass left, Gtk.WidgetClass right) => left -> right -> IO Gtk.VPaned
	makeVPaned left right = do
		paned <- Gtk.vPanedNew
		Gtk.panedAdd1 paned left
		Gtk.panedAdd2 paned right
		return paned

	presentWindow :: (Gtk.WindowClass window) => window -> IO ()
	presentWindow window = do
		Gtk.widgetShowAll window
		Gtk.windowPresent window

	makeFileChooser :: Maybe Gtk.Window -> String ->
		Maybe FilePath -> Gtk.FileChooserAction ->
		String -> (FilePath -> IO Bool) -> IO Gtk.FileChooserDialog
	makeFileChooser parent title prevFile chooserAction yesLabel yesAction = do
		chooser <- Gtk.fileChooserDialogNew (Just title) parent chooserAction
			[(yesLabel, Gtk.ResponseAccept), ("gtk-cancel", Gtk.ResponseCancel)]

		when (isJust prevFile) $ do
			Gtk.fileChooserSetFilename chooser (fromJust prevFile)
			return ()

		Gtk.onResponse chooser $ \response -> case response of
			Gtk.ResponseAccept -> do
				maybeFilename <- Gtk.fileChooserGetFilename chooser
				let Just filename = maybeFilename
				close <- if isJust maybeFilename
					then yesAction filename
					else return False
				when close $ Gtk.widgetDestroy chooser
			_ -> Gtk.widgetDestroy chooser

		Gtk.widgetShowAll chooser
		return chooser

	makeToolButton :: Gtk.Toolbar -> Bool -> Int -> Maybe Gtk.StockId -> Maybe String -> IO Gtk.ToolButton
	makeToolButton toolbar important pos icon label = do
		button <- if isJust icon
			then do
				button <- Gtk.toolButtonNewFromStock (fromJust icon)
				Gtk.toolButtonSetLabel button label
				return button
			else Gtk.toolButtonNew (Nothing :: Maybe Gtk.Button) label
		Gtk.toolbarInsert toolbar button pos
		Gtk.set toolbar [ Gtk.toolbarChildHomogeneous button Gtk.:= False ]
		when important $ Gtk.set button [ Gtk.toolItemIsImportant Gtk.:= True ]
		return button

	fileChooserAddFilters :: Gtk.FileChooserClass chooser => chooser -> [(String, String)] -> IO ()
	fileChooserAddFilters chooser patternXdescs = do
		filters <- mapM (\(pattern, desc) -> do
			filter <- Gtk.fileFilterNew
			Gtk.fileFilterAddPattern filter pattern
			Gtk.fileFilterSetName filter desc
			Gtk.fileChooserAddFilter chooser filter
			return filter) patternXdescs

		when (not (null filters)) $ do
			Gtk.fileChooserSetFilter chooser $ last filters
			return ()

	makeRelativeFilename :: FilePath -> FilePath -> FilePath
	makeRelativeFilename relativeFile ('/':name) = concat (replicate (pathCount - sharedDirCount) "../") ++
		joinWith "/" (drop sharedDirCount split)
		where
			pathCount = length relativePath
			sharedDirCount = sharedLength relativePath split
			split = splitFilename name
			splitFilename = filter (/= "") . splitWith "/"
			relativePath = init $ splitFilename relativeFile

			sharedLength (a:as) (b:bs) | a == b = 1 + sharedLength as bs
			sharedLength _ _ = 0
	makeRelativeFilename _ name = name

	data TableOptionValue =
		  TableOptionValueString String
		| TableOptionValueEnum Int
		| TableOptionValueBool Bool
		| TableOptionValueColour Colour
		| TableOptionValueInt Int

	data TableOptionType =
		  TableOptionTypeString
		| TableOptionTypeEnum [String]
		| TableOptionTypeBool
		| TableOptionTypeColour
		| TableOptionTypeIntSpin Int Int -- low, high

	data TableOption =
		  TableOption {
			tableOptionName :: String,
			tableOptionType :: TableOptionType,
			tableOptionExtra :: Maybe Gtk.Widget,
			tableOptionGet :: IO TableOptionValue,
			tableOptionSet :: TableOptionValue -> IO () }
		| TableOptionSpacer {
			tableSpacerName :: String,
			tableSpacerWidth :: Int }

	colourToGtkColor :: Colour -> Gtk.Color
	colourToGtkColor (r,g,b) = Gtk.Color (scaleComp r) (scaleComp g) (scaleComp b)
		where scaleComp comp = floor $ comp * 65535

	gtkColorToColour :: Gtk.Color -> Colour
	gtkColorToColour (Gtk.Color r g b) = (scaleComp r, scaleComp g, scaleComp b)
		where scaleComp comp = (fromIntegral comp) / 65535.0

	tableOptionMakeTableEntry :: Gtk.Table -> Int -> Int -> TableOption -> IO ()
	tableOptionMakeTableEntry table col row (TableOptionSpacer name width) = do
		label <- Gtk.labelNew $ Just name
		Gtk.labelSetJustify label Gtk.JustifyLeft
		Gtk.miscSetAlignment label 0.0 0.5
		attach label col row []
		where
			attach widget col row xOpts = Gtk.tableAttach table widget col (col + width) row (row + 1)
				(Gtk.Fill:xOpts) [Gtk.Fill] 2 2
	tableOptionMakeTableEntry table col row opt@(TableOption {}) = do
		valueWidget <- case tableOptionType opt of
			TableOptionTypeString -> do
				entry <- Gtk.entryNew
				TableOptionValueString value <- tableOptionGet opt
				Gtk.entrySetText entry value
				Gtk.onEditableChanged entry $ do
					value <- Gtk.entryGetText entry
					tableOptionSet opt $ TableOptionValueString value
				return $ Gtk.toWidget entry
			TableOptionTypeEnum elems -> do
				combo <- Gtk.comboBoxNewText
				model <- Gtk.comboBoxGetModelText combo
				Gtk.listStoreClear model
				mapM_ (Gtk.listStoreAppend model) elems
				TableOptionValueEnum value <- tableOptionGet opt
				-- FIXME, make immediate/`apply' update an option rather than always doing it?
				Gtk.comboBoxSetActive combo value
				Gtk.on combo Gtk.changed $ do
					value <- Gtk.comboBoxGetActive combo
					tableOptionSet opt $ TableOptionValueEnum value
				return $ Gtk.toWidget combo
			TableOptionTypeBool -> do
				button <- Gtk.checkButtonNew
				TableOptionValueBool value <- tableOptionGet opt
				Gtk.toggleButtonSetActive button value
				Gtk.onToggled button $ do
					value <- Gtk.toggleButtonGetActive button
					tableOptionSet opt $ TableOptionValueBool value
				return $ Gtk.toWidget button
			TableOptionTypeColour -> do
				colourButton <- Gtk.colorButtonNew
				TableOptionValueColour value <- tableOptionGet opt
				Gtk.colorButtonSetColor colourButton $ colourToGtkColor value
				Gtk.onColorSet colourButton $ do
					colour <- Gtk.colorButtonGetColor colourButton
					tableOptionSet opt $ TableOptionValueColour $ gtkColorToColour colour
				return $ Gtk.toWidget colourButton
			TableOptionTypeIntSpin low high -> do
				spin <- Gtk.spinButtonNewWithRange (fromIntegral low) (fromIntegral high) 1
				TableOptionValueInt value <- tableOptionGet opt
				Gtk.spinButtonSetValue spin $ fromIntegral value
				Gtk.onValueSpinned spin $ do
					value <- Gtk.spinButtonGetValue spin
					tableOptionSet opt $ TableOptionValueInt $ floor value
				return $ Gtk.toWidget spin

		tableAddNameValueExtra table col row (tableOptionName opt) valueWidget $ tableOptionExtra opt
		return ()

	tableAddNameValueExtra :: (Gtk.WidgetClass valueWidget, Gtk.WidgetClass extraWidget) =>
		Gtk.Table -> Int -> Int -> String -> valueWidget -> Maybe extraWidget -> IO Gtk.Label
	tableAddNameValueExtra table col row labelString valueWidget extraWidget = do
		label <- Gtk.labelNew $ Just $ labelString ++ ":"
		Gtk.labelSetJustify label Gtk.JustifyRight
		Gtk.miscSetAlignment label 1.0 0.5
		attach label col row []
		attach valueWidget (col + 1) row [Gtk.Expand]
		when (isJust extraWidget) $ attach (fromJust extraWidget) (col + 2) row []
		return label
		where
			attach widget col row xOpts = Gtk.tableAttach table widget col (col + 1) row (row + 1)
				(Gtk.Fill:xOpts) [Gtk.Fill] 2 2

	tableAddNameValue :: Gtk.WidgetClass valueWidget =>
		Gtk.Table -> Int -> Int -> String -> valueWidget -> IO Gtk.Label
	tableAddNameValue table col row labelString valueWidget =
		tableAddNameValueExtra table col row labelString valueWidget (Nothing :: Maybe Gtk.Widget)

	-- makeSourceErrorViewWindow : make a window displaying the source files for the given error messages.
	--	`filename' is the full path of the top level file and is used to make relative paths for reported error
	--	filenames
	makeSourceErrorViewWindow :: PosContext posContext => Gtk.Window -> String -> Maybe posContext ->
		String -> [Report] -> IO Gtk.Window
	makeSourceErrorViewWindow parent title posContext filename errors = do
		let
			reportPos (Report pos _) = pos

			toFile (PosFile name _) = Just name
			toFile _ = Nothing

			findPosFiles pos = nub $ mapMaybe toFile $ posGetRoots posContext pos

			posFiles = mapMaybe toFile $ nub $ concatMap (posGetRoots posContext . reportPos) errors

			shortenFilename = makeRelativeFilename filename

		errorTree <- Gtk.treeViewNew
		errorSelection <- Gtk.treeViewGetSelection errorTree
		fileNotebook <- Gtk.notebookNew

		files <- filterM canReadFile posFiles

		let
			makeNotebookPage (i, file) = do
				buffer <- Gtk.textBufferNew Nothing
				iter <- Gtk.textBufferGetIterAtLine buffer 0
				contents <- readFile file
				Gtk.textBufferInsert buffer iter contents
				text <- Gtk.textViewNewWithBuffer buffer
				Gtk.textViewSetEditable text False
				let shortName = shortenFilename file

				errorTag <- Gtk.textTagNew (Just "Errors")
				Gtk.set errorTag [ Gtk.textTagBackground Gtk.:= "pink" ]
				tagTable <- Gtk.textBufferGetTagTable buffer
				Gtk.textTagTableAdd tagTable errorTag

				scrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
				Gtk.set scrolledWindow [
					Gtk.scrolledWindowHscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic,
					Gtk.scrolledWindowVscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic ]
				Gtk.containerAdd scrolledWindow text

				Gtk.notebookAppendPage fileNotebook scrolledWindow shortName
				Gtk.textViewSetCursorVisible text False

				let
					matchFile f1 (PosFile f2 _) = f1 == f2
					matchFile _ _ = False

					findLocalError (Report pos _) = isJust (find (matchFile file) (posGetRoots posContext pos))

					localErrors = filter findLocalError errors

					highlightError (Report pos _) = do
						let line = posGetLine pos
						start <- Gtk.textBufferGetIterAtLine buffer (line - 1)
						end <- Gtk.textBufferGetIterAtLine buffer line
						Gtk.textBufferApplyTag buffer errorTag start end

				mapM_ highlightError localErrors

				Gtk.on text Gtk.buttonPressEvent $ do
					(x, y) <- eventCoordinates
					(_, bY) <- lift $ Gtk.textViewWindowToBufferCoords text Gtk.TextWindowWidget (floor x, floor y)
					(line, _) <- lift $ Gtk.textViewGetLineAtY text bY
					lineNo <- liftM (+1) $ lift $ Gtk.textIterGetLine line
					let
						findError (Report pos _) = posGetLine pos == lineNo &&
							isJust (find (matchFile file) (posGetRoots posContext pos))

						maybeErrorNo = findIndex findError errors
						Just errorNo = maybeErrorNo
					lift $ when (isJust maybeErrorNo) $ do
						Gtk.treeSelectionSelectPath errorSelection [errorNo]
					return True -- important that this is True

				return (file, (i, buffer, text))

		fileTexts <- mapM makeNotebookPage $ zip [0..] files

		pageSwitchConnect <- Gtk.onSwitchPage fileNotebook $ \_ -> do
			Gtk.treeSelectionUnselectAll errorSelection

		model <- Gtk.listStoreNew errors
		renderer <- Gtk.cellRendererTextNew

		positionColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle positionColumn "Position"
		Gtk.treeViewColumnSetResizable positionColumn True
		Gtk.treeViewColumnPackStart positionColumn renderer True

		errorColumn <- Gtk.treeViewColumnNew
		Gtk.treeViewColumnSetTitle errorColumn "Error"
		Gtk.treeViewColumnSetResizable errorColumn True
		Gtk.treeViewColumnPackStart errorColumn renderer True

		let
			showPosition (Report pos _) = [ Gtk.cellText Gtk.:= showPosAdjustFilename shortenFilename posContext pos ]
			showError (Report _ msg) = [ Gtk.cellText Gtk.:= msg ]

		Gtk.cellLayoutSetAttributes positionColumn renderer model showPosition
		Gtk.cellLayoutSetAttributes errorColumn renderer model showError

		Gtk.treeViewAppendColumn errorTree positionColumn
		Gtk.treeViewAppendColumn errorTree errorColumn

		Gtk.treeViewSetModel errorTree model

		errorSelection <- Gtk.treeViewGetSelection errorTree
		Gtk.onSelectionChanged errorSelection $ do
			rows <- Gtk.treeSelectionGetSelectedRows errorSelection

			case rows of
				[[row]] -> do
					Report pos _ <- Gtk.listStoreGetValue model row
					let files = findPosFiles pos
					case files of
						[file] -> do
							let
								Just (page, buffer, view) = lookup file fileTexts
								line = posGetLine pos
								column = posGetColumn pos

								selectWholeLine = True

							(start, end) <- if selectWholeLine
								then do
									start <- Gtk.textBufferGetIterAtLine buffer (line - 1)
									end <- Gtk.textBufferGetIterAtLine buffer line
									return (start, end)
								else do
									start <- Gtk.textBufferGetIterAtLineOffset buffer (line - 1) (column - 1)
									end <- Gtk.textBufferGetIterAtLineOffset buffer (line - 1) column
									return (start, end)
							-- cursor <- Gtk.textBufferGetIterAtLineOffset buffer (line - 1) (column - 1)
							-- Gtk.textBufferPlaceCursor buffer cursor
							Gtk.textViewScrollToIter view start 0 Nothing
							Gtk.textBufferSelectRange buffer start end
							Gtk.signalBlock pageSwitchConnect
							Gtk.notebookSetCurrentPage fileNotebook page
							Gtk.signalUnblock pageSwitchConnect
						_ -> return ()
				[] -> do
					mapM (\(_, (_, buffer, _)) -> do
						start <- Gtk.textBufferGetStartIter buffer
						Gtk.textBufferSelectRange buffer start start
						) fileTexts
					return ()
				_ -> return ()

		paned <- makeVPaned fileNotebook errorTree
		Gtk.set paned [ Gtk.panedPosition Gtk.:= (height `div` 2)]
		close <- Gtk.buttonNewFromStock Gtk.stockClose
		window <- makeDialogue parent title [close] paned
		Gtk.widgetSetSizeRequest window width height
		Gtk.onClicked close $ Gtk.widgetHide window
		Gtk.on window Gtk.deleteEvent $ lift $ Gtk.widgetHide window >> return True
		Gtk.widgetShowAll window
		Gtk.treeSelectionSelectPath errorSelection [0]
		return window
		where
			width = 600
			height = 500

	makeAlignedScrolledWindow :: Gtk.WidgetClass widget => widget -> IO Gtk.ScrolledWindow
	makeAlignedScrolledWindow child = do
		scrolledWindow <- Gtk.scrolledWindowNew Nothing Nothing
		Gtk.set scrolledWindow [
			Gtk.scrolledWindowHscrollbarPolicy Gtk.:= Gtk.PolicyNever,
			Gtk.scrolledWindowVscrollbarPolicy Gtk.:= Gtk.PolicyAutomatic ]
		-- Gtk.containerAdd scrolledWindow table
		alignment <- Gtk.alignmentNew 1 1 1 1
		Gtk.containerAdd alignment child
		Gtk.scrolledWindowAddWithViewport scrolledWindow alignment
		return scrolledWindow

	data GuiDisplayMode = GuiDot | GuiSpring
		deriving (Eq, Enum)

	defaultGuiOptions :: [GuiOption]
	defaultGuiOptions = [GuiDisplayMode GuiDot, GuiDiffDepth 1, GuiOnlyPassingLeads, GuiShowLinkWidthColours,
		GuiShowMonitorEvents]

	instance Show GuiDisplayMode where
		showsPrec _ GuiSpring = showString "spring"
		showsPrec _ GuiDot = showString "dot"

	instance Read GuiDisplayMode where
		readsPrec _ str = maybeToList $ do
			(token, rest) <- maybeLex str
			case token of
				"spring" -> return (GuiSpring, rest)
				"dot" -> return (GuiDot, rest)
				_ -> fail ""

	data GuiOption = GuiDisplayMode GuiDisplayMode
		| GuiOnlyPassingLeads
		| GuiShowMonitorEvents
		| GuiShowLinkWidthColours
		| GuiDiffWhenStepping
		| GuiDiffDepth Int
		| GuiShowTimeWindow
		| GuiShowOWindow
		| GuiShowNetworkDiffWindow
		| GuiTimeStepSize Integer

	instance SubOption GuiOption where
		matchSubOption (GuiDisplayMode _) (GuiDisplayMode _) = True
		matchSubOption (GuiDiffDepth _) (GuiDiffDepth _) = True
		matchSubOption GuiOnlyPassingLeads GuiOnlyPassingLeads = True
		matchSubOption GuiShowMonitorEvents GuiShowMonitorEvents = True
		matchSubOption GuiShowLinkWidthColours GuiShowLinkWidthColours = True
		matchSubOption GuiDiffWhenStepping GuiDiffWhenStepping = True
		matchSubOption GuiShowTimeWindow GuiShowTimeWindow = True
		matchSubOption GuiShowOWindow GuiShowOWindow = True
		matchSubOption GuiShowNetworkDiffWindow GuiShowNetworkDiffWindow = True
		matchSubOption (GuiTimeStepSize {}) (GuiTimeStepSize {}) = True
		matchSubOption _ _ = False

	instance Show GuiOption where
		showsPrec _ (GuiDisplayMode mode) = shows mode
		showsPrec _ (GuiDiffDepth depth) = shows depth
		showsPrec _ (GuiTimeStepSize step) = shows step
		showsPrec _ _ = id

	guiOptionUsage :: SubOptionUsages GuiOption
	guiOptionUsage = SubOptionUsages "gui" show Nothing (Just defaultGuiOptions) [
		("display", SubOptionUsage False "mode" "network display mode {spring,dot}" "dot"
			(not . null . (reads :: String -> [(GuiDisplayMode, String)])) (\arg ->
				([GuiDisplayMode (read arg)], []))),
		("diff-depth", SubOptionUsage False "depth" "depth of netlist diff. component view" "1"
			(not . null . (reads :: String -> [(Int, String)])) (\arg ->
				([GuiDiffDepth (read arg)], []))),
		("only-passing-leads", boolSubOption "show only optimisation leads" GuiOnlyPassingLeads),
		("show-step-diff", boolSubOption "show network diff. when stepping" GuiDiffWhenStepping),
		("show-link-widths", boolSubOption "show link widths as colours" GuiShowLinkWidthColours),
		("show-monitor-events", boolSubOption "show monitor events on links" GuiShowMonitorEvents),
		("show-time-window", boolSubOption "show time window at startup" GuiShowTimeWindow),
		("show-o-window", boolSubOption "show O component window at startup" GuiShowOWindow),
		("show-diff-window", boolSubOption "show network diff. window at startup" GuiShowNetworkDiffWindow),
		("time-step", SubOptionUsage False "time" "size of steps in Time window" "1"
			(not . null . (reads :: String -> [(Integer, String)])) (\arg ->
				([GuiTimeStepSize (read arg)], [])))
		]
