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

module Render (
	renderSchematic,
	renderYScale,
	RenderOptions (..)
	) where

	import Data.Ix
	import Data.List
	import Graphics.Rendering.Cairo

	import Types
	import Picture
	import Patterns

	freqCount :: Eq a => [a] -> [(Int, a)]
	freqCount [] = []
	freqCount (e:es) = body 1 e es
		where
			body n e [] = [(n, e)]
			body n e (e2:es)
				| e == e2 = body (n + 1) e es
				| otherwise = (n, e) : body 1 e2 es

	posToDPos :: Double -> Pos -> DPos
	posToDPos scaleY (x, y) = (fromIntegral x, scaleY * (fromIntegral y))

	addDPos :: DPos -> DPos -> DPos
	addDPos (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

	subDPos :: DPos -> DPos -> DPos
	subDPos (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

	mulDPos :: DPos -> Double -> DPos
	mulDPos (x,y) m = (m * x, m * y)

	yScale :: Double
	yScale = 1.5

	textScale :: Double
	textScale = 1.68

	textVertOffset :: Double
	textVertOffset = 0.35

	ySc :: Double -> Double
	ySc y = yScale * y

	mySc :: Double -> Double
	mySc y = - (ySc y)

	renderYScale :: Double
	renderYScale = yScale

	schematicScale :: Pos -> DPos
	schematicScale = posToDPos yScale

	showGridPoints :: Pos -> Pos -> Render ()
	showGridPoints tl br = mapM_ (cross . schematicScale) (range (tl,br))

	{-
	showGrid :: Pos -> Pos -> Render ()
	showGrid (w,n) (e,s) = do
		mapM_ showVert [w..e-1]
		mapM_ showHoriz [n..s-1]
		where
			showVert x = strokePath [offsetX (schematicScale (x,s)), offsetX (schematicScale (x,n))]
			showHoriz y = strokePath [offsetY (schematicScale (w,y)), offsetY (schematicScale (e,y))]

			offsetX = addDPos (0.5,0)
			offsetY = addDPos (0,yScale / 2)
			-}

	-- showWire paths = showListWithSep nl (strokePath . map schematicScale) paths
	showWire :: ([DPos] -> Render a) -> [[Pos]] -> Render ()
	showWire stroke paths = mapM_ (stroke . map schematicScale) paths

	cross :: DPos -> Render ()
	cross pos = atRotate pos 0 (strokePath [(-0.25,0), (0.25,0)] >>
		strokePath [(0,-0.25), (0,0.25)])

	{-
	saltire :: DPos -> Render ()
	saltire pos = atRotate pos 0 (strokePath [(-0.25,-0.25), (0.25,0.25)] >>
		strokePath [(0.25,-0.25), (-0.25,0.25)])
		-}

	showLabel :: Cell -> Render ()
	showLabel (Cell _ _ (Label labelPos name)) = showTextCentre pos name
		where
			labelW = (schematicScale labelPos)
			pos = addDPos (fromIntegral ((length name) - 1) / 2.0, yScale / 2) labelW
	showLabel _ = error "showLabel: not a label"

	showComment :: Cell -> Render ()
	showComment (Cell pos _ (Comment str)) = showTextLeft (yScale / 2) (schematicScale pos) str
	showComment _ = error "showComment: not a comment"

	showDots :: [[[Pos]]] -> Render ()
	showDots wires = mapM_ (showDot . schematicScale) twiceUsedDPoss
		where
			allDPoss = sort $ concat (concat wires)
			twiceUsedDPoss = map snd $ filter ((>= 2) . fst) $ freqCount allDPoss

	showDot :: DPos -> Render ()
	showDot pos = do
		(uncurry arc pos) 0.3 0 (2 * pi) -- OK
		fill

	c :: Render ()
	c = do
		w <- getLineWidth
		setLineWidth 0.4
		let
			halfRoot2 = (sqrt 2) / 2
		moveTo halfRoot2 halfRoot2
		arc 0 0 1 (pi * 0.25) (pi * 1.75) -- OK
		stroke
		setLineWidth w

	showComp :: Cell -> Render ()
	showComp (Cell pos OrientE (Comp CompAnd inverts "C")) = showAnd (schematicScale pos) 0 >>
		atRotate (addDPos (2.5,0) (schematicScale pos)) 0 c >>
		showInverts bubble pos inverts andOrientE noOffsets
	showComp (Cell pos OrientW (Comp CompAnd inverts "C")) = showAnd (schematicScale pos) pi >>
		atRotate (addDPos (-2.25,0) (schematicScale pos)) 0 c >>
		showInverts bubble pos inverts andOrientW noOffsets
	showComp (Cell pos OrientE (Comp _ inverts "drcm")) = showCompletion (schematicScale pos) 0 >>
		showInverts bubble pos inverts andOrientE noOffsets
	showComp (Cell pos OrientW (Comp _ inverts "drcm")) = showCompletion (schematicScale (moveE 5 pos)) pi >>
		showInverts bubble pos inverts andOrientW noOffsets
	showComp (Cell pos OrientE (Comp CompBuf inverts _)) = showBuf (schematicScale pos) 0 >>
		showInverts bubble pos inverts bufOrientE noOffsets
	showComp (Cell pos OrientW (Comp CompBuf inverts _)) = showBuf (schematicScale (moveE 2 pos)) pi >>
		showInverts bubble pos inverts bufOrientW noOffsets

	showComp (Cell pos OrientE (Comp CompAnd inverts "or")) = showOr (schematicScale pos) 0 >>
		showInverts bubble pos inverts andOrientE orEOffsets
	showComp (Cell pos OrientW (Comp CompAnd inverts "or")) = showOr (schematicScale pos) pi >>
		showInverts bubble pos inverts andOrientW orWOffsets

	showComp (Cell pos OrientE (Comp CompAnd inverts "xor")) = showXor (schematicScale pos) 0 >>
		showInverts bubble pos inverts andOrientE orEOffsets
	showComp (Cell pos OrientW (Comp CompAnd inverts "xor")) = showXor (schematicScale pos) pi >>
		showInverts bubble pos inverts andOrientW orWOffsets

	showComp (Cell pos OrientE (Comp CompAnd inverts _)) = showAnd (schematicScale pos) 0 >>
		showInverts bubble pos inverts andOrientE noOffsets
	showComp (Cell pos OrientW (Comp CompAnd inverts _)) = showAnd (schematicScale pos) pi >>
		showInverts bubble pos inverts andOrientW noOffsets

	showComp (Cell pos OrientW (Comp CompTrans inverts _)) = showTrans (schematicScale pos) 0 >>
		showInverts smallBubble pos inverts transOrientW transWOffsets

	showComp (Cell pos _ (Comp CompGnd _ _)) = showGnd (schematicScale pos) 0
	showComp (Cell pos _ (Comp CompVdd _ _)) = showVdd (schematicScale pos) 0

	showComp _ = return ()

	orBackCurve :: (Double, Double, Double)
	orBackCurve = (r, theta, pinOffset)
		where
			dX = 0.5
			dY = ySc 1.5
			pinDY = ySc 1
			r = (dX ** 2 + dY ** 2) / (2 * dX)
			theta = {- (180 / pi) * -} atan (dY / (r - dX))
			pinOffset = r - sqrt (r ** 2 - pinDY ** 2)

	noOffsets :: [DPos]
	noOffsets = repeat (0,0)

	orEOffsets :: [DPos]
	orEOffsets = [(-orPinOffset,0), (0,0), (-orPinOffset,0)] ++ noOffsets
		where (_, _, orPinOffset) = orBackCurve

	orWOffsets :: [DPos]
	orWOffsets = [(orPinOffset,0), (0,0), (orPinOffset,0)] ++ noOffsets
		where (_, _, orPinOffset) = orBackCurve

	transWOffsets :: [DPos]
	transWOffsets = [(0.2,0)] ++ noOffsets

	showInverts :: (DPos -> Render a) -> Pos -> [Int] -> Types.Pattern -> [DPos] -> Render ()
	showInverts bubble origin inverts pattern glyphOffsets = mapM_ showInvert invertPoss
		where
			invertPoss = map (\i -> (patternPorts pattern !! i, glyphOffsets !! i)) inverts
			showInvert ((pos, orient), glyphOffset) = bubble pos'
				where
					pos' = addDPos (addDPos bubbleOffset glyphOffset) (schematicScale (addPos origin pos))
					bubbleOffset = case orient of
						OrientN -> (0,-vertOffset)
						OrientS -> (0,vertOffset)
						OrientW -> (-0.5,0)
						OrientE -> (0.5,0)
						_ -> error "showInverts: orientation must be N,S,E,W"
					vertOffset = (yScale / 2) + 0.5

	bubble :: DPos -> Render ()
	bubble pos = inLineWidth 0.25 $ (uncurry arc pos) 0.5 0 (2 * pi) >> strokeFill 1 -- OK

	smallBubble :: DPos -> Render ()
	smallBubble pos = inLineWidth 0.1 $ (uncurry arc pos) 0.3 0 (2 * pi) >> strokeFill 1 -- OK

	showTextLeft :: Double -> DPos -> String -> Render ()
	showTextLeft offset pos str = do
		uncurry moveTo pos
		relMoveTo offset textVertOffset
		showText str

	showTextCentre :: DPos -> String -> Render ()
	showTextCentre (x, y) str = do
		ext <- textExtents str
		let
			x' = x - textExtentsWidth ext / 2
			-- y' = y + textExtentsHeight ext / 2
		moveTo x' y
		showText str

	showTextRight :: Double -> DPos -> String -> Render ()
	showTextRight offset (x, y) str = do
		ext <- textExtents str
		let
			x' = offset + x - textExtentsWidth ext
			y' = y + textVertOffset
		{-
		moveTo x' y'
		lineTo x' (y' - textVertOffset)
		stroke
		moveTo (offset + x) y
		relLineTo 0 (- textVertOffset)
		stroke
		-}
		moveTo x' y'
		showText str

	showAnd :: DPos -> Double -> Render ()
	showAnd pos angle = do
		atRotate pos angle $ do
			uncurry moveTo (0.0,mySc 1.5)
			arc (5 - ySc 1.5) 0 (ySc 1.5) (pi * (-0.5)) (pi * 0.5)
			uncurry lineTo (0.0,ySc 1.5)
		strokeFill 1

	inLineWidth :: Double -> Render a -> Render ()
	inLineWidth width body = do
		oldWidth <- getLineWidth
		setLineWidth width
		body
		setLineWidth oldWidth

	showTrans :: DPos -> Double -> Render ()
	showTrans pos angle = atRotate pos angle $ do
		whiteBox (0.5,mySc 0.5) (1.5,ySc 0.5)
		setGrey 1
		fill
		setGrey 0
		inLineWidth 0.15 (strokePath [(1,mySc 0.7), (1,mySc 0.5), (0.5,mySc 0.5),
			(0.5,ySc 0.5), (1,ySc 0.5), (1,ySc 0.7)])
		inLineWidth 0.25 (strokePath [(0.5,mySc 0.6), (0.5,ySc 0.6)] >>
			strokePath [(0,mySc 0.6), (0,ySc 0.6)])

	showGnd :: DPos -> Double -> Render ()
	showGnd pos angle = atRotate pos angle $ do
		whiteBox (-1,ySc 0.5) (1,ySc 1.5)
		inLineWidth 0.25 (strokePath [(-1,ySc 0.5), (1,ySc 0.5)])
		inLineWidth 0.20 (strokePath [(-0.7,ySc 0.8), (0.7,ySc 0.8)])
		inLineWidth 0.20 (strokePath [(-0.4,ySc 1.1), (0.4,ySc 1.1)])

	showVdd :: DPos -> Double -> Render ()
	showVdd pos angle =
		atRotate pos angle $
		inLineWidth 0.25 (strokePath [(-0.5,mySc 0.5), (0,mySc 1), (0.5,mySc 0.5)])

	arctWide :: DPos -> DPos -> Render ()
	arctWide (fromX,fromY) (toX,toY)
		| fromY > toY = error "arctWide: fromY > toY"
		| otherwise = (uncurry arc centre) r startAngle endAngle
		where
			dX = abs (fromX - toX)
			dY = abs (fromY - toY)

			theta = atan (dY / dX)
			chord = sqrt (dX ** 2 + dY ** 2)
			r = chord / (2 * sin theta)

			(startAngle, endAngle, centre) = if fromX < toX
				then (pi * 1.5, (pi + 1.5) + (2 * theta), (fromX, fromY + r))
				else (pi * 0.5 - (2 * theta), pi * 0.5, (toX, toY - r))

	drawOrPath :: Double -> Render ()
	drawOrPath leftOffset = do
		uncurry moveTo (leftOffset, mySc 1.5)
		arctWide (1,mySc 1.5) (5,0) -- OK
		arctWide (5,0) (1,ySc 1.5) -- OK
		arcNegative (-r + leftOffset) 0 r theta ((2 * pi) - theta) -- OK
		where (r, theta, _) = orBackCurve

	showOr :: DPos -> Double -> Render ()
	showOr pos angle = atRotate pos angle $ drawOrPath 0 >> strokeFill 1

	showXor :: DPos -> Double -> Render ()
	showXor pos angle = atRotate pos angle $ do
		drawOrPath 0
		setGrey 1
		fill
		setGrey 0
		drawOrPath 0.5
		closePathStroke
		arcNegative (-r) 0 r theta ((2 * pi) - theta) -- OK
		stroke
		where (r, theta, _) = orBackCurve

	showBuf :: DPos -> Double -> Render ()
	showBuf pos angle = atRotate pos angle $ do
		drawPath [(0,mySc 0.707), (2,0), (0,ySc 0.707)]
		strokeFill 1

	pointyBox :: DPos -> Double -> Double -> Render ()
	pointyBox origin x y = path [(0,-halfY), (x-halfY,-halfY), (x,0),
		(x-halfY,halfY), (0,halfY)]
		where
			halfY = (y * yScale) / 2
			path = drawPath . map (\point -> subDPos point origin)

	showCompletion :: DPos -> Double -> Render ()
	showCompletion pos angle = atRotate pos angle $ do
		pointyBox (0,0) 5 3
		strokeFill 1

	showPortSymbol :: DPos -> Double -> Render ()
	showPortSymbol pos angle = atRotate pos angle $ do
		pointyBox (0.5,0) 1 0.5
		strokeFill 0.5

	showPort :: Cell -> Render ()
	showPort (Cell pos OrientE (Port Input name)) = showPortSymbol (schematicScale pos) 0 >>
		showTextRight (-1) (schematicScale pos) name
	showPort (Cell pos OrientE (Port Output name)) = showPortSymbol (schematicScale pos) pi >>
		showTextRight (-1) (schematicScale pos) name
	showPort (Cell pos OrientW (Port Input name)) = showPortSymbol (schematicScale pos) pi >>
		showTextLeft 1 (schematicScale pos) name
	showPort (Cell pos OrientW (Port Output name)) = showPortSymbol (schematicScale pos) 0 >>
		showTextLeft 1 (schematicScale pos) name
	showPort _ = return ()

	drawPath :: [DPos] -> Render ()
	drawPath [] = return ()
	drawPath (pos1:poss) = do
		uncurry moveTo pos1
		mapM_ (uncurry lineTo) poss

	lineAngleAndLength :: DPos -> DPos -> (Double, Double)
	lineAngleAndLength p0 p1 = (atan2 dy dx, l)
		where
			(dx, dy) = subDPos p1 p0
			l = sqrt (dx ** 2 + dy ** 2)

	-- normaliseAngle : normalise an angle to the range 0..2 * pi
	normaliseAngle :: Double -> Double
	normaliseAngle angle
		| angle < 0 = normaliseAngle (2 * pi + angle)
		| angle > (2 * pi) = normaliseAngle (angle - 2 * pi)
		| otherwise = angle

	arct :: DPos -> DPos -> Double -> Render ()
	arct p1 p2 r = do
		p0 <- getCurrentPoint
		let
			p0' = subDPos p0 p1
			p2' = subDPos p2 p1

			(thetaA, lA) = lineAngleAndLength p0 p1
			(thetaB, lB) = lineAngleAndLength p1 p2

			e = mulDPos p0' (1 / lA)
			f = mulDPos p2' (1 / lB)
			(thetaEF, lEF) = lineAngleAndLength e f

			phi = acos (lEF / 2)

			thetaCL = thetaEF + (pi / 2)

			anticlockwise = normaliseAngle (thetaB - thetaA) > pi

			l = r / cos phi

			(xc, yc) = (if anticlockwise then subDPos else addDPos)
				p1 (mulDPos (cos thetaCL, sin thetaCL) l)

		{-
		save
		stroke
		saltire (addDPos e p1)
		cross (addDPos f p1)
		cross (xc, yc)
		stroke
		restore
		save
		moveTo xc yc
		relMoveTo 0.4 0.4
		setFontSize 0.5
		let
			showAngle angle = show $ floor $ (180 / pi) * angle

		showText $ showAngle thetaA ++ " " ++ showAngle (normaliseAngle thetaA)
		moveTo xc yc
		relMoveTo 0.4 0.9
		showText $ showAngle thetaB ++ " " ++ showAngle (normaliseAngle thetaB)
		moveTo xc yc
		relMoveTo 0.4 1.4
		showText (show anticlockwise)
		stroke
		-- setSourceRGB 0 0 0
		restore
		inLineWidth 0.01 $ do
			uncurry moveTo p1
			relLineTo (cos thetaCL) (sin thetaCL)
			stroke
		-}
		uncurry moveTo p0
		if anticlockwise
			then arcNegative xc yc r (thetaCL + phi) (thetaCL - phi)
			else arc xc yc r ((pi + thetaCL) - phi) ((pi + thetaCL) + phi)

	drawArcPath :: [DPos] -> Render ()
	drawArcPath [] = return ()
	drawArcPath (pos1:poss) = uncurry moveTo pos1 >> rest poss
		where
			rest [] = return ()
			rest [pos] = uncurry lineTo pos
			rest (pos1:pos2:poss) = arct pos1 pos2 0.5 >> rest (pos2:poss) -- CHECK

	strokePath :: [DPos] -> Render ()
	strokePath path = drawPath path >> stroke

	strokeArcPath :: [DPos] -> Render ()
	strokeArcPath path = drawArcPath path >> stroke

	closePathStroke :: Render ()
	closePathStroke = closePath >> stroke

	-- polygon path = drawPath path >> closePathStroke

	box :: DPos -> DPos -> Render ()
	box tl br = boxPath tl br >> closePathStroke

	boxPath :: DPos -> DPos -> Render ()
	boxPath (w,n) (e,s) = drawPath [(w,n), (e,n), (e,s), (w,s)]

	whiteBox :: DPos -> DPos -> Render ()
	whiteBox tl br = do
		boxPath tl br
		setGrey 1
		fill
		setGrey 0

	strokeFill :: Double -> Render ()
	strokeFill grey = do
		closePath
		save
		setGrey grey
		fillPreserve
		restore
		stroke

	atRotate :: DPos -> Double -> Render a -> Render ()
	atRotate pos angle draw = do
		save
		uncurry translate pos
		rotate angle
		moveTo 0 0
		draw
		restore

	setGrey :: Double -> Render ()
	setGrey grey = setSourceRGB grey grey grey

	data RenderOptions = RenderArcWires | RenderShowGrid | RenderShowSource
		deriving (Show, Eq)

	renderSchematic :: [RenderOptions] -> (Pos, Pos) -> [Cell] -> [Cell] ->
		[[[Pos]]] -> [Cell] -> [Cell] -> Picture -> Render ()
	renderSchematic options (tl, br) comps ports wires wireLabels comments picture = do
		-- scale mmScale mmScale
		-- scale 2 2
		translate (-1) 0
		selectFontFace font FontSlantNormal FontWeightNormal
		setFontSize textScale
		setLineWidth 0.05
		setGrey 0.8
		onOption RenderShowGrid (showGridPoints (moveE 1 tl) (moveW 1 br))
		setGrey 0
		setLineWidth 0.05
		box (schematicScale (moveN 1 tl)) (schematicScale br')
		setLineWidth 0.15
		mapM_ showWire' wires
		mapM_ showLabel wireLabels
		newPath
		showDots wires
		setLineWidth 0.2
		mapM_ showPort ports
		setLineWidth 0.3
		mapM_ showComp comps
		mapM_ showComment comments
		onOption RenderShowSource showGreyLetters
		showPage
		where
			br' = moveS 1 br
			showWire' = showWire $ if RenderArcWires `elem` options
				then strokeArcPath
				else strokePath

			onOption option action
				| option `elem` options = action
				| otherwise = return ()

			font = "Courier"
			-- font = "Helvetica-Bold"

			showGreyLetters = do
				setGrey 0.6
				mapM_ showChar (pictureAllPoss picture)

			showChar pos = showTextCentre (subDPos (schematicScale pos) (0, textVertOffset)) str
				where
					chr = pictureAt picture pos
					str
						| chr `elem` "%()\\" = ['\\', chr]
						| otherwise = [chr]
