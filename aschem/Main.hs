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

module Main (
	main ) where

	import Maybe
	import List
	import System
	import Char (isAlpha, isAlphaNum)
	import Monad
	import Graphics.Rendering.Cairo

	import Picture
	import Types
	import Patterns
	import Render

	isSpace :: Char -> Bool
	isSpace = (== ' ')

	isFirstLabelChar :: Char -> Bool
	isFirstLabelChar char = isAlpha char

	isLabelChar :: Char -> Bool
	isLabelChar char = isAlphaNum char || char `elem` "[]+-"

	isHorizWire :: Char -> Bool
	isHorizWire = (== '-')

	findLabel :: Bool -> Picture -> Pos -> String
	findLabel labelToE picture pos
		| first == pastSecond = ""
		| otherwise = if labelToE
			then pictureGetString picture first (moveW 1 pastSecond)
			else pictureGetString picture (moveE 1 pastSecond) first
		where
			move = (if labelToE then moveE else moveW) 1
			first = pictureMoveWhile isSpace move picture pos
			pastSecond = pictureMoveWhile isLabelChar move picture first

	findPorts :: Picture -> [Cell]
	findPorts picture = mapPicture picture ((`elem` "?!") . pictureAt picture) handleIn
		where
			handleIn pos
				| wireToE = Cell pos OrientE (Port dir (findLabel False picture (moveW 1 pos)))
				| wireToW = Cell pos OrientW (Port dir (findLabel True picture (moveE 1 pos)))
				| otherwise = error $ "port at " ++ show pos ++ " must be connected to wire '-'"
				where
					wireToE = isHorizWire $ at (moveE 1 pos)
					wireToW = isHorizWire $ at (moveW 1 pos)
					dir = if at pos == '?' then Input else Output

					at = pictureAt picture

	findComps :: Picture -> [Cell]
	findComps picture = concatMap findComp patterns
		where
			findComp (shape, pattern) = mapPicture picture (pictureMatchPattern pattern picture)
				(handleComp shape pattern)

			handleComp shape pattern pos = Cell origin (patternOrient pattern) (Comp shape inverts label)
				where
					origin = addPos pos (subPos (patternOrigin pattern) (pictureNW (patternPicture pattern)))
					labelPos = patternLabelPos pattern
					label = if isJust labelPos
						then filter (/= ' ') $ pictureGetStringRel picture origin $ fromJust labelPos
						else ""
					inverts = findPortInverts picture pattern origin

	sortPath :: [Pos] -> [Pos]
	sortPath [] = []
	sortPath points | head points < last points = points
	sortPath points = reverse points

	uniq :: Eq a => [a] -> [a]
	uniq [] = []
	uniq (e1:e2:es)
		| e1 == e2 = uniq (e2:es)
	uniq (e1:es) = e1 : uniq es

	-- followWire : follow a wire in the given direction, starting from `pos', in the direction of `orient'
	--	but with an already accumulate path `path' (useful for chasing from `invalid' chars. such as ports).
	followWire :: Picture -> [Pos] -> Pos -> Orient -> [[Pos]]
	followWire picture path pos orient = sort $ map (sortPath . uniq) $ followBody path pos orient
		where
			followBody path pos orient = case (at pos, orient) of
				('o', _) -> follow
				('-', OrientE) -> follow
				('-', OrientW) -> follow
				('|', OrientN) -> follow
				('|', OrientNE) | s == ' ' -> turn OrientN
				('|', OrientNW) | s == ' ' -> turn OrientN
				('|', OrientSE) | n == ' ' -> turn OrientS
				('|', OrientSW) | n == ' ' -> turn OrientS
				('|', OrientS) -> follow
				('`', OrientW) -> turn OrientN
				('`', OrientNW) -> turn OrientN
				('`', OrientS) -> turn OrientE
				('`', OrientSE) -> turn OrientE
				('\'', OrientE) -> turn OrientN
				('\'', OrientNE) -> turn OrientN
				('\'', OrientS) -> turn OrientW
				('\'', OrientSW) -> turn OrientW
				('.', OrientE) -> turn OrientS
				('.', OrientSE) -> turn OrientS
				('.', OrientN) -> turn OrientW
				('.', OrientNW) -> turn OrientW
				(',', OrientW) -> turn OrientS
				(',', OrientSW) -> turn OrientS
				(',', OrientN) -> turn OrientE
				(',', OrientNE) -> turn OrientE
				('+', _) -> follow
				('<', _) -> turns [OrientN, OrientS, OrientW]
				('>', _) -> turns [OrientN, OrientS, OrientE]
				('^', _) -> turns [OrientN, OrientW, OrientE]
				('v', _) -> turns [OrientS, OrientW, OrientE]
				('*', _) -> turns [OrientN, OrientS, OrientW, OrientE]

				-- Meet component port
				(' ', OrientS) | nw == '_' || ne == '_' -> [path']

				-- Start diagonals
				(' ', OrientN) | backslashOrX w -> turnDiag OrientNW
				(' ', OrientN) | slashOrX e -> turnDiag OrientNE
				(' ', OrientS) | slashOrX w -> turnDiag OrientSW
				(' ', OrientS) | backslashOrX e -> turnDiag OrientSE
				(' ', OrientW) | backslashOrX n -> turnDiag OrientNW
				(' ', OrientW) | slashOrX s -> turnDiag OrientSW
				(' ', OrientE) | slashOrX n -> turnDiag OrientNE
				(' ', OrientE) | backslashOrX s -> turnDiag OrientSE

				-- Continue diagonals
				('\\', _) -> follow
				('/', _) -> follow
				('X', _) -> follow

				-- Terminating conditions
				-- Meet component port
				('|', OrientW) -> [path']
				('|', OrientE) -> [path']
				('[', OrientS) -> [path']
				('[', OrientN) -> [path']
				('_', OrientN) -> [path']
				-- Meet port
				('?', _) -> [path']
				('!', _) -> [path']
				_ -> error $ "Badly terminated signal at: " ++ showErrorInfo
				where
					showErrorInfo = show pos ++ " " ++ show orient ++ " `" ++ [at pos] ++ "' (" ++ show path ++ ")"
					backslashOrX c = c == '\\' || c == 'X'
					slashOrX c = c == '/' || c == 'X'
					nw = at $ moveNW 1 pos
					ne = at $ moveNE 1 pos
					-- sw = at $ moveSW 1 pos
					-- se = at $ moveSE 1 pos
					n = at $ moveN 1 pos
					s = at $ moveS 1 pos
					w = at $ moveW 1 pos
					e = at $ moveE 1 pos
					follow = followBody path ((orientToMove orient) 1 pos) orient
					turns orients
						| back `elem` orients = path' : concatMap fork (orients \\ [back])
						| otherwise = error $ "Must approach from one of: " ++ show (map aboutFace orients) ++
							" at: " ++ showErrorInfo
						where back = aboutFace orient
					turn orient = followBody path' ((orientToMove orient) 1 pos) orient
					fork orient = followBody [pos] ((orientToMove orient) 1 pos) orient
					turnDiag newOrient = followBody (path ++ [back]) (orientToMove newOrient 1 back) newOrient
						where back = orientToMove (aboutFace orient) 1 pos

					path' = path ++ [pos]

			at = pictureAt picture

	followWireFromPort :: Picture -> Cell -> [[Pos]]
	followWireFromPort picture (Cell pos orient (Port _ _)) = followWire picture [pos] firstWirePos orient
		where
			firstWirePos = orientToMove orient 1 pos
	followWireFromPort _ _ = error "followWireFromPort: not a port"

	findPortInverts :: Picture -> Types.Pattern -> Pos -> [Int]
	findPortInverts picture pattern compOrigin = findIndices portInvert (patternPorts pattern)
		where
			portInvert (pos, orient) = char == 'o'
				where char = pictureAt picture $ orientToMove orient 1 $ addPos compOrigin pos

	followWireFromCompPorts :: Picture -> Cell -> [[[Pos]]]
	followWireFromCompPorts picture (Cell pos orient (Comp shape _ _name)) = filter (not . null) $ map follow ports
		where
			matchPattern (shape, orient) (shape2, pattern) = shape == shape2 && orient == patternOrient pattern

			ports = patternPorts $ snd $ fromJust $ find (matchPattern (shape, orient)) patterns

			follow (portRelPos, portOrient)
				| not blocked = followWire picture [portPos] (orientToMove portOrient 1 portPos) portOrient 
				| otherwise = []
				where
					portPos = addPos pos portRelPos
					wirePos = orientToMove portOrient 1 portPos
					wireChar = pictureAt picture wirePos

					blocked = case (portOrient, wireChar) of
						(OrientS, '_') -> True
						(OrientN, '_') -> True
						(OrientN, '-') -> True
						(OrientS, '-') -> True
						(OrientW, '|') -> True
						(OrientE, '|') -> True
						(_ , ' ') -> True
						_ -> False
	followWireFromCompPorts _ _ = error "followWireFromCompPorts: not a component port"

	pathToSegments :: [Pos] -> [(Pos, Pos)]
	pathToSegments (p1:p2:ps) = (p1,p2) : pathToSegments (p2:ps)
	pathToSegments _ = []

	findWireNames :: Picture -> [[Pos]] -> [Cell]
	findWireNames picture wire = mapMaybe segmentName $ filter horizSegment segments
		where
			segments = concatMap pathToSegments wire
			horizSegment ((_,y1), (_, y2)) = y1 == y2

			segmentName (pos1,pos2) = case name of
				"" -> Nothing
				"o" -> Nothing -- FIXME, special
				_ -> Just (Cell (averagePos pos1 pos2) OrientE (Label labelPos name))
				where
					w = moveN 1 (min pos1 pos2)
					e = moveN 1 (max pos1 pos2)
					w' = pictureMoveWhile isLabelChar (moveW 1) picture w
					e' = pictureMoveWhile isLabelChar (moveE 1) picture e
					rawStr = pictureGetString picture w' e'
					str = dropWhile (not . isFirstLabelChar) rawStr
					name = takeWhile isLabelChar str
					labelPos = moveE (length rawStr - length str) w'

	findComments :: Picture -> [Cell]
	findComments picture = catMaybes $ mapPicture picture startComment getComment
		where
			startComment pos = at pos == '"' && at (moveE 1 pos) /= ' '

			getComment pos
				| w /= e = Just $ Cell pos OrientE $ Comment $ pictureGetString picture w (moveW 1 e)
				| otherwise = Nothing
				where
					w = moveE 1 pos
					e = pictureMoveWhile (/= '"') (moveE 1) picture w

			at = pictureAt picture

	main :: IO ()
	main = do
		args <- getArgs
		let
			parseFiles [inFile] = return (inFile, inFile ++ ".ps")
			parseFiles [inFile, outFile] = return (inFile, outFile)
			parseFiles _ = error "usage: aschem [ -v ] <aschem-file> [<aschem-out-file>]"

		(verbose, (inFile, outFile)) <- case args of
			("-v":rest) -> do
				files <- parseFiles rest
				return (True, files)
			args -> do
				files <- parseFiles args
				return (False, files)
		contents <- readFile inFile
		let
			contentsLines = lines contents
			picture = makePicture contentsLines

		when verbose $ do
			putStrLn $ "% " ++ show (pictureWidth picture) ++ " x " ++ show (pictureHeight picture)
			mapM_ putStrLn contentsLines

		let
			ports = findPorts picture
			portName (Cell _ _ (Port _ name)) = name
			portName _ = error "portName: can't find port name"
			portNames = map portName ports
			comps = findComps picture
			portWires = map (followWireFromPort picture) ports
			compWires = nub (concatMap (followWireFromCompPorts picture) comps) \\ portWires
			wires = nub (portWires ++ compWires)

			compWireNames = map (findWireNames picture) compWires
			comments = findComments picture

		when verbose $ do
			mapM_ print $ ports
			mapM_ print $ comps
			mapM_ print $ zip portNames portWires
			mapM_ print $ compWires
			mapM_ print $ filter (not . null) compWireNames
			mapM_ print $ comments

		let
			(left, top) = pictureNW picture
			(right, bottom) = pictureSE picture

			mmScale = 72 / 25.4
			plotScale = 2 * mmScale

			width = plotScale * fromIntegral (0 + right - left)
			height = renderYScale * plotScale * fromIntegral (2 + bottom - top)

			border = mmScale * 2

		withPSSurface outFile (width + border * 2) (height + border * 2) $ \surface -> renderWith surface $ do
			translate border border
			scale plotScale plotScale
			renderSchematic [RenderArcWires]
				(pictureNW picture, pictureSE picture) comps ports wires
				(concat compWireNames) comments picture

		return ()
