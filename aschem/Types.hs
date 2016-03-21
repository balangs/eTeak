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

module Types (
	Direction (..),
	CompShape (..),
	CellType (..),
	Cell (..),
	DPos,
	Pos,
	Picture (..),
	Orient (..),
	Pattern (..)
	) where

	import Data.Array

	type Pos = (Int, Int)
	type DPos = (Double, Double)

	data Direction = Input | Output
		deriving Show

	data CompShape = CompAnd | CompBuf | CompComplete | CompTrans | CompGnd | CompVdd
		deriving (Show, Eq)

	data CellType = Port Direction String
		| Comp CompShape [Int] String
		| Label Pos String
		| Comment String
		deriving (Show)

	data Cell = Cell Pos Orient CellType
		deriving (Show)

	data Picture = Picture { pictureNW :: Pos, pictureSE :: Pos,
		pictureMap :: Array Pos Char }
		deriving (Show)

	data Pattern = Pattern {
		patternOrient :: Orient,
		patternOrigin :: Pos,
		patternPicture :: Picture,
		patternPorts :: [(Pos, Orient)],
		patternLabelPos :: Maybe (Pos, Pos)
		}
		deriving (Show)

	data Orient = OrientN | OrientNE | OrientE | OrientSE
		| OrientS | OrientSW | OrientW | OrientNW
		deriving (Show, Eq)
