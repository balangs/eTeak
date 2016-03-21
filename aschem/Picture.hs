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

module Picture (
    pictureW,
    pictureN,
    pictureE,
    pictureS,
    pictureXBounds,
    pictureYBounds,
    pictureWidth,
    pictureHeight,
    moveW,
    moveE,
    moveS,
    moveN,
    moveNW,
    moveNE,
    moveSW,
    moveSE,
    addPos,
    subPos,
    averagePos,
    pictureAllPoss,
    pictureMoveWhile,
    maybePictureAt,
    pictureAt,
    mapPicture,
    pictureGetString,
    pictureGetStringRel,
    orientToMove,
    aboutFace,
    orientToAngle,
    makePicture,
    pictureMatchPattern
    ) where

    import Data.Maybe
    import Data.Array

    import Types

    orientToMove :: Orient -> Int -> Pos -> Pos
    orientToMove OrientE = moveE
    orientToMove OrientW = moveW
    orientToMove OrientN = moveN
    orientToMove OrientS = moveS
    orientToMove OrientNE = moveNE
    orientToMove OrientNW = moveNW
    orientToMove OrientSE = moveSE
    orientToMove OrientSW = moveSW

    aboutFace :: Orient -> Orient
    aboutFace OrientE = OrientW
    aboutFace OrientW = OrientE
    aboutFace OrientN = OrientS
    aboutFace OrientS = OrientN
    aboutFace OrientNE = OrientSW
    aboutFace OrientNW = OrientSE
    aboutFace OrientSE = OrientNW
    aboutFace OrientSW = OrientNE

    orientToAngle :: Orient -> Int
    orientToAngle OrientE = 0
    orientToAngle OrientW = 180
    orientToAngle OrientS = 90 -- clockwise?
    orientToAngle OrientN = -90
    orientToAngle OrientNE = -45
    orientToAngle OrientNW = -135
    orientToAngle OrientSE = 45
    orientToAngle OrientSW = 135

    pictureW :: Picture -> Int
    pictureW = fst . pictureNW

    pictureN :: Picture -> Int
    pictureN = snd . pictureNW

    pictureE :: Picture -> Int
    pictureE = fst . pictureSE

    pictureS :: Picture -> Int
    pictureS = snd . pictureSE

    pictureBounds :: Picture -> (Pos, Pos)
    pictureBounds picture = (pictureNW picture, pictureSE picture)

    pictureXBounds :: Picture -> Pos
    pictureXBounds picture = (pictureW picture, pictureE picture)

    pictureYBounds :: Picture -> Pos
    pictureYBounds picture = (pictureN picture, pictureS picture)

    pictureWidth :: Picture -> Int
    pictureWidth picture = 1 + pictureE picture - pictureW picture

    pictureHeight :: Picture -> Int
    pictureHeight picture = 1 + pictureS picture - pictureN picture

    moveW :: Int -> Pos -> Pos
    moveW d = addPos (-d,0)

    moveE :: Int -> Pos -> Pos
    moveE d = addPos (d,0)

    moveN :: Int -> Pos -> Pos
    moveN d = addPos (0,-d)

    moveS :: Int -> Pos -> Pos
    moveS d = addPos (0,d)

    moveNW :: Int -> Pos -> Pos
    moveNW d = addPos (-d,-d)

    moveNE :: Int -> Pos -> Pos
    moveNE d = addPos (d,-d)

    moveSW :: Int -> Pos -> Pos
    moveSW d = addPos (-d,d)

    moveSE :: Int -> Pos -> Pos
    moveSE d = addPos (d,d)

    addPos :: Pos -> Pos -> Pos
    addPos (x, y) (dx, dy) = (x + dx, y + dy)

    subPos :: Pos -> Pos -> Pos
    subPos (x, y) (dx, dy) = (x - dx, y - dy)

    averagePos :: Pos -> Pos -> Pos
    averagePos (x1,y1) (x2,y2) = ((x1+x2) `div` 2, (y1+y2) `div` 2)

    pictureAllPoss :: Picture -> [Pos]
    pictureAllPoss picture = [(x,y) | y <- range (pictureYBounds picture), x <- range (pictureXBounds picture)]

    maybePictureAt :: Picture -> Pos -> Maybe Char
    maybePictureAt picture pos
        | inRange (pictureBounds picture) pos = Just $ pictureMap picture ! pos
        | otherwise = Nothing

    pictureAt :: Picture -> Pos -> Char
    pictureAt picture pos = fromMaybe ' ' $ maybePictureAt picture pos

    pictureMoveWhile :: (Char -> Bool) -> (Pos -> Pos) -> Picture -> Pos -> Pos
    pictureMoveWhile p move picture pos = fromMaybe pos $ do
        char <- maybePictureAt picture pos
        if p char
            then return $ pictureMoveWhile p move picture (move pos)
            else return pos

    mapPicture :: Picture -> (Pos -> Bool) -> (Pos -> a) -> [a]
    mapPicture picture p f = mapMaybe body (pictureAllPoss picture)
        where
            body pos = if p pos
                then Just (f pos)
                else Nothing

    pictureGetString :: Picture -> Pos -> Pos -> String
    pictureGetString picture (x1, y1) (x2, y2)
        | y1 /= y2 = error $ "pictureGetString: ys must match: " ++ show (x1, y1) ++ " " ++ show (x2, y2)
        | otherwise = map (\x -> pictureAt picture (x, y1)) [min x1 x2..max x1 x2]

    pictureGetStringRel :: Picture -> Pos -> (Pos, Pos) -> String
    pictureGetStringRel picture pos (relPosL, relPosR) =
        pictureGetString picture (addPos pos relPosL) (addPos pos relPosR)

    makePicture :: [String] -> Picture
    makePicture lines = Picture nw se
        (array (nw, se) $ concatMap makeArrayLine $ zip [n..] lines)
        where
            n = 1
            w = 1
            nw = (n,w)
            width = maximum (map length lines)
            height = length lines
            se = addPos nw (width-1,height-1)

            padLine line = line ++ replicate (width - (length line)) ' '
            makeArrayLine (y, line) = zip (zip [w..] (repeat y)) (padLine line)

    pictureMatchPattern :: Pattern -> Picture -> Pos -> Bool
    pictureMatchPattern pattern picture pos = all charMatches matchPoss
        where
            match = patternPicture pattern
            matchPoss = pictureAllPoss match

            pictureOrigin = subPos pos (pictureNW match)
            at matchPos = pictureAt picture (addPos matchPos pictureOrigin)

            charMatches matchPos = matchChar == '?' || at matchPos == matchChar
                where matchChar = pictureAt match matchPos
