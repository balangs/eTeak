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

module Patterns (
    andOrientE,
    andOrientW,
    transOrientW,
    bufOrientE,
    bufOrientW,
    gnd,
    vdd,
    patterns
    ) where

    import Picture
    import Types

    andOrientE :: Pattern
    andOrientE = Pattern {
        patternOrient = OrientE,
        patternOrigin = (1,3),
        patternPicture = makePicture
            [" _?_??",
             "|   \\",
             "|????|",
             "|___/?"],
        patternPorts = [((0,-1), OrientW), ((0,0), OrientW), ((0,1), OrientW), ((5,0), OrientE),
            ((2,-1), OrientN), ((2,1), OrientS)],
        patternLabelPos = Just ((1,0), (4,0)) }

    andOrientW :: Pattern
    andOrientW = Pattern {
        patternOrient = OrientW,
        patternOrigin = (6,3),
        patternPicture = makePicture
            ["? _?_",
             "?/   |",
             "|????|",
             "?\\___|"],
        patternPorts = [((0,-1), OrientE), ((0,0), OrientE), ((0,1), OrientE), ((-5,0), OrientW),
            ((-2,-1), OrientN), ((-2,1), OrientS)],
        patternLabelPos = Just ((-4,0), (-1,0)) }

    transOrientW :: Pattern
    transOrientW = Pattern {
        patternOrient = OrientW,
        patternOrigin = (1,1),
        patternPicture = makePicture ["|["],
        patternPorts = [((0,0), OrientW), ((1,0), OrientN), ((1,0), OrientS)],
        patternLabelPos = Nothing }

    bufOrientE :: Pattern
    bufOrientE = Pattern {
        patternOrient = OrientE,
        patternOrigin = (1,1),
        patternPicture = makePicture ["|>|"],
        patternPorts = [((0,0), OrientW), ((2,0), OrientE)],
        patternLabelPos = Nothing }

    bufOrientW :: Pattern
    bufOrientW = Pattern {
        patternOrient = OrientW,
        patternOrigin = (1,1),
        patternPicture = makePicture ["|<|"],
        patternPorts = [((2,0), OrientE), ((0,0), OrientW)],
        patternLabelPos = Nothing }

    gnd :: Pattern
    gnd = Pattern {
        patternOrient = OrientE,
        patternOrigin = (2,1),
        patternPicture = makePicture
            ["_|_",
             "/ /"],
        patternPorts = [((0,0), OrientS)],
        patternLabelPos = Nothing }

    vdd :: Pattern
    vdd = Pattern {
        patternOrient = OrientE,
        patternOrigin = (2,2),
        patternPicture = makePicture
            ["/_\\",
             " | "],
        patternPorts = [((0,0), OrientN)],
        patternLabelPos = Nothing }

    patterns :: [(CompShape, Pattern)]
    patterns = [(CompAnd, andOrientW), (CompAnd, andOrientE),
        (CompBuf, bufOrientW), (CompBuf, bufOrientE), (CompTrans, transOrientW),
        (CompGnd, gnd), (CompVdd, vdd)]
