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

module Bits (
    boolToBit,
    bitNot,
    bitmaskToIntervals,
    extractBitfield,
    insertBitfield,
    recoverSign,
    sliceToImplicants,
    splitIntervalsBelow,
    splitIntervalsByInterval,
    filterIntervals,
    cropInt,
    removeImpsFromImp,
    impIsCoveredByImp,
    impExpand,
    impShiftRight,
    dcInWidth,
    showImp,
    digitChars,
    intWidth,
    Slice (sliceWidth, sliceOffset),
    sliceLow,
    sliceHigh,
    showSliceHL,
    balsaShowSlice,
    verilogShowSlice,
    emptySlice,
    isEmptySlice,
    sliceFromOW,
    sliceFromLH,
    sliceFromHL,
    sliceShiftL,
    sliceShiftR,
    sliceInRange,
    sliceIndex,
    sliceIndices,
    sliceOverlap,
    sliceOfSlice,
    sliceToBitmask,
    Implicant (..),
    (-:),
    (+:),
    (>:),
    (<:)
    ) where

    import Misc
    import Show

    import Data.Bits
    import Data.List
    import Data.Array
    import Data.Maybe

    intWidth :: Integral i => i -> Int
    intWidth 0 = 1
    intWidth n = log2 (2 * n)

    boolToBit :: Bool -> Integer
    boolToBit True = 1
    boolToBit False = 0

    bitNot :: (Num i, Bits i) => Int -> i -> i
    bitNot width arg = (bit width - 1) - arg

    -- extractBitfield : extract an unsigned bitfield from the (positive, negative or zero) integer
    --    val.  Negative integers are treated as 2s complemented unsigned numbers.
    extractBitfield :: Slice Int -> Integer -> Integer
    extractBitfield slice val = (cropInt (sliceHigh slice + 1) val) `shiftR` (sliceLow slice)

    insertBitfield :: Integer -> Slice Int -> Integer -> Integer
    insertBitfield value slice bitfield = valueHigh + bitfield' + valueLow
        where
            bitfield' = bitfield `shiftL` (sliceLow slice)
            valueHigh = (value `shiftR` (sliceHigh slice + 1)) `shiftL` (sliceHigh slice + 1)
            valueLow = value .&. (bit (sliceLow slice) - 1)

    -- cropInt : crop an integer to the given number of bits.  If 'val' is negative then
    --    returns the cropped 2s value of that integer.  The result is, therefore, always non-negative
    cropInt :: Int -> Integer -> Integer
    cropInt width val = crop $ (crop val) + maxPlus1
        where
            crop i = i .&. (maxPlus1 - 1)
            maxPlus1 = bit width

    -- recoverSign : take a number with a sign bit in the MSB at bit width-1 and convert to the
    --    the positive or negative Integer it represents
    recoverSign :: Int -> Integer -> Integer
    recoverSign width val
        | signBit == 1 = maxNegative + val
        | otherwise = val
        where
            signBit = extractBitfield (sliceFromOW (width - 1) 1) val
            maxNegative = - (bit width)

    minWithLSDCs :: Integer -> Int -> Integer
    minWithLSDCs value dcCount = value - (value .&. (bit dcCount - 1))

    rangeWithLSDCs :: Integer -> Int -> (Integer, Integer)
    rangeWithLSDCs val dcCount = (minVal, minVal + bit dcCount - 1)
        where minVal = minWithLSDCs val dcCount

    data Implicant = Imp Integer Integer -- value, dcbits (1 for DC, 0 for use value)
        deriving (Show, Read, Eq, Ord)

    dcInWidth :: Int -> Implicant
    dcInWidth width = Imp 0 (bit width - 1)

    impDifference :: Implicant -> Implicant -> [Implicant]
    impDifference l@(Imp _ lDcs) r@(Imp rValue rDcs)
        | impDisjoint l r = [l]
        | otherwise = body [] lDcs 0 l
        where
            -- body ret dontCares result bitNo
            body ret 0 _ _ = ret
            body ret dontCares bitNo result@(Imp resultValue resultDcs)
                | testBit rDcs bitNo = body ret dontCaresWithBitClear nextBit result
                | testBit lDcs bitNo = body (newImp:ret) dontCaresWithBitClear nextBit result'
                | otherwise = body ret dontCares nextBit result
                where
                    nextBit = bitNo + 1
                    dontCaresWithBitClear = clearBit dontCares bitNo
                    resultValueWithBitSet = setBit resultValue bitNo
                    resultDcsWithBitClear = clearBit resultDcs bitNo
                    rBitSet = testBit rValue bitNo
                    result' = Imp
                        (if rBitSet then resultValueWithBitSet else resultValue)
                        resultDcsWithBitClear
                    newImp = Imp
                        (if not rBitSet then resultValueWithBitSet else resultValue)
                        resultDcsWithBitClear

    impDisjoint :: Implicant -> Implicant -> Bool
    impDisjoint (Imp lValue lDcs) (Imp rValue rDcs) = rWithL /= lWithR
        where
            rWithL = rValue .|. (lValue .&. rDcs)
            lWithR = lValue .|. (rValue .&. lDcs)

    removeImpFromImps :: [Implicant] -> Implicant -> [Implicant]
    removeImpFromImps imps imp = nub $ sort $ concat impss
        where impss = map (\from -> impDifference from imp) imps

    removeImpsFromImp :: Implicant -> [Implicant] -> [Implicant]
    removeImpsFromImp imp remove = foldl' removeImpFromImps [imp] remove

    -- impIsCoveredByImp : does 'match' cover all of the values in 'test'
    --    (match.dcs & test.dcs) == test.dcs && (test.value & ~ match.dcs) == match.value
    impIsCoveredByImp :: Int -> Implicant -> Implicant -> Bool
    impIsCoveredByImp width (Imp testValue testDcs) (Imp matchValue matchDcs) =
        (matchDcs .&. testDcs) == testDcs &&
        (testValue .&. (bitNot width matchDcs)) == matchValue

    -- impExpand : expand an implicant into all its matching integers in ascending value
    impExpand :: Implicant -> [Integer]
    impExpand (Imp value 0) = [value]
    impExpand (Imp value dcs) = impExpand (Imp (clearBit value topDC) dcs') ++
        impExpand (Imp (setBit value topDC) dcs')
        where
            topDC = log2 dcs
            dcs' = clearBit dcs topDC

    impShiftRight :: Implicant -> Int -> Implicant
    impShiftRight (Imp value dcs) dist = Imp (value `shiftR` dist) (dcs `shiftR` dist)

    showImp :: Bool -> Bool -> Int -> Implicant -> String
    showImp chooseBestRadix showRadixPrefix spacing (Imp val dcs) = radixPrefix ++ body
        where
            radix
                | dcs == 0 = 10
                | chooseBestRadix && all (== 15) (splitIntoDigits 16 dcs) = 16
                | otherwise = 2

            radixPrefix
                | showRadixPrefix = case radix of
                    16 -> "0x"
                    2 -> "0b"
                    _ -> ""
                | otherwise = ""

            rawBody = showRadixImp radix val dcs

            body
                | spacing == 0 = rawBody
                | otherwise = reverse $ joinWith "_" $ mapN spacing id $ reverse $ rawBody

    splitIntoDigits :: Int -> Integer -> [Int]
    splitIntoDigits _ 0 = [0]
    splitIntoDigits radix val = reverse $ body val
        where
            intRadix :: Integer
            intRadix = fromIntegral radix

            divRadix = (`div` intRadix)
            modRadix = fromIntegral . (`mod` intRadix)

            body 0 = []
            body remainder = modRadix remainder : body (divRadix remainder)

    showRadixImp :: Int -> Integer -> Integer -> String
    showRadixImp radix valIn dcsIn = map makeDigit $ prePadZip 0 dcsDigits valDigits
        where
            dcsDigits = splitIntoDigits radix dcsIn
            valDigits = splitIntoDigits radix valIn

            allOnes = radix - 1

            makeDigit (dcs, val)
                | dcs == allOnes = 'X'
                | otherwise = digitChars ! val

    digitString :: String
    digitString = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    digitChars :: Array Int Char
    digitChars = listArray (0, length digitString - 1) digitString

    -- Slice : Slice is used to represent contiguous, ascending ranges on Integral types in
    --    a similar way to Ix's `range' values but with the inclusion of empty ranges.
    --    This type can be used for index ranges, bitfield ranges (hence the name Slice), or
    --    for representing other integral intervals

    data {- Integral i => -} Slice i = Slice {
        sliceOffset :: i,
        sliceWidth :: i } 

    instance (Integral i, Show i) => Show (Slice i) where
        showsPrec prec slice
            | isEmptySlice slice = showString "0+:0"
            | otherwise = showParen (prec > sliceConsPrecedence) $
                shows (sliceOffset slice) . showString "+:" . shows (sliceWidth slice)

    instance (Read i, Integral i, Show i) => Read (Slice i) where
        readsPrec _ = readParen False readSlice

    readSlice :: (Read i, Integral i, Show i) => ReadS (Slice i)
    readSlice str = maybeToList $ do
        (headToken, rest) <- maybeLex str
        left <- maybeRead headToken
        (op, rest2) <- maybeLex rest
        (rightToken, rest3) <- maybeLex rest2
        right <- maybeRead rightToken
        ret <- case op of
            "-:" -> return $ left -: right
            "+:" -> return $ left +: right
            ">:" -> return $ left >: right
            "<:" -> return $ left <: right
            _ -> fail ""
        return (ret, rest3)

    instance Integral i => Eq (Slice i) where
        l == r = (isEmptySlice l && isEmptySlice r) || (sliceOffset l == sliceOffset r &&
            sliceWidth l == sliceWidth r)

    -- compare : slices are sorted by emptiness, then offset, then width
    instance Integral i => Ord (Slice i) where
        compare l r = if isEmptySlice l
            then if isEmptySlice r
                then EQ
                else LT
            else if sliceOffset l /= sliceOffset r
                then compare (sliceOffset l) (sliceOffset r)
                else compare (sliceWidth l) (sliceWidth r)

    -- sliceLow : lowest value in slice
    sliceLow :: Integral i => Slice i -> i
    sliceLow slice
        | isEmptySlice slice = error "sliceLow: can't get low index of empty Slice"
        | otherwise = sliceOffset slice

    -- sliceHigh : highest value in slice
    sliceHigh :: Integral i => Slice i -> i
    sliceHigh slice
        | isEmptySlice slice = error "sliceHigh: can't get high index of empty Slice"
        | otherwise = sliceLow slice + sliceWidth slice - 1

    -- showSliceHL : show a high downto low slice with the given bracketing and separator
    showSliceHL :: (Integral i, Show i) => String -> String -> String -> Slice i -> ShowS
    showSliceHL open sep close slice = showString open . shows (sliceHigh slice) .
        showString sep . shows (sliceLow slice) . showString close

    -- showSliceLH : show a high downto low slice with the given bracketing and separator
    showSliceLH :: (Integral i, Show i) => String -> String -> String -> Slice i -> ShowS
    showSliceLH open sep close slice = showString open . shows (sliceLow slice) .
        showString sep . shows (sliceHigh slice) . showString close

    -- balsaShowSlice : show a slice as [high..low]
    balsaShowSlice :: (Integral i, Show i) => Slice i -> ShowS
    balsaShowSlice slice
        | isEmptySlice slice = showString "(empty)"
        | otherwise = showSliceLH "[" ".." "]" slice

    -- verilogShowSlice : show a slice as [high:low], as in Verilog
    verilogShowSlice :: (Integral i, Show i) => Slice i -> ShowS
    verilogShowSlice slice
        | isEmptySlice slice = showString "(empty)"
        | otherwise = showSliceHL "[" ":" "]" slice

    -- emptySlice : slice covering no values
    emptySlice :: Integral i => Slice i
    emptySlice = Slice 0 0

    -- isEmptySlice : test for empty slice.  Doesn't care about offset
    isEmptySlice :: Integral i => Slice i -> Bool
    isEmptySlice = (== 0) . sliceWidth

    --- Slice constructors

    -- sliceFromOW : make a Slice from an offset/width pair
    sliceFromOW :: (Integral i, Show i) => i -> i -> Slice i
    sliceFromOW offset 0
        | offset /= 0 = error $ "sliceFromOW: offset `" ++ show offset ++ "' must be 0 for emptySlice"
        | otherwise = emptySlice
    sliceFromOW offset width
        | offset < 0 || width < 0 = error $ "sliceFromOW: bad slice `sliceFromOW " ++ show offset ++ " "
            ++ show width ++ "'"
        | otherwise = Slice offset width

    -- sliceFromLH : make a Slice from low/high values.  Makes an emptySlice if high < low
    sliceFromLH :: (Show i, Integral i) => i -> i -> Slice i
    sliceFromLH low high
        | low < 0 || high < 0 = error $ "sliceFromLH: bad slice `sliceFromLH " ++ show low ++ " "
            ++ show high ++ "'"
        | high < low = emptySlice
        | otherwise = Slice low (1 + high - low)

    -- sliceFromHL : as sliceFromLH but with the arguments the other way around
    sliceFromHL :: (Show i, Integral i) => i -> i -> Slice i
    sliceFromHL = flip sliceFromLH

    sliceConsPrecedence :: Int
    sliceConsPrecedence = 7

    -- {(-:), (+:), (>:), (<:)} : infix constructors
    infix 7 -:
    infix 7 +:
    infix 7 >:
    infix 7 <:

    (-:) :: (Integral i, Show i) => i -> i -> Slice i
    highOffset -: width
        | offset >= 0 = sliceFromOW offset width
        | otherwise = error $ "(-:): offset is negative `" ++ show offset ++ "' for `" ++ show highOffset
            ++ " -: " ++ show width ++ "'"
        where offset = 1 + highOffset - width

    (+:) :: (Integral i, Show i) => i -> i -> Slice i
    offset +: width = sliceFromOW offset width

    (>:) :: (Integral i, Show i) => i -> i -> Slice i
    high >: low = sliceFromHL high low

    (<:) :: (Integral i, Show i) => i -> i -> Slice i
    low <: high = sliceFromHL high low

    --- Slice operators

    -- sliceShiftL :: increase a slice's offset by `shift'
    sliceShiftL :: Integral i => Slice i -> i -> Slice i
    sliceShiftL slice shift = slice { sliceOffset = sliceOffset slice + shift }

    -- sliceShiftR :: decrease a slice's offset by `shift'
    sliceShiftR :: (Show i, Integral i) => Slice i -> i -> Slice i
    sliceShiftR slice shift
        | sliceOffset slice <= shift = error $ "sliceShiftR: can't shift `" ++ show slice ++ "' " ++ show shift
            ++ " places to the right"
        | otherwise = slice { sliceOffset = sliceOffset slice - shift }

    -- sliceInRange : like Ix's inRange, but for Slices
    sliceInRange :: Integral i => Slice i -> i -> Bool
    sliceInRange slice i
        | isEmptySlice slice = False
        | otherwise = sliceLow slice <= i && sliceHigh slice >= i

    -- sliceIndex : like Ix's index, but for Slices
    sliceIndex :: Integral i => Slice i -> i -> i
    sliceIndex slice i
        | isEmptySlice slice = error "sliceIndex: can't index into an emptySlice"
        | otherwise = i - sliceLow slice

    -- sliceOverlap : return the (Maybe) overlap between two slices
    sliceOverlap :: (Integral i, Show i) => Slice i -> Slice i -> Maybe (Slice i)
    sliceOverlap slice1 slice2
        | sliceLow slice1 > sliceHigh slice2 = Nothing
        | sliceLow slice2 > sliceHigh slice1 = Nothing
        | otherwise = Just $ sliceFromLH
            (max (sliceLow slice1) (sliceLow slice2)) (min (sliceHigh slice1) (sliceHigh slice2))

    -- sliceIndices : Ix.indices for slices
    sliceIndices :: Integral i => Slice i -> [i]
    sliceIndices slice
        | isEmptySlice slice = []
        | otherwise = [sliceLow slice .. sliceHigh slice]

    -- sliceOfSlice : take a subSlice of a slice assuming the indexing for the bottom bit of the slice in the
    --    subSlice is 0.  So sliceOfSlice (15>:7) (3>:1) == (10>:8)
    sliceOfSlice :: Integral i => Slice i -> Slice i -> Slice i
    sliceOfSlice slice subSlice = subSlice `sliceShiftL` (sliceLow slice)

    -- `intervals' are listed of contiguous, non-overlapping, increasing slices

    -- splitIntervalsBelow : split intervals on an integer to give a list with: exists j . sliceLow (ret !! j) == n
    splitIntervalsBelow :: (Show i, Integral i) => [Slice i] -> i -> [Slice i]
    splitIntervalsBelow [] _ = []
    splitIntervalsBelow (r:rs) n
        | n == low = r:rs
        | n > high = r:splitIntervalsBelow rs n
        | n > low = (sliceFromLH low (n - 1)):(sliceFromLH n high):rs
        | otherwise = error "splitIntervalsBelow: can't happen"
        where (low, high) = (sliceLow r, sliceHigh r)

    -- splitIntervalsByInterval : split the intervals so that the given single interval can be constructed
    --    from a sub-list of the resulting intervals
    splitIntervalsByInterval :: (Integral i, Show i) => [Slice i] -> Slice i -> [Slice i]
    splitIntervalsByInterval intervals slice = splitIntervalsBelow
        (splitIntervalsBelow intervals (high + 1)) low
        where (low, high) = (sliceLow slice, sliceHigh slice)

    -- filterIntervals : find the intervals which are contained in or overlap the given single interval
    --    (intervals are in ascending, non-overlapping order)
    filterIntervals :: (Integral i, Show i) => [Slice i] -> Slice i -> [Slice i]
    filterIntervals intervals interval = filter (isJust . sliceOverlap interval) intervals

    -- sliceToImplicants : produces a list of implicants which exactly match the given slice range
    sliceToImplicants :: Slice Integer -> [Implicant]
    sliceToImplicants slice
        | isEmptySlice slice = error "sliceToImplicants: can't convert an empty slice to implicants"
        | otherwise = body low0 (low0 + 1) 0 []
        where
            (low0, high0) = (sliceLow slice, sliceHigh slice)

            body low next dcCount implicants
                | low > high0 = implicants
                -- try to add another don't care to the bottom
                | minR >= low && maxR <= high0 = body low (maxR + 1) (dcCount + 1) implicants
                -- otherwise be happy with these don't cares and start again
                | otherwise = body next (next + 1) 0 (implicant:implicants)
                where
                    implicant = Imp low (bit dcCount - 1)
                    (minR, maxR) = rangeWithLSDCs low (dcCount + 1)

    -- bitmaskToSlices : return intervals matching the longest spans of 1s (by bit index) in the given integer
    bitmaskToIntervals :: (Bits i, Num i, Integral j, Show j) => i -> [Slice j]
    bitmaskToIntervals mask = notInSlice 0 mask
        where
            notInSlice _ 0 = []
            notInSlice i mask
                | testBit mask 0 = inSlice i i' mask'
                | otherwise = notInSlice i' mask'
                where
                    mask' = mask `shiftR` 1
                    i' = i + 1

            inSlice from to 0 = [from<:(to - 1)]
            inSlice from to mask
                | testBit mask 0 = inSlice from (to + 1) (mask `shiftR` 1)
                | otherwise = (from<:(to - 1)) : notInSlice to mask 

    -- sliceToBitmask : return a bitmask with 1s at each of the bit indices of the given slice
    sliceToBitmask :: (Bits j, Num j, Integral i) => Slice i -> j
    sliceToBitmask slice = ((bit $ fromIntegral $ sliceWidth slice) - 1) `shiftL` (fromIntegral (sliceOffset slice))
