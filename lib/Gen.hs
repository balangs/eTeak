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

module Gen (
    genPartToGateNetlist,
    GenPartToGateOption (..),
    mappingNetlistToTechMapping,
    -- mappingNetlistToCosts,
    genMakeComp,
    genTechMap,
    fullBundles2,
    genMakeGatesFile,
    TechMapping,
    GateCosts
    ) where

    import Misc
    import NetParts
    import ParseTree
    import Data.List
    import qualified Data.Map as DM
    import Gates
    import Type
    import Data.Bits
    import Bits

    import Data.Maybe
    import Control.Monad
    import Data.Char
    import Numeric
    import System.IO
    import System.Time

    data RelDir = Forward | Reverse
        deriving (Show, Read, Eq)

    invertDir :: Direction -> Direction
    invertDir Input = Output
    invertDir Output = Input

    data Portion = R0 | R1 | A | N | R | G
        deriving (Show, Read)

    bundle :: String -> Int -> Portion -> String
    bundle name index A = name ++ "_" ++ show index ++ "a"
    bundle name index R0 = name ++ "_" ++ show index ++ "r0"
    bundle name index R1 = name ++ "_" ++ show index ++ "r1"
    bundle name index R = name ++ "_" ++ show index ++ "r"
    bundle name index N = name ++ "_" ++ show index
    bundle name _ G = name

    fullBundles :: String -> Int -> Int -> [(String, Int, RelDir)]
    fullBundles name width index
        | width == 0 = [(portion R, 1, Forward), (portion A, 1, Reverse)]
        | otherwise = [(portion R0, width, Forward), (portion R1, width, Forward), (portion A, 1, Reverse)]
        where portion = bundle name index

    fullBundles2 :: String -> Int -> Int -> [(String, Int, Bool)]   -----Ture=Forward False = Reverse
    fullBundles2 name width index
        | width == 0 = [(portion R, 1, True), (portion A, 1, False)]
        | otherwise = [(portion R0, width, True), (portion R1, width, True), (portion A, 1, False)]
        where portion = bundle name index


    relDirXDirToPortDir :: RelDir -> Direction -> Direction
    relDirXDirToPortDir Forward = id
    relDirXDirToPortDir Reverse = invertDir

    mkPorts :: String -> Direction -> Int -> [Int] -> [GatePort]
    mkPorts name dir count widths = map mkPort $ concatMap bundle $ zip widths [0..count - 1]
        where
            mkPort (name, width, relDir) = GatePort name (relDirXDirToPortDir relDir dir) width
            bundle (width, i) = fullBundles name width i

    nets :: String -> Int -> Int -> [GateElem]
    nets _ _ 0 = []
    nets name count width = map (\i -> GateNet (bundle name i N) width) [0..count - 1]

    netsW :: String -> Int -> [Int] -> [GateElem]
    netsW name count widths = map (\(i, width) -> GateNet (bundle name i N) width) $ zip [0..count - 1] widths

    netsIW :: String -> [Int] -> [Int] -> [GateElem]
    netsIW name indices widths = map (\(i, width) -> GateNet (bundle name i N) width) $ zip indices widths

    conn :: Portion -> String -> Int -> Slice Int -> [GateConn]
    conn portion name index slice = [GateConn (bundle name index portion) slice]

    connSlice :: Slice Int -> [GateConn] -> [GateConn]
    connSlice slice conns
        | retLength == 0 = error $ "connSlice: no slices: " ++ show conns ++ " " ++ verilogShowSlice slice ""
        | otherwise = ret
        where
            retLength = length ret
            ret = body 0 (sliceWidth slice) conns
            topBit = sliceHigh slice
            offset = sliceOffset slice

            body _ 0 [] = []
            body _ _ [] = error $ "connSlice: not enough bits: " ++ show conns ++ " " ++ verilogShowSlice slice ""
            body thisOffset remainingWidth ((GateConn name connSlice):cs)
                | nextOffset <= offset = next remainingWidth cs
                | beyondSlice && remainingWidth /= 0 = error "connSlice: not enough bits"
                | beyondSlice = []
                | otherwise = (GateConn name ((connOffset + newConnShift) +:
                    (min remainingWidth (connWidth - newConnShift)))) : next (remainingWidth - newConnWidth) cs
                where
                    connWidth = sliceWidth connSlice
                    connOffset = sliceOffset connSlice
                    beyondSlice = thisOffset > topBit

                    newConnShift = max 0 (offset - thisOffset)
                    newConnWidth = min remainingWidth (connWidth - newConnShift)

                    nextOffset = thisOffset + connWidth
                    next = body nextOffset

    connWidth :: [GateConn] -> Int
    connWidth conns = sum $ map gateConnWidth conns

    smash :: [GateConn] -> [GateConn]
    smash conns = concatMap smashConn conns
        where smashConn (GateConn name slice) = map makeGate $ sliceIndices slice
                where makeGate i = GateConn name (i +: 1)

    smashSplit :: [GateConn] -> [[GateConn]]
    smashSplit = map (:[]) . smash

    gateConnWidth :: GateConn -> Int
    gateConnWidth (GateConn _ slice) = sliceWidth slice

    gate :: String -> [[GateConn]] -> [GateElem]
    gate _ [] = []
    gate gateName connss
        | length widths > 0 && any (/= width) widths = error $ "bad widths in `" ++ gateName ++ "': " ++ show connss
        | otherwise = map makeGate [0..width-1]
            where
                makeGate i = GateInstance gateName (map (connSlice (i +: 1)) connss)
                widths = map connWidth connss
                width = head widths

    singleGate :: String -> [[GateConn]] -> [GateElem]
    singleGate gateName connss = [GateInstance gateName connss]

    singleGateParam :: String -> [GateParam] -> [[GateConn]] -> [GateElem]
    singleGateParam gateName params connss = [GateInstanceParam gateName params connss]

    gateSome :: String -> [Some [GateConn]] -> [GateElem]
    gateSome gateName connss = gate gateName $ flattenSome $ Some connss

    each :: Int -> Portion -> String -> Slice Int -> [[GateConn]]
    each count portion name slice = map
        (\index -> conn portion name index slice) [0..count-1]

    eachW :: Int -> Portion -> String -> Int -> [Int] -> [[GateConn]]
    eachW count portion name offset widths = map
        (\(index, width) -> conn portion name index (offset +: width)) $ zip [0..count-1] widths

    eachIW :: Portion -> String -> Int -> [Int] -> [Int] -> [[GateConn]]
    eachIW portion name offset indices widths = map
        (\(index, width) -> conn portion name index (offset +: width)) $ zip indices widths

    dupEach :: Int -> [[GateConn]] -> [[GateConn]]
    dupEach w connss = map (concat . replicate w) $ connss
    -- dup w connss = map (replicate w . concat) $ connss
    dupEachW :: [Int] -> [[GateConn]] -> [[GateConn]]
    dupEachW ws connss = map (\(w, cons) -> concat (replicate w cons)) $ zip ws connss

    makeGateTree :: String -> String -> Int -> Bool -> [GateConn] -> [GateConn] -> [GateElem]
    makeGateTree prefix gateType maxFanIn useInvGates output input =
        gateTree (1 :: Int) invOutputIfLast firstStageGate (smashSplit input)
        where
            fullGateInverts = gateInverts gateType
            invOutputIfLast = if useInvGates then not fullGateInverts else fullGateInverts
            invGateType = if fullGateInverts then gateType else invertGateOutput gateType
            firstStageGate = if useInvGates then invGateType else invertGateOutput invGateType

            gateTree _ _ _ [] = error "makeGateTree: zero inputs"
            gateTree level invOutputIfLast stageGate inputs
                | inputCount <= maxFanIn = gateSome lastGate [One output, Many inputs]
                --   | stageOutputWidth /= length groupedInputs = error "!!!!"
                | otherwise = stageNets ++ stageGates ++ rest
                where
                    inputCount = length inputs

                    lastGate = gateMap inputCount $ if invOutputIfLast
                        then invertGateOutput stageGate
                        else stageGate

                    stageOutputName = prefix ++ show level
                    stageOutputWidth = (inputCount + maxFanIn - 1) `div` maxFanIn
                    stageNets = nets stageOutputName 1 stageOutputWidth
                    stageOutputConn = conn N stageOutputName 0 (0 +: stageOutputWidth)
                    stageOutputs = smashSplit stageOutputConn

                    groupedInputs = mapN maxFanIn id inputs
                    stageGates = map makeGate $ zip stageOutputs groupedInputs

                    makeGate (output, inputs) = head $ gateSome mappedGateName [One output, Many inputs]
                        where mappedGateName = gateMap (length inputs) stageGate

                    rest = if useInvGates
                        then gateTree (level + 1) (not invOutputIfLast) (deMorgansOpposite stageGate) stageOutputs
                        else gateTree (level + 1) invOutputIfLast stageGate stageOutputs

    deMorgansOpposite :: String -> String
    deMorgansOpposite gateType = case gateType of
        "and" -> "or"
        "buff" -> "buff"
        "c" -> "c"
        "inv" -> "inv"
        "nand" -> "nor"
        "nc" -> "nc"
        "nor" -> "nand"
        "or" -> "and"
        _ -> error $ "deMorgansOpposite: unrecognised gate `" ++ gateType ++ "'"

    invertGateOutput :: String -> String
    invertGateOutput gateType = case gateType of
        "and" -> "nand"
        "buff" -> "inv"
        "c" -> "nc"
        "inv" -> "buff"
        "nand" -> "and"
        "nc" -> "c"
        "nor" -> "or"
        "or" -> "nor"
        _ -> error $ "invertGateOutput: unrecognised gate `" ++ gateType ++ "'"

    gateInverts :: String -> Bool
    gateInverts gateType = case gateType of
        "and" -> False
        "buff" -> False
        "c" -> False
        "inv" -> True
        "nand" -> True
        "nc" -> True
        "nor" -> True
        "or" -> False
        _ -> error $ "gateInverts: unrecognised gate `" ++ gateType ++ "'"

    gateMap :: Int -> String -> String
    gateMap 1 gate = if gateInverts gate then "inv" else "buff"
    gateMap width gate = gate ++ show width

    drCompletion :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    drCompletion tempName connF connT connResult = concat [
        nets tempName 1 width,
        gate "or" [temp, connF, connT],
        gateSome "c" [One connResult, Many (smashSplit temp)] ]
        where
            width = connWidth connF
            temp = conn N tempName 0 (0 +: width)

    drCompletions :: String -> [[GateConn]] -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    drCompletions tempName d0s d1s outputs = concatMap makeCompletion $ zip4 ([0..] :: [Int]) d0s d1s outputs
        where makeCompletion (i, d0, d1, output) = drCompletion (tempName ++ show i) d0 d1 output

    reset :: [GateConn]
    reset = conn G "reset" 0 (0 +: 1)

    pipeLatch :: String -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    pipeLatch tempName inpF inpT inpA outF outT outA = concat [
        nets nackName 1 1,
        gate "c2r1" [outF, inpF, dupWidth nack, dupWidth reset],
        gate "c2r1" [outT, inpT, dupWidth nack, dupWidth reset],
        gate "inv" [nack, outA],
        drCompletion (tempName ++ "comp") outF outT inpA ]
        where
            nackName = tempName ++ "na"
            width = connWidth inpF
            nack = conn N nackName 0 (0 +: 1)
            dupWidth conn = concat (dupEach width [conn])

    pipeLatch0 :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    pipeLatch0 tempName inpR inpA outR outA = concat [
        nets tempName 1 1,
        gate "c2r1" [outR, inpR, temp, reset],
        gate "inv" [temp, outA],
        gateSome "connect" [One outR, Many [inpA]] ]
        where temp = conn N tempName 0 (0 +: 1)

    pipeLatchN :: String -> Int -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    pipeLatchN _ 0 inpF inpT inpA outF outT outA = concat [
        gateSome "connect" [One inpF, Many [outF]],
        gateSome "connect" [One inpT, Many [outT]],
        gateSome "connect" [One outA, Many [inpA]] ]
    pipeLatchN tempName 1 inpF inpT inpA outF outT outA = pipeLatch tempName inpF inpT inpA outF outT outA
    pipeLatchN tempName depth inpF inpT inpA outF outT outA = concat [
        nets tempFName 1 width,
        nets tempTName 1 width,
        nets tempAName 1 1,
        pipeLatch (tempName ++ "b") inpF inpT inpA tempF tempT tempA,
        pipeLatchN (tempName ++ "o") (depth - 1) tempF tempT tempA outF outT outA ]
        where
            width = connWidth inpF
            tempFName = tempName ++ "f"
            tempTName = tempName ++ "t"
            tempAName = tempName ++ "a"
            tempF = conn N tempFName 0 (0 +: width)
            tempT = conn N tempTName 0 (0 +: width)
            tempA = conn N tempAName 0 (0 +: 1)

    pipeLatch0N :: String -> Int -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    pipeLatch0N _ 0 inpR inpA outR outA = concat [
        gateSome "connect" [One inpR, Many [outR]],
        gateSome "connect" [One outA, Many [inpA]] ]
    pipeLatch0N tempName 1 inpR inpA outR outA = pipeLatch0 tempName inpR inpA outR outA
    pipeLatch0N tempName depth inpR inpA outR outA = concat [
        nets tempRName 1 1,
        nets tempAName 1 1,
        pipeLatch0 (tempName ++ "b") inpR inpA tempR tempA,
        pipeLatch0N (tempName ++ "o") (depth - 1) tempR tempA outR outA ]
        where
            tempRName = tempName ++ "f"
            tempAName = tempName ++ "a"
            tempR = conn N tempRName 0 (0 +: 1)
            tempA = conn N tempAName 0 (0 +: 1)

    bundlesAtIndicesW :: [Int] -> Portion -> String -> Int -> [Int] -> [[GateConn]]
    bundlesAtIndicesW indices portion name offset widths = map
        (\index -> conn portion name index (offset +: (widths !! index))) indices

    bundlesAtIndices :: [Int] -> Portion -> String -> Slice Int -> [[GateConn]]
    bundlesAtIndices indices portion name slice = map
        (\index -> conn portion name index slice) indices

    connectBundleSlices :: [Slice Int] -> [GateConn] -> [GateConn] -> Portion -> String -> [GateElem]
    connectBundleSlices slices complete input outputPortion outputName = elems
        where
            elems = concatMap makeBundle $ zip [0..] slices
            inpWidth = connWidth input

            makeBundle (index, slice)
                | isEmptySlice slice = []
                | sliceWidth slice == inpWidth = gateSome "connect" [One (connSlice slice input),
                    Many [conn outputPortion outputName index (0 +: sliceWidth slice)]]
                | otherwise = concat [
                    gate "c" [conn outputPortion outputName index (0 +: 1),
                        connSlice (sliceOffset slice +: 1) input, complete],
                    if sliceWidth slice > 1
                        then gateSome "connect"
                            [One (connSlice ((sliceOffset slice + 1) +: (sliceWidth slice - 1)) input),
                            Many [conn outputPortion outputName index (1 +: (sliceWidth slice - 1))]]
                        else []
                    ]

    drLatch :: String -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    drLatch tempName inpF inpT en inpA outF outT reset = concat [
        nets gfName 1 width,
        nets gtName 1 width,
        gate "and" [gf, inpF, en],
        gate "and" [gt, inpT, en],
        gate "nor" [outF, outT, gt],
        gate "nor" [outT, outF, gf, concat (dupEach width [reset])],
        gate "ao22" [inpA, gf, outF, gt, outT] ]
        where
            gfName = tempName ++ "gf"
            gtName = tempName ++ "gt"
            gf = conn N gfName 0 (0 +: width)
            gt = conn N gtName 0 (0 +: width)

            width = connWidth inpF

    -- findInputBits : from write offsets and widths give a list of input bits which form part of this
    --    output.  Returns (inpIndex, inpBit) pairs
    findInputBits :: [Slice Int] -> Int -> [(Int, Int)]
    findInputBits slices index = concatMap isAtThisBit $ zip [0..] slices
        where
            isAtThisBit (inpNo, slice)
                | sliceInRange slice index = [(inpNo, sliceIndex slice index)]
                | otherwise = []

    connsAtBit :: [[GateConn]] -> [(Int, Int)] -> [[GateConn]]
    connsAtBit inps bitSelection = map makeSlice bitSelection
        where makeSlice (inpNo, inpBit) = connSlice (inpBit +: 1) (inps !! inpNo)

    connectWrites :: String -> [Int] -> [[GateConn]] -> [[GateConn]] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [[GateConn]] -> [[GateConn]] ->
        [GateConn] -> [GateConn] -> [GateElem]
    connectWrites tempName writeOffsets wgFs wgTs wF wT anyRead writeAcks inpCompletes bitEns bitAcks = concat [
        nets igcName count 1,
        nets igcanwName 1 1,
        netsW gitName count widths,
        netsW gifName count widths,
        nets igName count 1,
        gate "and" [concat gif, concat wgFs,
            concat (dupEachW widths (each count N igName (0 +: 1)))],
        gate "and" [concat git, concat wgTs,
            concat (dupEachW widths (each count N igName (0 +: 1)))],
        gateSome "connect" [One (concat inpCompletes), Many [concat (each count N igcName (0 +: 1))]],
        gate "c1u1" [concat (each count N igName (0 +: 1)),
            concat (each count N igcName (0 +: 1)),
            concat (dupEach count [conn N igcanwName 0 (0 +: 1)])],
        gateSome "nor" [One (conn N igcanwName 0 (0 +: 1)), Many (anyRead : each count N igName (0 +: 1))],
        concatMap muxForBit [0..width - 1],
        concatMap completeInput $ zip4 writeOffsets widths writeAcks (each count N igName (0 +: 1))
        ]
        where
            gif = eachW count N gifName 0 widths
            git = eachW count N gitName 0 widths
            -- allInpCompletes = concat inpCompletes

            muxForBit i = concat [
                gateSome "or" [One (connSlice (i +: 1) wF), Many (connsAtBit gif inputBits)],
                gateSome "or" [One (connSlice (i +: 1) wT), Many (connsAtBit git inputBits)],
                -- or together inpCompletes for each bit to form enable to latches
                gateSome "or" [One (connSlice (i +: 1) bitEns), Many (map ((inpCompletes !!) . fst) inputBits)]
                ]
                where inputBits = findInputBits (zipWith (+:) writeOffsets widths) i

            completeInput (offset, width, inpAck, ig) = gateSome "c" [One inpAck, One ig, Many
                (map (\i -> connSlice (i +: 1) bitAcks) [offset..offset + width - 1])]

            width = connWidth wF
            widths = map connWidth wgFs

            igcName = tempName ++ "igc"
            igcanwName = tempName ++ "igcanw"
            gitName = tempName ++ "git"
            gifName = tempName ++ "gif"
            igName = tempName ++ "ig"

            count = length writeOffsets

    -- FIXME
    handleBuiltinWrites :: String -> [Int] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    handleBuiltinWrites name offsets bitEns wF wT dT = concatMap makeReref offsets
        where
            makeReref offset = singleGateParam "tkr_builtin_var_write_reref"
                [GateParamString name, GateParamInt width, GateParamInt offset]
                [bitEns, dT, wF, wT]
            width = connWidth dT

    handleBuiltinReads :: String -> [Int] -> [Int] -> [[GateConn]] -> [GateConn] -> [GateElem]
    handleBuiltinReads name builtinOffsets readOffsets readGos dT = concatMap makeReref $ zip readOffsets readGos
        where
            makeReref (offset, readGo)
                | offset `elem` builtinOffsets = singleGateParam "tkr_builtin_var_read_reref"
                    [GateParamString name, GateParamInt width, GateParamInt offset]
                    [dT, readGo]
                | otherwise = []
            width = connWidth dT

    connectReads :: String -> [Slice Int] -> [GateConn] -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    connectReads andGate slices inp gos outs = concatMap makeOutputGates $
        zip4 ([0..] :: [Int]) slices gos outs
        where
            makeOutputGates (_, slice, go, out)
                | not (isEmptySlice slice) =
                    gate andGate [out, connSlice slice inp, concat (dupEach (sliceWidth slice) [go])]
                | otherwise = []

    steerMatches :: String -> Slice Int -> [[Implicant]] -> [GateConn] -> [GateConn] -> [[GateConn]] -> [GateElem]
    steerMatches tempName slice impss inpF inpT sels = concatMap steerMatch $ zip3 ([0..] :: [Int]) impss sels
        where
            steerMatch (i, imps, sel) = concat [
                nets orImpName 1 impCount,
                gateSome "or" [One sel, Many (smashSplit (conn N orImpName 0 (0 +: impCount)))],
                concatMap (\(i, imp) -> gateSome "c" [One (conn N orImpName 0 (i +: 1)), Many
                    (implicantMatchConns imp)]) $ zip [0..] imps
                ]
                where
                    orImpName = tempName ++ show i
                    impCount = length imps
                    -- width = connWidth inpF

                    implicantMatchConns (Imp value dcs) = mapMaybe bitMatch [0..sliceWidth slice - 1]
                        where
                            bitMatch i
                                | not (testBit dcs i) = Just $ connSlice ((sliceOffset slice + i) +: 1)
                                    (if testBit value i then inpT else inpF)
                                | otherwise = Nothing

    oTermNeedsGo :: TeakOTerm -> Bool
    oTermNeedsGo (TeakOConstant {}) = True
    oTermNeedsGo (TeakOBuiltin {}) = True
    oTermNeedsGo _ = False

    minTerms2 :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    minTerms2 termName lF lT hF hT = concat [
        nets termName 1 4,
        gate "c" [conn N termName 0 (0 +: 4), concat [hF, hF, hT, hT], concat [lF, lT, lF, lT]] ]

    type DRBin = String -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]

    drBin :: [Int] -> [Int] -> DRBin
    drBin minTermsF minTermsT outName outF outT aF aT bF bT = concat [
        minTerms2 outName aF aT bF bT,
        gateSome "or" [One outF, Many (map minTerm minTermsF)],
        gateSome "or" [One outT, Many (map minTerm minTermsT)] ]
        where minTerm i = conn N outName 0 (i +: 1)

    drOr :: DRBin
    drOr = drBin [0] [1,2,3]

    drAnd :: DRBin
    drAnd = drBin [0,1,2] [3]

    drXor :: DRBin
    drXor = drBin [0,3] [1,2]

    {-
    drNor :: DRBin
    drNor = drBin [1,2,3] [0]

    drNand :: DRBin
    drNand = drBin [3] [0,1,2]
    -}

    drXnor :: DRBin
    drXnor = drBin [1,2] [0,3]

    drTree :: DRBin -> String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    drTree moduleFunc prefix outf outt inpf inpt = binModuleTree moduleFunc' prefix [outf,outt] [inpf,inpt]
        where
            moduleFunc' p [outf,outt] [lf,lt] [hf,ht] = moduleFunc p outf outt lf lt hf ht
            moduleFunc' _ _ _ _ = error "moduleFunc': bad args"

    -- binModuleTree : make a tree pair combining nth elements of `inputs' into nth elements of `outputs'
    --    using modules created by applying `moduleFunc'
    binModuleTree :: (String -> [[GateConn]] -> [[GateConn]] -> [[GateConn]] -> [GateElem]) ->
        String -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    binModuleTree moduleFunc prefix outputs inputs
        | inputWidth == 0 = error "binModuleTree: inputs must be wider than 0 bits"
        | inputWidth == 1 = concatMap connect $ zip outputs inputs
        | otherwise = concat [
            newOutputNets,
            nextRank,
            if odd then topBitAlias else [],
            concatMap (\i ->
                moduleFunc (prefix ++ show i)
                    (map (connSlice (i +: 1)) newOutputs)
                    (map (connSlice ((i * 2) +: 1)) inputs)
                    (map (connSlice ((1 + i * 2) +: 1)) inputs)
                ) [0..halfWidth-1] ]
        where
            (newOutputNets, newOutputs, nextRank) = if inputWidth == 2
                then ([], outputs, [])
                else (nets newOutputName count newOutputWidth, each count N newOutputName (0 +: newOutputWidth),
                    binModuleTree moduleFunc (prefix ++ "r") outputs newOutputs)

            topBitAlias = concatMap connect $ zip (map topBitOut newOutputs) (map topBitInp inputs)
            topBitOut = connSlice ((newOutputWidth - 1) +: 1)
            topBitInp = connSlice ((inputWidth - 1) +: 1)

            newOutputName = prefix ++ "o"
            newOutputWidth = inputWidth - halfWidth

            connect (out, inp) = gateSome "connect" [One inp, Many [out]]
            inputWidth = connWidth (head inputs)
            halfWidth = inputWidth `div` 2
            odd = inputWidth `mod` 2 == 1
            count = length inputs

    combineELG :: String -> [[GateConn]] -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    combineELG prefix [oeq,olt,ogt] [leq,llt,lgt] [heq,hlt,hgt] = concat [
        nets ltint 1 1,
        nets gtint 1 1,
        gate "c" [oeq, leq, heq],
        gate "c" [conn N ltint 0 (0 +: 1), llt, heq],
        gate "c" [conn N gtint 0 (0 +: 1), lgt, heq],
        gate "or" [olt, conn N ltint 0 (0 +: 1), hlt],
        gate "or" [ogt, conn N gtint 0 (0 +: 1), hgt] ]
        where
            ltint = prefix ++ "ltint"
            gtint = prefix ++ "gtint"
    combineELG _ _ _ _ = error "combineELG: can't happen"

    halfAdder0 :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    halfAdder0 prefix outF outT coF coT lF lT rF rT = concat [
        minTerms2 prefix lF lT rF rT,
        gate "or" [coF, minTerm 0, minTerm 1, minTerm 2],
        gate "or" [coT, minTerm 3],
        gate "or" [outF, minTerm 0, minTerm 3],
        gate "or" [outT, minTerm 1, minTerm 2] ]
        where minTerm i = conn N prefix 0 (i +: 1)

    halfAdder1 :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    halfAdder1 prefix outF outT coF coT lF lT rF rT = concat [
        minTerms2 prefix lF lT rF rT,
        gate "or" [coF, minTerm 0],
        gate "or" [coT, minTerm 1, minTerm 2, minTerm 3],
        gate "or" [outF, minTerm 1, minTerm 2],
        gate "or" [outT, minTerm 0, minTerm 3] ]
        where minTerm i = conn N prefix 0 (i +: 1)

    {-
    fullAdder prefix outF outT coF coT lF lT rF rT ciF ciT = concat [
        nets cintNameF 1 2,
        nets cintNameT 1 2,
        nets sintNameF 1 1,
        nets sintNameT 1 1,
        halfAdder0 (prefix ++ "ha0") sintF sintT (cintF 0) (cintT 0) lF lT rF rT,
        halfAdder0 (prefix ++ "ha1") outF outT (cintF 1) (cintT 1) sintF sintT ciF ciT,
        drOr (prefix ++ "cor") coF coT (cintF 0) (cintT 0) (cintF 1) (cintT 1) ]
        where
            cintNameF = prefix ++ "cintf"
            cintNameT = prefix ++ "cintt"
            cintF i = conn N cintNameF 0 i 1
            cintT i = conn N cintNameT 0 i 1
            sintNameF = prefix ++ "sintf"
            sintNameT = prefix ++ "sintt"
            sintF = conn N sintNameF 0 0 1
            sintT = conn N sintNameT 0 0 1
            -}

    fullAdder :: String -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateConn] -> [GateElem]
    fullAdder prefix outF outT coF coT lF lT rF rT ciF ciT = concat [
        nets min 1 8,
        gate "c" [conn N min 0 (0 +: 8),
            concat (concat (replicate 1 ((replicate 4 ciF) ++ (replicate 4 ciT)))),
            concat (concat (replicate 2 ((replicate 2 rF) ++ (replicate 2 rT)))),
            concat (concat (replicate 4 ((replicate 1 lF) ++ (replicate 1 lT)))) ],
        gate "or" [outF, conn N min 0 (0 +: 1), conn N min 0 (3 +: 1), conn N min 0 (5 +: 1), conn N min 0 (6 +: 1)],
        gate "or" [outT, conn N min 0 (1 +: 1), conn N min 0 (2 +: 1), conn N min 0 (4 +: 1), conn N min 0 (7 +: 1)],
        gate "ao222" [coT, lT, rT, lT, ciT, rT, ciT],
        gate "ao222" [coF, lF, rF, lF, ciF, rF, ciF] ]
        where min = prefix ++ "min"

    handleOBuiltin :: String -> String -> Int -> [TeakParam] -> [GateConn] ->
        [GateConn] -> [GateConn] -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    handleOBuiltin _ name _ params go outF outT inFs inTs = body name where
        body "String" = concat [
            singleGateParam "tkr_string" [GateParamString string] [go, outF, outT]
            ]
            -- where [ExprFuncActual _ (ValueExpr _ _ (StringValue string))] = params
            where [TeakParamString string] = params
        body "tWriteMessage" = concat [
            -- nets done 1 1,
            singleGate "tkr_print" [inFs !! 0, inTs !! 0] -- , conn N done 0 0 1]
            ]
        body "ToString" = concat [
            singleGateParam "tkr_to_string" [GateParamInt (widthOfType [] typ)] [inFs !! 0, inTs !! 0,
                outF, outT]
            ]
            -- where [TypeFuncActual typ] = params
            where [TeakParamType typ] = params
        body "NumberToString" = concat [
            singleGateParam "tkr_number_to_string" [GateParamInt (widthOfType [] typ)] [inFs !! 0, inTs !! 0,
                inFs !! 1, inTs !! 1,
                inFs !! 2, inTs !! 2,
                inFs !! 3, inTs !! 3,
                outF, outT]
            ]
            -- where [TypeFuncActual typ] = params
            where [TeakParamType typ] = params
        body "StringAppend" = concat [
            singleGate "tkr_string_append" [inFs !! 0, inTs !! 0, inFs !! 1, inTs !! 1, outF, outT]
            ]
        body "Chr" = concat [
            singleGate "tkr_chr" [inFs !! 0, inTs !! 0, outF, outT]
            ]
        body "BalsaSimulationStop" = concat [
            singleGate "tkr_stop" [go],
            gate "gnd" [outF],
            gate "gnd" [outT]
            ]
        body name = error $ "Don't recognise builtin " ++ name

        -- ack done = concat [
        --    gateSome "connect" [One go, Many (smashSplit outT)],
        --    gate "gnd" [outF] ]

    makeOTerms :: [(Int, TeakOTerm)] -> [GateConn] -> Maybe [GateConn] -> [[GateConn]] -> [[GateConn]] -> [GateElem]
    makeOTerms terms go done termF termT = concatMap makeOTerm terms
        where
            oneIndices width value = filter (testBit value) [0..width - 1]

            termIndexMapping = (0, 0) : zip (map fst terms) [1..]
            findTermIndex i
                | isNothing termPos = error $ "makeOTerms: bad term index " ++ show i
                | otherwise = fromJust $ termPos
                where termPos = lookup i termIndexMapping
            osliceConn termConns (i, slice) = connSlice slice (termConns !! findTermIndex i)

            makeOTerm (i, term) = case term of
                TeakOConstant width value -> concat [
                    gateSome "connect" [One go, Many (filterBits outT value)],
                    gateSome "gnd" [Many (filterBits outF value)],
                    gateSome "connect" [One go, Many (filterBits outF notValue)],
                    gateSome "gnd" [Many (filterBits outT notValue)] ]
                    where
                        filterBits conn value = map (smashSplit conn !!) (oneIndices width value)
                        notValue = (bit width) - (1 + value)
                TeakOAppend count slices -> concatMap connect (map (*slicesWidth) [0..count - 1])
                    where
                        slicesWidth = sum $ map oSliceWidth slices
                        connect offset = concat [
                            gateSome "connect" [One (concatMap (osliceConn termF) slices),
                                Many [connSlice (offset +: slicesWidth) outF]],
                            gateSome "connect" [One (concatMap (osliceConn termT) slices),
                                Many [connSlice (offset +: slicesWidth) outT]]
                            ]
                TeakOBuiltin name width params slices -> concat [
                    -- nets iComp iCount 1,
                    -- nets termGo 1 1,
                    -- gateSome "c" [One (conn N termGo 0 0 1), Many (each iCount N iComp 0 1), One go],
                    -- drCompletions ("comp" ++ show i) inF inT (each iCount N iComp 0 1),
                    handleOBuiltin ("b" ++ show i)
                        name width params {- (conn N termGo 0 0 1) -} go outF outT
                        (map (osliceConn termF) slices)
                        (map (osliceConn termT) slices),
                    -- FIXME, need to thread go/done
                    if isJust done
                        then gateSome "connect" [One go, Many [fromJust done]]
                        else []
                    ]
                TeakOp TeakOpAdd [l, r] -> add True l r
                TeakOp TeakOpSub [l, r] -> add False l r
                TeakOp TeakOpOr [l, r] -> bin drOr l r
                TeakOp TeakOpAnd [l, r] -> bin drAnd l r
                TeakOp TeakOpXor [l, r] -> bin drXor l r
                TeakOp TeakOpNot [r] -> concatMap (\i -> concat [
                    gateSome "connect" [One (connSlice (i +: 1) (osliceConn termF r)), Many [connSlice (i +: 1) outT]],
                    gateSome "connect" [One (connSlice (i +: 1) (osliceConn termT r)), Many [connSlice (i +: 1) outF]] ]
                    ) [0..width - 1]
                    where width = oSliceWidth r
                TeakOp op [l, r]
                    | op == TeakOpEqual -> concat [eqNets, bitEqs, drTree drAnd comb outF outT eqConnF eqConnT]
                    | op == TeakOpNotEqual -> concat [eqNets, bitEqs, drTree drAnd comb outT outF eqConnF eqConnT]
                    where
                        eqNets = nets eqF 1 width ++ nets eqT 1 width
                        bitEqs = binChooseOut eqConnF eqConnT drXnor l r
                        eqF = "xf" ++ show i
                        eqT = "xt" ++ show i
                        eqConnF = conn N eqF 0 (0 +: width)
                        eqConnT = conn N eqT 0 (0 +: width)
                        comb = "c" ++ show i
                        width = oSliceWidth r
                TeakOp op [l, r]
                    | op `elem` compares -> concat [
                        nets bitEqs 1 width,
                        nets bitGts 1 width,
                        nets bitLts 1 width,
                        nets outEq 1 1,
                        nets outGt 1 1,
                        nets outLt 1 1,
                        nets mt0 1 width,
                        nets mt3 1 width,
                        gate "c" [conn N mt0 0 (0 +: width), lConnF, rConnF],
                        gate "c" [conn N mt3 0 (0 +: width), lConnT, rConnT],
                        gate "c" [conn N bitLts 0 (0 +: width), lConnF, rConnT],
                        gate "c" [conn N bitGts 0 (0 +: width), lConnT, rConnF],
                        gate "or" [conn N bitEqs 0 (0 +: width), conn N mt0 0 (0 +: width), conn N mt3 0 (0 +: width)],
                        binModuleTree combineELG ("comb" ++ show i)
                            [outEqConn, outLtConn, outGtConn]
                            [conn N bitEqs 0 (0 +: width), conn N bitLts 0 (0 +: width), conn N bitGts 0 (0 +: width)],
                        case op of
                            TeakOpUnsignedGT -> concat [
                                gate "or" [outF, outLtConn, outEqConn],
                                gateSome "connect" [One outGtConn, Many [outT]] ]
                            TeakOpUnsignedGE -> concat [
                                gateSome "connect" [One outLtConn, Many [outF]],
                                gate "or" [outT, outGtConn, outEqConn] ]
                            _ -> error "FIXME signed comparisons"
                            -- FIXME, signed comparisons
                        ]
                    where
                        mt0 = "mt0_" ++ show i
                        mt3 = "mt3_" ++ show i
                        bitEqs = "eq"  ++ show i
                        bitLts = "lt"  ++ show i
                        bitGts = "gt"  ++ show i
                        outEq = "oeq"  ++ show i
                        outLt = "olt"  ++ show i
                        outGt = "ogt"  ++ show i
                        outEqConn = conn N outEq 0 (0 +: 1)
                        outLtConn = conn N outLt 0 (0 +: 1)
                        outGtConn = conn N outGt 0 (0 +: 1)
                        compares = [TeakOpUnsignedGT, TeakOpUnsignedGE]
                        lConnF = osliceConn termF l
                        lConnT = osliceConn termT l
                        rConnF = osliceConn termF r
                        rConnT = osliceConn termT r
                        width = oSliceWidth r
                TeakOMux spec (selSlice:slices) -> concat [
                    nets gintF c w,
                    nets gintT c w,
                    nets selcomp c 1,
                    nets sel c 1,
                    nets selg c 1,
                    nets icomplete 1 1,
                    nets scomplete 1 1,
                    drCompletions ("comp" ++ show i)
                        (map (osliceConn termF) slices)
                        (map (osliceConn termT) slices)
                        (each c N selcomp (0 +: 1)),
                    drCompletion ("dcomp" ++ show i) selF selT (conn N scomplete 0 (0 +: 1)),
                    gateSome "c" [One (conn N icomplete 0 (0 +: 1)),
                        Many ((conn N scomplete 0 (0 +: 1)):(each c N selcomp (0 +: 1)))],
                    gateSome "or" [One outF, Many (each c N gintF (0 +: w))],
                    gateSome "or" [One outT, Many (each c N gintT (0 +: w))],
                    gate "c2r1" [concat (each c N sel (0 +: 1)), concat (each c N selg (0 +: 1)),
                        concat (dupEach c [conn N icomplete 0 (0 +: 1)]), cReset],
                    -- FIXME, C elements, can use AND gates?
                    gate "c2r1" [concat (each c N gintF (0 +: w)),
                        concat (dupEach w (each c N sel (0 +: 1))), concatMap (osliceConn termF) slices, wcReset],
                    gate "c2r1" [concat (each c N gintT (0 +: w)),
                        concat (dupEach w (each c N sel (0 +: 1))), concatMap (osliceConn termT) slices, wcReset],
                    -- FIXME, don't do full completion on inputs to start with
                    -- steerMatches would need to generate true and complement to do this
                    steerMatches match (0 +: selWidth) spec selF selT (each c N selg (0 +: 1))
                    ]
                    where
                        gintF = "gfint" ++ show i
                        gintT = "gtint" ++ show i
                        c = length spec
                        w = oSliceWidth $ head slices
                        selWidth = oSliceWidth selSlice
                        sel = "sel" ++ show i
                        selg = "selg" ++ show i
                        selcomp = "selcomp" ++ show i
                        icomplete = "icomplete" ++ show i
                        scomplete = "scomplete" ++ show i
                        selF = osliceConn termF selSlice
                        selT = osliceConn termT selSlice
                        match = "match" ++ show i
                        wcReset = concat $ dupEach (w * c) [reset]
                        cReset = concat $ dupEach c [reset]
                op -> error $ "Gen.hs.makeOTerms: FIXME Unhandled TeakOp " ++ show op ++ " " ++ showNameOTerm op
                where
                    binChooseOut outF outT opFunc l r = concat [
                        concatMap (\i -> opFunc (op ++ show i)
                            (connSlice (i +: 1) outF) (connSlice (i +: 1) outT)
                            (connSlice (i +: 1) lConnF) (connSlice (i +: 1) lConnT)
                            (connSlice (i +: 1) rConnF) (connSlice (i +: 1) rConnT)
                            ) [0..width - 1] ]
                        where
                            op = "op" ++ show i ++ "_"
                            lConnF = osliceConn termF l
                            lConnT = osliceConn termT l
                            rConnF = osliceConn termF r
                            rConnT = osliceConn termT r
                            width = oSliceWidth l

                    bin = binChooseOut outF outT

                    add addNsub l r = concat [
                        nets cf 1 width,
                        nets ct 1 width,
                        (if addNsub then halfAdder0 else halfAdder1) ha
                            (connSlice (0 +: 1) outF) (connSlice (0 +: 1) outT)
                            (conn N cf 0 (0 +: 1)) (conn N ct 0 (0 +: 1))
                            (connSlice (0 +: 1) lConnF) (connSlice (0 +: 1) lConnT)
                            (rF 0) (rT 0),
                        concatMap (\i -> fullAdder (fa ++ show i)
                            (connSlice (i +: 1) outF) (connSlice (i +: 1) outT)
                            (conn N cf 0 (i +: 1)) (conn N ct 0 (i +: 1))
                            (connSlice (i +: 1) lConnF) (connSlice (i +: 1) lConnT)
                            (rF i) (rT i)
                            (conn N cf 0 ((i - 1) +: 1)) (conn N ct 0 ((i - 1) +: 1))
                            ) [1..width - 1] ]
                        where
                            fa = "fa" ++ show i ++ "_"
                            ha = "ha" ++ show i ++ "_"
                            cf = "cf" ++ show i ++ "_"
                            ct = "ct" ++ show i ++ "_"
                            lConnF = osliceConn termF l
                            lConnT = osliceConn termT l
                            rConnF = osliceConn termF r
                            rConnT = osliceConn termT r
                            width = oSliceWidth l

                            rF i = connSlice (i +: 1) $ if addNsub then rConnF else rConnT
                            rT i = connSlice (i +: 1) $ if addNsub then rConnT else rConnF
                    outF = termF !! (findTermIndex i)
                    outT = termT !! (findTermIndex i)

    genMake :: TeakCompType -> [Some Int] -> GateNetlist
    genMake typ@TeakJ widths@[Many inWidths, One outWidth]
        | outWidth /= (sum inWidths) = error $ "J: bad widths: " ++ show widths
        -- Just token links
        | outWidth == 0 = netlist [
            gateSome "c" [One (conn R "o" 0 (0 +: 1)), Many (each c R "i" (0 +: 1))],
            gateSome "connect" [One (conn A "o" 0 (0 +: 1)), Many (each c A "i" (0 +: 1))] ]
        -- Single input, just flow through
        | inWidths == [outWidth] = netlist [
            gateSome "connect" [One (conn R0 "i" 0 (0 +: 1)), One (conn R0 "o" 0 (0 +: 1))],
            gateSome "connect" [One (conn R1 "i" 0 (0 +: 1)), One (conn R1 "o" 0 (0 +: 1))],
            gateSome "connect" [One (conn A "o" 0 (0 +: 1)), Many (each c A "i" (0 +: 1))]
            ]
        -- Must be at least two inputs and go bottom output bit needs guarding
        | otherwise = netlist [
            nets "icomplete" 1 1,
            nets "joinf" 1 outWidth,
            nets "joint" 1 outWidth,
            gateSome "connect" [One (concat (bundlesAtIndicesW nonZeroIndices R0 "i" 0 inWidths)),
                Many [conn N "joinf" 0 (0 +: outWidth)]],
            gateSome "connect" [One (concat (bundlesAtIndicesW nonZeroIndices R1 "i" 0 inWidths)),
                Many [conn N "joint" 0 (0 +: outWidth)]],
            -- Guard with only token inputs, or from data
            if nonZeroCount >= 2
                then concat [
                    nets "dcomplete" (nonZeroCount - 1) 1,
                    gateSome "or" [
                        One (concat (each (nonZeroCount - 1) N "dcomplete" (0 +: 1))),
                        One (concat (eachIW R0 "i" 0 (tail nonZeroIndices) (repeat 1))),
                        One (concat (eachIW R1 "i" 0 (tail nonZeroIndices) (repeat 1))) ],
                    gateSome "c" [One icomplete, Many zeroRequests,
                        Many (each (nonZeroCount - 1) N "dcomplete" (0 +: 1))]
                    ]
                else -- Only token guards.  There must be at least one here
                    gateSome "c" [One icomplete, Many zeroRequests],
            gate "c" [conn R0 "o" 0 (0 +: 1), conn N "joinf" 0 (0 +: 1), icomplete],
            gate "c" [conn R1 "o" 0 (0 +: 1), conn N "joint" 0 (0 +: 1), icomplete],
            if outWidth > 1
                then concat [
                    gateSome "connect" [One (conn N "joinf" 0 (1 +: (outWidth - 1))),
                        Many [conn R0 "o" 0 (1 +: (outWidth - 1))]],
                    gateSome "connect" [One (conn N "joint" 0 (1 +: (outWidth - 1))),
                        Many [conn R1 "o" 0 (1 +: (outWidth - 1))]]
                    ]
                else [],
            gateSome "connect" [One (conn A "o" 0 (0 +: 1)), Many (each c A "i" (0 +: 1))] ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            c = length inWidths
            icomplete = conn N "icomplete" 0 (0 +: 1)
            ports = mkPorts "i" Input c inWidths ++ mkPorts "o" Output 1 [outWidth]
            zeroIndices = findIndices (== 0) inWidths
            nonZeroIndices = findIndices (/= 0) inWidths
            zeroRequests = bundlesAtIndices zeroIndices R "i" (0 +: 1)
            nonZeroCount = length nonZeroIndices

    genMake typ@TeakM widths@[Many inWidths, One outWidth]
        | any (/= w) inWidths = error $ "M: bad widths: " ++ show widths
        | w == 0 = netlist [
            nets "nchosen" 1 1,
            nets "choice" c 1,
            gate "c2r1" [concat (each c N "choice" (0 +: 1)), concat (each c R "i" (0 +: 1)), -- 1
                dupC (conn N "nchosen" 0 (0 +: 1)), dupC reset],
            gate "nor" [conn N "nchosen" 0 (0 +: 1), conn R "o" 0 (0 +: 1), conn A "o" 0 (0 +: 1)], -- 2
            gateSome "or" [One $ conn R "o" 0 (0 +: 1), Many $ each c N "choice" (0 +: 1)], -- 3
            gate "c2r1" [concat (each c A "i" (0 +: 1)), concat (each c N "choice" (0 +: 1)), -- 4
                dupC (conn A "o" 0 (0 +: 1)), dupC reset] ]
        | otherwise = netlist [
            nets "gfint" c w,
            nets "gtint" c w,
            nets "choice" c 1,
            nets "anychoice" 1 1,
            nets "icomp" c 1,
            nets "nchosen" 1 1,
            gateSome "or" [One (conn R0 "o" 0 (0 +: w)), Many (each c N "gfint" (0 +: w))], -- 1
            gateSome "or" [One (conn R1 "o" 0 (0 +: w)), Many (each c N "gtint" (0 +: w))], -- 2
            gate "and" [concat (each c N "gtint" (0 +: w)), -- 3
                concat (dupEach w (each c N "choice" (0 +: 1))), concat (each c R1 "i" (0 +: w))],
            gate "and" [concat (each c N "gfint" (0 +: w)), -- 4
                concat (dupEach w (each c N "choice" (0 +: 1))), concat (each c R0 "i" (0 +: w))],
            drCompletions "comp" (each c R0 "i" (0 +: w)) (each c R1 "i" (0 +: w)) (each c N "icomp" (0 +: 1)), -- 5
            gate "c2r1" [concat (each c N "choice" (0 +: 1)), concat (each c N "icomp" (0 +: 1)), -- 6
                dupC (conn N "nchosen" 0 (0 +: 1)), dupC reset],
            gateSome "or" [One (conn N "anychoice" 0 (0 +: 1)), Many (each c N "choice" (0 +: 1))], -- 7
            gate "nor" [conn N "nchosen" 0 (0 +: 1), conn N "anychoice" 0 (0 +: 1), conn A "o" 0 (0 +: 1)], -- 8
            gate "c2r1" [concat (each c A "i" (0 +: 1)), concat (each c N "choice" (0 +: 1)), -- 9
                dupC (conn A "o" 0 (0 +: 1)), dupC reset] ]
        where
            w = outWidth
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            c = length inWidths
            ports = mkPorts "i" Input c inWidths ++ mkPorts "o" Output 1 [outWidth]
            dupC conn = concat (dupEach c [conn])

    -- FIXME, need to make sure that input is complete before allowing outputs to become complete
    -- use bottom bit as usual?  Carry bottom bit complete to lowest output bit for each output
    -- which isn't 0-index-based
    genMake typ@(TeakF offsets) widths@[One inWidth, Many outWidths]
        | any (> inWidth) (map (uncurry (+)) $ zip outWidths offsets) = error $ "F: bad widths: " ++ show widths
        | inWidth == 0 = netlist [
            gateSome "connect" [One (conn R "i" 0 (0 +: 1)), Many (each c R "o" (0 +: 1))],
            gateSome "c" [One (conn A "i" 0 (0 +: 1)), Many (each c A "o" (0 +: 1))] ]
        | otherwise = netlist [
            nets "acomplete" 1 1,
            nets "icomplete" 1 1,
            if strict
                then concat [
                    drCompletion "comp"
                        (conn R0 "i" 0 (0 +: inWidth))
                        (conn R1 "i" 0 (0 +: inWidth))
                        icomplete,
                    gateSome "connect" [One icomplete, One acomplete]
                    ]
                else concat [
                    gate "or" [ icomplete, conn R0 "i" 0 (0 +: 1), conn R1 "i" 0 (0 +: 1) ],
                    if null unusedSlices
                        then gateSome "connect" [One icomplete, One acomplete]
                        else concat [
                            nets "ucomplete" 1 1,
                            drCompletion "comp"
                                (concatMap (conn R0 "i" 0) unusedSlices)
                                (concatMap (conn R1 "i" 0) unusedSlices)
                                ucomplete,
                            gate "c" [acomplete, ucomplete, icomplete]
                            ]
                    ],
            connectBundleSlices slices icomplete (conn R0 "i" 0 (0 +: inWidth)) R0 "o",
            connectBundleSlices slices icomplete (conn R1 "i" 0 (0 +: inWidth)) R1 "o",
            gateSome "connect" [One icomplete, Many (bundlesAtIndices zeroIndices R "o" (0 +: 1))],
            gateSome "c" [One (conn A "i" 0 (0 +: 1)), One acomplete, Many (each c A "o" (0 +: 1))] ]
        where
            strict = False -- FIXME, this must be an option/style
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            c = length outWidths
            icomplete = conn N "icomplete" 0 (0 +: 1)
            acomplete = conn N "acomplete" 0 (0 +: 1)
            ucomplete = conn N "ucomplete" 0 (0 +: 1)
            ports = mkPorts "i" Input 1 [inWidth] ++ mkPorts "o" Output c outWidths
            slices = zipWith (+:) offsets outWidths
            zeroIndices = findIndices ((== 0) . sliceWidth) slices
            usedBitmask = foldl' (.|.) (0 :: Integer) $ map sliceToBitmask slices
            unusedSlices = bitmaskToIntervals (bitNot inWidth usedBitmask :: Integer)

    genMake typ@(TeakS selSlice specs) widths@[One inWidth, Many outWidths]
        | any (> inWidth) outWidths = error $ "S: bad widths: " ++ show widths
        | otherwise = netlist [
            nets "icomplete" 1 1,
            nets "sel" c 1,
            nets "gsel" c 1,
            nets "oack" 1 1,
            steerMatches "match" selSlice impss
                (conn R0 "i" 0 (0 +: inWidth)) (conn R1 "i" 0 (0 +: inWidth)) (each c N "sel" (0 +: 1)),
            gate "c" [concat (each c N "gsel" (0 +: 1)), concat (each c N "sel" (0 +: 1)),
                concat (dupEach c [conn N "icomplete" 0 (0 +: 1)])],
            drCompletion "comp" (conn R0 "i" 0 (0 +: inWidth)) (conn R1 "i" 0 (0 +: inWidth))
                (conn N "icomplete" 0 (0 +: 1)),
            connectReads "c" outSlices (conn R0 "i" 0 (0 +: inWidth)) (each c N "gsel" (0 +: 1))
                (eachW c R0 "o" 0 outWidths),
            connectReads "c" outSlices (conn R1 "i" 0 (0 +: inWidth)) (each c N "gsel" (0 +: 1))
                (eachW c R1 "o" 0 outWidths),
            gateSome "connect" [One (concat (bundlesAtIndices zeroWidthIndices N "gsel" (0 +: 1))),
                Many [concat (bundlesAtIndices zeroWidthIndices R "o" (0 +: 1))]],
            gateSome "or" [One (conn N "oack" 0 (0 +: 1)), Many (each c A "o" (0 +: 1))],
            gate "c" [conn A "i" 0 (0 +: 1), conn N "oack" 0 (0 +: 1), conn N "icomplete" 0 (0 +: 1)]
            ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            c = length outWidths
            ports = mkPorts "i" Input 1 [inWidth] ++ mkPorts "o" Output c outWidths
            (impss, outOffsets) = unzip specs
            outSlices = zipWith (+:) outOffsets outWidths
            zeroWidthIndices = findIndices (== 0) outWidths

    genMake typ@(TeakO terms) widths@[One inWidth, One outWidth] = netlist [
            if needGo && inWidth /= 0 then concat [
                nets "go" 1 1,
                -- gate "or" [conn N "go" 0 0 1, conn R0 "i" 0 0 1, conn R1 "i" 0 0 1]
                drCompletion "gocomp" (conn R0 "i" 0 (0 +: inWidth)) (conn R1 "i" 0 (0 +: inWidth))
                    (conn N "go" 0 (0 +: 1))
            ] else [],
            netsIW "termf" termIndices termWidths,
            netsIW "termt" termIndices termWidths,
            -- FIXME, zero width in/out
            makeOTerms terms go (if outWidth == 0 then Just (conn R "o" 0 (0 +: 1)) else Nothing)
                (conn R0 "i" 0 (0 +: inWidth) :
                    eachIW N "termf" 0 termIndices termWidths ++ [conn R0 "o" 0 (0 +: outWidth)])
                (conn R1 "i" 0 (0 +: inWidth) :
                    eachIW N "termt" 0 termIndices termWidths ++ [conn R1 "o" 0 (0 +: outWidth)]),
            gateSome "connect" [One (conn A "o" 0 (0 +: 1)), One (conn A "i" 0 (0 +: 1))]
        ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            go = if inWidth /= 0 then conn N "go" 0 (0 +: 1) else conn R "i" 0 (0 +: 1)
            needGo = or (map (oTermNeedsGo . snd) terms)
            termWidths = map (oTermResultWidth . snd) $ init terms
            termIndices = map fst $ init terms
            ports = mkPorts "i" Input 1 [inWidth] ++ mkPorts "o" Output 1 [outWidth]

    genMake typ@(TeakV name width bs ws rs) widths@[Many wgWidths, Many wdWidths, Many rgWidths, Many rdWidths]
        | any (/= 0) wdWidths || any (/= 0) rgWidths ||
            any (> width) wgWidths || any (> width) rdWidths ||
            length wgWidths /= length wdWidths || length rgWidths /= length rdWidths ||
            any (>= width) ws || any (>= width) rs =
                error $ "V: bad widths: " ++ show widths
        | otherwise = netlist [
            -- nets "reset" 1 1,
            nets "wf" 1 width,
            nets "wt" 1 width,
            nets "df" 1 width,
            nets "dt" 1 width,
            nets "wc" wc 1,
            nets "wacks" 1 width,
            nets "wenr" 1 width,
            nets "wen" 1 width,
            nets "anyread" 1 1,
            nets "nreset" 1 1,
            gateSome "inv" [One (conn N "nreset" 0 (0 +: 1)), One reset],
            gateSome "and" [One (conn N "wen" 0 (0 +: width)), Many [conn N "wenr" 0 (0 +: width),
                concat (dupEach width [conn N "nreset" 0 (0 +: 1)])]],
            drLatch "drl" (conn N "wf" 0 (0 +: width)) (conn N "wt" 0 (0 +: width))
                (conn N "wen" 0 (0 +: width)) (conn N "wacks" 0 (0 +: width))
                (conn N "df" 0 (0 +: width)) (conn N "dt" 0 (0 +: width))
                reset,
            drCompletions "comp" (eachW wc R0 "wg" 0 wgWidths) (eachW wc R1 "wg" 0 wgWidths)
                (each wc N "wc" (0 +: 1)),
            connectWrites "conw" ws
                (eachW wc R0 "wg" 0 wgWidths) (eachW wc R1 "wg" 0 wgWidths)
                (conn N "wf" 0 (0 +: width)) (conn N "wt" 0 (0 +: width))
                (conn N "anyread" 0 (0 +: 1))
                (each wc R "wd" (0 +: 1))
                (each wc N "wc" (0 +: 1))
                (conn N "wenr" 0 (0 +: width))
                (conn N "wacks" 0 (0 +: width)),
            handleBuiltinWrites name bs (conn N "wen" 0 (0 +: width))
                (conn N "wf" 0 (0 +: width)) (conn N "wt" 0 (0 +: width))
                (conn N "dt" 0 (0 +: width)),
            handleBuiltinReads name bs rs (each rc R "rg" (0 +: 1)) (conn N "dt" 0 (0 +: width)),
            connectReads "and" readSlices (conn N "df" 0 (0 +: width))
                (each rc R "rg" (0 +: 1)) (eachW rc R0 "rd" 0 rdWidths),
            connectReads "and" readSlices (conn N "dt" 0 (0 +: width))
                (each rc R "rg" (0 +: 1)) (eachW rc R1 "rd" 0 rdWidths),
            gateSome "or" [One (conn N "anyread" 0 (0 +: 1)), Many (each rc R "rg" (0 +: 1)),
                Many (each rc A "rg" (0 +: 1))],
            gateSome "connect" [One (concat (each wc A "wd" (0 +: 1))), Many [concat (each wc A "wg" (0 +: 1))]],
            gateSome "connect" [One (concat (each rc A "rd" (0 +: 1))), Many [concat (each rc A "rg" (0 +: 1))]]
            ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            wc = length wgWidths
            rc = length rdWidths
            readSlices = zipWith (+:) rs rdWidths
            ports = mkPorts "wg" Input wc wgWidths ++ mkPorts "wd" Output wc (repeat 0) ++
                mkPorts "rg" Input rc (repeat 0) ++ mkPorts "rd" Output rc rdWidths

    genMake typ@TeakA widths@[Many inWidths, One outWidth]
        | any (/= outWidth) inWidths = error $ "A: bad widths: " ++ show widths
        | c /= 2 = error $ "A: must be exactly two inputs: " ++ show widths
        | outWidth == 0 = netlist [
            nets "sel" 1 c,
            gateSome "or" [One (conn R "o" 0 (0 +: 1)), Many (smashSplit (conn N "sel" 0 (0 +: c)))],
            gateSome "connect" [One (concat (each c R "i" (0 +: 1))), Many [conn N "sel" 0 (0 +: c)]],
            gate "c2r1" [concat (each c A "i" (0 +: 1)), concat (each c N "sel" (0 +: 1)),
                concat (each c A "o" (0 +: 1)), concat (dupEach c [reset])] ]
        | otherwise = netlist [
            nets "sel" 2 1,
            nets "gsel" 2 1,
            nets "nia" 2 1,
            nets "selcomp" c 1,
            nets "gfint" c outWidth,
            nets "gtint" c outWidth,

            -- input completion
            drCompletions "comp" (each c R0 "i" (0 +: outWidth)) (each c R1 "i" (0 +: outWidth))
                (each c N "selcomp" (0 +: 1)),
            -- generate sels
            gate "inv" [concat (each c N "nia" (0 +: 1)), concat (each c A "i" (0 +: 1))],
            gate "and" [concat (each c N "sel" (0 +: 1)), concat (reverse (each c N "nia" (0 +: 1))),
                concat (each c N "gsel" (0 +: 1))],
            gate "mutex" [conn N "selcomp" 0 (0 +: 1), conn N "selcomp" 1 (0 +: 1),
                conn N "gsel" 0 (0 +: 1), conn N "gsel" 1 (0 +: 1)],
            -- multiplexing
            gateSome "or" [One (conn R0 "o" 0 (0 +: outWidth)), Many (each c N "gfint" (0 +: outWidth))],
            gateSome "or" [One (conn R1 "o" 0 (0 +: outWidth)), Many (each c N "gtint" (0 +: outWidth))],
            gate "and" [concat (each c N "gtint" (0 +: outWidth)),
                concat (dupEach outWidth (each c N "sel" (0 +: 1))), concat (each c R1 "i" (0 +: outWidth))],
            gate "and" [concat (each c N "gfint" (0 +: outWidth)),
                concat (dupEach outWidth (each c N "sel" (0 +: 1))), concat (each c R0 "i" (0 +: outWidth))],
            -- ack steering
            gate "c2r1" [concat (each c A "i" (0 +: 1)), concat (each c N "sel" (0 +: 1)),
                concat (dupEach c [conn A "o" 0 (0 +: 1)]), concat (dupEach c [reset])] ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            c = length inWidths
            ports = mkPorts "i" Input c inWidths ++ mkPorts "o" Output 1 [outWidth]

    genMake typ@TeakI widths = netlist [
        nets "nreset" 1 1,
        nets "firsthsa" 1 1,
        nets "nfirsthsa" 1 1,
        nets "firsthsd" 1 1,
        nets "noa" 1 1,
        gate "inv" [conn N "nreset" 0 (0 +: 1), conn G "reset" 0 (0 +: 1)],
        gate "inv" [conn N "nfirsthsa" 0 (0 +: 1), conn N "firsthsa" 0 (0 +: 1)],
        gate "inv" [conn N "noa" 0 (0 +: 1), conn A "o" 0 (0 +: 1)],
        gate "ao22" [conn R "o" 0 (0 +: 1), conn N "nreset" 0 (0 +: 1),
            conn N "nfirsthsa" 0 (0 +: 1), conn R "i" 0 (0 +: 1), conn N "firsthsd" 0 (0 +: 1)],
        gate "c1u1" [conn N "firsthsa" 0 (0 +: 1), conn N "nreset" 0 (0 +: 1), conn A "o" 0 (0 +: 1)],
        gate "c1u1" [conn N "firsthsd" 0 (0 +: 1), conn N "firsthsa" 0 (0 +: 1), conn N "noa" 0 (0 +: 1)],
        gate "and" [conn A "i" 0 (0 +: 1), conn A "o" 0 (0 +: 1), conn N "firsthsd" 0 (0 +: 1)] ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            ports = mkPorts "i" Input 1 [0] ++ mkPorts "o" Output 1 [0]

    genMake typ@TeakR widths = netlist [
        nets "fb1" 1 1,
        nets "fb2" 1 1,
        gate "nor" [conn N "fb1" 0 (0 +: 1), conn G "reset" 0 (0 +: 1), conn N "fb2" 0 (0 +: 1)],
        gate "nor" [conn N "fb2" 0 (0 +: 1), conn A "o" 0 (0 +: 1), conn N "fb1" 0 (0 +: 1)],
        gate "nor" [conn R "o" 0 (0 +: 1), conn G "reset" 0 (0 +: 1), conn N "fb1" 0 (0 +: 1)] ]
        where
            netlist = GateNetlist (genTeakName typ widths) ports [] . concat
            ports = mkPorts "o" Output 1 [0]

    genMake _ _ = error "genMake: can't happen"

    latchName :: Int -> Int -> String
    latchName width depth = "tkl" ++ show width ++ "x" ++ show depth

    makeLatch :: TechMapping -> Int -> Int -> GateNetlist
    makeLatch mapping width depth = addGlobalPorts $ genTechMap mapping $ body width
        where
            body 0 = netlist [
                pipeLatch0N "b" depth (conn R "i" 0 (0 +: 1)) (conn A "i" 0 (0 +: 1))
                    (conn R "o" 0 (0 +: 1)) (conn A "o" 0 (0 +: 1)) ]
            body _ = netlist [
                pipeLatchN "b" depth (conn R0 "i" 0 (0 +: width)) (conn R1 "i" 0 (0 +: width)) (conn A "i" 0 (0 +: 1))
                    (conn R0 "o" 0 (0 +: width)) (conn R1 "o" 0 (0 +: width)) (conn A "o" 0 (0 +: 1)) ]

            netlist = GateNetlist (latchName width depth) ports [] . concat
            ports = mkPorts "i" Input 1 [width] ++ mkPorts "o" Output 1 [width]

    nwEscapeName :: String -> String
    nwEscapeName name = concatMap escChar name
        where
            escChar '[' = ['_']
            escChar ']' = []
            escChar chr
                | isAlphaNum chr = [chr]
                | otherwise = []

    genShowImp :: Implicant -> String
    genShowImp (Imp value 0) = showHex value ""
    genShowImp (Imp value dcs) = showHex value "" ++ "c" ++ showHex dcs ""

    showNameOTerm :: TeakOTerm -> String
    showNameOTerm (TeakOConstant width value) = "nm" ++ show width ++ "b" ++ showHex value ""
    showNameOTerm (TeakOAppend 1 slices) = "ap" ++ concatMap showNameOSlice slices
    showNameOTerm (TeakOp op slices) = fst (teakOOpNames op) ++ concatMap showNameOSlice slices
    showNameOTerm (TeakOAppend count slices) = "ap" ++ show count ++ "x" ++ concatMap showNameOSlice slices
    showNameOTerm (TeakOBuiltin name width params slices) = "bi" ++ name ++ "_" ++ show width ++
        "_" ++ nwEscapeName (show params) ++ "_" ++ concatMap showNameOSlice slices
    showNameOTerm (TeakOMux spec slices) = "mx" ++
        joinWith "_" (map (joinWith "o" . map showNameImp) spec) ++ "_" ++ concatMap showNameOSlice slices
    -- showNameOTerm term = nwEscapeName (show term)

    showNameImp :: Implicant -> String
    showNameImp (Imp val 0) = show val
    showNameImp (Imp val dcs) = show val ++ "m" ++ show dcs

    showNameOSlice :: TeakOSlice -> String
    showNameOSlice (0, slice) = "i" ++ show (sliceOffset slice) ++ "w" ++ show (sliceWidth slice) ++ "b"
    showNameOSlice (term, slice) = "t" ++ show term ++ "o" ++ show (sliceOffset slice) ++
        "w" ++ show (sliceWidth slice) ++ "b"

    showSlice :: Slice Int -> String
    showSlice slice = "o" ++ show (sliceOffset slice) ++ "w" ++ show (sliceWidth slice)

    genTeakName :: TeakCompType -> [Some Int] -> String
    genTeakName TeakJ [Many inWidths, One outWidth] =
        "tkj" ++ show outWidth ++ "m" ++ joinWith "_" (map show inWidths)
    genTeakName TeakM [Many inWidths, One outWidth] =
        "tkm" ++ show c ++ "x" ++ show outWidth ++ "b"
        where c = length inWidths
    genTeakName (TeakF offsets) [One inWidth, Many outWidths] =
        "tkf" ++ show inWidth ++ "m" ++ joinWith "_" (map showSlice (zipWith (+:) offsets outWidths))
    genTeakName (TeakS selSlice specs) [One inWidth, Many outWidths] =
        "tks" ++ show inWidth ++ "_" ++ showSlice selSlice ++ "_" ++
        joinWith "_" (map showSpec (zip specs outWidths))
        where showSpec ((imps, offset), width) = joinWith "m" (map genShowImp imps) ++ showSlice (offset +: width)
    genTeakName (TeakO terms) [One inWidth, One outWidth] =
        "tko" ++ show inWidth ++ "m" ++ show outWidth ++
            concatMap (\(i, term) -> "_" ++ show i ++ showNameOTerm term) terms
    genTeakName (TeakV vName width bs ws rs) [Many wgWidths, Many _, Many _, Many rdWidths] =
        "tkv" ++ name ++ show width ++ (if (null bs)
            then ""
            else "_b" ++ concatMap show bs) ++
            "_w" ++ concatMap showSlice (zipWith (+:) ws wgWidths) ++
            "_r" ++ concatMap showSlice (zipWith (+:) rs rdWidths)
        where name = nwEscapeName vName
    genTeakName TeakA [Many inWidths, One outWidth] =
        "tka" ++ show c ++ "x" ++ show outWidth ++ "b"
        where c = length inWidths
    genTeakName TeakI _ = "tki"
    genTeakName TeakR _ = "tkr"
    genTeakName _ _ = error "genTeakName: can't happen"

    treeGates :: [String]
    treeGates = ["and", "or", "nand", "nor"]
    nonInvTreeGates :: [String]
    nonInvTreeGates = ["c", "nc"]

    type TechMapping = String -> GateElem -> [GateElem]

    genToSimpleGates :: String -> GateElem -> [GateElem]
    genToSimpleGates _ (GateInstance "connect" (from:tos)) = map makeGate tos
        where makeGate to = GateInstance "buff" [to, from]
    genToSimpleGates _ (GateInstance "gnd" tos) = map makeGate tos
        where makeGate to = GateInstance "gnd" [to]
    genToSimpleGates prefix (GateInstance name (to:froms))
        | name `elem` treeGates = makeGateTree prefix name 3 True to (concat froms)
        | name `elem` nonInvTreeGates = makeGateTree prefix name 3 False to (concat froms)
    genToSimpleGates _ elem = [elem]

    globalPorts :: [GatePort]
    globalPorts = [GatePort "reset" Input 1]
    globalConns :: [[GateConn]]
    globalConns = [reset]

    genTechMap :: TechMapping -> GateNetlist -> GateNetlist
    genTechMap mapping (GateNetlist name ports props elems) = GateNetlist name ports props simpleElems
        where
            n0 = [0..] :: [Int]
            simpleElems = concatMap makeTech $ zip n0 $ concatMap makeSimple $ zip n0 elems
            makeTech (i, elem) = mapping ("tech" ++ show i ++ "_") elem
            makeSimple (i, elem) = genToSimpleGates ("simp" ++ show i) elem

    addGlobalPorts :: GateNetlist -> GateNetlist
    addGlobalPorts (GateNetlist name ports props elems) = GateNetlist name (ports ++ globalPorts) props elems

    genMakeComp :: TechMapping -> TeakCompType -> [Some Int] -> GateNetlist
    genMakeComp mapping typ widths = addGlobalPorts $ genTechMap mapping $ genMake typ widths

    data GenPartToGateOption = GenPartToGateProtocolTest
        deriving (Show, Eq)

    genPartToGateNetlist :: NetworkIF network => [GenPartToGateOption] -> Part network ->
        (GateNetlist, [(String, TeakCompType, [Some Int])], [(Int, Int)])
    genPartToGateNetlist options (Part partName ports body) = tryNetwork body $ do
        (names, gateNetss, netCompss, latchesToMakes) <- liftM unzip4 $ nwMapLinks makeLink
        let linkNames = DM.fromList names
        (gateComps, modulesToMake) <- liftM unzip $ nwMapComps $ makeComp linkNames
        -- gateNetss <- nwMapLinks (makeNets linkNames)
        testComps <- if GenPartToGateProtocolTest `elem` options
            then liftM concat $ nwMapLinks $ makeProtocolTester linkNames
            else return []
        let
            netlist = GateNetlist gateName
                (gatePorts ++ globalPorts)
                []
                -- (portAliasComps ++ concat gateNetss ++ gateComps ++ testComps)
                (concat gateNetss ++ gateComps ++ concat netCompss ++ testComps)
        return (netlist, catMaybes modulesToMake, nub $ concat latchesToMakes)
        where
            makeLink link = do
                let ref = refLink link
                Just pas <- nwGetLinkUsage Passive ref
                Just act <- nwGetLinkUsage Active ref
                width <- nwGetLinkWidth ref
                depth <- liftM latchingDepth $ nwGetLinkLatching ref

                let
                    defName = defaultLinkName ref
                    nets name = map makeNet $ fullBundles name width 0
                    conn name = map makeConn $ fullBundles name width 0
                    portName accessRef = nwEscapeName $ nwPortName port
                        where Just port = nwFindPortByRef ports accessRef
                    latch from to = ([GateInstance name (conn from ++ conn to ++ globalConns)],
                        [(width, depth)])
                        where name = latchName width depth
                    alias from to = (concat [
                        concatMap alias $ zip forwardFrom forwardTo,
                        concatMap alias $ zip reverseTo reverseFrom ], [])
                        where
                            (forwardFrom, reverseFrom) = partition isForward $ fullBundles from width 0
                            (forwardTo, reverseTo) = partition isForward $ fullBundles to width 0

                            isForward (_, _, Forward) = True
                            isForward _ = False

                            alias ((fromName, width, _), (toName, _, _)) = gateSome "connect"
                                [One [GateConn fromName (0 +: width)], Many [[GateConn toName (0 +: width)]]]
                    noConnect _ _ = ([], [])

                    (pasName, actName, netDecls, connectFunc) = case (pas, act) of
                        (LinkComp {}, LinkComp {})
                            | depth == 0 -> (defName, defName, nets defName, noConnect)
                            | otherwise -> (pasDefName, actDefName, nets pasDefName ++ nets actDefName, latch)
                            where
                                pasDefName = defName ++ "P"
                                actDefName = defName ++ "A"
                        (LinkComp {}, LinkAccess from _)
                            | depth == 0 -> (fromPortName, fromPortName, [], noConnect)
                            | otherwise -> (defName, fromPortName, nets defName, latch)
                            where fromPortName = portName from
                        (LinkAccess to _, LinkComp {})
                            | depth == 0 -> (toPortName, toPortName, [], noConnect)
                            | otherwise -> (toPortName, defName, nets defName, latch)
                            where toPortName = portName to
                        (LinkAccess to _, LinkAccess from _) -> (toPortName, fromPortName, [],
                            if depth == 0 then alias else latch)
                            where
                                fromPortName = portName from
                                toPortName = portName to
                        _ -> (defName, defName, nets defName, noConnect)

                    (connectInsts, latchesReqd) = connectFunc actName pasName
                return ((ref, (pasName, actName)), netDecls, connectInsts, latchesReqd)

            -- (name, ports, nw) = netlistThingSplitPartNetlist part
            gateName = "teak_" ++ partName

            defaultLinkName link = show link

            makeProtocolTester linkNames link
                | pasName == actName = return [makeTester actName "L"]
                | otherwise = return [makeTester pasName "P", makeTester actName "A"]
                where
                    (pasName, actName) = linkNames DM.! (refLink link)

                    width = nwLinkWidth link

                    makeTester name end = case width of
                        0 -> GateInstanceParam "tkr_ra_monitor" [GateParamString site] conns
                        _ -> GateInstanceParam "tkr_dr_monitor" [GateParamString site, GateParamInt width] conns
                        where
                            conns = map makeConn $ fullBundles name width 0
                            site = partName ++ "." ++ defaultLinkName (refLink link) ++ "." ++ end

            makeNet (name, width, _) = GateNet name width

            makeComp linkNames comp = do
                let
                    links = nwCompLinks comp
                    senses = nwCompPortSenses comp
                    flatten (One link) sense = [(link, sense)]
                    flatten (Many links) sense = zip links $ repeat sense
                    flatten (Some links) sense = concat $ zipWith flatten links $ repeat sense

                let
                    flatLinks = concat $ zipWith flatten links senses
                linkWidths <- mapM nwGetLinkWidth $ map fst flatLinks
                let linkConns = concat $ zipWith (makeLinkConns linkNames) flatLinks linkWidths
                case comp of
                    TeakComp _ typ _ _ -> do
                        let
                            (Some someWidths, []) = mapOverSome (\_ w -> w) (Some links) linkWidths
                            name = genTeakName typ someWidths
                        return (GateInstance name (linkConns ++ globalConns), Just (name, typ, someWidths))
                    InstanceComp _ name _ _ _ -> do
                        return (GateInstance ("teak_" ++ name) (linkConns ++ globalConns), Nothing)

            makeLinkConns linkNames (link, sense) width = map makeConn $ fullBundles name width 0
                where
                    (pasName, actName) = linkNames DM.! link
                    name = case sense of
                        Passive -> pasName
                        Active -> actName

            makeConn (name, width, _) = [GateConn name (0 +: width)]

            gatePorts = concatMap makePort ports

            makePort (NetworkPort name dir width _) = mkPorts (nwEscapeName name) dir 1 [width]

            -- gateNets = concatMap makeChan

    -- simpleGateNames : map of all valid gate names that may be defined in mapping files with tkg_ prefices
    simpleGateNames :: DM.Map String ()
    simpleGateNames = DM.fromList $ map (\name -> (name,())) [ "and2", "and3",
        "ao22", "ao222",
        "buff",
        "c1u1", "c2", "c2r1", "c3",
        "gnd",
        "inv",
        "mutex",
        "nand2", "nand3",
        "nor2", "nor3",
        "or2", "or3" ]

    type GateCosts = DM.Map String (DM.Map String Int)

    {-
    mappingNetlistToCosts :: [GateNetlist] -> GateCosts
    mappingNetlistToCosts netlists = DM.fromList $ map findCost netlists
        where
            findCost (GateNetlist name _ props _) = fromMaybe (name, 0) $ do
                cost <- lookup "cost" props
                (costVal, _) <- listToMaybe $ reads cost
                return (name, costVal)
                -}

    -- mappingNetlistToTechMapping : make a TechMapping function from a set of netlists which
    --    map tkg_... gates into technology specific gates
    mappingNetlistToTechMapping :: [GateNetlist] -> TechMapping
    mappingNetlistToTechMapping netlists = topMappingFunc
        where
            netlistToMapping (GateNetlist name ports _ elems) = (name, (map netName nets, map makeInstMapping insts))
                -- (instCell, map connsToIndex instConns))
                where
                    nets = filter isGateNet elems
                    insts = filter isGateInstance elems

                    makeInstMapping (GateInstance instCell instConns) = (instCell, map connsToIndex instConns)
                    makeInstMapping _ = error "not an instance"

                    netName (GateNet name _) = name
                    netName _ = error "not a net"
                    portName (GatePort name _ _) = name

                    portIndices :: DM.Map String Int
                    portIndices = DM.fromList $ zip (map portName ports) [0..]

                    netIndices :: DM.Map String Int
                    netIndices = DM.fromList $ zip (map netName nets) [0..]

                    connsToIndex conns@[GateConn connName slice]
                        | slice /= (0 +: 1) = error $ "Bad mapping conns `" ++ show conns ++
                            "', in module `" ++ name ++ "'"
                        | isJust portNo = (True, fromJust portNo)
                        | isJust netNo = (False, fromJust netNo)
                        where
                            portNo = DM.lookup connName portIndices
                            netNo = DM.lookup connName netIndices
                    connsToIndex conns = error $ "Bad mapping conns `" ++ show conns ++
                        "', in module `" ++ name ++ "'"
            -- netlistToMapping netlist = error $ "Bad mapping netlist `" ++ show netlist ++ "'"

            -- Port names, Local nets, [(CellName, CellsCons)]
            mappings :: DM.Map String ([String], [(String, [(Bool, Int)])])
            mappings = DM.fromList $ map netlistToMapping netlists

            -- Tack tkg_ onto gates, ie. instances whose names don't start tkr_
            topMappingFunc prefix (GateInstance name conss)
                | isJust (DM.lookup name simpleGateNames) = mappingFunc prefix $ GateInstance ("tkg_" ++ name) conss
            topMappingFunc prefix inst = mappingFunc prefix inst

            mappingFunc prefix inst@(GateInstance name connss)
                | isJust mapping = map makeNet localNetNames' ++ mappedInsts
                | otherwise = [inst]
                where
                    n0 = [0..] :: [Int]
                    mapping = DM.lookup name mappings
                    Just (localNetNames, instMappings) = mapping
                    localNetNames' = map (prefix ++) localNetNames

                    makeInst (instCell, connIndices) = GateInstance instCell $ map mapConn connIndices
                        where
                            mapConn (True, portNo) = connss !! portNo
                            mapConn (False, netNo) = [GateConn (localNetNames' !! netNo) (0 +: 1)]
                    makeNet name = GateNet name 1

                    insts = map makeInst instMappings
                    mappedInsts = concatMap (\(i, inst) -> mappingFunc (prefix ++ show i ++ "_") inst) $
                        zip n0 insts
            mappingFunc _ elem = [elem]

    netlistCost :: GateNetlist -> GateCosts -> (String, DM.Map String Int)
    netlistCost (GateNetlist name _ _ elems) costs = (name,
        foldl (\ret elem -> DM.unionWith (+) ret $ findCost elem) DM.empty elems)
        where
            findCost (GateInstance name _) = fromMaybe (DM.fromList [(name, 1)]) $ DM.lookup name costs
            findCost _ = DM.empty

    genMakeGatesFile :: NetworkIF network => Bool -> [GenPartToGateOption] -> String -> TechMapping ->
        FilePath -> [Part network] -> IO ()
    genMakeGatesFile verbose options flatArgs mapping filename parts = do
        let
            partToGate = genPartToGateNetlist options
            (procedureNetlists, modulesToMakeByProc, latchesToMakes) = unzip3 $ map partToGate parts
            modulesToMake = nub $ concat modulesToMakeByProc
            latchesToMake = nub $ concat latchesToMakes

            makeProcNetlist (_, typ, widths) = genMakeComp mapping typ widths

            lineFormat handle comment str = do
                hPutStrLn handle $ comment ++ " " ++ head lines
                mapM_ (\l -> hPutStrLn handle $ comment ++ "   " ++ l) $ tail lines
                where lines = filter (not . (all isSpace)) $
                        concatMap (mapN 100 id) $ splitWith "\n" str

            timeStamp f = do
                time <- getClockTime
                hPutStrLn f $ "// Generated on: " ++ show time

            clipNames = False

        file <- openFile filename WriteMode
        hPutStrLn file $ "//"
        when (not (null flatArgs)) $ do
            hPutStrLn file $ "// " ++ flatArgs
            hPutStrLn file $ "//"
        timeStamp file
        hPutStrLn file $ "//\n"

        hPutStrLn file "\n`timescale 1ns/1ps\n"

        compCosts <- mapM (\(name, typ, widths) -> do
            when verbose $ do
                lineFormat file "//" $ name ++ " " ++ show typ ++ " " ++ show widths
                lineFormat stdout "--" $ name ++ "\n" ++ show typ ++ "\n" ++ showSomeHaskell show (Some widths)
                hFlush stdout
            let netlist = makeProcNetlist (name, typ, widths)
            hPutStrLn file $ showVerilog clipNames netlist ""
            return $ netlistCost netlist DM.empty
            ) modulesToMake

        latchCosts <- mapM (\(width, depth) -> do
            let name = latchName width depth
            when verbose $ do
                lineFormat file "//" $ "latch " ++ name ++ " width = " ++ show width ++ ", depth = " ++ show depth
                lineFormat stdout "--" $ "latch " ++ name ++ " width = " ++ show width ++ ", depth = " ++ show depth
                hFlush stdout
            let netlist = makeLatch mapping width depth
            hPutStrLn file $ showVerilog clipNames netlist ""
            return $ netlistCost netlist DM.empty
            ) latchesToMake

        let
            mappedNetlists = map (genTechMap mapping) procedureNetlists
            costs' = DM.union (DM.fromList compCosts) (DM.fromList latchCosts)
            netlistCosts = foldl (\costs netlist -> let
                (name, cost) = netlistCost netlist costs
                in DM.insert name cost costs) costs' mappedNetlists

            showCostPair (name, count) = name ++ "*" ++ show count

        mapM_ (\nl -> do
            hPutStrLn file $ showVerilog clipNames nl ""
            ) mappedNetlists

        hPutStrLn file "// Netlist costs:"
        mapM_ (\(name, cost) -> do
            hPutStrLn file $ "// " ++ name ++ ": " ++ joinWith " " (map showCostPair $ DM.toList cost)) $
            DM.toList netlistCosts

        hClose file

{-

    nwGetPortSignals :: (NetworkIF network) => NetworkComp ->
        NetworkMonad network [(NetworkLinkRef, [(String, Int, Gen.RelDir)])]
    nwGetPortSignals comp = do
        let
            compType = nwTeakType comp
            conns = nwCompLinks comp

            mapSomeM m (One i) = do
                r <- m i
                return $ One r
            mapSomeM m (Many iss) = do
                rs <- mapM m iss
                return $ Many rs

        compPortWidths <- mapM (mapSomeM nwGetLinkWidth) conns

        let
            compPortInfo = teakCompPortInfo $ nwTeakCompInfo compType
            compPortNames = map networkPortName compPortInfo

            namesXportWidths = zip compPortNames compPortWidths

            makePorts (name, widths) = case widths of
                Many ws -> zipWith (fullBundles name) ws [0..]
                One w -> [fullBundles name w 0]

            ports = concatMap makePorts namesXportWidths

        return $ zip (concatMap flattenSome conns) ports
-}
