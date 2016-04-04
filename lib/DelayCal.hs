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

module DelayCal (
     nwGetDelay,
    nwGetCompDelay
    )
    where
    import NetParts
    import Gen
    import Gates
    import Misc
    import ParseTree


    import Data.Maybe
    import State
    import qualified Data.Map as DM
    import qualified Data.IntMap as IM
    import Control.Monad.ST
    import Data.Array.ST (STArray, newArray, readArray, writeArray)
    import Data.List

    nwGetDelay :: (NetworkIF network) => NetworkLinkRef -> NetworkLinkRef -> NetworkComp -> TechMapping -> NetworkMonad network (Double, Double)
    nwGetDelay l1 l2 comp mapping = do
        infoList <- nwGetLinkSignals comp
        let
            infoListMap = DM.fromList infoList
            forwardDelayInputSignals  = map (\(name,_,_)->Wire name) $ filter (\(_,_,dir) -> dir == True) $ fromJust $ DM.lookup l1 infoListMap
            forwardDelayOutputSignals = map (\(name,_,_)->Wire name) $ filter (\(_,_,dir) -> dir == True) $ fromJust $ DM.lookup l2 infoListMap
            reverseDelayInputSignals   = map (\(name,_,_)->Wire name) $ filter (\(_,_,dir) -> dir == False) $ fromJust $ DM.lookup l2 infoListMap
            reverseDelayOutputSignals  = map (\(name,_,_)->Wire name) $ filter (\(_,_,dir) -> dir == False) $ fromJust $ DM.lookup l1 infoListMap

        forwardDelay <- nwDelayCal forwardDelayInputSignals forwardDelayOutputSignals comp mapping
        reverseDelay <- nwDelayCal reverseDelayInputSignals reverseDelayOutputSignals comp mapping
        return (forwardDelay, reverseDelay)


    nwGetCompDelay :: (NetworkIF network) => NetworkComp -> TechMapping -> NetworkMonad network Double
    nwGetCompDelay comp mapping = do
        infoList <- nwGetLinkSignals comp
        let
            infoListMap = DM.fromList infoList
            signals = map (\(name,_,_) -> Wire name) $ concat $ DM.elems infoListMap
        delay <- nwDelayCal signals [] comp mapping
        return delay


    nwDelayCal :: (NetworkIF network) => [GnWireRef] -> [GnWireRef] -> NetworkComp -> TechMapping -> NetworkMonad network Double
    nwDelayCal inputSignals outputSignals comp mapping = do
        Just (typ,width) <- getKey comp
            let
            netlist = genMakeComp mapping typ width
            gatenetwork = genGateNetwork netlist
            delay = caldelay gatenetwork inputSignals outputSignals
        return delay

    caldelay :: GateNetwork -> [GnWireRef] -> [GnWireRef] -> Double
    caldelay gatenetwork inputSignals outputSignals = 
        let
            forest = concat $ map (dfs gatenetwork outputSignals) $ map (\x->[x]) inputSignals
            paths = getWirePaths forest
            longdelay = getLongestPathDelay paths gatenetwork
        in longdelay    


    getWirePaths :: [WireTree GnWireRef] -> [[GnWireRef]]
    getWirePaths [] = []
    getWirePaths (WireNode w [] : us) = [[(w)]] ++ getWirePaths us
    getWirePaths (WireNode w ts : us)
            = (map (\x -> w:x) (getWirePaths ts)) ++ getWirePaths us


    getLongestPathDelay :: [[GnWireRef]] -> GateNetwork -> Double
    getLongestPathDelay paths gatenetwork = 
        let
            delays = map (calPathDelay gatenetwork) paths
        in maximum delays
    
    
    calPathDelay :: GateNetwork -> [GnWireRef] -> Double
    calPathDelay _ [] = 0.0
    calPathDelay _ (x:[]) = 0.0
    calPathDelay gatenetwork (x:y:xs) = (addInbetweenGateDelay x y gatenetwork) + (calPathDelay gatenetwork (y:xs))
        where
            addInbetweenGateDelay x y gatenetwork = 
                let 
                    Just connx = fst $ runGateNetwork gatenetwork $ gnGetWireUsage x
                    Just conny = fst $ runGateNetwork gatenetwork $ gnGetWireUsage y
                    xinstanRefs = map (\(ref,_,_) -> ref) $ filter (\(_,dir,_)->dir == Input ) connx
                    yinstanRefs = map (\(ref,_,_) -> ref) $ filter (\(_,dir,_)->dir == Output) conny
                    inbetweenRef = intersect xinstanRefs yinstanRefs
                    inbetweenInstanceNames = map (getInstanNames gatenetwork) inbetweenRef
                    delaylist = map (\name -> fromJust $ DM.lookup name gateDelayInfo) inbetweenInstanceNames
                    getInstanNames gatenetwork instanRef = gnInstanceName instanceIT
                        where
                            Just instanceIT = fst $ runGateNetwork gatenetwork $ gnGetInstance instanRef
                in maximum delaylist

    getKey :: (NetworkIF network) => NetworkComp -> NetworkMonad network (Maybe (TeakCompType, [Some Int]))
    getKey comp = do 
        let  
            links = nwCompLinks comp 
            senses = nwCompPortSenses comp 
            flatten (One link) sense = [(link, sense)]
            flatten (Many links) sense = zip links $ repeat sense
            flatten (Some links) sense = concat $ zipWith flatten links $ repeat sense

        let  
            flatLinks = concat $ zipWith flatten links senses
        linkWidths <- mapM nwGetLinkWidth $ map fst flatLinks
        case comp of
            TeakComp _ typ _ _ -> do
                let  
                    (Some someWidths, []) = mapOverSome (\_ w -> w) (Some links) linkWidths
                return $ Just (typ, someWidths)
            InstanceComp _ name _ _ _ -> do
                return Nothing


    data WireTree wire = WireNode wire (WireForest wire) deriving (Eq, Read, Show)
    type WireForest wire = [WireTree wire]
       
    generateWT :: (GateNetworkIF gatenetwork) => gatenetwork -> GnWireRef -> WireTree GnWireRef 
    generateWT gatenetwork wire = WireNode wire (map (generateWT gatenetwork) outputwires)
        where outputwires = getNextwires gatenetwork wire

    getNextwires :: (GateNetworkIF gatenetwork) => gatenetwork -> GnWireRef -> [GnWireRef]  
    getNextwires gatenework wire = fst $ runGateNetwork gatenework $ do
        wireconn <- gnGetWireUsage wire
        let 
            instanceRefs = map (\(instanRef,_,_)->instanRef) $ filter (\(_,dir,_)->dir==Input) $ fromJust wireconn
            gnGetInstanceHere instanRef = fromJust $ fst $ runGateNetwork gatenework $ do gnGetInstance instanRef
        return $ concat $ map gnInstanceOutputWires ((map gnGetInstanceHere) instanceRefs)
    
    prune :: (GateNetworkIF gatenetwork) => WireBounds -> [WireTree GnWireRef] -> gatenetwork -> [GnWireRef] -> [WireTree GnWireRef]
    prune bnds ts gatenetwork ends = run bnds (chop ts gatenetwork ends)

    chop :: (GateNetworkIF gatenetwork) => [WireTree GnWireRef] -> gatenetwork -> [GnWireRef] -> SetM s [WireTree GnWireRef]
    chop [] _ _ = return []
    chop (WireNode w ts : us) gatenetwork ends = do
           let 
            wi = gnWireIndex $ fst $ runGateNetwork gatenetwork $ gnGetWire w
            stop = elem w ends
        visited <- contains wi
        if stop && (not visited)
            then do
                include wi
                 cs <- chop us gatenetwork ends
                return (WireNode w [] : cs)
            else do
                 if visited 
                    then
                          chop us gatenetwork ends
                           else do
                          include wi
                               as <- chop ts gatenetwork ends
                          bs <- chop us gatenetwork ends
                          return (WireNode w as : bs)


    type GnWireIndex = Int
    type WireBounds = (GnWireIndex, GnWireIndex)

    newtype SetM s a = SetM { runSetM :: STArray s GnWireIndex Bool -> ST s a }

    instance Monad (SetM s) where
        return x     = SetM $ const (return x)
        SetM v >>= f = SetM $ \ s -> do { x <- v s; runSetM (f x) s }

    run      :: WireBounds -> (forall s. SetM s a) -> a
    run bnds act  = runST (newArray bnds False >>= runSetM act)

    contains     :: GnWireIndex -> SetM s Bool
    contains v    = SetM $ \ m -> readArray m v

    include      :: GnWireIndex -> SetM s ()
    include v     = SetM $ \ m -> writeArray m v True


    dfs :: GateNetwork -> [GnWireRef] -> [GnWireRef] -> [WireTree GnWireRef]
    dfs gatenetwork ends ws = prune (wirebounds gatenetwork) (map (generateWT gatenetwork) ws) gatenetwork ends
        where wirebounds gatenetwork = (1,gnWireCount gatenetwork)
    
    nwGetLinkSignals :: (NetworkIF network) => NetworkComp -> NetworkMonad network [(NetworkLinkRef, [(String, Int, Bool)])]
    nwGetLinkSignals comp = do
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
                Many ws -> zipWith (fullBundles2 name) ws [0..]
                One w -> [fullBundles2 name w 0]
    
            ports = concatMap makePorts namesXportWidths

        return $ zip (concatMap flattenSome conns) ports

------------------------------------GateNetwork------------------------

    data GateNetwork = GateNetwork {
        gnPortCount :: Int,
        gnWireCount :: Int,
        gnInstanceCount :: Int,
        gnPorts :: DM.Map String GnPort,
        gnWires :: DM.Map String GnWire,
        gnWireUsage :: DM.Map String [GnWireConn],
        gnInstances :: IM.IntMap GnInstance
        }
        deriving (Show, Read)

    data GnPort = GnPort {
        gnPortWire :: GnWire,
        gnPortDir :: Direction
        }
        deriving (Show, Read)

    data GnWire = GnWire {
        gnWireIndex :: Int, 
        gnWireName :: String,
        gnWireWidth :: Int
        }
        deriving (Show, Read, Eq)

    newtype GnWireRef = Wire {gnWire :: String} deriving (Show, Read, Eq)

    type GnWireConn = (GnInstanceRef, Direction, GnPortPos) 

    newtype GnInstanceRef = Instan {gnInstan :: Int} deriving (Show, Read, Eq)

    type GnPortPos = Int

    data GnInstance = GnInstance {
        gnInstanceIndex :: Int,
        gnInstanceName :: String,
        gnInstanceWires :: [(GnWireRef,Direction,GnPortPos)],
        gnInstanceInputWires :: [GnWireRef],
        gnInstanceOutputWires :: [GnWireRef]
        }
        deriving (Show, Read)
    

    newtype {- GateNetworkIF gatenetwork => -}
        GateNetworkMonad gatenetwork a = GateNetworkMonad { gatenetworkMonadM :: State gatenetwork a }

    instance GateNetworkIF gatenetwork => Monad (GateNetworkMonad gatenetwork) where
        l >>= k = GateNetworkMonad $ (gatenetworkMonadM l) >>= k'
            where k' v = gatenetworkMonadM $ k v
        return r = GateNetworkMonad $ return r
        fail str = GateNetworkMonad $ fail str

    class GateNetworkIF gatenetwork where
        gnAddInstance :: GnInstance -> GateNetworkMonad gatenetwork ()
        gnAddWire :: GnWire -> GateNetworkMonad gatenetwork ()
        gnAddPort :: GnPort -> GateNetworkMonad gatenetwork ()
        gnGetInstance :: GnInstanceRef -> GateNetworkMonad gatenetwork (Maybe GnInstance)
        gnGetWire :: GnWireRef -> GateNetworkMonad gatenetwork GnWire
        gnGetWireUsage :: GnWireRef -> GateNetworkMonad gatenetwork (Maybe [GnWireConn])
        gnNewGateNetwork :: gatenetwork
    
    instance GateNetworkIF GateNetwork where

             gnNewGateNetwork = GateNetwork 0 0 0 DM.empty DM.empty DM.empty IM.empty

        gnAddPort port = GateNetworkMonad $ state op
            where op gn = ((), gn {
                gnPorts = ports',
                gnWires = wires',
                gnWireUsage = usage',
                gnWireCount = wirecount,
                gnPortCount = portcount })
                where
                    portcount = gnPortCount gn + 1
                    wirecount = gnWireCount gn + 1
                    wire = gnPortWire port
                    wire' = wire {gnWireIndex = wirecount}
                    port' = GnPort wire' (gnPortDir port)
                    wirename = gnWireName wire
--                    port' = port {gnPortWire = wire'}
                    ports' = DM.insert wirename port' (gnPorts gn)
                    wires' = DM.insert wirename wire' (gnWires gn)
                    usage' = DM.insert wirename [] (gnWireUsage gn)
               
        gnAddWire wire = GateNetworkMonad $ state op
            where op gn = ((), gn {
                gnWires = wires',
                gnWireUsage = usage',
                gnWireCount = wirecount})
                where
                    wirecount = gnWireCount gn + 1
                    wire' = wire {gnWireIndex = wirecount}
                    wirename = gnWireName wire'
                    wires' = DM.insert wirename wire' (gnWires gn)
                    usage' = DM.insert wirename [] (gnWireUsage gn)

        gnAddInstance instan = GateNetworkMonad $ state op
            where op gn = ((), gn {
                gnInstances = instans',
                gnWireUsage = usage',
                gnInstanceCount = count })
                where
                    count = gnInstanceCount gn + 1
                    instan' = instan { gnInstanceIndex = count }
                    instans' = IM.insert count instan' (gnInstances gn)
                    usage' = foldl addUsage (gnWireUsage gn) (gnInstanceWires instan)  --fold right?
                        where addUsage usage (wireRef, dir, pos) = 
                            DM.adjust ((Instan count, dir, pos):) (gnWire wireRef) usage

               gnGetWire (Wire a) = GateNetworkMonad $ state op
            where op gn
                | isJust wire = (fromJust wire, gn)
                | otherwise = error $ "nwGetWire: can't find wire at index " ++ show a
                where wire = DM.lookup a (gnWires gn)
    
        gnGetInstance (Instan i) = GateNetworkMonad $ state op
            where op gn = (IM.lookup i (gnInstances gn), gn)

        gnGetWireUsage wire = GateNetworkMonad $ state op
            where op gn = (DM.lookup (gnWire wire) (gnWireUsage gn), gn)


    runGateNetwork :: GateNetworkIF gatenetwork => gatenetwork -> GateNetworkMonad gatenetwork a -> (a, gatenetwork)
    runGateNetwork gn gnm = runState (gatenetworkMonadM gnm) gn

    
    genGateNetwork :: GateNetlist -> GateNetwork
    genGateNetwork (GateNetlist name gatePorts _ gateElems) = 
        let 
            gn0 = gnNewGateNetwork
            portAdded = foldl addPorts gn0 gatePorts
            wireXinstanceAdded = foldl addWireInstances portAdded gateElems
            
            addPorts gn (GatePort name dir width) = snd $ runGateNetwork gn $ do
                let
                    portwire = GnWire 999999 name width
                    port = GnPort portwire dir
                gnAddPort port
            
            addWireInstances gn gelem = 
                case gelem of
                    GateNet name width -> snd $ runGateNetwork gn $ do
                        gnAddWire $ GnWire 999999 name width
                    GateInstance name gateconns -> addInstance gn $ GateInstance name gateconns
                    _ -> error "Unexpected situation!"
            
            addInstance gn (GateInstance name gateconns) = snd $ runGateNetwork gn $ do
                let 
                    gateconnsXinOutput = zip (concat gateconns) $ fromJust $ DM.lookup name gatePortInOutput
                    gateconnsXinOutputXXposition = zip3 (concat gateconns) (fromJust (DM.lookup name gatePortInOutput)) [1..]
                    inputWires  = map (\((GateConn name _),_) -> Wire name) $ filter (\(_,dir)->dir==Input) gateconnsXinOutput
                    outputWires = map (\((GateConn name _),_) -> Wire name) $ filter (\(_,dir)->dir==Output) gateconnsXinOutput
                    wires = map (\((GateConn name _),dir,pos)->(Wire name, dir, pos)) gateconnsXinOutputXXposition
                gnAddInstance $ GnInstance 9999 name wires inputWires outputWires
        in wireXinstanceAdded
                


    gatePortInOutput :: DM.Map String [Direction]
    gatePortInOutput = DM.fromList [("AN2EHD",[Output, Input, Input]),
        ("AN3EHD",[Output, Input, Input, Input]),
        ("OR2EHD", [Output, Input, Input]),
        ("OR3EHD", [Output, Input, Input, Input]),
        ("ND2HHD", [Output, Input, Input]),
        ("ND3EHD", [Output, Input, Input, Input]),
        ("NR2EHD", [Output, Input, Input]),
        ("NR3EHD", [Output, Input, Input, Input]),
        ("AO222EHD", [Output, Input, Input, Output, Input, Output, Input]),
        ("AN2B1CHD", [Output, Input, Input]),    ------------Is this table correct??
        ("AO12EHD", [Output, Input, Input, Input]),
        ("AO22EHD", [Output, Input, Input, Input, Input]),
        ("TIE0DND", [Output]),
        ("INVHHD", [Output, Input]),
        ("BUFEHD", [Output, Input]),
        ("MUTEX", [Input, Input, Output, Output])]
{-
    gateDelayInfo :: DM.Map String Double
    gateDelayInfo = DM.fromList [("AN2EHD", 1.0),
        ("AN3EHD", 1.0),
        ("OR2EHD", 1.0),
        ("OR3EHD", 1.0),
        ("ND2HHD", 1.0),
        ("ND3EHD", 1.0),
        ("NR2EHD", 1.0),
        ("NR3EHD", 1.0),
        ("AO222EHD", 1.0),
        ("AN2B1CHD", 1.0),
        ("AO12EHD", 1.0),
        ("AO22EHD", 1.0),
        ("TIE0DND", 1.0),
        ("INVHHD", 1.0),
        ("BUFEHD", 1.0),
        ("MUTEX", 1.0)]
-}

    gateDelayInfo :: DM.Map String Double
    gateDelayInfo = DM.fromList [("AN2EHD", 83.1975),
        ("AN3EHD", 89.11),
        ("OR2EHD", 83.125),
        ("OR3EHD", 96.285),
        ("ND2HHD", 44.0425),
        ("ND3EHD", 56.1967),
        ("NR2EHD", 52.0475),
        ("NR3EHD", 112.495),
        ("AO222EHD", 118.1831),
        ("AN2B1CHD", 81.7475),
        ("AO12EHD", 88.368),
        ("AO22EHD", 93.1896),
        ("TIE0DND", 36.7),   --cannot find the corresponding gate TIE0DND in umc130f yet
        ("INVHHD", 36.7),
        ("BUFEHD", 81.64),
        ("MUTEX", 236.3662)] --cannot find the corresponding gate MUTEX in umc130f yet Using two C gates's delay instead
