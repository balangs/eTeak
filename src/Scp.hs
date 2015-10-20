-- Teak synthesiser for the Balsa language
-- Scp.hs : Structural Critical Path Searching functions, modified and pared down from:

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file ../LICENCE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A version of the graph algorithms described in:
--
--   /Lazy Depth-First Search and Linear Graph Algorithms in Haskell/,
--   by David King and John Launchbury.
--
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}

module Scp(
	nwInsertScpLatches,
	scp
	) where

	import NetParts
	-- import Network
	import Gen
	-- import Gates
	import DelayCal
	-- import Misc
	-- import Report
	import ParseTree
	-- import Options

	-- import State
	import Control.Monad
	import Control.Monad.ST
	import Data.Array.ST (STArray, newArray {- , readArray, writeArray -})
	-- import Data.Array
	-- import Data.Maybe
	import Data.List
	import qualified Data.Map as DM

	type Vertex = Maybe NetworkCompRef
	type Link   = NetworkLinkRef
	type BuffDepth = Int

	type Edge = (Link, (Vertex, Vertex,BuffDepth))
	type Graph = [Edge]
	
	type Bounds = (Link, Link)

     	elBounds :: [Edge] -> Bounds
     	elBounds el = (minimum flatEl, maximum flatEl)
           	where flatEl = foldl' (\x (l,(_,_,_)) -> l:x) [] el	

	data Tree a b c d e= Node a b c d (Forest a b c d e)
		deriving (Eq, Read, Show)

	type Forest a b c d e = [Tree a b c d e]					

        generate :: Graph -> (Link, (Vertex, Vertex, BuffDepth)) -> Tree Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth)
        generate g (l, (v0,v1,b)) = Node l v0 v1 b (map (generate g) connectedlinks)
                        where connectedlinks = findNextLinks g l

	findNextLinks :: Graph -> Link -> [(Link, (Vertex, Vertex, BuffDepth))]
	findNextLinks g l = filter (\(_, (v0, _, _)) -> test v0 l ) g
			where test v0 l = 
				case v0 of
					Nothing -> False
					_ -> case lookup l g of
						Just (v1, v2, depth) -> v2==v0
						_ -> False


	prune :: Bounds -> Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth)  -> Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth) 
	prune bnds ts = run bnds (chop2 ts)

        --chop whenever reaching a latched link 
	chop2  :: Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth) -> SetM s (Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth))
        chop2 []       = return []
        chop2 (Node l v0 v1 b ts : us)
              = do
                if (b>0 || v1==Nothing) then do     --Also consider the links that are accesses to the out
		  cs <- chop2 us
		  return (Node l v0 v1 b [] : cs)
                 else do
                  as <- chop2 ts
                  bs <- chop2 us
                  return (Node l v0 v1 b as : bs)


	newtype SetM s a = SetM { runSetM :: STArray s Link Bool -> ST s a }
	
	instance Monad (SetM s) where
		return x     = SetM $ const (return x)
		SetM l >>= f = SetM $ \ s -> do { x <- l s; runSetM (f x) s }
	
	run          :: Bounds -> (forall s. SetM s a) -> a
	run bnds act  = runST (newArray bnds False >>= runSetM act)

	dfs :: Graph -> Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth)
	dfs g = correctdfs $ dfs' g 
		where 
			correctdfs [] = []
		        correctdfs (Node l v0 v1 b ts : us)
                		   = (Node l v0 v1 1 ts : correctdfs us)
			dfs' g = prune (elBounds g) (map (generate g) (trick $ getLatchedLinks g))
		                where 
					getLatchedLinks g = (filter (\(_,(_,_,b)) -> b>0) g) ++ (filter (\edg -> test edg) g)
						where test (l,(v0,v1,b)) = if v0==Nothing then True else False   --Add the edges from go or other accesses
					trick vs = map (\(l,(v0,v1,b))->(l,(v0,v1,0))) vs

	getAllPaths :: Forest Link Vertex Vertex BuffDepth (Link, Vertex, Vertex, BuffDepth) -> [[Edge]]
	getAllPaths [] = []
	getAllPaths (Node l v0 v1 b [] : us) = [[(l,(v0,v1,b))]] ++ getAllPaths us
	getAllPaths (Node l v0 v1 b ts : us)
			= (map (\x -> (l,(v0,v1,b)):x) (getAllPaths ts)) ++ getAllPaths us


--	calPathCost :: (NetworkIF network) => [Edge] -> TechMapping -> DM.Map String Double -> NetworkMonad network Double
	calPathCost [] compDelayList = return 0.0
        calPathCost (x:[]) compDelayList = return 0.0
        calPathCost (x:y:xs) compDelayList = do
		delay1 <- calComLink2LinkCost x y compDelayList 
		delay2 <- calPathCost (y:xs) compDelayList
		return (delay1 + delay2)
	
--	calComLink2LinkCost :: (NetworkIF network) => Edge -> Edge -> TechMapping -> DM.Map String Double -> NetworkMonad network Double 
	calComLink2LinkCost  (l1,(v0,v1,b1))  (l2,(v11,v2,b2)) compDelayList = do
		let 
			Just v = v1
			Just delay = DM.lookup v compDelayList
		return delay

--	calComCost :: (NetworkIF network) => Edge -> TechMapping -> DM.Map String Double -> NetworkMonad network Double 
        calComCost (l,(v0,v1,b1)) compDelayList = do
                case v1 of
			Just v -> do
				let Just delay = DM.lookup v compDelayList
				return delay
			Nothing -> return 0.0

--	zipLD :: (NetworkIF network) => [[Edge]] -> TechMapping -> DM.Map String Double -> NetworkMonad network [([Edge], Double)]
	zipLD xs compDelayList = do
		delays <- mapM (\x -> (calPathCost x compDelayList)) xs
		return $ zip xs delays

	

--	maximumL :: (NetworkIF network) => [[Edge]] -> TechMapping -> DM.Map String Double -> NetworkMonad network ([Edge], Double)
	maximumL [] _ = error "PathList is Empty"
	maximumL xs compDelayList = do
		pathDelayList <- zipLD xs compDelayList
		let
			longestPathXDelay = foldl1 max pathDelayList
			max x y = case cmp x y of 
				GT -> x
				_  -> y
			cmp (es1,x) (es2,y) = if x > y then GT else LT
		return longestPathXDelay


--	scp :: (NetworkIF network) => Part network -> TechMapping -> DM.Map String Double -> ([Edge], Double)
	scp part mapping partDelayList = 
		let
			graph = buildGraph part
			compDelayList = DM.fromList $ tryPart part $ nwMapComps $ \comp -> do
                                case comp of
                                        TeakComp a b c d -> do    
                                                delay <- nwGetCompDelay comp mapping
                                                return (Comp (nwCompIndex comp), delay)
                                        InstanceComp i name j k m -> do
                                                let
                                                        Just delay = DM.lookup name partDelayList
                                                return (Comp (nwCompIndex comp), delay)
			longestPath = fst $ runPart part $ maximumL (getAllPaths (dfs graph)) compDelayList
		in longestPath

--	furtherLatch :: (NetworkIF network) => Double -> Part network -> TechMapping -> DM.Map String Double -> [Link]
	furtherLatch limit part mapping partDelayList = fst $ runPart part $ do
		let
			graph = buildGraph part
			compDelayList = DM.fromList $ tryPart part $ nwMapComps $ \comp -> do
                                case comp of
                                        TeakComp a b c d -> do
                                                delay <- nwGetCompDelay comp mapping
                                                return (Comp (nwCompIndex comp), delay)
                                        InstanceComp i name j k m -> do
                                                let
                                                        Just delay = DM.lookup name partDelayList
                                                return (Comp (nwCompIndex comp), delay)
			paths = getAllPaths (dfs graph)
		pathdelays <- zipLD paths compDelayList
		let
			longpaths = map (\(path,_)->path) $ filter (\(path,delay) -> (delay>limit)) pathdelays
			links = map selectLinks longpaths
				where 
					selectLinks list = snd $ foldl func (0,[]) list
						where
							func (acc1,acc2) x@(l,(v0,v1,d)) = if ((acc1 + (calD x)) > limit) && d<1
                                                        					then (0, l:acc2)
			                                                                        else (acc1+(calD x), acc2)
								where
									calD x = fst $ runPart part $ calComCost x compDelayList
		return $ nub $ concat links


	nwInsertScpLatches :: (NetworkIF network) => Part network -> Double -> TechMapping -> DM.Map String Double -> Part network

	nwInsertScpLatches part limit mapping partDelayList = snd $ runPart part $ do
			mapM_ (uncurry nwLatchLink) links
				where links = zip (furtherLatch limit part mapping partDelayList) (repeat 1)

	buildGraph :: (NetworkIF network) => Part network -> [Edge]
	buildGraph part = fst $ runPart part $ do
		nwMapLinks $ \link -> do
		let latching = nwLinkLatching link
		conn1 <- nwGetLinkUsage Active (refLink link)
		conn2 <- nwGetLinkUsage Passive (refLink link)
		return (refLink link, ((modifyconn conn1), (modifyconn conn2), modifylatch latching))
			where
				modifyconn conn =
					case conn of
						Just (LinkComp ref addr) -> Just ref
						_ -> Nothing   -- fixme?

				modifylatch (HalfBuffer depth) = depth
	
	nwLatchLink :: NetworkIF network => NetworkLinkRef -> Int -> NetworkMonad network ()
        nwLatchLink link depth = do
                unused <- nwLinkIsUnused link
                when (not unused) $ do
                        linkDecl <- nwGetLink link
                        nwUpdateLink $ linkDecl { nwLinkLatching = HalfBuffer depth }
                        return ()


	analyzeAll :: (NetworkIF network) => Part network -> TechMapping -> DM.Map String Double -> [Double]
	analyzeAll part mapping partDelayList = 
		let
			graph = buildGraph part
			compDelayList = DM.fromList $ tryPart part $ nwMapComps $ \comp -> do
                                case comp of
                                        TeakComp a b c d -> do
                                                delay <- nwGetCompDelay comp mapping
                                                return (Comp (nwCompIndex comp), delay)
                                        InstanceComp i name j k m -> do
                                                let
                                                        Just delay = DM.lookup name partDelayList
                                                return (Comp (nwCompIndex comp), delay)
			pathdelays = fst $ runPart part $ zipLD (getAllPaths (dfs graph)) compDelayList
			alldelays = map (\(_,f)->f) pathdelays
		in alldelays

{-

------------------------------------------------------------------------Test-----------------------------------------------
        main = do
        let
                readNetwork :: String -> WhyT IO [Part Network]
                readNetwork = readNetworkFile

        Why comp parts <- runWhyT $ readNetwork "alu.teak"

        gatelist <- tryWhyT $ do
--                gates <- parseVerilogFile "/home/rhg_001/teakHome/share/teak/tech/umc130f-mapping.v"
		gates <- parseVerilogFile "/home/visitor2/hongguar/WorkSpace/teakHome/share/teak/tech/umc130f-mapping.v"
                return gates
        let
                mapping = mappingNetlistToTechMapping gatelist
		partDelayList = foldl func DM.empty parts
			where
				func partDelayList part = DM.insert (networkName part) (snd (scp part mapping partDelayList)) partDelayList
		alldelays = map (\part->analyzeAll part mapping partDelayList) parts
		partxxx = map (\part->nwInsertScpLatches part 30 mapping partDelayList) parts
	print partDelayList
	print alldelays
	print partxxx
-}
