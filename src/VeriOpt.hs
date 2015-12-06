{--
	eTeak synthesiser a GALS back-end for the Balsa language
	Copyright (C) 2012- The University of Manchester

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

	Mahdi Jelodari <m.j.1989@ieee.org> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
--}


module VeriOpt (

	makePartFSM,
	stLinkToSt

	) where 

	import Misc
	import SimTypes
	import SimBuiltin
	import ParseTree hiding (Chan)
	import NetParts
	import Bits
	import Dot
	import Show
	import State
	import GuiSupport (colours, black)

	import qualified Data.Map as DM
	import Data.List
	import Data.Bits
	import Control.Monad.State
	import Data.Maybe
	import Data.Char
	import System.IO

	newtype StLink = Link { fsmLink :: Int }
		deriving (Eq, Ord, Show)



		
	fsmAddSt :: Sts -> MkFSMonad m value (Sts) 
	fsmAddSt st = state op
			where op nw = (st', nw {				
				fsmLabel = label',
				fsmStsCount = count',
				fsmSts = sts'})
				where		
					st' = st	
					label' = fsmLabel nw				
					count' = fsmStsCount nw + 1
					sts' = DM.insert count' st (fsmSts nw) 

	fsmAddName :: String -> MkFSMonad m value [String] 
	fsmAddName str = state op
			where op nw = (label', nw {
				fsmLabel = label'})
				where			
					label' = [str]				

	
	stLinkToSt ::  StNo -> NetworkLinkRef -> FSM -> Maybe StNo
	stLinkToSt stNoRef link fsm = takeNO link fsm
		where 	
			takeNO link fsm = mostCloseSt stNoRef $ map (isEq link) $ DM.assocs $ fsmStsS fsm
				where 
					mostCloseSt stNoRef xs = do

						let 	xs' = filter (\x -> x /= Nothing) $ xs						

						if null xs' == True then Nothing
						else do

							let 	xs'' = filter (\(Just x) -> x > stNoRef) xs'
						
							if null xs'' == True then Nothing
							else head xs''				

					--FIXME this might cause backward links to be filtered. choosing the head means that the closest.

					isEq link (stNo,st) = if (findIndex (== link) $ inpLinks st) == Nothing then Nothing
								 else Just stNo



	getLinkAccs :: (NetworkIF network) => Part network -> [NetworkLinkRef]
	getLinkAccs part = tryPart part $ do
		let
		portLinks <- liftM concat $ mapM' (nwGetPortAccess . fromJust . nwPortRef) $ networkPorts part
		return portLinks   



	--this function returns the compoenet conenncted to the Go
	getFirstComp :: (NetworkIF network) => Part network -> NetworkComp
	getFirstComp part = tryPart part $ do
		let
		portLinks <- liftM head $ nwGetPortAccess $ fromJust $ nwPortRef  $ (networkPorts part) !! go
		Just (comp , _) <- nwLinkToComp Passive portLinks
		return comp

		where 		
			(Just go) = findIndex (fromMaybe False . liftM (== GoAccess) . nwPortRef) $ networkPorts part 

	getLastComp :: (NetworkIF network) => Part network -> NetworkComp
	getLastComp part = tryPart part $ do
			let
				--FIXME just one output port is being selected!
				outLink = head $ map (\out -> tryPart part $ liftM head $ nwGetPortAccess $ fromJust $ nwPortRef $ (networkPorts part) !! out) outsIndices
			Just (comp , _) <- nwLinkToComp Active outLink
			return comp

				where 		
				outsIndices = findIndices ((== Output) . nwPortDirection) $ networkPorts part


	visitComp :: NetworkIF network => Part network -> NetworkLinkRef -> NetworkComp -> Maybe (Sts)
	visitComp part upLink (TeakComp compNo teakTyp links _) = do
		let	
			 	
			chanLinks = links
			linkWidths = tryPart part $ mapM' (mapM' nwGetLinkWidth . flattenSome) links
			partName = networkName part

		do
			case (teakTyp, chanLinks, linkWidths) of
				(TeakR, [One out], [outWidth]) -> Just (Sts compNo teakTyp links [] outWidth [] [] [])
				(TeakI, [One inp, One out], [inpWidths, outWidths]) -> Just (Sts compNo teakTyp links inpWidths outWidths [] [] [inp])
				(TeakM, [Many inp, One out], [inpWidths, [outWidth]]) -> Just (Sts compNo teakTyp [One upLink, One out] inpWidths [outWidth] [] [] inp) 
				(TeakS guardSlice impsXOffsets, [One inp, Many out], [[inpWidth], outWidths]) -> let

					(impss, offsets) = unzip impsXOffsets
					outSlices = zipWith (+:) offsets outWidths
					in Just (Sts compNo teakTyp links [inpWidth] outWidths outSlices impss [inp])

				(TeakF offsets, [One inp, Many out], [inpWidths, outWidths]) ->	Just (Sts compNo teakTyp links inpWidths outWidths [] [] [inp])
				(TeakJ, [Many inp, One out], [inpWidths, outWidths]) -> Just (Sts compNo teakTyp links inpWidths outWidths [] [] inp)
				(TeakA, [Many inp, One out], [inpWidths, outWidths]) -> Just (Sts compNo teakTyp links inpWidths outWidths [] [] inp)
				(TeakO terms, [One inp, One out], [inpWidths, outWidths]) -> Just (Sts compNo teakTyp links inpWidths outWidths [] [] [inp])
				(TeakV _ width _ wOffsets rOffsets, [Many wg, Many wd, Many rg, Many rd],[wWidths, _, _, rWidths]) -> Just (Sts compNo teakTyp [One upLink, Many 					wg, Many wd, Many rg, Many rd] wWidths rWidths [] [] (wg ++ rg) )
				(_, _, _) -> error $ "makeComp: unrecognised component " ++ show teakTyp


	-- nwConnectedComps : returns all the components reachable from the given
	--	one to the requested depth
	getNextComps :: NetworkIF network => Part network -> NetworkLinkRef -> NetworkCompRef -> [Integer] -> NetworkMonad network [(NetworkCompRef,NetworkLinkRef)]
	getNextComps part link comp valSteer = connectedComps1 comp link
		where
			connectedComps1 compRef link = do
				comp <- nwGetComp compRef
				if isJust comp
					then do
--						let links = nwCompLinks (fromJust comp)
--						liftM concat $ mapM linkComps $ concatMap flattenSome links
						let chanLinks = nwCompLinks (fromJust comp)
						let compType =  nwTeakType (fromJust comp)
						let val = head valSteer
				
						--return just output links
						case (compType, chanLinks) of
							(TeakR, [One out]) -> liftM concat $ mapM linkComps [out]
							(TeakI, [One inp, One out]) -> liftM concat $ mapM linkComps [out]
							(TeakM, [Many inp, One out]) -> liftM concat $ mapM linkComps [out]
							(TeakS _ matches, [One inp, Many out]) -> liftM concat $ mapM linkComps $ takeOne val matches out
							(TeakF _, [One inp, Many out]) -> liftM concat $ mapM linkComps out						
							(TeakJ, [Many inp, One out]) -> liftM concat $ mapM linkComps [out]	
							(TeakA, [Many inp, One out]) -> liftM concat $ mapM linkComps [out]
							(TeakO _, [One inp, One out]) -> liftM concat $ mapM linkComps [out]				
							(TeakV _ _ _ _ _, [Many wg, Many wd, Many rg, Many rd]) -> liftM concat $ mapM linkCompsV [link]
					else return []
			
			--this fucntion select only one out link based on the value of valSteer which is coming from a teakO
			takeOne val matches outs = do
	
				let 	matches' = concat $ map (\(xs,_) -> xs) matches									
					index = findIndex (\(Imp value dcs) -> value == val) matches'
				if index == Nothing then outs
				else [outs !! (fromJust index)]

						 
			-- gets the link and returns the associated component in form of (component, link)
			linkComps link = do
				pas <- nwLinkToComp Passive link
				return $ map ((\x -> tryPart part $ attachLink x link) . fst) $ catMaybes [pas]

				where
					attachLink newComp upLink = do 	
					
						return $ (\x -> (x,upLink)) $ refComp newComp

			-- this function is to detect the right component next to the variable 
			linkCompsV link = do
				pas <- nwLinkToComp Passive link --thiscomp
				return $ map ((\x -> tryPart part $ nexToVar x link) . fst) $ catMaybes [pas]

				where
					-- this fucntion simply skipps the Variables and considers them a part of data flow rather than control flow
					nexToVar thisComp link = do 	
					
						let 	(TeakComp compNo teakTyp links _) = thisComp
							[Many wg, Many wd, Many rg, Many rd] = links

						if (head $ wg) == link then do 
									
							let wd' = head wd
	
							pp <- nwLinkToComp Passive wd'

							if pp == Nothing then 
								return $ (\x -> (x,wd')) $ refComp $ getLastComp part
							else	return $ (\x -> (x,wd')) $ refComp $ fst $ fromJust pp

						else do
			
							let 	rgNo = elemIndex link rg
							when (isNothing rgNo) $
								error $ "Read go link is not pointing at the variable with read go list of " ++ show rg	
							let 	rd' = rd !! (fromJust rgNo)							
			
							pp <- nwLinkToComp Passive $ rd' 

							if pp == Nothing then
								return $ (\x -> (x,rd')) $ refComp $ getLastComp part
							else	return $ (\x -> (x,rd')) $ refComp $ fst $ fromJust pp

	rmDup ::  [NetworkCompRef] -> [NetworkCompRef]
	rmDup [] = []
	rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)


	makePartFSM :: NetworkIF network => Part network -> ([(NetworkCompRef,Int)], FSM)
	makePartFSM part = runMkFSMonad0 $ body [] [] compSet
		where 
			tempLink = NetParts.Link {nwLink = 0}
			compSet = (fstCompRef,fstCompRef,tempLink) --each component with its predecessor component, this will help us to detect if a comp is already marked by a fork 	
			fstCompRef = refComp $ getFirstComp part -- who is 1st comp? the one that is connected to the go signal!
			lstCompRef = refComp $ getLastComp part	


			body valForSteer visited compS
					| partialVisited lstCompRef compS visited part = return visited
					| otherwise = do
						
						let 	
							(topComp,compref,upLink) = compS
							valForSteer' = constTeakO part compref valForSteer
 							newCompRefs' =  tryPart part $ getNextComps part upLink compref valForSteer'
							valForSteer'' = consumeConstTeakO part upLink compref valForSteer'
						--FIXME rmDup temproraly removed.
							newCompRefs = map (\(a,b) -> (compref,a,b)) newCompRefs'		
						
						fsmAddSt $ fromJust $ visitComp part upLink $ tryPart part $ refToComp compref

						 --before passing visited I should inserted the corresponding components in the list
						let 	newVisited = foldl (markJoin part) visited newCompRefs'		
		
						foldM (body valForSteer'') newVisited newCompRefs
				
			refToComp compref = do
				
				comp <- nwGetComp compref
				when (isNothing comp) $
					error $ "nwLinkToCompUsage: no component at compNo " ++ show comp
				return $ fromJust comp
					

			consumeConstTeakO part upLink thisComp valSteer = do

				let (TeakComp _ teakTyp _ _) = fromJust $ tryPart part $ nwGetComp thisComp
				case (teakTyp) of

				--	(TeakV _ _ _ _ _) -> do
				--			let width = tryPart part $ nwGetLinkWidth upLink
				--			if width == 0 	then valSteer
				--					else drop 1 valSteer						
	
					(TeakS _ _) -> drop 1 valSteer
					(_) ->  valSteer			
		

			constTeakO part thisComp valSteer =  do
				
				let (TeakComp _ teakTyp links _) = fromJust $ tryPart part $ nwGetComp thisComp
				case (teakTyp) of

					(TeakO terms) -> case last terms of

								(_, TeakOConstant _ value) -> value:valSteer
								(_) -> valSteer --if it is not a constant then drop the previous value

					(_) -> valSteer
					
			partialVisited lstComp compS visited part = fromMaybe False $  do


				let	(topComp, thisComp , _) = compS
					(TeakComp _ teakTyp _ _) = fromJust $ tryPart part $ nwGetComp thisComp
					(TeakComp _ topTyp _ _) = fromJust $ tryPart part $ nwGetComp topComp

				if lstComp == thisComp then do return True
				
				else do
					case (teakTyp) of
				
						(TeakJ) -> do

							if hasPortInput thisComp part then do
									return False
								-- yes it does have inputports and iteration is allowed to continue
							else do
								case (topTyp) of
									
									--(TeakF _) -> do  return True

									--	if exactFullyVisited thisComp visited part then do
									--		return False
									--	else 	return True	

									(_) -> do 
										return $ not $ fullyVisited thisComp visited part
										
						(_) -> do return False

			markJoin part visited thisComp = do

				let 	(thisComp', _) = thisComp
					(TeakComp compNo teakTyp links _) = fromJust $ tryPart part $ nwGetComp thisComp'
			
				case (teakTyp) of
	
					(TeakJ) -> do

						if notMember thisComp' visited then do				
							insertIn thisComp' 1 visited							

						else do
							let Just markValue = lookUp thisComp' visited
							insertIn thisComp' ((+) 1 markValue) $ deleteOut thisComp' visited
							
					(_) -> visited	
 		
			notMember thisComp visited = notElem thisComp visited'
					where 
						visited' = map (\(a,b) -> a) visited
					
			insertIn thisComp mark visited = (thisComp,mark):visited

			lookUp thisComp visited = lookup thisComp visited			

			deleteOut thisComp visited = do
					let	visited' = map (\(a,b) -> a) visited
						index = elemIndex thisComp visited'
					if index == Nothing then visited
					else f (fromJust index) visited
						where
							f index visited = take index visited ++ (f index . drop (index+1)) visited	


			fullyVisited thisComp visited part = fromMaybe False $ do
					
				let (TeakComp compNo teakTyp [Many inp, One out] _) = fromJust $ tryPart part $ nwGetComp thisComp 
		
				exist <- lookUp thisComp visited

				if (length inp) == exist then do 
						let refined = visited
-- DM.delete thisComp visited
						return True  
--DM.insert thisComp 0 $
				else do		return False 

			hasPortInput thisComp part = fromMaybe False $ do

				let (TeakComp compNo teakTyp [Many inp, One out] _) = fromJust $ tryPart part $ nwGetComp thisComp 			

				let checked = tryPart part $ mapM nwLinkIsPort inp
				 
				return $ elem True checked 

