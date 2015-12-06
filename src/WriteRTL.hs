{-
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
-}

module WriteRTL (

	writeRTL,
	transStToExpr

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
	import VeriOpt

	import qualified Data.Map as DM
	import Data.List
	import Data.Bits
	import Control.Monad.State
	import Data.Maybe
	import Data.Char
	import System.IO

	transStToExpr :: StNo -> Sts -> FSM-> String
	transStToExpr stNo st fsm  = do

				let
					typ = compLabel st
					chanLinks = links st 
					inpW = inpWidths st
					outW = outWidths st
					outSlc = outSlice st
					impS = imps st

				case (typ , chanLinks) of
					(TeakJ, [Many inp, One out]) -> (showConcat stNo inp out inpW) $ ""
					(TeakF offset, [One inp, Many out]) -> (showFork stNo offset inp out inpW outW) $ ""
					(TeakM, [One  inp, One out]) -> (showChoice stNo inp out ) $ ""
					(TeakV str _ _ _ _, [One upLink, Many wg, Many wd, Many rg, Many rd]) -> (showVar stNo str upLink wg wd rg rd inpW outW) $ ""
					(TeakS _ _, [One inp, Many out]) -> (showSteer stNo inp out outSlc impS) $ ""
					(TeakO terms , [One inp, One out]) ->  (showTerms stNo inp out terms ) $ "" 
					(_) ->  ""
				where 					
	
					showTerms stNo inp out terms = (showString $ intercalate " " $ map (\x -> showTerm x "") terms ) . showNextSt out
						where 
							showNextSt out = showString "ns <= " . (shows $ stLinkToSt stNo out fsm)  . showString ";"

							showTerm (i, term) = case term of

								TeakOConstant width value -> shows stNo .
									showString ": ". showString "temp". showString " = ". shows value. showString ";" 

								TeakOAppend count slices -> shows out . showString " = " . 
									wrapBracket (intercalate "," $ map (\(i,slice) -> printName i. verilogShowSlice slice $ "") slices) .
										 showString ""
									where
										wrapBracket str = showString "{" . showString str . showString "};"
										printName i = if i == 0 then shows inp else showString "temp"

								_ -> shows "error" 

					showFork stNo offset inp out inpW outW = 
					--	if head inpW == 0 then showString ""
						 shows stNo . showString ": " . showImp inp out . showNextSt stNo 
							where  								
											
								showNextSt stNo = showString "ns <= " . shows (stNo + 1) . showString ";"

								showImp inp out = showString $ intercalate " " $ 
										map (\(x,(y,z)) -> (show x ++ " = " ++ show y ++ verilogShowSlice z "" ++ ";" ) ) imps
								slice = zipWith (+:) offset outW
								inp' = map (\x -> (inp,x)) slice									
								imps = zip out $ filter (\(x,y) -> not $ isEmptySlice y) inp'
	
							--FIXME remove the links that have 0 width because they are not data links 
							--	zeroWs = elemIndices 0 outW
							--	out' = filter zeroWs out
							
	
					showVar stNo str link wg wd rg rd inpW outW = 

						if (head $ wg) == link then 
							shows stNo . showString ": " . showString str . showString " = " . shows link . showString ";"
						else  
							shows stNo . showString ": " . shows rd' . showString " = " . showString str . showString ";"
								where
									rd' = rd !! (fromJust $ elemIndex link rg)


					showConcat stNo inp out inpW = shows stNo .
							showString ": " . shows out . showString " = " . showChar '{' . (showLinks inp) . showString "};" . showNextSt out
						where
							showNextSt out = showString "ns <= " . (shows $  stLinkToSt stNo out fsm) . showString ";" 	
							showLinks inp = showString $ intercalate "," $ map (\(x,y) -> show x ) inp' 
								where 
								--	inp' = filter (\(x,y) -> y /= 0) $ zip inp inpW
									inp' = zip inp inpW --FIXME I have to remove the links with 0 width

					showChoice stNo inp out = shows stNo . showString ": " . shows out . showString " = " . shows inp . showString ";" . showNextSt out

						where 
							showNextSt out = showString "ns <= " . (shows $  stLinkToSt stNo out fsm) . showString ";"

					showSteer stNo inp out outSlc impS = shows stNo . showString ": " . showString "case (" . shows inp . showString "):" . 
									showString "\n" . showLinks out inp impS
						where 

						showLinks out inp impS = showString $ intercalate "\n" $ map (\x -> takeImps x inp "") $ zip impS out	
							 
							where 
							takeImps (imp',out') inp = showString $ intercalate "\n" $ map (\x -> printImp x out' inp "") imp'

								where 
								printImp (Imp choice _) out' inp= showString "\t". shows choice . showString ": " .
									 showString "ns <= " .(shows $  stLinkToSt stNo out' fsm) . showString ";" 								

		


	writeRTL :: FilePath -> String -> FSM -> IO ()
	writeRTL file _partName fsm = do
		handle <- openFile file WriteMode
		hPutStr handle "module "
		hPutStr handle _partName
		hPutStrLn handle " (  ); "
		hPutStrLn handle ""
		hPutStrLn handle "always@(posedge clk) begin"
		hPutStrLn handle ""
		hPutStrLn handle "case(state)"

		mapM_ (printSt handle) $ DM.assocs $ fsmStsS fsm

		hPutStrLn handle "default : $display();" 	
		hPutStrLn handle "endcase"
		hPutStrLn handle ""
		hPutStrLn handle "end"
		hPutStr handle "endmodule"
			where 	
				printSt handle (stNo,st) = do
				let
					typ = compLabel st
					chanLinks = links st 
					inpW = inpWidths st
					outW = outWidths st
					outSlc = outSlice st
				 	impS = imps st


				case (typ , chanLinks) of
					(TeakJ, [Many inp, One out]) -> hPutStrLn handle $ (showConcat stNo inp out inpW) $ ""
					(TeakF offset, [One inp, Many out]) -> hPutStrLn handle $ (showFork stNo offset inp out inpW outW) $ ""
					(TeakM, [One  inp, One out]) -> hPutStrLn handle $ (showChoice stNo inp out ) $ ""
					(TeakV str _ _ _ _, [One upLink, Many wg, Many wd, Many rg, Many rd]) -> hPutStrLn handle $ 
												(showVar stNo str upLink wg wd rg rd inpW outW) $ ""
					(TeakS _ _, [One inp, Many out]) -> hPutStrLn handle $ (showSteer stNo inp out outSlc impS) $ ""
					(TeakO terms , [One inp, One out]) -> hPutStrLn handle $ (showTerms stNo inp out terms ) $ "" 
					(_) -> hPutStr handle ""
				where 					
	
					showTerms stNo inp out terms = (showString $ intercalate " " $ map (\x -> showTerm x "") terms ) . showNextSt out
						where 
							showNextSt out = showString "ns <= " . (shows $  stLinkToSt stNo out fsm) . showString ";"

							showTerm (i, term) = case term of

								TeakOConstant width value -> shows stNo .
									showString ": ". showString "temp". showString " = ". shows value. showString ";" 

								TeakOAppend count slices -> shows out . showString " = " . 
									wrapBracket (intercalate "," $ map (\(i,slice) -> printName i. verilogShowSlice slice $ "") slices) .
										 showString ""
									where
										wrapBracket str = showString "{" . showString str . showString "};"
										printName i = if i == 0 then shows inp else showString "temp"

								_ -> shows "error" 

					showFork stNo offset inp out inpW outW = 
						if head inpW == 0 then showString ""
						else shows stNo . showString ": " . showImp inp out . showNextSt stNo 
							where  								
											
								showNextSt stNo = showString "ns <= " . shows (stNo + 1) . showString ";"

								showImp inp out = showString $ intercalate " " $ 
										map (\(x,(y,z)) -> (show x ++ " = " ++ show y ++ verilogShowSlice z "" ++ ";" ) ) imps
								slice = zipWith (+:) offset outW
								inp' = map (\x -> (inp,x)) slice									
								imps = zip out $ filter (\(x,y) -> not $ isEmptySlice y) inp'
	
							--FIXME remove the links that have 0 width because they are not data links 
							--	zeroWs = elemIndices 0 outW
							--	out' = filter zeroWs out
							
	
					showVar stNo str link wg wd rg rd inpW outW = 

						if (head $ wg) == link then 
							shows stNo . showString ": " . showString str . showString " = " . shows link . showString ";"
						else  
							shows stNo . showString ": " . shows rd' . showString " = " . showString str . showString ";"
								where
									rd' = rd !! (fromJust $ elemIndex link rg)


					showConcat stNo inp out inpW = shows stNo .
							showString ": " . shows out . showString " = " . showChar '{' . (showLinks inp) . showString "};" . showNextSt out
						where
							showNextSt out = showString "ns <= " . (shows $  stLinkToSt stNo out fsm) . showString ";" 	
							showLinks inp = showString $ intercalate "," $ map (\(x,y) -> show x ) inp'
								where
									inp' = filter (\(x,y) -> y /= 0) $ zip inp inpW


					showChoice stNo inp out = shows stNo . showString ": " . shows out . showString " = " . shows inp . showString ";" . showNextSt out

						where 
							showNextSt out = showString "ns <= " . (shows $  stLinkToSt stNo out fsm) . showString ";"

					showSteer stNo inp out outSlc impS = shows stNo . showString ": " . showString "case (" . shows inp . showString "):" . 
									showString "\n" . showLinks out inp impS
						where 

						showLinks out inp impS = showString $ intercalate "\n" $ map (\x -> takeImps x inp "") $ zip impS out	
							 
							where 
							takeImps (imp',out') inp = showString $ intercalate "\n" $ map (\x -> printImp x out' inp "") imp'

								where 
								printImp (Imp choice _) out' inp= showString "\t". shows choice . showString ": " .
									 showString "ns <= " . (shows $  stLinkToSt stNo out' fsm) . showString ";" 								

