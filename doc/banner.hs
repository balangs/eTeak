import IO
import List
import Char
import Array
import System

justWS = all isSpace

headEq (a:_) (b:_) = a == b

extractFont ls = array (' ','z') [(c, space) | c <- [' '..'z']] // font
	where
		l = filter (not . justWS) ls
		height = length l - 1
		maxLen = maximum (map length l)
		space = replicate height "  "
		notSpace = not . isSpace . head
		font = map reformat $ groupBy headEq $ filter notSpace $ transpose $ map (pad maxLen) l

		reformat ls@((c:_):_) = (c, transpose $ map tail ls)

render font str = map concat $ transpose $ map (font !) str

pad maxLen str = str ++ replicate (maxLen - len) ' '
	where len = length str

main = do
	args <- getArgs

	c <- readFile "font"
	let
		font = extractFont $ lines c
		printBanner str = mapM_ putStrLn $ render font str

	mapM_ printBanner args
	
	return ()
