import NetParts
import Network
import Report
import Monad

printDotDotDot :: Show a => Int -> a -> IO ()
printDotDotDot n l = do
    let str = show l
    putStr $ take n str
    when (not (null (drop n str))) $ putStr " ..."
    putStr "\n"

main = do
    let
        readNetwork :: String -> WhyT IO [Part Network]
        readNetwork = readNetworkFile

    putStrLn "*** Trying example.teak"
    Why comp parts <- runWhyT $ readNetwork "example.teak"
    bad <- printCompleteness noPosContext comp
    when (not bad) $ printDotDotDot 100 parts

    putStrLn "*** Trying broken-example.teak"
    Why comp parts <- runWhyT $ readNetwork "broken-example.teak"
    bad <- printCompleteness noPosContext comp
    when (not bad) $ printDotDotDot 100 parts
