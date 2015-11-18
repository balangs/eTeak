import Distribution.Simple
import Distribution.Simple.Program.Types
import System.Process
import System.Exit

manualProgs = ["make","ps2pdf","ps2epsi","lout","convert","dot"]

main = defaultMainWithHooks $ autoconfUserHooks {
         postBuild = makeManual,
         hookedPrograms =  map simpleProgram manualProgs,
         postClean = makeClean
       }

makeManual _ _ _ _ =
    runCommand "make" >>= waitForProcess >>= exitWith

makeClean _ _ _ _ =
    runCommand "make clean" >>= waitForProcess >>= exitWith
