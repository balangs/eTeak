import Distribution.Simple
import Distribution.Simple.Program.Types
import System.Process
import System.Exit

manualProgs = ["make","ps2pdf","ps2epsi","lout","convert","dot"]

main = defaultMainWithHooks $ autoconfUserHooks {
         postBuild = makeManual,
         postInst = installManual,
         hookedPrograms =  map simpleProgram manualProgs,
         postClean = cleanManual
       }

makeManual _ _ _ _ =
    runCommand "make" >>= waitForProcess >>= exitWith

cleanManual _ _ _ _ =
    runCommand "make clean" >>= waitForProcess >>= exitWith

installManual _ _ _ _ =
    runCommand "make install" >>= waitForProcess >>= exitWith
