-- |
-- Module     : Main.hs
-- License     : GPLv3 (see COPYING)
--
-- This module is the entry point for a Go to ?? compiler

module Main where

import           Control.Monad      (forM_)
import           Language.Transpile (transpileFile)
import           System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  forM_ files transpileFile
