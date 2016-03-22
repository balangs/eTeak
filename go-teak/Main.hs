-- |
-- Module     : Main.hs
-- License     : GPLv3 (see COPYING)
--
-- This module is the entry point for a Go to ?? compiler

module Main where

import           Control.Monad      (forM_)
import           Language.Go.Parser (goParse)
import           System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \f -> do
    s <- readFile f
    print f
    print $ goParse f s
