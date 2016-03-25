{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.ProcessSpec where

import           Control.Monad.Except      (runExcept)
import qualified Data.Vector               as U
import           Language.Go.Parser        (goParse)
import qualified Language.Go.Syntax.AST    as Go
import qualified Language.SimpleGo.AST     as S
import           Language.SimpleGo.Process (compile)
import           Test.Hspec

spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile the trivial program" $ do
      let p = Go.GoSource (Go.GoId "main") [] []
      runExcept (compile p) `shouldBe` (Right $ S.Program U.empty)
    it "should compile a single const" $ do
      let Right a = goParse "main.go" "package main; const a uint32 = 4"
          expected = S.Program $ U.fromList [
            S.Const (S.Id "a") (S.TypeName (S.Id "uint32")) (S.Prim (S.Literal (S.LitInt 4)))
                                            ]
      runExcept (compile a) `shouldBe` Right expected
