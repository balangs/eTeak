{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.ProcessSpec where

import           Control.Monad.Except      (runExcept)
import qualified Data.Vector               as U
import qualified Language.Go.Syntax.AST    as Go
import qualified Language.SimpleGo.AST     as S
import qualified Language.SimpleGo.Helpers as Helpers
import qualified Language.SimpleGo.Process as Process
import           Test.Hspec



spec :: Spec
spec = do
  describe "compile" $ do
    it "should compile the trivial program" $ do
      let p = Go.GoSource (Go.GoId "main") [] []
      runExcept (Process.compile p) `shouldBe` (Right $ S.Program U.empty)
  describe "compileDecl" $ do
    it "should compile a trivial single const" $ do
      let Right a = Helpers.parseDecl "const a uint32 = 4"
          expected = U.fromList [
            S.Const (S.Id "a") (S.TypeName (S.Id "uint32")) (S.Prim (S.LitInt 4))
            ]
      runExcept (Process.compileDecl a) `shouldBe` Right expected
    it "should substitute iota" $ do
      let Right a = Helpers.parseDecl "const (a int = iota \n b int = iota)"
          expected = U.fromList [
            S.Const (S.Id "a") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 0)),
            S.Const (S.Id "b") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 1))
            ]
      runExcept (Process.compileDecl a) `shouldBe` Right expected
    it "should repeat the last expression" $ do
      let Right a = Helpers.parseDecl "const (a int = iota \n b)"
          expected = U.fromList [
            S.Const (S.Id "a") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 0)),
            S.Const (S.Id "b") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 1))
            ]
      runExcept (Process.compileDecl a) `shouldBe` Right expected
