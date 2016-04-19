{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.ProcessSpec where

import           Control.Monad.Except      (runExcept)
import qualified Data.Vector               as U
import           Test.Hspec

import qualified Language.Go.Syntax.AST    as Go
import qualified Language.SimpleGo.AST     as S
import qualified Language.SimpleGo.Helpers as Helpers
import qualified Language.SimpleGo.Process as Process


shouldParseDecl :: String -> [S.Declaration] -> Expectation
source `shouldParseDecl` ds = do
  let processed = Helpers.parseDecl source >>= (runExcept . Process.compileDecl)
  processed `shouldBe` (Right $ U.fromList ds)

spec :: Spec
spec = do
  describe "compile" $
    it "should compile the trivial program" $ do
      let p = Go.GoSource (Go.GoId "main") [] []
      runExcept (Process.compile p) `shouldBe` (Right $ S.Program U.empty)

  describe "compileDecl" $ do
    describe "const decls" $ do
      it "should compile a trivial single const" $
        "const a uint32 = 4" `shouldParseDecl` [
          S.Const (S.Id "a") (S.TypeName (S.Id "uint32")) (S.Prim (S.LitInt 4))
          ]
      it "should substitute iota" $
        "const (a int = iota \n b int = iota)" `shouldParseDecl` [
              S.Const (S.Id "a") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 0)),
              S.Const (S.Id "b") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 1))
              ]
      it "should repeat the last expression" $
        "const (a int = iota \n b)" `shouldParseDecl` [
              S.Const (S.Id "a") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 0)),
              S.Const (S.Id "b") (S.TypeName (S.Id "int")) (S.Prim (S.LitInt 1))
              ]

    describe "type decls" $ do
      it "should compile a type alias" $
        "type a byte" `shouldParseDecl` [S.Type (S.Id "a") (S.TypeName (S.Id "byte"))]

      it "should compile a struct" $
        "type a struct { \n b bool \n }" `shouldParseDecl` [
        S.Type (S.Id "a") (S.Struct [(S.Id "b", S.TypeName (S.Id "bool"))])
        ]
