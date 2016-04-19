{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.BalsaSpec where

import qualified Language.SimpleGo.AST                as S
import qualified Language.SimpleGo.Balsa              as B
import qualified Language.SimpleGo.Balsa.Declarations as D
import qualified ParseTree                            as PT

import           Test.Hspec

spec :: Spec
spec = do
  describe "balsaType" $ do
    it "should convert a struct" $ do
      let struct = S.Struct [(S.Id "b", S.TypeName (S.Id "bool"))]
      t <- B.runTranslateT $ B.typeDecl struct
      t `shouldBe` (Right $ D.Type $ PT.RecordType D.pos [PT.RecordElem D.pos "b" (PT.NameType D.pos "bool")] PT.NoType)
