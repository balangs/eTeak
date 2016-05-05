{-# LANGUAGE OverloadedStrings #-}

-- |

module Language.SimpleGo.Eval (
  eval,
  IntegralType(..), FloatType(..), Result(..)
  ) where

import qualified Data.Text             as T
import           Language.SimpleGo.AST

data IntegralType = Int8
                  | Int16
                  | Int32
                  | Int64
                  | Uint8
                  | Uint16
                  | Uint32
                  | Uint64
                  | GoInt
                  | GoUint
                  deriving (Eq, Show)

data FloatType = GoFloat
               | GoDouble
               deriving (Eq, Show)

data Result = IntegralR !IntegralType !Integer
            | FloatR !FloatType !Float
            | CharR !Char
            | StringR !T.Text


eval :: Expr -> Maybe Result
eval (Prim p) = evalPrim p
eval Zero = Nothing
eval _ = Nothing

evalPrim :: Prim -> Maybe Result
evalPrim (LitInt i) = return $ IntegralR GoInt i
evalPrim (LitReal f) = return $ FloatR GoFloat f
evalPrim (LitChar c) = return $ CharR c
evalPrim (LitStr t) = return $ StringR t
evalPrim (Call (Qual Nothing (Id "byte")) [e] _) = do
  a <- eval e
  case a of
    IntegralR _ i -> return $ IntegralR Uint8 i
    _ -> Nothing
evalPrim _ = Nothing
