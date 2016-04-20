{-# LANGUAGE OverloadedStrings #-}
-- |

module Language.SimpleGo.Monad (
  TranslateT, Msg(..),
  runTranslateT, unsupported, declare, popContext, newContext, notDefined, lookup, typeError, fresh
  ) where

import           Control.Monad.Except  (ExceptT (..), runExceptT, throwError)
import           Control.Monad.State   (StateT, evalStateT, gets, modify')
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Language.SimpleGo.Env as Env
import           Prelude               hiding (lookup)

data Msg = Unsupported String
         | EnvironmentError Env.Error
         | TypeError String String
         | Undefined String
         deriving (Show, Eq)


data TranslationState decl = TranslationState {
  env    :: Env.Env decl,
  idents :: Integer
} deriving (Show, Eq)

modifyEnv :: (Env.Env a -> Env.Env a) -> TranslationState a -> TranslationState a
modifyEnv f t = TranslationState{env=f $ env t}

def :: TranslationState decl
def = TranslationState {
  env = Env.new,
  idents = 0
  }

type TranslateT m decl = ExceptT Msg (StateT (TranslationState decl) m)

runTranslateT :: (Monad m) => TranslateT m decl a -> m (Either Msg a)
runTranslateT m =  evalStateT (runExceptT m) def

unsupported :: (Monad m, Show a) => String -> a -> TranslateT m decl b
unsupported construct a = throwError $ Unsupported $ "unsupported " ++ construct ++ " :" ++ show a

notDefined :: (Monad m) => String -> TranslateT m decl a
notDefined id' = throwError $ Undefined id'

typeError :: (Monad m) => String -> String -> TranslateT m decl a
typeError expected actual = throwError $ TypeError expected actual

declare :: (Monad m) => T.Text -> decl -> TranslateT m decl ()
declare t d = do
  env' <- gets env
  case Env.insert env' t d of
    Left err -> throwError $ EnvironmentError err
    Right e -> modify' $ modifyEnv (const e)

newContext :: (Monad m) => TranslateT m decl ()
newContext = modify' (modifyEnv Env.fresh)

lookup :: (Monad m) => T.Text -> TranslateT m decl (Maybe decl)
lookup t = do
  env' <- gets env
  return $ Env.lookup env' t

popContext :: (Monad m) => TranslateT m decl [(T.Text, decl)]
popContext = do
  env' <- gets env
  case Env.pop env' of
    Left err -> throwError $ EnvironmentError err
    Right (context, e)  -> do
      modify' $ modifyEnv (const e)
      return context

fresh :: (Monad m) => TranslateT m decl T.Text
fresh = do
  i <- gets idents
  modify' $ \t -> t{idents = succ i}
  return $ "$:" <> T.pack (show i)
