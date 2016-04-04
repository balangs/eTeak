-- |

module Language.Helpers
       (bind, eval, finish, teak, writeTeak)
       where

import qualified Bind                   as B
import qualified Context                as C
import           Control.Monad.Except   (ExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.IO.Class (liftIO)
import qualified Eval                   as E
import qualified Finish                 as F
import           NetParts               (Part, writeNetworkFile)
import qualified Network                as Net
import qualified ParseTree              as PT
import           Report                 (Completeness (..), Why (..))
import qualified Teak

type Context = C.Context PT.Decl

whyToExcept :: Monad m => (a -> Why b) -> a -> ExceptT String m b
whyToExcept f a = case c of
  Wrong rs -> throwError $ unlines $ map show rs
  _ -> return b
  where
    Why c b = f a

bind :: Monad m => Context -> ExceptT String m Context
bind = whyToExcept B.bind


eval :: Monad m => Context -> ExceptT String m Context
eval = whyToExcept E.evalContext

finish :: Monad m => Context -> ExceptT String m Context
finish = return . F.finish

teak :: Monad m => [Teak.TeakOption] -> Context -> ExceptT String m [Part Net.Network]
teak = whyToExcept . Teak.teak

writeTeak :: MonadIO m => String -> [Part Net.Network] -> ExceptT String m ()
writeTeak f p = liftIO $ writeNetworkFile True "go-teak" f p
