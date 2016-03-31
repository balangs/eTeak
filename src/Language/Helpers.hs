-- |

module Language.Helpers
       (bind, eval, finish)
       where

import qualified Bind                 as B
import qualified Context              as C
import           Control.Monad.Except (ExceptT, throwError)
import qualified Eval                 as E
import qualified Finish               as F
import qualified ParseTree            as PT
import           Report               (Completeness (..), Why (..))

type Context = C.Context PT.Decl

whyToExcept :: Monad m => (a -> Why b) -> a -> ExceptT String m b
whyToExcept f a = case c of
  Wrong rs -> throwError $ show rs
  _ -> return b
  where
    Why c b = f a

bind :: Monad m => Context -> ExceptT String m Context
bind = whyToExcept B.bind


eval :: Monad m => Context -> ExceptT String m Context
eval = whyToExcept E.evalContext

finish :: Monad m => Context -> ExceptT String m Context
finish = return . F.finish
