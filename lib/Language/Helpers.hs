-- |

module Language.Helpers
       (bind, eval, finish, teak, writeTeak, writeGates)
       where

import qualified Bind                   as B
import qualified Context                as C
import           Control.Monad.Except   (ExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Eval                   as E
import qualified Finish                 as F
import           Gen                    (TechMapping, genMakeGatesFile)
import           NetParts               (Part, writeNetworkFile)
import qualified Network                as Net
import qualified ParseTree              as PT
import           Report                 (Completeness (..), Why (..), runWhyT)
import qualified Teak
import           ToolOptions            (ToolOptions, defaultToolOptions,
                                         optTech, teakFindTechMapping)

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
writeTeak f p = liftIO $ writeNetworkFile False "go-teak" f p

writeGates :: MonadIO m => String -> [Part Net.Network] -> ExceptT String m ()
writeGates f parts = do
  tech <- techMapping opts (optTech opts)
  liftIO $ genMakeGatesFile True [] f tech (f ++ ".v") parts
  where
    opts :: ToolOptions Net.Network
    opts = defaultToolOptions
    techMapping :: (MonadIO m) => ToolOptions Net.Network -> String -> ExceptT String m TechMapping
    techMapping a b = do
      w <- liftIO $ runWhyT $ teakFindTechMapping a b
      whyToExcept id w
