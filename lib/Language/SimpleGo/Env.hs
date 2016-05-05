-- |

module Language.SimpleGo.Env (
  Env, Error,
  lookup, insert,
  new,
  fresh, pop
  ) where

import           Data.Function (on)
import           Data.List     (sortBy)
import qualified Data.Map      as M
import qualified Data.Text     as T
import           Prelude       hiding (lookup)

-- Should really be a nonempty list here
-- A data type representing an environment of nested contexts
data Env a = Env [(Int, M.Map T.Text (Int, a))] deriving (Eq, Show)

data Error = AlreadyDeclared
           | EmptyEnv
           deriving (Eq, Show)

-- Lookup a given entry in the environment
lookup :: Env a -> T.Text -> Maybe a
lookup (Env envs) k = go $ map (M.lookup k . snd) envs
  where
    go [] = Nothing
    go (Just (_,a):_) = Just a
    go (_:as) = go as


-- Insert a new entry into the environment
insert :: Env a -> T.Text -> a -> Either Error (Env a)
insert (Env []) _ _ = Left EmptyEnv
insert (Env ((i, e):es)) k a= case M.lookup k e of
  Just _ -> Left AlreadyDeclared
  Nothing -> Right $ Env ((succ i, M.insert k (i, a) e):es)

-- Add a new context to the environment
fresh :: Env a -> Env a
fresh (Env es) = Env ((0, M.empty):es)

-- Remove the latests context
pop :: Env a -> Either Error ([(T.Text, a)], Env a)
pop (Env []) = Left EmptyEnv
pop (Env ((_, a):as)) = Right (o, Env as)
  where
    o = map output $ sortBy (compare `on` inserted) $ M.toList a
    output (k, (_,v)) = (k, v)
    inserted (_, (i, _)) = i

-- An initial environment
new :: Env a
new = Env [(0, M.empty)]
