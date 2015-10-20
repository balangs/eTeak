{-
	Teak synthesiser for the Balsa language
	Copyright (C) 2007-2010 The University of Manchester

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Andrew Bardsley <bardsley@cs.man.ac.uk> (and others, see AUTHORS)
	School of Computer Science, The University of Manchester
	Oxford Road, MANCHESTER, M13 9PL, UK
-}

module State (
	State,
	StateT (..),
	runStateT_,
	MaybeT (..),
	runState,
	runMaybeTD,
	liftPred,
	liftIfM,
	state,
	liftIf
	) where

	import Control.Monad.State
	import Data.Maybe

	runStateT_ :: Monad m => StateT b m a -> m a
	runStateT_ mon = liftM fst $ runStateT mon undefined

	newtype {- Monad m => -} MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

	instance Monad m => Monad (MaybeT m) where
		l >>= k = MaybeT $ runMaybeT l >>= maybe (return Nothing) (runMaybeT . k)
		return r = MaybeT $ return $ Just r
		fail _ = MaybeT $ return Nothing

	instance MonadTrans MaybeT where
		lift = MaybeT . liftM Just

	runMaybeTD :: Monad m => a -> MaybeT m a -> m a
	runMaybeTD def mon = liftM (fromMaybe def) $ runMaybeT mon

	liftPred :: (MonadTrans m, Monad n, Monad (m n)) => (a -> Bool) -> n a -> m n a
	liftPred f m = do
		r <- lift m
		if f r
			then return r
			else fail ""

	liftIfM :: (MonadTrans m, Monad n, Monad (m n)) => n Bool -> m n Bool
	liftIfM m = liftPred id m

	liftIf :: (MonadTrans m, Monad n, Monad (m n)) => Bool -> m n Bool
	liftIf = liftIfM . return
