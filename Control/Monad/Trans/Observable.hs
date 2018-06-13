module Control.Monad.Trans.Observable (Observable (..)) where

import Control.Monad.Trans.Cont (ContT (..))

newtype Mock r f a = Mock { mock :: f r }

newtype Observable f b a = Observable
	{ unobserved :: ContT b (Mock b f) a }

instance Functor (Observable f b) where
	fmap f (Observable m) = Observable $ f <$> m

instance Applicative (Observable f b) where
	pure x  = Observable $ pure x
	Observable f <*> Observable v = Observable $ f <*> v

instance Monad (Observable f b) where
	(Observable m) >>= k = Observable . ContT $
		\c -> runContT m $ \x -> runContT (unobserved $ k x) c
