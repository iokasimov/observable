module Control.Observable (Observable, dispatch, subscribe, notify, obs) where

import Control.Monad (forever)
import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.Trans.Class (lift)

newtype Mock r f a = Mock { mock :: f r }

type Observable f a b = ContT b (Mock b f) a

dispatch :: ContT b f a -> Observable f a b
dispatch f = ContT $ \h -> Mock $ runContT f (mock . h)

obs :: Monad f => f a -> Observable f a b
obs action = dispatch (lift action)

subscribe :: Applicative f => Observable f a b -> (a -> f b) -> f b
subscribe r f = forever $ mock $ runContT r (Mock . f)

notify :: Observable f a b -> (a -> f b) -> f b
notify r f = mock $ runContT r (Mock . f)
