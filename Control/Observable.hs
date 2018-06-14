module Control.Observable (Observable, dispatch, subscribe, notify, obs) where

import Control.Monad (forever)
import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.Trans.Class (lift)

newtype Mock r f a = Mock { mock :: f r }

-- | Mock used here as delimiter
type Observable f a r = ContT r (Mock r f) a

-- | Make continuation to be observable
dispatch :: ContT r f a -> Observable f a r
dispatch f = ContT $ \h -> Mock $ runContT f (mock . h)

-- | Make monadic action to be observable
obs :: Monad f => f a -> Observable f a r
obs action = dispatch (lift action)

-- | Listen all event from action, forever
subscribe :: Applicative f => Observable f a r -> (a -> f r) -> f r
subscribe r f = forever $ mock $ runContT r (Mock . f)

-- | Listen only first event, just once
notify :: Observable f a r -> (a -> f r) -> f r
notify r f = mock $ runContT r (Mock . f)
