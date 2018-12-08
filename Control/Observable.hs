module Control.Observable (Observable, (*=>), dispatch, subscribe, notify, obs, bypass) where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Monad (Monad, forever)
import "base" Data.Function (($), (.), flip)
import "base" Data.Traversable (Traversable (traverse))
import "transformers" Control.Monad.Trans.Cont (ContT (..))
import "transformers" Control.Monad.Trans.Class (lift)

newtype Capture r f a = Capture { captured :: f r }

-- | Capture used here as delimiter
type Observable f a r = ContT r (Capture r f) a

-- | Make continuation to be observable
dispatch :: ContT r f a -> Observable f a r
dispatch f = ContT $ \h -> Capture $ runContT f (captured . h)

-- | Make monadic action to be observable
obs :: Monad f => f a -> Observable f a r
obs action = dispatch $ lift action

-- | Listen all events from action, just once
subscribe :: Applicative f => Observable f a r -> (a -> f r) -> f r
subscribe r f = forever $ captured $ runContT r (Capture . f)

-- | Listen only first event, just once
notify :: Observable f a r -> (a -> f r) -> f r
notify r f = captured $ runContT r (Capture . f)

-- | Listen only first event, forever
uprise :: Applicative f => Observable f a r -> (a -> f r) -> f r
uprise r f = captured $ runContT r (Capture . forever . f)

-- | Looks like a bind, but it has another semantics
(*=>) :: Monad f => f a -> (a -> f r) -> f r
action *=> handler = subscribe (obs action) handler

-- | Yield all a over some t with callback function
bypass :: (Monad f, Traversable t) => t a -> (a -> f r) -> f (t r)
bypass xs h = traverse (flip notify h . dispatch . pure) xs
