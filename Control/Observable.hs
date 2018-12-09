module Control.Observable
	( Observable, dispatch, obs
	, notify, chase, subscribe, watch
	, (.:~.), (.:~*), (*:~.), (*:~*)
	) where

import "base" Control.Applicative (Applicative (pure))
import "base" Control.Monad (Monad, forever)
import "base" Data.Function (($), (.), flip)
import "base" Data.Traversable (Traversable (traverse))
import "transformers" Control.Monad.Trans.Cont (ContT (..))
import "transformers" Control.Monad.Trans.Class (lift)

newtype Capture r f a = Capture { captured :: f r }

-- | Capture used here as limiter of continuation
type Observable f a r = ContT r (Capture r f) a

-- | Make continuation to be observable
dispatch :: ContT r f a -> Observable f a r
dispatch f = ContT $ \h -> Capture $ runContT f (captured . h)

-- | Make monadic action to be observable
obs :: Monad f => f a -> Observable f a r
obs action = dispatch $ lift action

-- | Listen only first event, call back just once
notify :: Observable f a r -> (a -> f r) -> f r
notify r f = captured $ runContT r (Capture . f)

-- | Infix version of 'notify'
(.:~.) :: Observable f a r -> (a -> f r) -> f r
(.:~.) = notify

-- | Listen only first event, call back forever
chase :: Applicative f => Observable f a r -> (a -> f r) -> f r
chase r f = captured $ runContT r (Capture . forever . f)

-- | Infix version of 'chase'
(.:~*) :: Applicative f => Observable f a r -> (a -> f r) -> f r
(.:~*) = chase

-- | Listen all events from action, call back just once
subscribe :: Applicative f => Observable f a r -> (a -> f r) -> f r
subscribe r f = forever $ captured $ runContT r (Capture . f)

-- | Infix version of 'subscribe'
(*:~.) :: Applicative f => Observable f a r -> (a -> f r) -> f r
(*:~.) = subscribe

-- | Listen all events from action, call back forever
watch :: Applicative f => Observable f a r -> (a -> f r) -> f r
watch r f = forever $ captured $ runContT r (Capture . forever . f)

-- | Infix version of 'watch'
(*:~*) :: Applicative f => Observable f a r -> (a -> f r) -> f r
(*:~*) = watch
