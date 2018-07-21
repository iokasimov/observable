module Control.Observable.Usage.Heartbeat (alive) where

import "base" Control.Applicative ((*>), pure)
import "base" Control.Concurrent (threadDelay)
import "base" Control.Monad ((>>=))
import "base" Data.Either (either)
import "base" Data.Function (($), (.), const)
import "base" Data.Int (Int)
import "base" System.IO (IO)
import "async" Control.Concurrent.Async (race)

import Control.Observable (Observable, obs, notify)

data Beaten a = Alive a | Dead

-- | Listen event from action untill time limit is up
alive :: Int -> Observable IO a r -> (a -> IO r) -> IO (Beaten r)
alive limit r f = race (threadDelay limit) (notify r f)
	>>= either (const . pure $ Dead) ((*>) (alive limit r f) . pure . Alive)
