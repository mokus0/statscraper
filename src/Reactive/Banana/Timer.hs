module Reactive.Banana.Timer where

import Control.Concurrent
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks

timer :: Frameworks t => Double -> Moment t (Event t ())
timer interval = do
    (tickE, onTick) <- newEvent
    
    -- todo: account for drift
    let wait = threadDelay (round (interval * 1000000))
    liftIOLater (() <$ forkIO (forever (onTick () >> wait)))
    
    return tickE

