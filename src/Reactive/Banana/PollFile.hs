module Reactive.Banana.PollFile where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Timer


pollFile interval file = do
    (contentsE, onContents) <- newEvent
    
    tickE <- timer interval
    reactimate ((readFile file >>= onContents) <$ tickE)
    
    return contentsE

