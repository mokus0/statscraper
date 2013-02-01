module Reactive.Banana.HostName where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Process
import System.Process

hostname expiry = do
    -- TODO: this should be demand-driven, but cached for 'expiry' seconds
    initial     <- liftIO (readProcess "hostname" [] "")
    hostnameE   <- pollProcess expiry "hostname" []
    
    let hostnameB = stepper initial hostnameE
    return (takeWhile ('\n' /=) <$> hostnameB)
