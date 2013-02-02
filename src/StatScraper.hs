{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Control.Concurrent
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.HostName
import Reactive.Banana.PollFile
import Reactive.Banana.Stats.IfStat
import Reactive.Banana.Stats.IOStat
import Reactive.Banana.Stats.StatsD
import Reactive.Banana.Stats.Uptime
import Network.StatsD

publishStats' statsd stat = do
    reactimate (mapM_ (putStrLn . showStat) <$> stat)
    publishStats statsd stat

main = do
    statsd <- openStatsD "stats.thecave.lan" "8125" ["system"]
    
    actuate =<< do
        compile $ do
            hostnameB <- fmap (fmap (takeWhile (/= '.'))) (hostname 300)
            
            uptimeE <- uptime 60
            publishStats' statsd (uptimeStats <$> hostnameB <@> uptimeE)
            
            ifstatE <- ifstat 5
            publishStats' statsd (ifStats <$> hostnameB <@> ifstatE)
            
            iostatE <- iostat 2
            publishStats' statsd (ioStats <$> hostnameB <@> iostatE)
            
    forever (threadDelay 10000000)

uptimeStats host uptime = 
    [ stat [host, "uptime"] (round (duration uptime)) "g" Nothing
    , stat [host, "users"]  (users           uptime)  "g" Nothing
    ]

ifStats host ifstats = 
    [ stat [host, "net", direction, interface] value "g" Nothing
    | ((interface, direction), value) <- ifstats
    ]

ioStats host iostats = 
    [ stat [host, sectionBucket section, subsection] value "g" Nothing
    | (section, subsection, value) <- iostats
    ] where
        sectionBucket CPU           = "cpu"
        sectionBucket Load          = "load"
        sectionBucket (Disk disk)   = "disk." ++ disk
