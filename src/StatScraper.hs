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
import Reactive.Banana.Timer
import Network.StatsD
import System.Environment
import System.Exit

publishStats' statsd stat = do
    reactimate (mapM_ (putStrLn . showStat) <$> stat)
    publishStats statsd stat

main = do
    args <- getArgs
    let duration = case args of 
            []  -> 300
            x:_ -> read x
    
    statsd <- openStatsD "stats.thecave.lan" "8125" ["system"]
    done <- newEmptyMVar
    
    actuate =<< do
        compile $ do
            timerE <- timer 1
            let timeE = accumE 0 ((+1) <$ timerE)
            reactimate (putMVar done () <$ filterE (> duration) timeE)
            
            hostnameB <- fmap (fmap (takeWhile (/= '.'))) (hostname 300)
            
            uptimeE <- uptime 60
            publishStats' statsd (uptimeStats <$> hostnameB <@> uptimeE)
            
            ifstatE <- ifstat 5
            publishStats' statsd (ifStats <$> hostnameB <@> ifstatE)
            
            iostatE <- iostat 2
            publishStats' statsd (ioStats <$> hostnameB <@> iostatE)
    
    takeMVar done

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
