module Reactive.Banana.Stats.StatsD where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Network.StatsD

publishStat statsd = publishStats statsd . collect

publishStats statsd event = do
    reactimate (push statsd <$> event)