module Reactive.Banana.Stats.IOStat
    ( IOStatSection(..)
    , iostat
    ) where

import qualified Reactive.Banana.Stats.IOStat.Linux as Linux
import qualified Reactive.Banana.Stats.IOStat.MacOS as MacOS
import Reactive.Banana.Stats.IOStat.MacOS (IOStatSection(..))

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Process
import System.Exit

isLinuxLike = do
    (exit, _, _) <- readProcessWithExitCode "iostat" ["-N"] ""
    return (exit == ExitSuccess)

iostat :: Frameworks t => Int -> Moment t (Event t [(IOStatSection, String, Float)])
iostat interval = liftIO isLinuxLike >>= \linux ->
    if linux
        then do
            (cpuE, diskE) <- Linux.iostat interval
            
            let cpu  xs  = [ (CPU, col, row) | (col, row) <- xs]
                disk xss = [ (Disk disk, col, row) | (disk, xs) <- xss, (col, row) <- xs]
            
            return (union (cpu <$> cpuE) (disk <$> diskE))
        else do
            MacOS.iostat interval
