module Reactive.Banana.Stats.IOStat.Linux
    ( iostat
    ) where

import Data.Char
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Process

data Section
    = CPU
    | Disks
    deriving (Eq, Show)

data Mode
    = Start
    | Waiting
    | InSection !Section
    | Finished  !Section
    deriving (Eq, Show)

data IOStatState = IOStatState
    { mode          :: Mode
    , headerLine    :: [String]
    , columns       :: [String]
    , rows          :: [[String]]
    } deriving (Eq, Show)

initState = IOStatState Start [] [] []

accumState [] state = case mode state of
    InSection s -> state { mode = Finished s }
    _           -> state { mode = Waiting }
accumState line@(first : rest) state = case mode state of
    Start       -> state { mode = Waiting, headerLine = line }
    InSection _ -> state { rows = line : rows state }
    _           -> case first of
        "avg-cpu:"  -> state { mode = InSection CPU,   columns = rest, rows = [] }
        "Device:"   -> state { mode = InSection Disks, columns = rest, rows = [] }

getFinishedSection (Finished s) = Just s
getFinishedSection _            = Nothing

emit state = do
    section <- getFinishedSection (mode state)
    return $ case section of
        -- TODO: not use head
        CPU     -> Left (zip (columns state) (map read (head (rows state))))
        Disks   -> Right 
            [ (disk, zip (columns state) (map read row))
            | (disk: row) <- rows state
            ]

iostat :: Frameworks t => Int -> Moment t (Event t [(String, Float)], Event t [(String, [(String, Float)])])
iostat interval = do
    (outE, errE, excE, eofE) <- monitorProcess "iostat" ["-N", show interval]
    let stateE = accumE initState (accumState . words <$> outE)
    
    return (split (filterJust (emit <$> stateE)))