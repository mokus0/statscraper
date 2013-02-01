module Reactive.Banana.Stats.IfStat
    ( ifstat
    ) where

import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Process

data IfStatLine
    = Interfaces [String]
    | Units [(String, String)]
    | Data [Float]
    deriving (Eq, Show)

outLine :: [String] -> IfStatLine
outLine ws = case mapM tryRead ws of
    Nothing -> case asUnits ws of
        Nothing -> Interfaces ws
        Just us -> Units      us
    Just ds -> Data ds

asUnits :: [String] -> Maybe [(String, String)]
asUnits (i:"in":o:"out":rest) = fmap ((i, o) :) (asUnits rest)
asUnits [] = Just []
asUnits _ = Nothing

data IfStatState = IfStatState
    { ifs       :: [String]
    , units     :: [(String, String)]
    , values    :: Maybe [Float]
    } deriving (Eq, Show)

initState = IfStatState [] [] Nothing

accumState (Interfaces is) state = state {values = Nothing, ifs = is}
accumState (Units      us) state = state {values = Nothing, units = us}
accumState (Data       vs) state = state {values = Just vs}

emit state = do
    ds <- values state
    let buckets = 
            [ (i, u) -- TODO: normalize units
            | (i, us) <- zip (ifs state) (units state)
            , u <- ["in", "out"]
            ]
    
    guard (length buckets == length ds)
    
    return (zip buckets ds)

ifstat :: Frameworks t => Int -> Moment t (Event t [((String, String), Float)])
ifstat interval = do
    (outE, errE, _excE, _eofE) <- monitorProcess "ifstat" [show interval]
    let stateE = accumE initState (accumState . outLine . words <$> outE)
    
    return (filterJust (emit <$> stateE))

-- general utils

tryRead :: Read a => String -> Maybe a
tryRead s = case reads s of
    (x, "") : _ -> Just x
    _           -> Nothing
