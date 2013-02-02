module Reactive.Banana.Stats.IOStat.MacOS
    ( IOStatSection(..)
    , iostat
    ) where

import Data.Char
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Process

data IOStatSection
    = Disk String
    | CPU
    | Load
    deriving (Eq, Show)

getSections ("cpu"            : rest) = CPU       : getSections rest
getSections ("load":"average" : rest) = Load      : getSections rest
getSections (disk             : rest) = Disk disk : getSections rest
getSections [] = []

-- TODO: proper error propagation
groupSubSections [] = []
groupSubSections subSections
    = subSection : groupSubSections rest
    where 
        ~(subSection, rest) = splitAt 3 subSections

data IOStatLine
    = Sections [String]
    | SubSections [String]
    | Values [Float]
    deriving (Eq, Show)

outLine ws = case mapM tryRead ws of
    Nothing
        | "cpu" `elem` ws   -> Sections ws
        | otherwise         -> SubSections ws
    Just vs -> Values vs

data IOStatState = IOStatState
    { sections      :: [IOStatSection]
    , subSections   :: [[String]]
    , values        :: Maybe [Float]
    } deriving (Eq, Show)

initState = IOStatState [] [] Nothing

accumState (Sections    ss) state = state {values = Nothing, sections = getSections ss}
accumState (SubSections ss) state = state {values = Nothing, subSections = groupSubSections ss}
accumState (Values      vs) state = state {values = Just vs}

emit state = do
    vs <- values state
    let buckets = 
            [ (section, subSection) 
            | (section, subSections) <- zip (sections state) (subSections state)
            , subSection <- subSections
            ]
    
    return (zip buckets vs)

iostat interval = do
    (outE, errE, excE, eofE) <- monitorProcess "iostat" [show interval]
    let stateE = accumE initState (accumState . outLine . words <$> outE)
    return (filterJust (emit <$> stateE))

tryRead :: Read a => String -> Maybe a
tryRead s = case reads s of
    (x, "") : _ -> Just x
    _           -> Nothing
