{-# LANGUAGE FlexibleContexts #-}
module Reactive.Banana.Stats.Uptime
    ( Uptime(..), LoadAverages(..)
    , uptime
    ) where

import Data.Time
import Reactive.Banana hiding (many, optional)
import Reactive.Banana.Frameworks
import Reactive.Banana.Process
import Text.Parsec
import Text.Parsec.Char

data Uptime = Uptime 
    { sampleTime    :: !TimeOfDay
    , duration      :: !DiffTime
    , users         :: !Int
    , load          :: !LoadAverages
    } deriving (Eq, Show)

data LoadAverages = LoadAverages !Float !Float !Float
    deriving (Eq, Show)

-- sample output from "uptime" command on a couple different systems
sample1 = "20:38  up 10 days, 22:45, 4 users, load averages: 0.84 0.65 0.61\n"
sample2 = " 17:23:09 up 71 days, 22:43,  5 users,  load average: 0.34, 0.15, 0.17\n"

uptime_ :: Stream s m Char => ParsecT s u m Uptime
uptime_ = Uptime 
    <$> (TimeOfDay <$> int <* char ':' <*> int <*> option 0 (char ':' *> int))
    <*  string " up "
    <*> (diffTime 
            <$> int <* string " days, "
            <*> int <* char ':' <*> int)
    <* string ", " <*> int 
    <* string " users, load average" <* optional (char 's') <* string ": "
    <*> loadAverages_
    where
        int :: (Stream s m Char, Num t) => ParsecT s u m t
        int = fromInteger . read <$> many digit
        
        diffTime d h m = secondsToDiffTime (((d * 24 + h) * 60 + m) * 60)

loadAverages_ :: Stream s m Char => ParsecT s u m LoadAverages
loadAverages_ = LoadAverages <$> float <* sep <*> float <* sep <*> float
    where
        float = read . concat <$> sequence [many digit, string ".", many digit]
        sep = optional (char ',') >> space

fixSpaces = unwords . words

parseUptime str = case parse uptime_ "uptime" (fixSpaces str) of
    Left err -> Nothing --Left (show err)
    Right x  -> Just x -- Right x

uptime :: Frameworks t => Double -> Moment t (Event t Uptime)
uptime interval = fmap (filterJust . fmap parseUptime) (pollProcess interval "uptime" [])
