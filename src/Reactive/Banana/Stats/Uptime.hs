{-# LANGUAGE FlexibleContexts #-}
module Reactive.Banana.Stats.Uptime
    ( Uptime(..), LoadAverages(..)
    , uptime
    ) where

import Data.Time
import Reactive.Banana hiding (many, optional, (<|>))
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
sample3 = " 12:41:19 up 46 min,  2 users,  load average: 0.00, 0.01, 0.05\n"
sample4 = " 18:55:22 up  6:49,  3 users,  load average: 0.03, 0.03, 0.05\n"

int :: (Stream s m Char, Num t) => ParsecT s u m t
int = fromInteger . read <$> many digit

diffTime :: (Stream s m Char) => ParsecT s u m DiffTime
diffTime =
    try (fromDHM
        <$> option 0 (try (int <* string " days, "))
        <*> int <* char ':' <*> int)
    <|> (fromDHM 0
        <$> int <* string " min"
        <*> pure 0)
    where
        fromDHM d h m = secondsToDiffTime (((d * 24 + h) * 60 + m) * 60)

uptime_ :: Stream s m Char => ParsecT s u m Uptime
uptime_ = Uptime 
    <$> (TimeOfDay <$> int <* char ':' <*> int <*> option 0 (char ':' *> int))
    <*  string " up " <*> diffTime
    <* string ", " <*> int 
    <* string " users, load average" <* optional (char 's') <* string ": "
    <*> loadAverages_

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
