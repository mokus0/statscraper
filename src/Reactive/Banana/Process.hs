module Reactive.Banana.Process where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Timer
import System.IO
import System.Process

handleLinesE :: Frameworks t => Handle -> Moment t (Event t String, Event t SomeException, Event t ())
handleLinesE h = do
    liftIO (hSetBuffering h LineBuffering)
    (lineE, onLine) <- newEvent
    (excE,  onExc)  <- newEvent
    (eofE,  onEof)  <- newEvent
    
    let readLines = forever (hGetLine h >>= onLine)
    
    liftIOLater (() <$ forkIO (handle onExc readLines `finally` onEof ()))
    return (lineE, excE, eofE)

monitorProcess name args = do
    (input, output, err, ph) <- liftIO (runInteractiveProcess name args Nothing Nothing)
    liftIO (hClose input)
    
    (outE, outExc, outEof) <- handleLinesE output
    (errE, errExc, errEof) <- handleLinesE err
    
    let latch e = accumB False (const True <$ e)
        eof = liftA2 (&&) (latch outEof) (latch errEof)
    
    return (outE, errE, union outExc errExc, eof)

pollProcess interval name args = do
    (contentsE, onContents) <- newEvent
    
    tickE <- timer interval
    reactimate ((readProcess name args "" >>= onContents) <$ tickE)
    
    return contentsE
