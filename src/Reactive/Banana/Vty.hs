module Reactive.Banana.Vty
    ( runVty
    ) where

import Control.Concurrent
import Control.Monad
import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

runVty :: AddHandler e 
       -> (   B.Event V.Event 
           -> B.Event e 
           -> Moment (Behavior Picture, B.Event ())) 
       -> IO ()
runVty addHandler describeNetwork = do
    cfg                   <- standardIOConfig
    vty                   <- mkVty cfg
    (addVtyHandler, fire) <- newAddHandler
    v                     <- newEmptyMVar
    network               <- compile $ do
        vtyE          <- fromAddHandler addVtyHandler
        e             <- fromAddHandler addHandler
        (picB, quitE) <- liftMoment $ describeNetwork vtyE e
        picE          <- changes picB
        reactimate' $ (fmap $ update vty)    <$> picE
        reactimate  $ (const $ putMVar v ()) <$> quitE
    tid <- forkIO $ forever $ nextEvent vty >>= fire
    actuate network
    readMVar v
    pause network
    killThread tid
    shutdown vty
