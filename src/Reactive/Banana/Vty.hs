{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Banana.Vty
    ( runVty
    , runVtyWithTimer
    ) where

import Control.Concurrent
import Control.Monad
import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

runVty :: AddHandler e 
       -> (   B.Event V.Event 
           -> B.Event e 
           -> MomentIO (Behavior Picture, B.Event ())) 
       -> IO ()
runVty addHandler describeNetwork = do
    cfg                   <- standardIOConfig
    vty                   <- mkVty cfg
    (addVtyHandler, fire) <- newAddHandler
    v                     <- newEmptyMVar
    network               <- compile $ do
        vtyE          <- fromAddHandler addVtyHandler
        e             <- fromAddHandler addHandler
        (picB, quitE) <- describeNetwork vtyE e
        picE          <- changes picB
        reactimate' $ (fmap $ update vty)    <$> picE
        reactimate  $ (const $ putMVar v ()) <$> quitE
    tid <- forkIO $ forever $ nextEvent vty >>= fire
    actuate network
    readMVar v
    pause network
    killThread tid
    shutdown vty

runVtyWithTimer :: forall e. AddHandler e 
                -> Int
                -> (   B.Event V.Event 
                    -> B.Event ()
                    -> B.Event e 
                    -> MomentIO (Behavior Picture, B.Event ())) 
                -> IO ()
runVtyWithTimer addHandler interval describeNetwork = do
    (addTimer, tick) <- newAddHandler
    tid <- forkIO $ forever $ threadDelay interval >> tick ()
    runVty (addHandler' addTimer) describe
    killThread tid
  where
    addHandler' :: AddHandler () -> AddHandler (Either () e)
    addHandler' addTimer = AddHandler $ \h -> do
        unregisterTick <- register addTimer $ h . Left
        unregisterE    <- register addHandler $ h . Right
        return $ unregisterTick >> unregisterE

    describe :: B.Event V.Event
             -> B.Event (Either () e)
             -> MomentIO (Behavior Picture, B.Event ())
    describe vtyE e = do
        let (tickE, e') = split e
        describeNetwork vtyE tickE e'
