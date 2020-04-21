-- |
-- Module:      Reactive.Banana.Vty
-- Copyright:   (c) 2020 by Dr. Lars Brünjes
-- Licence:     MIT
-- Maintainer:  Dr. Lars Brünjes <brunjlar@gmail.com>
-- Stability:   Provisional
-- Portability: portable
--
-- This module provides functions to create simple TUI-applications
-- using vty and Reactive Banana.

{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Banana.Vty
    ( runVty
    , runVty'
    , runVtyWithTimer
    , runVtyWithTimer'
    ) where

import Control.Concurrent
import Control.Monad
import Data.Void                  (Void)
import Graphics.Vty               as V
import Reactive.Banana            as B
import Reactive.Banana.Frameworks as B

-- | Run a vty application using a custom event.
runVty :: AddHandler e                                 -- ^ An @'AddHandler'@ for the custom event.
       -> (   B.Event V.Event
           -> B.Event e 
           -> MomentIO (Behavior Picture, B.Event ())) -- ^ Function to create the desired behavior of pictures and shutdown event, given the vty-event and the custom event.
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

-- | Run a vty application.
runVty' :: (B.Event V.Event -> MomentIO (Behavior Picture, B.Event ())) -- ^ Function to create the desired behavior of pictures and shutdown event, given the vty-event.
        -> IO ()
runVty' describeNetwork = do
    (addVoid, _) <- newAddHandler :: IO (AddHandler Void, Handler Void)
    runVty addVoid $ \vtyE _ -> describeNetwork vtyE

-- | Run a vty application using a custom event and a tick event.
runVtyWithTimer :: forall e. AddHandler e 
                -> Int                                          -- ^ Time between ticks in microseconds.
                -> (   B.Event V.Event
                    -> B.Event ()
                    -> B.Event e 
                    -> MomentIO (Behavior Picture, B.Event ())) -- ^ Function to create the desired behavior of pictures and shutdown event, 
                                                                -- given the vty-event, tick-event and custom-event.
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

-- | Run a vty application using a tick event.
runVtyWithTimer' :: Int                                          -- ^ Time between ticks in microseconds.
                 -> (   B.Event V.Event
                     -> B.Event ()
                     -> MomentIO (Behavior Picture, B.Event ())) -- ^ Function to create the desired behavior of pictures and shutdown event, 
                                                                 -- given the vty-event and tick-event.
                 -> IO ()
runVtyWithTimer' interval describeNetwork = do
    (addVoid, _) <- newAddHandler :: IO (AddHandler Void, Handler Void)
    runVtyWithTimer addVoid interval $ \vtyE tickE _ -> describeNetwork vtyE tickE
