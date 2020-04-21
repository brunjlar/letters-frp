{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import Control.Monad
import Graphics.Vty               as V
import Reactive.Banana            as B
import Reactive.Banana.Frameworks as B

main :: IO ()
main = do
    cfg                <- standardIOConfig
    vty                <- mkVty cfg
    (addHandler, fire) <- newAddHandler
    network            <- compile $ do
        vtyE   <- fromAddHandler addHandler
        countB <- describeNetwork vtyE
        countE <- changes countB
        reactimate' $ flip (fmap . fmap) countE $ \case
            Nothing -> shutdown vty
            Just c  -> render vty c
    actuate network
    go vty fire
  where
    go :: Vty -> (V.Event -> IO ()) -> IO ()
    go vty fire = do
        b  <- isShutdown vty
        unless b $ do
            me <- nextEventNonblocking vty
            case me of
                Nothing -> return ()
                Just e  -> fire e
            go vty fire

render :: Vty -> Int -> IO ()
render vty = update vty . picForImage . string defAttr . show

describeNetwork :: B.Event V.Event -> MomentIO (Behavior (Maybe Int))
describeNetwork vtyE = accumB (Just 0) $ f <$> vtyE
  where
    f :: V.Event -> Maybe Int -> Maybe Int
    f _                     Nothing  = Nothing
    f (EvKey KEsc        _) _        = Nothing 
    f (EvKey (KChar ' ') _) (Just c) = Just $ c + 1
    f _                     (Just c) = Just c
