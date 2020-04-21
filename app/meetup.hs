module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad
import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

main :: IO ()
main = do
    done <- newEmptyMVar
    cfg  <- standardIOConfig
    vty  <- mkVty cfg
    (addHandler, fire) <- newAddHandler
    network <- compile $ do
        vtyE  <- fromAddHandler addHandler
        mpicB <- describeNetwork vtyE
        mpicE <- changes mpicB
        reactimate' $ (fmap $ fmap $ f vty done) mpicE
    actuate network
    void $ forkIO $ forever $ nextEvent vty >>= fire
    takeMVar done

  where
    f :: Vty -> MVar () -> Maybe Picture -> IO ()
    f vty done Nothing    = shutdown vty >> putMVar done ()
    f vty _    (Just pic) = update vty pic

describeNetwork :: B.Event V.Event -> MomentIO (Behavior (Maybe Picture))
describeNetwork vtyE = do
    countB <- accumB (Just 0) $ fmap g vtyE
    return $ fmap toPic <$> countB
  where
    g :: V.Event -> Maybe Int -> Maybe Int
    g _                     Nothing  = Nothing
    g (EvKey KEsc _)        _        = Nothing
    g (EvKey (KChar ' ') _) (Just c) = Just $ c + 1
    g _                     mc       = mc

toPic :: Int -> Picture
toPic = picForImage . string defAttr . show
