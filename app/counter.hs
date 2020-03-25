module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad
import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

import Reactive.Banana.Vty (runVty)

main :: IO ()
main = do
    (addTick, tick) <- newAddHandler
    tid             <- forkIO $ forever $ threadDelay 1000000 >> tick ()
    runVty addTick describeNetwork 
    killThread tid

describeNetwork :: B.Event V.Event
                -> B.Event ()
                -> Moment (Behavior Picture, B.Event ())
describeNetwork _ tickE = do
    countE <- accumE (0 :: Int) $ const succ <$> tickE 
    countB <- stepper 0 countE
    let picB  = render <$> countB
        quitE = const () <$> filterE (> 10) countE
    return (picB, quitE)

render :: Int -> Picture
render = picForImage . string defAttr . show
