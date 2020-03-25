module Main
    ( main
    ) where

import Data.Void
import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

import Reactive.Banana.Vty (runVtyWithTimer)

main :: IO ()
main = do
    (addVoid, _) <- newAddHandler :: IO (AddHandler Void, Handler Void)
    runVtyWithTimer addVoid 1000000 describeNetwork 

describeNetwork :: B.Event V.Event
                -> B.Event ()
                -> B.Event Void
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork _ tickE _ = do
    countE <- accumE (0 :: Int) $ const succ <$> tickE 
    countB <- stepper 0 countE
    let picB  = render <$> countB
        quitE = const () <$> filterE (> 10) countE
    return (picB, quitE)

render :: Int -> Picture
render = picForImage . string defAttr . show
