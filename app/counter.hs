module Main
    ( main
    ) where

import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B

import Reactive.Banana.Vty (runVtyWithTimer')

main :: IO ()
main = runVtyWithTimer' 1000000 describeNetwork 

describeNetwork :: B.Event V.Event
                -> B.Event ()
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork _ tickE = do
    countE <- accumE (0 :: Int) $ const succ <$> tickE 
    countB <- stepper 0 countE
    let picB  = render <$> countB
        quitE = const () <$> filterE (> 10) countE
    return (picB, quitE)

render :: Int -> Picture
render = picForImage . string defAttr . show
