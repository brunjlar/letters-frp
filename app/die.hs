{-# LANGUAGE LambdaCase #-}

module Main
    ( main
    ) where

import Graphics.Vty as V
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B
import System.Random (randomRIO)

import Reactive.Banana.Vty (runVtyWithTimer')

main :: IO ()
main = runVtyWithTimer' 1000000 describeNetwork 

describeNetwork :: B.Event V.Event
                -> B.Event ()
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork vtyE tickE = do
    let quitE = filterJust $ flip fmap vtyE $ \case
            EvKey KEsc [] -> Just ()
            _             -> Nothing
    dieE <- execute $ const (liftIO $ randomRIO (1, 6)) <$> tickE
    dieB <- stepper 0 dieE
    return (render <$> dieB, quitE)

render :: Int -> Picture
render = picForImage . string defAttr . show
