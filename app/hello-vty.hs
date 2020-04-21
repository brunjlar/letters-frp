module Main
    ( main
    ) where

import Graphics.Vty

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    update vty $ picForImage $ string defAttr "Hello, Vty!"
    go vty
  where
    go :: Vty -> IO ()
    go vty = do
        e <- nextEvent vty
        case e of
            EvKey KEsc _ -> shutdown vty
            _            -> go vty
