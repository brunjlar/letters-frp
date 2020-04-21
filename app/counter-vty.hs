module Main
    ( main
    ) where

import Graphics.Vty

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    render vty 0
    go vty 0
  where
    go :: Vty -> Int -> IO ()
    go vty c = do
        e <- nextEvent vty
        case e of
            EvKey KEsc        _ -> shutdown vty
            EvKey (KChar ' ') _ -> let c' = succ c in render vty c' >> go vty c'
            _                   -> go vty c

    render :: Vty -> Int -> IO ()
    render vty = update vty . picForImage . string defAttr . show
