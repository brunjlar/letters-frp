module Main
    ( main
    ) where

main :: IO ()
main = putStrLn "counter"
{-
import Control.Monad.IO.Class (MonadIO (..))
import Data.Time
import Graphics.Vty           as V
import Reflex.Vty             as R

main :: IO ()
main = runVtyApp counterApp

counterApp :: VtyApp t m
counterApp _ e = do
    ticksE <- fmap (succ . _tickInfo_n) <$> (liftIO getCurrentTime >>= tickLossy 1)
    ticksB <- hold 0 ticksE
    return $ VtyResult (render <$> ticksB) $ fmapMaybe esc e
  where
    esc :: V.Event -> Maybe ()
    esc (EvKey KEsc []) = Just ()
    esc _               = Nothing

render :: Integer -> Picture
render n = picForImage $ V.string defAttr $ show n
-}
