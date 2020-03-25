{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
    ( main
    ) where

import Data.List as L
import Data.Maybe
import Data.Text.Lazy as T
import Graphics.Vty as V
import Optics
import Reactive.Banana as B
import Reactive.Banana.Frameworks as B
import System.Random (randomRIO)

import Reactive.Banana.Vty (runVtyWithTimer')

data LettersState = MkLettersState
    { _lActiveWords :: ![ActiveWord]
    , _lRows        :: !Int
    , _lCols        :: !Int
    , _lScore       :: !Int
    , _lLives       :: !Int
    }

data ActiveWord = MkActiveWord
    { _wWord  :: !Text
    , _wIndex :: !Int
    , _wRow   :: !Int
    , _wCol   :: !Int
    }

makeLenses ''LettersState
makeLenses ''ActiveWord

initialLettersState :: LettersState
initialLettersState = MkLettersState
    { _lActiveWords = [ MkActiveWord "foo" 1 3 10, MkActiveWord "bar" 2 10 70]
    , _lRows        = 25 
    , _lCols        = 80
    , _lScore       = 0
    , _lLives       = 1
    }

dropWords :: LettersState -> LettersState
dropWords l = l & lActiveWords .~ words'
                & lLives %~ (+ (L.length words' - L.length (l ^. lActiveWords)))
  where
    words' :: [ActiveWord]
    words' = mapMaybe dropWord $ l ^. lActiveWords

    dropWord :: ActiveWord -> Maybe ActiveWord
    dropWord w =
        let r = 1 + w ^. wRow
        in  if r >= l ^. lRows then Nothing else Just $ w & wRow .~ r

gameOver :: LettersState -> Bool
gameOver l = l ^. lLives <= 0

main :: IO ()
main = runVtyWithTimer' 100000 $ describeNetwork initialLettersState

describeNetwork :: LettersState
                -> B.Event V.Event
                -> B.Event ()
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork letters vtyE tickE = mdo
    let quitE = filterJust $ flip fmap vtyE $ \case
            EvKey KEsc [] -> Just ()
            _             -> Nothing
   
        gameOverB = gameOver <$> lettersB 
        activeB   = not <$> gameOverB
        dropE     = whenE activeB $ const dropWords <$> tickE 

    lettersB <- accumB letters dropE 

    return (render <$> lettersB, quitE)

render :: LettersState -> Picture
render l = picForImage $ vertCat $ go 0 (l ^. lActiveWords) <>
    [ charFill defAttr '-' (l ^. lCols) 1
    , string defAttr $ "Score: " ++ show (l ^. lScore)
    , string defAttr $ "Lives: " ++ show (l ^. lLives)
    ]
  where
    go :: Int -> [ActiveWord] -> [Image]
    go row [] = [backgroundFill (l ^. lCols) (l ^. lRows - row)]
    go row (w : ws)
        | w ^. wRow > row = backgroundFill (l ^. lCols) 1 : go (row + 1) (w : ws)
        | otherwise       = drawWord w : go (row + 1) ws

    drawWord :: ActiveWord -> Image
    drawWord w = 
        let (typed, untyped) = T.splitAt (fromIntegral $ w ^. wIndex) (w ^. wWord)
        in  string defAttr (L.replicate (w ^. wCol) ' ') V.<|>
            text (defAttr `withStyle` reverseVideo) typed V.<|>
            text defAttr untyped
                
