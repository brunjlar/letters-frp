{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
    ( main
    ) where

import           Data.Int                   (Int64)
import           Data.List                  as L
import           Data.Maybe
import           Data.Text.Lazy             as T
import           Data.Text.Lazy.IO          as T
import qualified Data.Vector                as A
import           Graphics.Vty               as V
import           Optics
import           Reactive.Banana            as B
import           Reactive.Banana.Frameworks as B
import           System.Environment         (getArgs)
import           System.Random              (randomRIO)

import           Reactive.Banana.Vty        (runVtyWithTimer')

data LettersState = MkLettersState
    { _lActiveWords  :: ![ActiveWord]
    , _lRows         :: !Int
    , _lCols         :: !Int
    , _lDropInterval :: !Int
    , _lScore        :: !Int
    , _lLives        :: !Int
    , _lNewWord      :: !(IO Text)
    }

data ActiveWord = MkActiveWord
    { _wWord  :: !Text
    , _wIndex :: !Int64
    , _wRow   :: !Int
    , _wCol   :: !Int
    }

makeLenses ''LettersState
makeLenses ''ActiveWord

mkWordGenerator :: FilePath -> IO (IO Text)
mkWordGenerator wordFile = do
    ws <- A.fromList . T.lines <$> T.readFile wordFile
    let c = A.length ws
    if c == 0
        then error $ "file '" ++ wordFile ++ "' contains no words"
        else return $ do
            i <- randomRIO (0, c - 1)
            return $ ws A.! i

initialLettersState :: IO Text -> LettersState
initialLettersState gen = MkLettersState
    { _lActiveWords  = []
    , _lRows         = 25 
    , _lCols         = 80
    , _lDropInterval = 10
    , _lScore        = 0
    , _lLives        = 3
    , _lNewWord      = gen
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

addWord :: ActiveWord -> LettersState -> LettersState
addWord w l = l & lActiveWords %~ (w :)

typeChar :: Char -> LettersState -> LettersState
typeChar c l = 
    let oldWords = l ^. lActiveWords
        newWords = mapMaybe f oldWords
    in  l & lActiveWords .~ newWords
          & lScore       %~ (+ (L.length oldWords - L.length newWords))
  where
    f :: ActiveWord -> Maybe ActiveWord
    f a
        | T.index w i /= c    = Just $ a & wIndex .~ 0
        | i >= T.length w - 1 = Nothing
        | otherwise           = Just $ a & wIndex %~ succ
      where
        w = a ^. wWord
        i = a ^. wIndex

gameOver :: LettersState -> Bool
gameOver l = l ^. lLives <= 0

main :: IO ()
main = do
    [wordFile] <- getArgs
    gen        <- mkWordGenerator wordFile
    runVtyWithTimer' 1000000 $ describeNetwork $ initialLettersState gen

describeNetwork :: LettersState
                -> B.Event V.Event
                -> B.Event ()
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork letters vtyE tickE = mdo
    let gameOverB = gameOver <$> lettersB 
        activeB   = not <$> gameOverB
        dropE     = whenE activeB $ const dropWords <$> tickE 

    newE <- whenE activeB <$> newWordE lettersB tickE
    let addE  = addWord <$> newE
        typeE = typeChar <$> keyE vtyE

    lettersB <- accumB letters $ unions [dropE, addE, typeE]

    return (render <$> lettersB, escE vtyE)

escE :: B.Event V.Event -> B.Event ()
escE vtyE = filterJust $ flip fmap vtyE $ \case
    EvKey KEsc [] -> Just ()
    _             -> Nothing

keyE :: B.Event V.Event -> B.Event Char
keyE vtyE = filterJust $ f <$> vtyE
  where
    f :: V.Event -> Maybe Char
    f (EvKey (KChar c) []) = Just c
    f _                    = Nothing

newWordE :: Behavior LettersState -> B.Event () -> MomentIO (B.Event ActiveWord)
newWordE lettersB = fmap filterJust . execute . fmap liftIO . apply (fmap f lettersB)
  where
    f :: LettersState -> () -> IO (Maybe ActiveWord)
    f l ()
        | topBlocked = return Nothing
        | otherwise  = do
            r <- randomRIO (0, l ^. lDropInterval)
            if r > 0 then return Nothing
                     else do
                w <- l ^. lNewWord
                c <- randomRIO (0, l ^. lCols - fromIntegral (T.length w))
                return $ Just MkActiveWord
                    { _wWord  = w
                    , _wIndex = 0
                    , _wRow   = 0
                    , _wCol   = c
                    }

      where
        topBlocked :: Bool
        topBlocked = case l ^. lActiveWords of
            []      -> False
            (w : _) -> w ^. wRow == 0

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
        let (typed, untyped) = T.splitAt (w ^. wIndex) (w ^. wWord)
        in  string defAttr (L.replicate (w ^. wCol) ' ') V.<|>
            text (defAttr `withStyle` reverseVideo) typed V.<|>
            text defAttr untyped
