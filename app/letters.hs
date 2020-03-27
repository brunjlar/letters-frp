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
    { _lActiveWords     :: ![ActiveWord]
    , _lRows            :: !Int
    , _lCols            :: !Int
    , _lNextDrop        :: !Int
    , _lDropInterval    :: !Int
    , _lMinDropInterval :: !Int
    , _lNewInterval     :: !Int
    , _lScore           :: !Int
    , _lLives           :: !Int
    , _lNewWord         :: !(IO Text)
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
    { _lActiveWords     = []
    , _lRows            = 25 
    , _lCols            = 80
    , _lNextDrop        = 100
    , _lDropInterval    = 100
    , _lMinDropInterval = 20
    , _lNewInterval     = 700
    , _lScore           = 0
    , _lLives           = 3
    , _lNewWord         = gen
    }

tickLetters :: LettersState -> LettersState
tickLetters l = l & lNextDrop %~ max 0 . pred

dropWords :: LettersState -> LettersState
dropWords l = l & lActiveWords .~ words'
                & lLives       %~ (+ (L.length words' - L.length (l ^. lActiveWords)))
                & lNextDrop    .~ l ^. lDropInterval
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
        delta    = fromIntegral $ sum (T.length . _wWord <$> oldWords) - sum (T.length ._wWord <$> newWords)
    in  l & lActiveWords  .~ newWords
          & lScore        %~ (+ delta)
          & lDropInterval .~ if delta > 0 
                then max (l ^. lMinDropInterval) (l ^. lDropInterval - 1)
                else l ^. lDropInterval
                                         
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
    runVtyWithTimer' 10000 $ describeNetwork $ initialLettersState gen

describeNetwork :: LettersState
                -> B.Event V.Event
                -> B.Event ()
                -> MomentIO (Behavior Picture, B.Event ())
describeNetwork letters vtyE tickE = mdo
    let gameOverB = gameOver <$> lettersB 
        activeB   = not <$> gameOverB

    newE <- newWordE lettersB tickE
    let addE   = addWord     <$> whenE activeB newE
        typeE  = typeChar    <$> whenE activeB (keyE vtyE)
        countE = tickLetters <$  whenE activeB tickE
        dropE  = dropWords   <$  whenE activeB (dropWordsE lettersB tickE)

    lettersB <- accumB letters $ unions [addE, dropE, typeE, countE]

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

dropWordsE :: Behavior LettersState -> B.Event () -> B.Event ()
dropWordsE lettersB = filterJust . apply (fmap f lettersB)
  where
    f :: LettersState -> () -> Maybe ()
    f l ()
        | l ^. lNextDrop == 0 = Just ()
        | otherwise           = Nothing

newWordE :: Behavior LettersState -> B.Event () -> MomentIO (B.Event ActiveWord)
newWordE lettersB = fmap filterJust . execute . fmap liftIO . apply (fmap f lettersB)
  where
    f :: LettersState -> () -> IO (Maybe ActiveWord)
    f l ()
        | topBlocked = return Nothing
        | otherwise  = do
            r <- randomRIO (0, l ^. lNewInterval)
            if r > 0 && not (L.null $ l ^. lActiveWords) 
                then return Nothing
                else do
                    w <- newWord
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

        newWord :: IO Text
        newWord = do
            w <- l ^. lNewWord
            if T.length w > fromIntegral (l ^. lCols)
                then newWord
                else return w

render :: LettersState -> Picture
render l = picForImage $ vertCat $ 
    topRow : (go 0 (l ^. lActiveWords) <> [middleRow, statsRow, bottomRow ])
  where
    go :: Int -> [ActiveWord] -> [Image]
    go row [] = L.replicate (l ^. lRows - row) emptyRow
    go row (w : ws)
        | w ^. wRow > row = emptyRow   : go (row + 1) (w : ws)
        | otherwise       = drawWord w : go (row + 1) ws

    drawWord :: ActiveWord -> Image
    drawWord a = 
        let w                = a ^. wWord
            (typed, untyped) = T.splitAt (a ^. wIndex) w
        in        bar 
            V.<|> backgroundFill (a ^. wCol) 1
            V.<|> text (defAttr `withStyle` reverseVideo) typed 
            V.<|> text defAttr untyped
            V.<|> backgroundFill (cols - a ^. wCol - fromIntegral (T.length w)) 1
            V.<|> bar

    cols :: Int
    cols = l ^. lCols

    bar, topRow, emptyRow, middleRow, bottomRow, statsRow :: Image
    bar       = char defAttr '│'
    topRow    = char defAttr '┌' V.<|> charFill defAttr '─' cols 1 V.<|> char defAttr '┐'
    emptyRow  = bar              V.<|> backgroundFill cols 1       V.<|> bar
    middleRow = char defAttr '├' V.<|> charFill defAttr '─' cols 1 V.<|> char defAttr '┤'
    bottomRow = char defAttr '└' V.<|> charFill defAttr '─' cols 1 V.<|> char defAttr '┘'
    statsRow  =
        let score  = "Score: " ++ show (l ^. lScore)
            lives  = L.replicate (l ^. lLives) '♥'
            middle = backgroundFill (cols - L.length score - L.length lives) 1
        in        bar 
            V.<|> string defAttr score
            V.<|> middle
            V.<|> string defAttr lives
            V.<|> bar
