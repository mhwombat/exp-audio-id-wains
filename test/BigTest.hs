------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn to classify numerals.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Util (shuffle)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.BrainInternal (makeBrain)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  Difference)
import ALife.Creatur.Wain.Audio.Pattern
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  numActions, correctActions)
import ALife.Creatur.Wain.AudioID.Experiment
import ALife.Creatur.Wain.Audio.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double, doubleToPM1)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Lens
import Control.Monad (when, foldM)
import Control.Monad.Random (evalRandIO)
import Data.ByteString as BS (readFile, writeFile)
import Data.List (minimumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Serialize as DS (decode, encode)
import Data.Word (Word8, Word64)
import System.Directory
import System.FilePath.Posix (takeFileName)
import System.Environment (getArgs)

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> AudioWain -> AudioWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  -- Reward should be the same as they get from imprinting. Otherwise
  -- they may be tempted to experiment with other classifications.
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w

data Params = Params
  {
    nTrainingAudios :: Int,
    reportNovelty :: Bool,
    dataDir :: FilePath,
    cr0 :: UIDouble,
    crf :: UIDouble,
    ctf :: Word64,
    cdt :: UIDouble,
    cSize :: Word64,
    pr0 :: UIDouble,
    prf :: UIDouble,
    ptf :: Word64,
    pdt :: UIDouble,
    defO :: [PM1Double],
    depth :: Word8
  } deriving (Show, Read)

testWain :: Params -> AudioWain
testWain p = w'
  where wName = "Fred"
        wAppearance = mkAudio $ replicate (172*39) 0
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 32 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec (cSize p) (cdt p) PatternTweaker
        wMuser = makeMuser (defO p) (depth p)
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictorSize = cSize p * fromIntegral numActions
        wPredictor = buildPredictor ep wPredictorSize (pdt p)
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams (cr0 p) (crf p) (ctf p)
        ep = LearningParams (pr0 p) (prf p) (ptf p)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

imprintOne :: AudioWain -> Object Action -> AudioWain
imprintOne w obj = w'
  where (_, _, _, _, w') = imprint [objectAppearance obj] a w
        a = correctActions !! (objectNum obj)

testOne :: Bool -> AudioWain -> Int -> Object Action -> IO Int
testOne rn w k obj = do
  let (lds, _, _, _, r, _) = chooseAction [objectAppearance obj] w
  when rn $
    putStrLn $ "Novelty of " ++ objectId obj ++ " is "
      ++ show (novelty lds)
  let a = view action r
  if correct a (objectNum obj)
    then return $ k+1
    else return k

novelty  :: [[(Label, Difference)]] -> UIDouble
novelty (lds:_) = 1 - snd (minimumBy (comparing snd) lds)
novelty _ = error "attempted to calculate novelty on empty list"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  evalRandIO (shuffle files)

readAudio2 :: FilePath -> IO (Object Action)
readAudio2 f = do
  img <- readAudio f 172
  return $ IObject img (takeFileName f)

trial :: [Object Action] -> [Object Action] -> Params -> IO ()
trial xs ys p = do
  w <- fetchWain p
  -- let wTrained = foldl' trainOne w xs
  let wTrained = foldl' imprintOne w xs
  k <- foldM (testOne (reportNovelty p) wTrained) 0 ys
  let fractionCorrect = fromIntegral k / fromIntegral (length ys) :: Double
  putStrLn $ show fractionCorrect ++ ", " ++ show k ++ ", " ++ show p
    ++ ", " ++ show (stats wTrained) ++ " " ++ versionInfo
  writeWain wTrained

wainFile :: FilePath
wainFile = "superwain"

fetchWain :: Params -> IO AudioWain
fetchWain p = do
  exists <- doesFileExist wainFile
  if exists
    then do
      putStrLn "Using existing wain"
      x <- BS.readFile wainFile
      let (Right w) = DS.decode x
      return w
    else do
      putStrLn "Creating new wain"
      return $ testWain p

writeWain :: AudioWain -> IO ()
writeWain w = do
  let x = DS.encode w
  BS.writeFile wainFile x

main :: IO ()
main = do
  params <- read . head <$> getArgs
  files <- drop 2 <$> readDirAndShuffle (dataDir params)
  audios <- mapM readAudio2 files
  let (imprintAudios, testAudios) = splitAt (nTrainingAudios params) audios
  trial imprintAudios testAudios params
