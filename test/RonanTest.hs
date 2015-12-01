------------------------------------------------------------------------
-- |
-- Module      :  RonanTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Generate data for publication with Ronan.
-- Imprint some audio samples, then test on the rest.
-- No learning occurs after the imprint (training) phase.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (makeBrain)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (ExponentialParams(..))
import ALife.Creatur.Wain.Audio.Pattern
import qualified ALife.Creatur.Wain.Audio.Wain as AW
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  correctActions)
import ALife.Creatur.Wain.AudioID.Experiment
import ALife.Creatur.Wain.Audio.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import ALife.Creatur.Util (shuffle)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath.Posix (takeFileName)

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> AudioWain -> AudioWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain :: AudioWain
testWain = w'
  where wName = "Fred"
        wAppearance = mkAudio $ replicate (172*39) 0
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 wIos
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize 0.00021 PatternTweaker
        wCSize = 2000
        wMuser = makeMuser [-0.01, -0.01, -0.01, -0.01] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        -- The classifier does most of its learning by round 100.
        ec = ExponentialParams 0.1 0.00015
        -- The predictor needs to keep learning longer.
        ep = ExponentialParams 0.1 0.00015
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

imprintOne :: AudioWain -> Object Action -> IO (AudioWain)
imprintOne w obj = do
  let a = correctActions !! (objectNum obj)
  putStrLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  return $ imprint [objectAppearance obj] a w

tryOne :: AudioWain -> Int -> Object Action -> IO Int
tryOne w k obj = do
  putStrLn $ "-----<br/>"
  putStrLn $ "stats=" ++ show (stats w)
  let (lds, _, _, _, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ lds
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " (alt. " ++ show cBMU2
    ++ ") and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putStrLn $ "Δh=" ++ show deltaH
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if correct a (objectNum obj)
    then do
      putStrLn " was correct"
      return $ k+1
    else do
      putStrLn " was wrong"
      return k

dir :: String
dir = "/home/eamybut/TI46/HTK_MFCC_endpointed/"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readAudio2 :: FilePath -> IO (Object Action)
readAudio2 f = do
  audio <- readAudio f 172
  return $ IObject audio (takeFileName f)

numImprints :: Int
numImprints = 400

main :: IO ()
main = do
  putStrLn $ "numImprints=" ++ show numImprints
  putStrLn $ "stats=" ++ show (stats testWain)
  files <- drop 2 <$> readDirAndShuffle dir
  audios <- mapM readAudio2 files
  let (imprintAudios, testAudios) = splitAt numImprints audios
  imprintedWain <- foldM imprintOne testWain imprintAudios
  putStrLn "Imprinted prediction models"
  mapM_ putStrLn $ AW.describePredictorModels imprintedWain
  k <- foldM (tryOne imprintedWain) 1 testAudios
  let fractionCorrect = fromIntegral k / fromIntegral (length testAudios) :: Double
  putStrLn $ show fractionCorrect ++ ", " ++ show k
    ++ ", " ++ show (stats imprintedWain) ++ " " ++ versionInfo
  putStrLn "test complete"

