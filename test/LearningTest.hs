------------------------------------------------------------------------
-- |
-- Module      :  LearningTest
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Verify that a wain can learn.
--
------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (classifier, predictor,
  makeBrain, scenarioReport, responseReport, decisionReport,
  decisionQuality)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..))
import ALife.Creatur.Wain.Audio.Pattern
import qualified ALife.Creatur.Wain.Audio.Wain as AW
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Audio.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct)
import ALife.Creatur.Wain.AudioID.Experiment
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
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath.Posix (takeFileName)

numTests :: Int
numTests = 5

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
        wClassifier = buildClassifier ec wCSize 0.2 PatternTweaker
        wCSize = 500
        wMuser = makeMuser [0, 0, 0, 0] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1
        wHappinessWeights = makeWeights [1, 0, 0]
        -- The classifier does most of its learning by round 100.
        ec = LearningParams 0.2 0.05 (fromIntegral numTests)
        -- The predictor needs to keep learning longer.
        ep = LearningParams 0.1 0.005 (fromIntegral numTests)
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

tryOne :: AudioWain -> Object Action -> IO (AudioWain)
tryOne w obj = do
  putStrLn $ "-----<br/>"
  putStrLn $ "stats=" ++ show (stats w)
  let (lds, sps, rplos, aos, r, wainAfterDecision) = chooseAction [objectAppearance obj] w
  let (cBMU, _) = minimumBy (comparing snd) . head $ lds
  mapM_ putStrLn $ scenarioReport sps
  mapM_ putStrLn $ responseReport rplos
  mapM_ putStrLn $ decisionReport aos
  putStrLn $ "DEBUG classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ wainAfterDecision)
  mapM_ putStrLn $ AW.describeClassifierModels wainAfterDecision
  mapM_ putStrLn $ AW.describePredictorModels wainAfterDecision
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  putStrLn $ show (objectAppearance obj)
  let wainRewarded = runAction a obj wainAfterDecision
  let deltaH = uiToDouble (happiness wainRewarded) - uiToDouble (happiness w)
  putStrLn $ "Δh=" ++ show deltaH
  putStrLn $ "condition before=" ++ show (condition w) ++ " after=" ++ show (condition wainRewarded)
  putStrLn $ "happiness before=" ++ show (happiness w) ++ " after=" ++ show (happiness wainRewarded)
  putStr $ "Choosing to " ++ show a ++ " in response to " ++ objectId obj
  if correct a (objectNum obj)
    then putStrLn " was wrong"
    else putStrLn " was correct"
  let (wainAfterReflection, err) = reflect [objectAppearance obj] r w wainRewarded
  putStrLn $ "err=" ++ show err
  -- keep the wain's energy constant
  let restorationEnergy = uiToDouble (view energy w) - uiToDouble (view energy wainRewarded)
  -- keep the wain's boredom constant
  let restorationBoredom = uiToDouble (view boredom w) - uiToDouble (view boredom wainRewarded)
  let (wainPartiallyRestored, _) = adjustEnergy restorationEnergy wainAfterReflection
  let (wainFinal, _) = adjustBoredom restorationBoredom wainPartiallyRestored
  putStrLn $ "classifier SQ=" ++ show (schemaQuality . view (brain . classifier) $ w)
  putStrLn $ "predictor SQ=" ++ show (schemaQuality . view (brain . predictor) $ w)
  putStrLn $ "DQ=" ++ show (decisionQuality . view brain $ w)
  return wainFinal

dir :: String
dir = "/home/eamybut/TI46/HTK_MFCC_endpointed/TRAIN-RAW"

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  files <- map (d ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readOneSample :: Int -> FilePath -> IO (Object Action)
readOneSample nvec f = do
  audio <- readAudio f nvec
  return $ IObject audio (takeFileName f)

main :: IO ()
main = do
  putStrLn $ "numTests=" ++ show numTests
  files <- take numTests . drop 2 <$> readDirAndShuffle dir
  audios <- mapM (readOneSample 110) files
  _ <- foldM tryOne testWain audios
  putStrLn "test complete"

