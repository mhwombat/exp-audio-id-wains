------------------------------------------------------------------------
-- |
-- Module      :  SingleWain
-- Copyright   :  (c) Amy de Buitléir 2015-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Imprint some audio samples, then test on the rest.
-- No learning occurs after the imprint (training) phase.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur (agentId)
import ALife.Creatur.Wain
import ALife.Creatur.Wain.BrainInternal (makeBrain, predictor)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..),
  modelMap)
import ALife.Creatur.Wain.Audio.Pattern
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  correctActions, numeralFor)
import ALife.Creatur.Wain.AudioID.Experiment
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Audio.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Response (action, outcomes)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import ALife.Creatur.Util (shuffle)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.Function (on)
import Data.List (sortBy, groupBy, maximumBy)
import qualified Data.Map.Strict as M
import Data.Map.Lazy (Map, insertWith, elems, empty, size)
import Data.Ord (comparing)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)

type Numeral = Char

reward :: Double
reward = 1

runAction
  :: Action -> Object Action (ResponseTweaker Action) -> PatternWain
    -> PatternWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain
  :: UIDouble -> UIDouble -> UIDouble -> UIDouble -> UIDouble
    -> PatternWain
testWain threshold r0c rfc r0p rfp = w'
  where wName = "Fred"
        wAppearance = mkAudio $ replicate (172*39) 0
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 128 wIos wRds
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize threshold PatternTweaker
        wCSize = 2000
        Right wMuser = makeMuser [-0.01, -0.01, -0.01, -0.01] 1
        wIos = [0.1, 0, 0, 0]
        wRds = [0.1, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1 ResponseTweaker
        wHappinessWeights = makeWeights [1, 0, 0, 0]
        ec = LearningParams r0c rfc 1594
        ep = LearningParams r0p rfp 1594
        w = buildWainAndGenerateGenome wName wAppearance wBrain
              wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta
        (w', _) = adjustEnergy 0.5 w

type ModelCreationData = Map Label (Numeral,Numeral)

updateModelCreationData
  :: Label -> Numeral -> ModelCreationData -> ModelCreationData
updateModelCreationData bmu numeral modelCreationData =
  insertWith f bmu (numeral, numeral) modelCreationData
  where f (x, _) (_, y) = (x, y)

trainOne
  :: (PatternWain, ModelCreationData)
    -> Object Action (ResponseTweaker Action)
      -> IO (PatternWain, ModelCreationData)
trainOne (w, modelCreationData) obj = do
  let numeral = head . show $ objectNum obj
  let a = correctActions !! (objectNum obj)
  putStrLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  -- putStrLn "Predictor models before"
  -- mapM_ putStrLn $ IW.describePredictorModels w
  let (_, sps, _, _, w') = imprint [objectAppearance obj] a w
  let bmu = head . fst $ maximumBy (comparing snd) sps
  -- putStrLn $ "lds=" ++ show lds
  -- putStrLn $ "sps=" ++ show sps
  -- putStrLn $ "pBMU=" ++ show pBMU
  -- putStrLn $ "Wain is learning " ++ show r
  -- putStrLn $ "predictor learning rate=" ++ show (currentLearningRate $ view (brain . predictor) w)
  -- putStrLn "Predictor models after"
  -- mapM_ putStrLn $ IW.describePredictorModels w'
  putStrLn $ objectId obj ++ "," ++ numeral : "," ++ show bmu
  let modelCreationData' = updateModelCreationData bmu numeral modelCreationData
  -- let originalNumeral = snd $ modelCreationData' ! bmu
  -- -- putStrLn $ "DEBUG: " ++ show bmu ++ " " ++ show (modelCreationData' ! bmu)
  -- when (numeral /= originalNumeral) $
  --   putStrLn $ "Model " ++ show bmu ++ " was created for numeral "
  --     ++ show originalNumeral
  --     ++ " but is now being used for numeral " ++ show numeral
  -- mapM_ putStrLn $ IW.describePredictorModels w'
  return (w', modelCreationData')

testOne
  :: PatternWain -> [(Numeral, Bool)]
    -> Object Action (ResponseTweaker Action)
      -> IO [(Numeral, Bool)]
testOne w testStats obj = do
  putStrLn $ "-----"
  let (ldss, _, _, _, r, _)
        = chooseAction [objectAppearance obj] w
  -- describePredictorModels wainAfterDecision
  -- putStrLn $ "DEBUG ldss=" ++ show ldss
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ ldss
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " (alt. " ++ show cBMU2
    ++ ") and chooses to " ++ show a
    ++ " predicting the outcomes " ++ show (view outcomes r)
  let numeral = head . show $ objectNum obj
  let answer = numeralFor a
  let wasCorrect = answer == numeral
  -- mapM_ putStrLn $ scenarioReport sps
  -- mapM_ putStrLn $ responseReport rplos
  -- mapM_ putStrLn $ decisionReport aos
  let novelty = minimum . map snd . head $ ldss :: UIDouble
  putStrLn $ objectId obj ++ "," ++ numeral : "," ++ show answer
    ++ "," ++ show wasCorrect ++ "," ++ show novelty
  return $ (numeral, wasCorrect):testStats

describePredictorModels :: PatternWain -> IO ()
describePredictorModels w = mapM_ (putStrLn . f) ms
  where ms = M.toList . modelMap . view (brain . predictor) $ w
        f (l, r) = view name w ++ "'s predictor model " ++ show l ++ ": "
                     ++ pretty r

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  let d2 = d ++ "/"
  files <- map (d2 ++) . filter (\s -> head s /= '.') <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readSamples
  :: Int -> FilePath -> IO [Object Action (ResponseTweaker Action)]
readSamples nvec dir = do
  files <- readDirAndShuffle dir
  mapM (readOneSample nvec) files

readOneSample
  :: Int -> FilePath -> IO (Object Action (ResponseTweaker Action))
readOneSample nvec f = do
  audio <- readAudio f nvec
  return $ PObject audio (takeFileName f)

numeralStats :: [(Numeral, Bool)] -> [(String, Int, Int, Double)]
numeralStats xs = ("all",total,totalCorrect,fraction):xs'
  where xs' = map summarise . groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ xs
        total = sum . map (\(_, x, _, _) -> x) $ xs'
        totalCorrect = sum . map (\(_, _, x, _) -> x) $ xs'
        fraction = fromIntegral totalCorrect / fromIntegral total

summarise :: [(Numeral, Bool)] -> (String, Int, Int, Double)
summarise xs = ([n], total, numCorrect, fromIntegral numCorrect / fromIntegral total)
  where numCorrect = length $ filter snd xs
        total = length xs
        n = fst . head $ xs

countModelChanges :: ModelCreationData -> (Int, Double)
countModelChanges modelCreationData = (numChanges, fraction)
  where numChanges = length . filter f . elems $ modelCreationData
        f (a, b) = a /= b
        fraction = fromIntegral numChanges /
                     fromIntegral (size modelCreationData)

main :: IO ()
main = do
  putStrLn versionInfo
  args <- getArgs
  let trainingDir = head args
  let testDir = args !! 1
  let threshold = read $ args !! 2
  let r0c = read $ args !! 3
  let rfc = read $ args !! 4
  let r0p = read $ args !! 5
  let rfp = read $ args !! 6
  let passes  = read $ args !! 7
  let nvec  = read $ args !! 8
  putStrLn $ "trainingDir=" ++ trainingDir
  putStrLn $ "testDir=" ++ testDir
  putStrLn $ "r0c=" ++ show r0c
  putStrLn $ "rfc=" ++ show rfc
  putStrLn $ "threshold=" ++ show threshold
  putStrLn $ "r0p=" ++ show r0p
  putStrLn $ "rfp=" ++ show rfp
  putStrLn $ "passes=" ++ show passes
  putStrLn $ "nvec=" ++ show nvec
  putStrLn "====="
  putStrLn "Training"
  putStrLn "====="
  trainingSamples <- concat . replicate passes <$> readSamples nvec trainingDir
  putStrLn "filename,numeral,label"
  (trainedWain, modelCreationData) <- foldM trainOne (testWain threshold r0c rfc r0p rfp, empty) trainingSamples
  putStrLn $ "stats=" ++ show (stats trainedWain)
  putStrLn ""
  putStrLn "====="
  putStrLn "Testing"
  putStrLn "====="
  testSamples <- readSamples nvec testDir
  putStrLn "filename,numeral,answer,correct,novelty,label,diff"
  stats2 <- foldM (testOne trainedWain) [] testSamples
  putStrLn ""
  putStrLn "====="
  putStrLn "Summary"
  putStrLn "====="
  putStrLn $ "stats=" ++ show (stats trainedWain)
  let (numModelsChanged, fractionModelsChanged) = countModelChanges modelCreationData
  putStrLn $ "number of models changed: " ++ show numModelsChanged
  putStrLn $ "fraction models changed: " ++ show fractionModelsChanged
  putStrLn "numeral,count,correct,accuracy"
  let stats3 = numeralStats stats2
  mapM_ (putStrLn . show) stats3
  putStrLn "test complete"

