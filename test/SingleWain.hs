------------------------------------------------------------------------
-- |
-- Module      :  SingleWain
-- Copyright   :  (c) Amy de Buitléir 2015
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
import ALife.Creatur.Wain.BrainInternal (makeBrain)
import ALife.Creatur.Wain.Classifier (buildClassifier)
import ALife.Creatur.Wain.GeneticSOMInternal (LearningParams(..))
import ALife.Creatur.Wain.Audio.Pattern
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  correctActions, numeralFor)
import ALife.Creatur.Wain.AudioID.Experiment
import ALife.Creatur.Wain.Audio.Object (Object(..), objectNum, objectId,
  objectAppearance)
import ALife.Creatur.Wain.PlusMinusOne (doubleToPM1)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.Response (action)
import ALife.Creatur.Wain.Statistics (stats)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import ALife.Creatur.Wain.Weights (makeWeights)
import ALife.Creatur.Util (shuffle)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.Function (on)
import Data.List (sortBy, groupBy, maximumBy)
import Data.Map.Lazy (Map, insertWith, elems, empty, size)
import Data.Ord (comparing)
import System.Directory
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)

type Numeral = Char

reward :: Double
reward = 0.1

runAction :: Action -> Object Action -> AudioWain -> AudioWain
runAction a obj w =
  if correct a (objectNum obj)
    then wCorrect
    else wIncorrect
  where (wCorrect, _) = adjustEnergy reward w
        (wIncorrect, _) = adjustEnergy (-reward) w
        
testWain :: UIDouble -> UIDouble -> UIDouble -> UIDouble -> UIDouble -> AudioWain
testWain threshold r0c rfc r0p rfp = w'
  where wName = "Fred"
        wAppearance = mkAudio $ replicate (172*39) 0
        Right wBrain = makeBrain wClassifier wMuser wPredictor wHappinessWeights 1 wIos
        wDevotion = 0.1
        wAgeOfMaturity = 100
        wPassionDelta = 0
        wBoredomDelta = 0
        wClassifier = buildClassifier ec wCSize threshold PatternTweaker
        wCSize = 2000
        wMuser = makeMuser [-0.01, -0.01, -0.01, -0.01] 1
        wIos = [doubleToPM1 reward, 0, 0, 0]
        wPredictor = buildPredictor ep (wCSize*11) 0.1
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

trainOne :: (AudioWain, ModelCreationData) -> Object Action -> IO (AudioWain, ModelCreationData)
trainOne (w, modelCreationData) obj = do
  let numeral = head . show $ objectNum obj
  let a = correctActions !! (objectNum obj)
  putStrLn $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ objectId obj ++ " is " ++ show a
  let (_, sps, w') = imprint [objectAppearance obj] a w
  let bmu = head . fst $ maximumBy (comparing snd) sps
  putStrLn $ objectId obj ++ "," ++ numeral : "," ++ show bmu
  let modelCreationData' = updateModelCreationData bmu numeral modelCreationData
  return (w', modelCreationData')

testOne :: AudioWain -> [(Numeral, Bool)] -> Object Action -> IO [(Numeral, Bool)]
testOne w testStats obj = do
  putStrLn $ "-----"
  let (lds, _, _, _, r, _) = chooseAction [objectAppearance obj] w
  let (cBMU, _):(cBMU2, _):_ = sortBy (comparing snd) . head $ lds
  let a = view action r
  putStrLn $ "Wain sees " ++ objectId obj ++ ", classifies it as "
    ++ show cBMU ++ " (alt. " ++ show cBMU2
    ++ ") and chooses to " ++ show a
  let numeral = head . show $ objectNum obj
  let answer = numeralFor a
  let wasCorrect = answer == numeral
  let novelty = minimum . map snd . head $ lds :: UIDouble
  putStrLn $ objectId obj ++ "," ++ numeral : "," ++ show answer
    ++ "," ++ show wasCorrect ++ "," ++ show novelty
  return $ (numeral, wasCorrect):testStats

readDirAndShuffle :: FilePath -> IO [FilePath]
readDirAndShuffle d = do
  let g = mkStdGen 263167 -- seed
  let d2 = d ++ "/"
  files <- map (d2 ++) . drop 2 <$> getDirectoryContents d
  return $ evalRand (shuffle files) g

readSamples :: Int -> FilePath -> IO [Object Action]
readSamples nvec dir = do
  files <- readDirAndShuffle dir
  mapM (readOneSample nvec) files

readOneSample :: Int -> FilePath -> IO (Object Action)
readOneSample nvec f = do
  audio <- readAudio f nvec
  return $ IObject audio (takeFileName f)

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

