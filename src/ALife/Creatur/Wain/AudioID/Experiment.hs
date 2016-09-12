------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.AudioID.Experiment
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.AudioID.Experiment
  (
    PatternWain,
    randomPatternWain,
    PatternTweaker(..),
    run,
    finishRound,
    schemaQuality,
    printStats,
    versionInfo,
    idealPopControlDeltaE -- exported for testing only
  ) where

import ALife.Creatur (agentId, isAlive, programVersion)
import ALife.Creatur.Task (checkPopSize)
import qualified ALife.Creatur.Wain as W
import qualified ALife.Creatur.Wain.Audio.Wain as AW
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  correctActions)
import ALife.Creatur.Wain.Audio.Pattern (Pattern, mkAudio)
import ALife.Creatur.Wain.Audio.PatternDB (PatternDB, anyPattern)
import ALife.Creatur.Wain.Audio.Tweaker (PatternTweaker(..))
import qualified ALife.Creatur.Wain.AudioID.Universe as U
import ALife.Creatur.Wain.Brain (makeBrain, predictor,
  scenarioReport, responseReport, decisionReport)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import qualified ALife.Creatur.Wain.Predictor as P
import ALife.Creatur.Wain.GeneticSOM (RandomLearningParams(..),
  randomLearningFunction, schemaQuality, modelMap)
import qualified ALife.Creatur.Wain.Audio.Object as O
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, _action, _outcomes)
import ALife.Creatur.Wain.SimpleResponseTweaker (ResponseTweaker(..))
import ALife.Creatur.Wain.UnitInterval (uiToDouble)
import qualified ALife.Creatur.Wain.Statistics as Stats
import ALife.Creatur.Persistent (putPS, getPS)
import ALife.Creatur.Wain.PersistentStatistics (updateStats, readStats,
  clearStats)
import ALife.Creatur.Wain.Statistics (summarise)
import ALife.Creatur.Wain.Weights (makeWeights)
import Control.Conditional (whenM)
import Control.Lens hiding (universe)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (Rand, RandomGen, getRandomR, getRandomRs,
  getRandom, evalRandIO, fromList)
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT,
  get, put)
import Data.List (intercalate, sortBy)
import Data.Map (toList)
import Data.Ord (comparing)
import Data.Version (showVersion)
import Data.Word (Word64)
import Paths_exp_audio_id_wains (version)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

versionInfo :: String
versionInfo
  = "exp-audio-id-wains-" ++ showVersion version
      ++ ", compiled with " ++ AW.packageVersion
      ++ ", " ++ W.packageVersion
      ++ ", " ++ ALife.Creatur.programVersion

type PatternWain
  = W.Wain Pattern PatternTweaker (ResponseTweaker Action) Action

randomPatternWain
  :: RandomGen r
    => String -> U.Universe PatternWain -> Word64 -> Word64
      -> Rand r PatternWain
randomPatternWain wName u classifierSize predictorSize = do
  let fcp = RandomLearningParams
              { _r0Range = view U.uClassifierR0Range u,
                _rfRange = view U.uClassifierRfRange u,
                _tfRange = view U.uClassifierTfRange u }
  fc <- randomLearningFunction fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  let c = Cl.buildClassifier fc classifierSize classifierThreshold
            PatternTweaker
  let fdp = RandomLearningParams
              { _r0Range = view U.uPredictorR0Range u,
                _rfRange = view U.uPredictorRfRange u,
                _tfRange = view U.uPredictorTfRange u }
  fd <- randomLearningFunction fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  let p = P.buildPredictor fd predictorSize predictorThreshold ResponseTweaker
  -- TODO: Allow a range of random weights
  -- hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let hw = makeWeights [0.7, 0.3, 0, 0.1]
  t <- getRandom
  s <- getRandomR (view U.uStrictnessRange u)
  dp <- getRandomR $ view U.uDepthRange u
  dos <- take 4 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  let (Right mr) = makeMuser dos dp
  ios <- take 4 <$> getRandomRs (view U.uImprintOutcomeRange u)
  rds <- take 4 <$> getRandomRs (view U.uReinforcementDeltasRange u)
  let (Right wBrain) = makeBrain c mr p hw t s ios rds
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  wPassionDelta <- getRandomR . view U.uPassionDeltaRange $ u
  let wBoredomDelta = 0
  let n = (view U.uNumVectors u)*(view U.uVectorLength u)
  let wAppearance = mkAudio $ replicate n 0
  return $ W.buildWainAndGenerateGenome wName wAppearance wBrain
    wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta

data Summary = Summary
  {
    _rPopSize :: Int,
    _rMetabolismDeltaE :: Double,
    _rPopControlDeltaE :: Double,
    _rCatDeltaE :: Double,
    _rFlirtingDeltaE :: Double,
    _rMatingDeltaE :: Double,
    _rOldAgeDeltaE :: Double,
    _rOtherMatingDeltaE :: Double,
    _rNetDeltaE :: Double,
    _rChildNetDeltaE :: Double,
    _rDeltaEToReflectOn :: Double,
    _rDeltaPToReflectOn :: Double,
    _rDeltaHToReflectOn :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rCatCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rDeathCount :: Int,
    _rMistakeCount :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rMetabolismDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rCatDeltaE = 0,
    _rFlirtingDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rDeltaEToReflectOn = 0,
    _rDeltaPToReflectOn = 0,
    _rDeltaHToReflectOn = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rCatCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rDeathCount = 0,
    _rMistakeCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "cat Δe" (view rCatDeltaE r),
    Stats.dStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "Δe to reflect on" (view rDeltaEToReflectOn r),
    Stats.dStat "Δp to reflect on" (view rDeltaPToReflectOn r),
    Stats.dStat "Δh to reflect on" (view rDeltaHToReflectOn r),
    Stats.dStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "classified" (view rCatCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "died" (view rDeathCount r),
    Stats.iStat "mistakes" (view rMistakeCount r)
  ]

data Experiment = Experiment
  {
    _subject :: PatternWain,
    _other :: O.Object Action (ResponseTweaker Action),
    _weanlings :: [PatternWain],
    _universe :: U.Universe PatternWain,
    _summary :: Summary
  }
makeLenses ''Experiment

finishRound :: FilePath -> StateT (U.Universe PatternWain) IO ()
finishRound f = do
  xss <- readStats f
  let yss = summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uAllowedPopulationRange
  checkPopSize (a, b)

report :: String -> StateT Experiment IO ()
report = zoom universe . U.writeToLog

run :: [PatternWain]
      -> StateT (U.Universe PatternWain) IO [PatternWain]
run (me:otherWain:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last two wains  standing!"
  p <- U.popSize
  u <- get
  x <- liftIO $ chooseObject (view U.uFrequencies u) otherWain
    (view U.uPatternDB u)
  let e = Experiment { _subject = me,
                       _other = x,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  put (view universe e')
  let modifiedAgents = O.addIfWain (view other e')
            $ view subject e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  reportAnyDeaths modifiedAgents
  return modifiedAgents
run _ = error "too few wains"

run' :: StateT Experiment IO ()
run' = do
  t <- zoom universe U.currentTime
  if (t < 100)
    then imprintCorrectAction
    else runNormal

runNormal :: StateT Experiment IO ()
runNormal = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  report $ "---------- " ++ agentId a ++ "'s turn ----------"
  report $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (Stats.stats a)
  runMetabolism
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  r <- chooseSubjectAction
  wainBeforeAction <- use subject
  runAction (_action r)
  letSubjectReflect wainBeforeAction r
  subject %= W.autoAdjustPassion
  subject %= W.incAge
  a' <- use subject
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  killIfTooOld
  agentStats <- ((Stats.stats a' ++) . summaryStats) <$> use summary
  report $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rMetabolismDeltaE s
         + _rPopControlDeltaE s
         + _rCatDeltaE s
         + _rFlirtingDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s,
    _rChildNetDeltaE = 0
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    report $ "WARNING: Adult energy equation doesn't balance"
    report $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    report $ "WARNING: Child energy equation doesn't balance"
    report $ "ec0=" ++ show ec0 ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  bmc <- use (universe . U.uBaseMetabolismDeltaE)
  cpcm <- use (universe . U.uEnergyCostPerClassifierModel)
  ccf <- use (universe . U.uChildCostFactor)
  let deltaE = AW.metabCost bmc cpcm 1 a
                 + sum (map (AW.metabCost bmc cpcm ccf)
                         (view W.litter a))
  AW.adjustEnergy subject deltaE rMetabolismDeltaE "metab." summary
    report

chooseSubjectAction
  :: StateT Experiment IO (Response Action)
chooseSubjectAction = do
  a <- use subject
  obj <- use other
  (r, a') <- zoom universe $ chooseAction3 a obj
  assign subject a'
  return r

chooseAction3
  :: PatternWain -> O.Object Action (ResponseTweaker Action)
    -> StateT (U.Universe PatternWain) IO
        (Response Action, PatternWain)
chooseAction3 w obj = do
  whenM (use U.uShowPredictorModels) $ do
    U.writeToLog "begin predictor models"
    describeModels w
    U.writeToLog "end predictor models"
  let (ldss, sps, rplos, aohs, r, w')
        = W.chooseAction [O.objectAppearance obj] w
  whenM (use U.uGenFmris)
    --writeFmri w
    (mapM_ U.writeToLog . AW.describeClassifierModels $ w)
  whenM (use U.uShowScenarioReport) $ do
    U.writeToLog "begin scenario report"
    mapM_ U.writeToLog $ scenarioReport sps
    U.writeToLog "end scenario report"
  whenM (use U.uShowResponseReport) $ do
    U.writeToLog "begin response report"
    mapM_ U.writeToLog $ responseReport rplos
    U.writeToLog "end response report"
  whenM (use U.uShowDecisionReport) $ do
    U.writeToLog "begin decision report"
    mapM_ U.writeToLog $ decisionReport aohs
    U.writeToLog "end decision report"
  let bmuInfo
        = formatBMUs . map fst . sortBy (comparing snd) . head $ ldss
  U.writeToLog $ agentId w ++ " sees " ++ O.objectId obj
    ++ ", classifies it as " ++ bmuInfo
    ++ " and chooses to " ++ show (_action r)
    ++ " predicting the outcomes " ++ show (_outcomes r)
  return (r, w')

describeModels
  :: PatternWain -> StateT (U.Universe PatternWain) IO ()
describeModels w = mapM_ (U.writeToLog . f) ms
  where ms = toList . modelMap . view (W.brain . predictor) $ w
        f (l, r) = view W.name w ++ "'s predictor model " ++ show l
                     ++ "=" ++ pretty r

formatBMUs :: [Cl.Label] -> String
formatBMUs (cBMU:cBMU2:_) = show cBMU ++ " (alt. " ++ show cBMU2 ++ ")"
formatBMUs (cBMU:_)       = show cBMU
formatBMUs _ = error "no BMUs"

chooseObject
  :: [Rational] -> PatternWain -> PatternDB
    -> IO (O.Object Action (ResponseTweaker Action))
chooseObject freqs w db = do
  (img1, audioId1) <- evalStateT anyPattern db
  fromList $ zip [O.PObject img1 audioId1, O.AObject w] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  obj <- use other
  unless (O.isPattern obj) flirt
  (summary.rFlirtCount) += 1

--
-- Categorise
--
runAction a = do
  obj <- use other
  let n = O.objectNum obj
  deltaE <- if correct a n then use (universe . U.uCorrectDeltaE)
                           else use (universe . U.uIncorrectDeltaE)
  AW.adjustEnergy subject deltaE rCatDeltaE "audio ID" summary report
  (summary.rCatCount) += 1

--
-- Utility functions
--

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  AW.adjustEnergy subject deltaE rPopControlDeltaE
    "pop. control" summary report

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  (O.AObject b) <- use other
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ W.mate a b babyName
  if null msgs
    then do
      report $ agentId a ++ " and " ++ agentId b ++ " mated"
      report $ "Contribution to child: " ++
        agentId a ++ "'s share is " ++ show aMatingDeltaE ++ " " ++ 
        agentId b ++ "'s share is " ++ show bMatingDeltaE
      assign subject a'
      assign other (O.AObject b')
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ report msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view W.litter a)

applyFlirtationEffects :: StateT Experiment IO ()
applyFlirtationEffects = do
  deltaE <- use (universe . U.uFlirtingDeltaE)
  report $ "Applying flirtation energy adjustment"
  AW.adjustEnergy subject deltaE rFlirtingDeltaE "flirting" summary
    report
  (summary.rFlirtCount) += 1

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- W.weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- W.pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view W.age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    AW.adjustEnergy subject (-100) rOldAgeDeltaE "old age" summary
      report

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe PatternWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    let (Just average) = Stats.lookup "avg. energy" xs
    let (Just total) = Stats.lookup "total energy" xs
    budget <- use U.uEnergyBudget
    pop <- U.popSize
    let c = idealPopControlDeltaE average total budget pop
    U.writeToLog $ "Current avg. energy = " ++ show average
    U.writeToLog $ "Current total energy = " ++ show total
    U.writeToLog $ "energy budget = " ++ show budget
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

-- TODO: Make the 0.8 configurable
idealPopControlDeltaE :: Double -> Double -> Double -> Int -> Double
idealPopControlDeltaE average total budget pop
  | average < 0.8 = (budget - total) / (fromIntegral pop)
  | otherwise     = 0.8 - average

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ O.objectEnergy <$> use other
  d <- W.childEnergy <$> use subject
  e <- O.objectChildEnergy <$> use other
  return (a + b, d + e)

printStats
  :: [[Stats.Statistic]] -> StateT (U.Universe PatternWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

letSubjectReflect
  :: PatternWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wBefore r = do
  w <- use subject
  obj <- use other
  let p = O.objectAppearance obj
  let energyBefore = view W.energy wBefore
  let passionBefore = view W.passion wBefore
  let happinessBefore = W.happiness wBefore
  energyAfter <- use (subject . W.energy)
  passionAfter <- use (subject . W.passion)
  happinessAfter <- W.happiness <$> use subject
  let deltaH = uiToDouble happinessAfter - uiToDouble happinessBefore
  assign (summary . rDeltaEToReflectOn)
    (uiToDouble energyAfter - uiToDouble energyBefore)
  assign (summary . rDeltaPToReflectOn)
    (uiToDouble passionAfter - uiToDouble passionBefore)
  assign (summary . rDeltaHToReflectOn) deltaH
  let (w', err) = W.reflect [p] r wBefore w
  assign subject w'
  assign (summary . rErr) err
  if (correct (_action r) (O.objectNum obj))
    then
      report $ agentId w ++ "'s choice to " ++ show (_action r)
        ++ " (with) " ++ O.objectId obj ++ " was correct"
    else do
      report $ agentId w ++ "'s choice to " ++ show (_action r)
        ++ " (with) " ++ O.objectId obj ++ " was wrong"
      (summary . rMistakeCount) += 1

imprintCorrectAction :: StateT Experiment IO ()
imprintCorrectAction = do
  w <- use subject
  obj <- use other
  let p = O.objectAppearance obj
  let a = correctActions !! (O.objectNum obj)
  report $ "Teaching " ++ agentId w ++ " that correct action for "
    ++ O.objectId obj ++ " is " ++ show a
  let (_, _, _, _, w') = W.imprint [p] a w
  assign subject w'

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe PatternWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths
  :: [PatternWain] -> StateT (U.Universe PatternWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age " ++ show (view W.age w))
