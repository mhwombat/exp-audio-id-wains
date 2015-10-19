------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.AudioID.Experiment
-- Copyright   :  (c) Amy de Buitléir 2012-2015
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
    AudioWain,
    PatternTweaker(..),
    run,
    randomAudioWain,
    finishRound,
    schemaQuality,
    printStats,
    versionInfo,
    -- items below are only exported for testing
    idealPopControlDeltaE
  ) where

import ALife.Creatur (agentId, isAlive, programVersion)
import ALife.Creatur.Task (checkPopSize)
import qualified ALife.Creatur.Wain as W
import qualified ALife.Creatur.Wain.Audio.Wain as AW
import ALife.Creatur.Wain.AudioID.Action (Action(..), correct,
  correctActions, numActions)
import ALife.Creatur.Wain.Audio.Pattern (mkAudio)
import ALife.Creatur.Wain.Audio.PatternDB (PatternDB, anyPattern)
import ALife.Creatur.Wain.Audio.Tweaker (PatternTweaker(..))
import qualified ALife.Creatur.Wain.AudioID.Universe as U
import ALife.Creatur.Wain.Brain (makeBrain, scenarioReport,
  responseReport, decisionReport)
import ALife.Creatur.Wain.Checkpoint (enforceAll)
import qualified ALife.Creatur.Wain.Classifier as Cl
import ALife.Creatur.Wain.Muser (makeMuser)
import ALife.Creatur.Wain.Predictor (buildPredictor)
import ALife.Creatur.Wain.GeneticSOM (RandomExponentialParams(..),
  randomExponential, schemaQuality)
import qualified ALife.Creatur.Wain.Audio.Object as O
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.Raw (raw)
import ALife.Creatur.Wain.Response (Response, _action, _outcomes)
import ALife.Creatur.Wain.UnitInterval (UIDouble, uiToDouble)
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
import Control.Monad.State.Lazy (StateT, execStateT, evalStateT, get)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Version (showVersion)
import Data.Word (Word16)
import Paths_exp_audio_id_wains (version)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName)

versionInfo :: String
versionInfo
  = "exp-audio-id-wains-" ++ showVersion version
      ++ ", compiled with " ++ W.packageVersion
      ++ ", " ++ ALife.Creatur.programVersion

type AudioWain = AW.AudioWain Action
type Object = O.Object Action

randomAudioWain
  :: RandomGen r
    => String -> U.Universe AudioWain -> Word16 -> Rand r AudioWain
randomAudioWain wName u classifierSize = do
  let fcp = RandomExponentialParams
               { _r0Range = view U.uClassifierR0Range u,
                 _dRange = view U.uClassifierDRange u }
  fc <- randomExponential fcp
  classifierThreshold <- getRandomR (view U.uClassifierThresholdRange u)
  let c = Cl.buildClassifier fc classifierSize classifierThreshold
            PatternTweaker
  let fdp = RandomExponentialParams
              { _r0Range = view U.uPredictorR0Range u,
                _dRange = view U.uPredictorDRange u }
  fd <- randomExponential fdp
  predictorThreshold <- getRandomR (view U.uPredictorThresholdRange u)
  let predictorSize = classifierSize * fromIntegral numActions
  let dr = buildPredictor fd predictorSize predictorThreshold
  -- TODO: Allow a range of random weights
  -- hw <- (makeWeights . take 3) <$> getRandomRs unitInterval
  let hw = makeWeights [0.7, 0.3, 0, 0.1]
  dOut <- take 4 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  dp <- getRandomR $ view U.uDepthRange u
  let mr = makeMuser dOut dp
  t <- getRandom
  ios <- take 4 <$> getRandomRs (view U.uImprintOutcomeRange u)
  let (Right wBrain) = makeBrain c mr dr hw t ios
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  wPassionDelta <- getRandomR . view U.uBoredomDeltaRange $ u
  wBoredomDelta <- getRandomR . view U.uPassionDeltaRange $ u
  let n = (view U.uMaxVectorCount u)*(view U.uVectorLength u)
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
    _rDeltaBToReflectOn :: Double,
    _rDeltaPToReflectOn :: Double,
    _rDeltaHToReflectOn :: Double,
    _rErr :: Double,
    _rBirthCount :: Int,
    _rWeanCount :: Int,
    _rEatCount :: Int,
    _rPlayCount :: Int,
    _rFlirtCount :: Int,
    _rMateCount :: Int,
    _rIgnoreCount :: Int,
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
    _rDeltaBToReflectOn = 0,
    _rDeltaPToReflectOn = 0,
    _rDeltaHToReflectOn = 0,
    _rErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rEatCount = 0,
    _rPlayCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rIgnoreCount = 0,
    _rDeathCount = 0,
    _rMistakeCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "cat Δe" (view rCatDeltaE r),
    Stats.dStat "flirting Δe" (view rFlirtingDeltaE r),
    Stats.dStat "mating Δe" (view rMatingDeltaE r),
    Stats.dStat "old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "Δe to reflect on" (view rDeltaEToReflectOn r),
    Stats.dStat "Δb to reflect on" (view rDeltaBToReflectOn r),
    Stats.dStat "Δp to reflect on" (view rDeltaPToReflectOn r),
    Stats.dStat "Δh to reflect on" (view rDeltaHToReflectOn r),
    Stats.dStat "err" (view rErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "ate" (view rEatCount r),
    Stats.iStat "played" (view rPlayCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "ignored" (view rIgnoreCount r),
    Stats.iStat "died" (view rDeathCount r),
    Stats.iStat "mistakes" (view rMistakeCount r)
  ]

data Experiment = Experiment
  {
    _subject :: AudioWain,
    _other :: Object,
    _weanlings :: [AudioWain],
    _universe :: U.Universe AudioWain,
    _summary :: Summary
  }
makeLenses ''Experiment

report :: String -> StateT Experiment IO ()
report = zoom universe . U.writeToLog

run :: [AudioWain] -> StateT (U.Universe AudioWain) IO [AudioWain]
run (me:w2:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last wain standing!"
  u <- get
  x <- liftIO $ chooseObject (view U.uFrequencies u) w2
                       (view U.uPatternDB u)
  p <- U.popSize
  let e = Experiment { _subject = me,
                       _other = x,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
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
--  subject %= W.autoAdjustBoredom
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
  :: AudioWain -> Object
    -> StateT (U.Universe AudioWain) IO
        (Response Action, AudioWain)
chooseAction3 w obj = do
  whenM (use U.uShowPredictorModels)
    (mapM_ U.writeToLog . AW.describePredictorModels $ w)
  let (lds, sps, rplos, aos, r, w')
        = W.chooseAction [O.objectAppearance obj] w
  -- whenM (use U.uGenFmris) (writeFmri w)
  whenM (use U.uGenFmris)
    (mapM_ U.writeToLog . AW.describeClassifierModels $ w)
  whenM (use U.uShowPredictions) $ do
    mapM_ U.writeToLog $ scenarioReport sps
    mapM_ U.writeToLog $ responseReport rplos
    mapM_ U.writeToLog $ decisionReport aos
  let bmuInfo
        = formatBMUs . map fst . sortBy (comparing snd) . head $ lds
  U.writeToLog $ agentId w ++ " sees " ++ O.objectId obj
    ++ ", classifies it as " ++ bmuInfo
    ++ " and chooses to " ++ show (_action r)
    ++ " predicting the outcomes " ++ show (_outcomes r)
  return (r, w')

formatBMUs :: [Cl.Label] -> String
formatBMUs (cBMU:cBMU2:_) = show cBMU ++ " (alt. " ++ show cBMU2 ++ ")"
formatBMUs (cBMU:_)       = show cBMU
formatBMUs _ = error "no BMUs"

chooseObject :: [Rational] -> AudioWain -> PatternDB -> IO Object
chooseObject freqs w db = do
  (img1, audioId1) <- evalStateT anyPattern db
  fromList $ zip [O.IObject img1 audioId1, O.AObject w] freqs

runAction :: Action -> StateT Experiment IO ()

--
-- Flirt
--
runAction Flirt = do
  applyFlirtationEffects
  obj <- use other
  unless (O.isAudio obj) flirt
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
  (summary.rEatCount) += 1

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
    else mapM_ (report) msgs

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

finishRound :: FilePath -> StateT (U.Universe AudioWain) IO ()
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

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe AudioWain) IO ()
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

-- TODO: Make the numbers configurable
idealPopControlDeltaE :: Double -> Double -> Double -> Int -> Double
idealPopControlDeltaE average total budget pop
  | average < 0.8 = min 0.08 $ (budget - total) / (fromIntegral pop)
  | otherwise     = 0.8 - average

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ otherWainEnergy
  d <- W.childEnergy <$> use subject
  e <- otherChildEnergy
  return (a + b, d + e)

otherWainEnergy :: StateT Experiment IO UIDouble
otherWainEnergy = do
  x <- use other
  case x of
    O.AObject w -> return $ view W.energy w
    _           -> return 0
  
otherChildEnergy :: StateT Experiment IO Double
otherChildEnergy = do
  x <- use other
  case x of
    O.AObject w -> return $ W.childEnergy w
    _           -> return 0
  
printStats :: [[Stats.Statistic]] -> StateT (U.Universe AudioWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

letSubjectReflect
  :: AudioWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wainBefore r = do
  w <- use subject
  obj <- use other
  let p = O.objectAppearance obj
  let energyBefore = view W.energy wainBefore
  let boredomBefore = view W.boredom wainBefore
  let passionBefore = view W.passion wainBefore
  let happinessBefore = W.happiness wainBefore
  energyAfter <- use (subject . W.energy)
  boredomAfter <- use (subject . W.boredom)
  passionAfter <- use (subject . W.passion)
  happinessAfter <- W.happiness <$> use subject
  let deltaH = uiToDouble happinessAfter - uiToDouble happinessBefore
  assign (summary . rDeltaEToReflectOn)
    (uiToDouble energyAfter - uiToDouble energyBefore)
  assign (summary . rDeltaBToReflectOn)
    (uiToDouble boredomAfter - uiToDouble boredomBefore)
  assign (summary . rDeltaPToReflectOn)
    (uiToDouble passionAfter - uiToDouble passionBefore)
  assign (summary . rDeltaHToReflectOn) deltaH
  let (w', err) = W.reflect [p] r wainBefore w
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
  let w' = W.imprint [p] a w
  assign subject w' 
  
writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe AudioWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths :: [AudioWain] -> StateT (U.Universe AudioWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age " ++ show (view W.age w))
