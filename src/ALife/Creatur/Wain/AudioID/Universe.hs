------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.AudioID.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Universe for audio mining agents
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.AudioID.Universe
  (
    -- * Constructors
    Universe(..),
    loadUniverse,
    U.Agent,
    -- * Lenses
    uExperimentName,
    uClock,
    uLogger,
    uDB,
    uNamer,
    uChecklist,
    uStatsFile,
    uRawStatsFile,
    uShowPredictorModels,
    uShowPredictions,
    uShowScenarioReport,
    uShowResponseReport,
    uShowDecisionReport,
    uGenFmris,
    uSleepBetweenTasks,
    uPatternDB,
    uNumVectors,
    uVectorLength,
    uClassifierSizeRange,
    uPredictorSizeRange,
    uDevotionRange,
    uMaturityRange,
    uMaxAge,
    uInitialPopulationSize,
    uEnergyBudget,
    uAllowedPopulationRange,
    uPopControl,
    uFrequencies,
    uBaseMetabolismDeltaE,
    uEnergyCostPerClassifierModel,
    uChildCostFactor,
    uCorrectDeltaE,
    uIncorrectDeltaE,
    uFlirtingDeltaE,
    uPopControlDeltaE,
    uClassifierThresholdRange,
    uClassifierR0Range,
    uClassifierRfRange,
    uClassifierTfRange,
    uPredictorThresholdRange,
    uPredictorR0Range,
    uPredictorRfRange,
    uPredictorTfRange,
    uDefaultOutcomeRange,
    uStrictnessRange,
    uImprintOutcomeRange,
    uReinforcementDeltasRange,
    uPassionDeltaRange,
    uDepthRange,
    uCheckpoints,
    -- * Other
    U.agentIds,
    U.currentTime,
    U.genName,
    U.getAgent,
    U.popSize,
    U.store,
    U.writeToLog
  ) where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import ALife.Creatur.Persistent (Persistent, mkPersistent)
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Checkpoint as CP
import ALife.Creatur.Wain.Audio.PatternDB (PatternDB, mkPatternDB)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Exception (SomeException, try)
import Control.Lens hiding (Setting)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word8, Word16, Word64)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    _uExperimentName :: String,
    _uClock :: K.PersistentCounter,
    _uLogger :: SL.SimpleLogger,
    _uDB :: CFS.CachedFSDatabase a,
    _uNamer :: N.SimpleNamer,
    _uChecklist :: CL.PersistentChecklist,
    _uStatsFile :: FilePath,
    _uRawStatsFile :: FilePath,
    _uShowPredictorModels :: Bool,
    _uShowPredictions :: Bool,
    _uShowScenarioReport :: Bool,
    _uShowResponseReport :: Bool,
    _uShowDecisionReport :: Bool,
    _uGenFmris :: Bool,
    _uSleepBetweenTasks :: Int,
    _uPatternDB :: PatternDB,
    _uNumVectors :: Int,
    _uVectorLength :: Int,
    _uClassifierSizeRange :: (Word64, Word64),
    _uPredictorSizeRange :: (Word64, Word64),
    _uDevotionRange :: (UIDouble, UIDouble),
    _uMaturityRange :: (Word16, Word16),
    _uMaxAge :: Int,
    _uInitialPopulationSize :: Int,
    _uEnergyBudget :: Double,
    _uAllowedPopulationRange :: (Int, Int),
    _uPopControl :: Bool,
    _uFrequencies :: [Rational],
    _uBaseMetabolismDeltaE :: Double,
    _uEnergyCostPerClassifierModel :: Double,
    _uChildCostFactor :: Double,
    _uCorrectDeltaE :: Double,
    _uIncorrectDeltaE :: Double,
    _uFlirtingDeltaE :: Double,
    _uPopControlDeltaE :: Persistent Double,
    _uClassifierThresholdRange :: (UIDouble, UIDouble),
    _uClassifierR0Range :: (UIDouble, UIDouble),
    _uClassifierRfRange :: (UIDouble, UIDouble),
    _uClassifierTfRange :: (Word64, Word64),
    _uPredictorThresholdRange :: (UIDouble, UIDouble),
    _uPredictorR0Range :: (UIDouble, UIDouble),
    _uPredictorRfRange :: (UIDouble, UIDouble),
    _uPredictorTfRange :: (Word64, Word64),
    _uDefaultOutcomeRange :: (PM1Double, PM1Double),
    _uStrictnessRange :: (Word64, Word64),
    _uImprintOutcomeRange :: (PM1Double, PM1Double),
    _uReinforcementDeltasRange :: (PM1Double, PM1Double),
    _uPassionDeltaRange :: (UIDouble, UIDouble),
    _uDepthRange :: (Word8, Word8),
    _uCheckpoints :: [CP.Checkpoint]
  } deriving Show
makeLenses ''Universe

instance (A.Agent a, D.SizedRecord a) => U.Universe (Universe a) where
  type Agent (Universe a) = a
  type Clock (Universe a) = K.PersistentCounter
  clock = _uClock
  setClock u c = u { _uClock=c }
  type Logger (Universe a) = SL.SimpleLogger
  logger = _uLogger
  setLogger u l = u { _uLogger=l }
  type AgentDB (Universe a) = CFS.CachedFSDatabase a
  agentDB = _uDB
  setAgentDB u d = u { _uDB=d }
  type Namer (Universe a) = N.SimpleNamer
  agentNamer = _uNamer
  setNamer u n = u { _uNamer=n }
  type Checklist (Universe a) = CL.PersistentChecklist
  checklist = _uChecklist
  setChecklist u cl = u { _uChecklist=cl }

requiredSetting :: String -> Setting a
requiredSetting key
  = Setting key (error $ key ++ " not defined in configuration")

cExperimentName :: Setting String
cExperimentName = requiredSetting "experimentName"

cWorkingDir :: Setting FilePath
cWorkingDir = requiredSetting "workingDir"

cCacheSize :: Setting Int
cCacheSize = requiredSetting "cacheSize"

cShowPredictorModels :: Setting Bool
cShowPredictorModels = requiredSetting "showPredictorModels"

cShowPredictions :: Setting Bool
cShowPredictions = requiredSetting "showPredictions"

cShowScenarioReport :: Setting Bool
cShowScenarioReport = requiredSetting "showScenarioReport"

cShowResponseReport :: Setting Bool
cShowResponseReport = requiredSetting "showResponseReport"

cShowDecisionReport :: Setting Bool
cShowDecisionReport = requiredSetting "showDecisionReport"

cGenFmris :: Setting Bool
cGenFmris = requiredSetting "genFMRIs"

cSleepBetweenTasks :: Setting Int
cSleepBetweenTasks = requiredSetting "sleepTimeBetweenTasks"

cAudioDir :: Setting FilePath
cAudioDir = requiredSetting "audioDir"

cNumVectors :: Setting Int
cNumVectors = requiredSetting "numVectors"

cVectorLength :: Setting Int
cVectorLength = requiredSetting "vectorLength"

cClassifierSizeRange :: Setting (Word64, Word64)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cPredictorSizeRange :: Setting (Word64, Word64)
cPredictorSizeRange
  = requiredSetting "predictorSizeRange"

cDevotionRange :: Setting (UIDouble, UIDouble)
cDevotionRange = requiredSetting "devotionRange"

cMaturityRange :: Setting (Word16, Word16)
cMaturityRange = requiredSetting "maturityRange"

cMaxAge :: Setting Int
cMaxAge = requiredSetting "maxAge"

cInitialPopulationSize :: Setting Int
cInitialPopulationSize = requiredSetting "initialPopSize"

cAllowedPopulationRange :: Setting (Double, Double)
cAllowedPopulationRange = requiredSetting "allowedPopRange"

cPopControl :: Setting Bool
cPopControl = requiredSetting "popControl"

cFrequencies :: Setting [Rational]
cFrequencies = requiredSetting "frequencies"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cEnergyCostPerClassifierModel :: Setting Double
cEnergyCostPerClassifierModel
  = requiredSetting "energyCostPerClassifierModel"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cCorrectDeltaE :: Setting Double
cCorrectDeltaE = requiredSetting "correctDeltaE"

cIncorrectDeltaE :: Setting Double
cIncorrectDeltaE = requiredSetting "incorrectDeltaE"

cFlirtingDeltaE :: Setting Double
cFlirtingDeltaE = requiredSetting "flirtingDeltaE"

cClassifierThresholdRange :: Setting (UIDouble, UIDouble)
cClassifierThresholdRange = requiredSetting "classifierThresholdRange"

cClassifierR0Range :: Setting (UIDouble, UIDouble)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierRfRange :: Setting (UIDouble, UIDouble)
cClassifierRfRange = requiredSetting "classifierRfRange"

cClassifierTfRange :: Setting (Word64, Word64)
cClassifierTfRange = requiredSetting "classifierTfRange"

cPredictorThresholdRange :: Setting (UIDouble, UIDouble)
cPredictorThresholdRange = requiredSetting "predictorThresholdRange"

cPredictorR0Range :: Setting (UIDouble, UIDouble)
cPredictorR0Range = requiredSetting "predictorR0Range"

cPredictorRfRange :: Setting (UIDouble, UIDouble)
cPredictorRfRange = requiredSetting "predictorRfRange"

cPredictorTfRange :: Setting (Word64, Word64)
cPredictorTfRange = requiredSetting "predictorTfRange"

cDefaultOutcomeRange :: Setting (PM1Double, PM1Double)
cDefaultOutcomeRange = requiredSetting "defaultOutcomeRange"

cStrictnessRange :: Setting (Word64, Word64)
cStrictnessRange = requiredSetting "strictnessRange"

cImprintOutcomeRange :: Setting (PM1Double, PM1Double)
cImprintOutcomeRange = requiredSetting "imprintOutcomeRange"

cReinforcementDeltasRange :: Setting (PM1Double, PM1Double)
cReinforcementDeltasRange = requiredSetting "reinforcementDeltasRange"

cPassionDeltaRange :: Setting (UIDouble, UIDouble)
cPassionDeltaRange = requiredSetting "passionDeltaRange"

cDepthRange :: Setting (Word8, Word8)
cDepthRange = requiredSetting "depthRange"

cCheckpoints :: Setting [CP.Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- Path <$> makeRelativeToCurrentDirectory "wain.config"
  readResult <- try $ readSettings configFile
  case readResult of
    Right (_, GetSetting getSetting) ->
      return $ config2Universe getSetting
    Left (x :: SomeException) ->
      error $ "Error reading the config file: " ++ show x

config2Universe :: (forall a. Read a => Setting a -> a) -> Universe b
config2Universe getSetting =
  Universe
    {
      _uExperimentName = en,
      _uClock = K.mkPersistentCounter (workDir ++ "/clock"),
      _uLogger = SL.mkSimpleLogger (workDir ++ "/log/" ++ en ++ ".log"),
      _uDB = CFS.mkCachedFSDatabase (workDir ++ "/db")
               (getSetting cCacheSize),
      _uNamer = N.mkSimpleNamer (en ++ "_") (workDir ++ "/namer"),
      _uChecklist = CL.mkPersistentChecklist (workDir ++ "/todo"),
      _uStatsFile = workDir ++ "/statsFile",
      _uRawStatsFile = workDir ++ "/rawStatsFile",
      _uShowPredictorModels = getSetting cShowPredictorModels,
      _uShowPredictions = getSetting cShowPredictions,
      _uShowScenarioReport = getSetting cShowScenarioReport,
      _uShowResponseReport = getSetting cShowResponseReport,
      _uShowDecisionReport = getSetting cShowDecisionReport,
      _uGenFmris = getSetting cGenFmris,
      _uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      _uPatternDB = mkPatternDB audioDir nv,
      _uNumVectors = nv,
      _uVectorLength = getSetting cVectorLength,
      _uClassifierSizeRange = getSetting cClassifierSizeRange,
      _uPredictorSizeRange = getSetting cPredictorSizeRange,
      _uDevotionRange = getSetting cDevotionRange,
      _uMaturityRange = getSetting cMaturityRange,
      _uMaxAge = getSetting cMaxAge,
      _uInitialPopulationSize = p0,
      _uEnergyBudget = fromIntegral p0 * 0.5,
      _uAllowedPopulationRange = (a', b'),
      _uPopControl = getSetting cPopControl,
      _uFrequencies = getSetting cFrequencies,
      _uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      _uEnergyCostPerClassifierModel
        = getSetting cEnergyCostPerClassifierModel,
      _uChildCostFactor = getSetting cChildCostFactor,
      _uFlirtingDeltaE = getSetting cFlirtingDeltaE,
      _uCorrectDeltaE = getSetting cCorrectDeltaE,
      _uIncorrectDeltaE = getSetting cIncorrectDeltaE,
      _uPopControlDeltaE
        = mkPersistent 0 (workDir ++ "/popControlDeltaE"),
      _uClassifierThresholdRange = getSetting cClassifierThresholdRange,
      _uClassifierR0Range = getSetting cClassifierR0Range,
      _uClassifierRfRange = getSetting cClassifierRfRange,
      _uClassifierTfRange = getSetting cClassifierTfRange,
      _uPredictorThresholdRange = getSetting cPredictorThresholdRange,
      _uPredictorR0Range = getSetting cPredictorR0Range,
      _uPredictorRfRange = getSetting cPredictorRfRange,
      _uPredictorTfRange = getSetting cPredictorTfRange,
      _uDefaultOutcomeRange = getSetting cDefaultOutcomeRange,
      _uStrictnessRange = getSetting cStrictnessRange,
      _uImprintOutcomeRange = getSetting cImprintOutcomeRange,
      _uReinforcementDeltasRange = getSetting cReinforcementDeltasRange,
      _uPassionDeltaRange = getSetting cPassionDeltaRange,
      _uDepthRange = getSetting cDepthRange,
      _uCheckpoints = getSetting cCheckpoints
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        audioDir = getSetting cAudioDir
        p0 = getSetting cInitialPopulationSize
        (a, b) = getSetting cAllowedPopulationRange
        a' = round (fromIntegral p0 * a)
        b' = round (fromIntegral p0 * b)
        nv = getSetting cNumVectors
