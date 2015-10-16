------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.AudioID.Action
-- Copyright   :  (c) Amy de Buitléir 2012-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module ALife.Creatur.Wain.AudioID.Action
  (
    Action(..),
    correct,
    correctActions,
    numActions
  ) where

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.Pretty (Pretty)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

-- The actions are listed in order of decreasing genetic dominance.
data Action = Flirt | Identify_0 | Identify_1 | Identify_2 | Identify_3
                | Identify_4 | Identify_5 | Identify_6 | Identify_7
                | Identify_8 | Identify_9
                deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Random Action where
  randomR (a,b) g = (toEnum n, g')
    where (n, g') = randomR (fromEnum a, fromEnum b) g
  random = randomR (minBound,maxBound)

correct :: Action -> Int -> Bool
correct Identify_0 0 = True
correct Identify_1 1 = True
correct Identify_2 2 = True
correct Identify_3 3 = True
correct Identify_4 4 = True
correct Identify_5 5 = True
correct Identify_6 6 = True
correct Identify_7 7 = True
correct Identify_8 8 = True
correct Identify_9 9 = True
correct _ _ = False

correctActions :: [Action]
correctActions = [ Identify_0, Identify_1, Identify_2, Identify_3,
                   Identify_4, Identify_5, Identify_6, Identify_7,
                   Identify_8, Identify_9, Flirt ]
numActions :: Int
numActions = 1 + fromEnum (maxBound :: Action)
