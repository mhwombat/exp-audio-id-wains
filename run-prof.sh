#!/bin/bash

trainingDir=$HOME/TI46/HTK_MFCC_not_endpointed/TRAIN-RAW
testDir=$HOME/TI46/HTK_MFCC_not_endpointed/TEST-RAW

function runit {
  echo $*
  cabal run exp-audio-id-wains-single-test \
    $trainingDir $testDir $1 $2 $3 $4 $5 $6 $7 -- +RTS -h -p > singleWain-prof.log 2>&1
  hp2ps -c exp-audio-id-wains-single-test.hp
}

runit 0.00018 0.1 0.001 0.1 0.001 1 159
