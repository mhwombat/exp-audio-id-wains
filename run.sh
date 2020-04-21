#!/bin/bash

trainingDir=$HOME/TI46/HTK_MFCC_not_endpointed/TRAIN-RAW
testDir=$HOME/TI46/HTK_MFCC_not_endpointed/TEST-RAW

function runit {
  echo $*
  time (~/nosync/sandboxes/exp-audio-id-wains/bin/exp-audio-id-wains-single-test \
    $trainingDir $testDir $1 $2 $3 $4 $5 $6 $7 $8) > singleWain-$1-$2-$3-$4-$5-$6-$7-$8.log 2>&1
}

#runit 0.00018 0.1 0.001 0.1 0.001 1 159 64

#runit 0.004 0.1 0.001 0.1 0.001 1 159 64
runit 0.005 0.1 0.001 0.1 0.001 1 159 64
#runit 0.005 0.1 0.001 0.1 0.001 1 159 128
#runit 0.006 0.1 0.001 0.1 0.001 1 159 64
#runit 0.01 0.1 0.001 0.1 0.001 1 159 64
#runit 0.01 0.1 0.001 0.1 0.001 1 159 128

