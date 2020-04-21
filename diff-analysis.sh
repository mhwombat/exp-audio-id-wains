#!/bin/bash

diff=/home/eamybut/nosync/sandboxes/exp-audio-id-wains/bin/exp-audio-id-diff-analysis
dir=$HOME/TI46/HTK_MFCC_not_endpointed/TRAIN-RAW/

function runit {
  echo $*
  /home/eamybut/nosync/sandboxes/exp-audio-id-wains/bin/exp-audio-id-diff-analysis \
    $dir $1 > diff-analysis-$1.log
}

# runit 10
# runit 20
# runit 30
# runit 40
# runit 50
# runit 60
# runit 70
# runit 80
# runit 90
# runit 100
# runit 110
# runit 120
# runit 130
# runit 140
# runit 150
runit 159
# runit 160

for file in diff-analysis-*.log
do
  echo -n "$file,"
  tail -n 5 $file | sed 's/.*=//' | tr '\n' ',' | sed 's/,test complete,/\n/'
done
