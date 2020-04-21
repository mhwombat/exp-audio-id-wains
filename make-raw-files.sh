#!/bin/sh

dir=$1
mkdir ${dir}-RAW

for filepath in `ls ${dir}/*.mfc`
do
    echo Converting $filepath
    file=$(basename "$filepath")
    file="${file%.*}"
    HList -r ${dir}/${file}.mfc > ${dir}-RAW/${file}.raw
done
