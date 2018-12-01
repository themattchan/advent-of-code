#! /bin/sh

cat input | awk '{s+=$1} END {print s}'
