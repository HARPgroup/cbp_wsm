#!/bin/csh

  set scenario = $argv[1]

  source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/

########## UCIs
  rm -r $tree/tmp/uci/river/$scenario

############ temp space
  rm -r $tree/run/river/$scenario

########## wdms
  rm -r $tree/tmp/wdm/river/$scenario


