#!/bin/csh

  set scenario = $argv[1]

  source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  source $tree/run/fragments/set_landuse

  set landuse = ($perlnds $implnds)

  
########## pltgens
  rm -r $tree/output/pltgen/land/$scenario

########## land 
  rm -r $tree/output/eos/Rvars/$scenario
  rm -r $tree/output/eos/annual/$scenario
  rm -r $tree/output/eos/monthly/$scenario
  rm -r $tree/output/eos/aveann/$scenario

########## river
  rm -r $tree/output/river/daily/$scenario
  rm -r $tree/output/river/aveann/$scenario
  rm -r $tree/output/river/peaks/$scenario
  rm -r $tree/output/river/annual/$scenario
  rm -r $tree/output/river/monthly/$scenario
  rm -r $tree/output/river/rating/$scenario
  rm -r $tree/output/river/stats/$scenario
  rm -r $tree/output/river/summary/$scenario
  rm -r $tree/output/river/window/$scenario
  rm -r $tree/output/river/cfd/$scenario

  rm -r $tree/output/data/annual/$scenario
  rm -r $tree/output/data/monthly/$scenario
  rm -r $tree/output/data/aveann/$scenario

  rm -r $tree/output/del/tfs/annual/$scenario
  rm -r $tree/output/del/tfs/aveann/$scenario
  rm -r $tree/output/del/dfs/annual/$scenario
  rm -r $tree/output/del/dfs/aveann/$scenario
  rm -r $tree/output/del/lseg/annual/$scenario
  rm -r $tree/output/del/lseg/aveann/$scenario
  rm -r $tree/output/del/lseg/monthly/$scenario

  rm -r $tree/output/pltgen/river/$scenario

