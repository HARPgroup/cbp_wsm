#!/bin/csh

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  remove_river_directories.csh scenario '
      echo ' or     remove_river_directories.csh scenario tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

########## UCIs
  rm -r $tree/tmp/uci/river/$scenario

########## wdms
  rm -r $tree/tmp/wdm/river/$scenario

########## output space
  rm -r $tree/output/eos/annual/$scenario
  rm -r $tree/output/eos/aveann/$scenario
  rm -r $tree/output/eos/daily/$scenario
  rm -r $tree/output/eos/monthly/$scenario
#  rm -r $tree/output/eos/summary/$scenario

  rm -r $tree/output/eof/annual/$scenario
  rm -r $tree/output/eof/aveann/$scenario
  rm -r $tree/output/eof/daily/$scenario
  rm -r $tree/output/eof/monthly/$scenario
#  rm -r $tree/output/eof/summary/$scenario

  rm -r $tree/output/river/annual/$scenario
  rm -r $tree/output/river/aveann/$scenario
  rm -r $tree/output/river/avemon/$scenario
  rm -r $tree/output/river/cfd/$scenario
  rm -r $tree/output/river/daily/$scenario
  rm -r $tree/output/river/monthly/$scenario
  rm -r $tree/output/river/peaks/$scenario
  rm -r $tree/output/river/rating/$scenario
  rm -r $tree/output/river/stats/$scenario
  rm -r $tree/output/river/summary/$scenario
  rm -r $tree/output/river/window/$scenario
  rm -r $tree/output/river/scenario_compare/$scenario

  rm -r $tree/output/del/tfs/annual/$scenario
  rm -r $tree/output/del/tfs/aveann/$scenario
  rm -r $tree/output/del/tfs/monthly/$scenario
  rm -r $tree/output/del/dfs/annual/$scenario
  rm -r $tree/output/del/dfs/aveann/$scenario
  rm -r $tree/output/del/dfs/monthly/$scenario
  rm -r $tree/output/del/lseg/annual/$scenario
  rm -r $tree/output/del/lseg/aveann/$scenario
  rm -r $tree/output/del/lseg/monthly/$scenario

  rm -r $tree/output/pltgen/river/$scenario

#  rm -r $tree/output/input/$scenario

  rm -r $tree/output/etm/$scenario

  rm -r $tree/output/hspf/river/out/$scenario
  rm -r $tree/output/hspf/river/ech/$scenario

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

