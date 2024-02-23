#!/bin/csh

  if (${#argv} != 2) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  make_river_directories.csh scenario '
      echo ' or     make_river_directories.csh scenario tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    set tempdir = $argv[2]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

########## UCIs
  mkdir -p $tree/tmp/uci/river/$scenario

########## wdms
  mkdir -p $tree/tmp/wdm/river/$scenario
  mkdir -p $tree/tmp/wdm/river/$scenario/eos
  mkdir -p $tree/tmp/wdm/river/$scenario/stream
  mkdir -p $tree/tmp/wdm/river/$scenario/data

########## output space
  mkdir -p $tree/output/eof/daily/$scenario
  mkdir -p $tree/output/eof/monthly/$scenario
  mkdir -p $tree/output/eof/annual/$scenario
  mkdir -p $tree/output/eof/aveann/$scenario
#  mkdir -p $tree/output/eof/summary/$scenario

  mkdir -p $tree/output/eos/daily/$scenario
  mkdir -p $tree/output/eos/monthly/$scenario
  mkdir -p $tree/output/eos/annual/$scenario
  mkdir -p $tree/output/eos/aveann/$scenario
#  mkdir -p $tree/output/eos/summary/$scenario

  mkdir -p $tree/output/eop/aveann/$scenario
  mkdir -p $tree/output/eor/aveann/$scenario

  mkdir -p $tree/output/river/daily/$scenario
  mkdir -p $tree/output/river/aveann/$scenario
  mkdir -p $tree/output/river/summary/$scenario
  mkdir -p $tree/output/river/annual/$scenario
  mkdir -p $tree/output/river/monthly/$scenario
  mkdir -p $tree/output/river/avemon/$scenario
  mkdir -p $tree/output/river/peaks/$scenario
  mkdir -p $tree/output/river/stats/$scenario
  mkdir -p $tree/output/river/window/$scenario
  mkdir -p $tree/output/river/rating/$scenario
  mkdir -p $tree/output/river/cfd/$scenario
  mkdir -p $tree/output/river/scenario_compare/$scenario

  mkdir -p $tree/output/del/tfs/monthly/$scenario
  mkdir -p $tree/output/del/tfs/annual/$scenario
  mkdir -p $tree/output/del/tfs/aveann/$scenario
  mkdir -p $tree/output/del/dfs/monthly/$scenario
  mkdir -p $tree/output/del/dfs/annual/$scenario
  mkdir -p $tree/output/del/dfs/aveann/$scenario
  mkdir -p $tree/output/del/lseg/monthly/$scenario
  mkdir -p $tree/output/del/lseg/annual/$scenario
  mkdir -p $tree/output/del/lseg/aveann/$scenario

  mkdir -p $tree/output/pltgen/river/$scenario

  mkdir -p $tree/output/input/$scenario

  mkdir -p $tree/sumout/aveann/$scenario
  mkdir -p $tree/sumout/annual/$scenario
  mkdir -p $tree/sumout/monthly/$scenario
  #mkdir -p $tree/sumout/aveann/${scenario}_DF_${DFscen}

  mkdir -p $tree/output/etm/$scenario

  mkdir -p $tree/output/hspf/river/out/$scenario
  mkdir -p $tree/output/hspf/river/ech/$scenario

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif

