#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set basin = $argv[2]
  source ../fragments/set_tree

  mkdir -p ../../tmp/scratch/temp$$
  cd ../../tmp/scratch/temp$$

  $tree/run/run_rug.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in rug '
    cat problem
    rm problem
    exit
  endif

  $tree/run/run_river.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    rm problem
    exit
  endif

  $tree/run/run_postproc.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    rm problem
    exit
  endif

  $tree/run/pltgen_river.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    rm problem
    exit
  endif

  cd ../
  rm -r temp$$

