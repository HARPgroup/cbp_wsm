#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage: splitseglists.csh seglistname number_of_splits'
      echo ' '
      echo ' '
      exit
    endif
  endif

  set seglistfile = $argv[1]
  set nsplits = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  if (-e problem) then
    rm problem
  endif

   echo $seglistfile $nsplits | $tree/code/bin/splitseglists.exe

  if (-e problem) then
    echo ' '
    cat problem
    rm problem
    exit
  endif

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

