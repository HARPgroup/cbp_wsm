#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo 'usage: read_dot_out.csh scenario basin year1 year2'
      echo ' or    read_dot_out.csh scenario basin year1 year2 tree'
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  if (${#argv} == 4) then
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  else
    set tree = $argv[5]
  endif

  echo $scenario $basin $year1 $year2 |  $tree/code/bin/read_dot_out_files.exe

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in reading dot out files'
    cat problem
    rm problem
    exit
  endif


