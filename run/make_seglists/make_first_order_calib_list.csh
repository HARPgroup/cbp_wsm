#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage: make_first_order_calib_list.csh river_scen seglist'
      echo ' '
      echo '  the .calib extension is assumed on the seglist'
      echo ' '
      exit
    endif
  endif

  set rscen = $argv[1]
  set seglistfile = $argv[2]
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

   echo $rscen $seglistfile | $tree/code/bin/make_first_order_calib_list.exe

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

