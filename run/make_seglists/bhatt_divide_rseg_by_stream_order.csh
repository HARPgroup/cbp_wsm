#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage: divide_rseg_by_stream_order.csh river_scen seglist temp_dir'
      echo ' '
      echo '  the .riv extension is assumed on the seglist'
      echo ' '
      exit
    endif
  endif

  set rscen = $argv[1]
  set seglistfile = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
#    mkdir -p ../../tmp/scratch/temp$$/
#    cd ../../tmp/scratch/temp$$/
    set tempdir = $argv[3]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

  if (-e problem) then
    rm problem
  endif

   echo $rscen $seglistfile | $tree/code/bin/divide_rseg_by_stream_order.exe

  if (-e problem) then
    echo ' '
    cat problem
    rm problem
    exit
  endif

  if (${#argv} == 3) then
    cd ../
#    rm -r temp$$
    rm -r $tempdir
  endif

