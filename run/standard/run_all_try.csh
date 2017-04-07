#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
#  if (${#argv} != 3) then
#    if (${#argv} != 2) then
#      echo ' '
#      echo 'usage:   runall.csh scenario basin'
#      echo ' or      runall.csh scenario basin tree'
#      echo ' '
#      exit
#    endif
#  endif

  set scenario = p532cal_062211  #$argv[1]
  set basin = stmary   #532hyd_PWATER  #stmary          #$argv[2]
  #if (${#argv} == 3) then
  #  set tree = $argv[3]
  #else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  #endif

  $tree/run/try_scripts/make_land_directories.csh $scenario $tree

  $tree/run/try_scripts/make_river_directories.csh $scenario $tree

  $tree/run/try_scripts/run_lug.csh $scenario $basin $tree   

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in lug '
    cat problem
    rm problem
    exit
  endif


  $tree/run/try_scripts/run_rug.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in rug '
    cat problem
    rm problem
    exit
  endif

echo "finished rug"
  $tree/run/try_scripts/run_land.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in land '
    cat problem
    rm problem
    exit
  endif

echo "Finished land"



  $tree/run/try_scripts/run_etm.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in etm '
    cat problem
    rm problem
    exit
  endif

 echo "Finished etm"

  $tree/run/try_scripts/run_river.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    rm problem
    exit
  endif

echo "Finished river"

  $tree/run/try_scripts/run_scenario_postproc.csh $scenario $basin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
    cat problem
    rm problem
    exit
  endif

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$/
  endif

echo "All finished"
