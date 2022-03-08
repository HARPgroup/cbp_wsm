#!/bin/csh

  if (${#argv} != 3) then
    if (${#argv} != 4) then
      echo ' '
      echo 'usage:  remove_land_directories.csh scenario '
      echo ' or     remove_land_directories.csh scenario tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set DFscen   = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    set tempdir = $argv[3]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
    #mkdir -p ../../tmp/scratch/temp$$/
    #cd ../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse

  set landuse = ($perlnds $implnds)

  
########## output space
  rm -r $tree/output/input/$scenario

  rm -r $tree/output/wqm_input/*/$scenario
  
  rm -r $tree/sumout/aveann/$scenario
  rm -r $tree/sumout/annual/$scenario
  rm -r $tree/sumout/monthly/$scenario
  rm -r $tree/sumout/aveann/${scenario}_DF_${DFscen}

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif
