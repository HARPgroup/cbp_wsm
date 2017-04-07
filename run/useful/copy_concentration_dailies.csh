#!/bin/csh

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo 'usage: copy_eos_wdms.csh from_scenario to_scenario basin'
      echo ' or    copy_eos_wdms.csh from_scenario to_scenario basin tree'
      exit
    endif
  endif

  set scen1 = $argv[1]
  set scen2 = $argv[2]
  set basin = $argv[3]
  if (${#argv} == 3) then
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  else
    set tree = $argv[4]
  endif

  source $tree/config/seglists/${basin}.riv

  foreach seg ($segments)
    cp -v $tree/output/river/daily/$scen1/${seg}.* $tree/output/river/daily/$scen2
    cp -v $tree/output/river/window/$scen1/${seg}.* $tree/output/river/window/$scen2
  end

