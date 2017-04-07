#!/bin/csh

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo 'usage: copy_dot_etm_files.csh from_scenario to_scenario basin'
      echo ' or    copy_dot_etm_files.csh from_scenario to_scenario basin tree'
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
    mv -v $tree/output/etm/$scen1/*to${seg}.etm $tree/output/etm/$scen2/
  end

