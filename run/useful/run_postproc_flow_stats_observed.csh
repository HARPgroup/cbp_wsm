#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    echo ' '
    echo 'usage: run_postproc_observed_flow_stats.csh river_scenario basin year1 year2 '
    echo ' '
    exit
  endif

  set obscen = alldata
  set rscen = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  source ../fragments/set_tree

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY, EOS OUTPUTS DEFINED BY THE BODY THEY FLOW INTO  ########
  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif


####### DO RIVER CALCULATIONS NEXT
  
  foreach seg ($segments)

    echo $obscen,$seg,$year1,$year2,$rscen | $tree/code/bin/obs_only_stats.exe
    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  end
