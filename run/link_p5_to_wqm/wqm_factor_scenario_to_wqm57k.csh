#!/bin/csh

  if (${#argv} != 4 ) then
    echo ' '
    echo ' '
    echo 'usage: run_nps_and_ps_to_wqm57k.csh RIVER_SCENARIO YEAR1 YEAR2 FACTOR_SCENARIO'
    echo ' '
    echo '   will look for files in '
    echo '   ../tmp/wdm/river/RIVER_SCENARIO/stream'
    echo ' '
    echo '   FACTOR_SCENARIO point to a directory under ./input/scenario/wqm/WqmFactors/'
    echo ' '
    exit
  endif

  set rscen = $argv[1]
  set year1 = $argv[2]
  set year2 = $argv[3]
  set modscen = $argv[4]

  source ../fragments/set_tree
  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  mkdir -p $tree/output/wqm_input/${rscen}_${modscen}/

  echo 'running'

  if (-e problem) then
    rm problem
  endif

  echo $rscen $modscen $year1 $year2 | $tree/code/bin/wqm_factor_scenario_ps_to_wqm57k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

  echo $rscen $modscen $year1 $year2 | $tree/code/bin/wqm_factor_scenario_nps_to_wqm57k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

