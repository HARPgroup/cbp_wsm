#!/bin/csh

  if (${#argv} != 4 ) then
    echo 'usage: summarize_nps_and_ps_from_wqm57k_inputs.csh WQM_SCENARIO RIVER_SCENARIO YEAR1 YEAR2'
    echo '   will look for WQM input files in '
    echo '   ../../output/wdm_input/WQM_SCENARIO/'
    echo ' '
    echo '   the RIVER_SCENARIO is just to populate the variables types'
    exit
  endif

  set wqmscen = $argv[1]
  set rscen = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  source ../fragments/abs_tree
  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  echo 'running'

  echo $wqmscen $rscen $year1 $year2 | $tree/code/bin/summarize_wqm57k_by_cell_ps.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

  echo $wqmscen $rscen $year1 $year2 | $tree/code/bin/summarize_wqm57k_by_cell_nps.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

  cd ../
  rm -r temp$$

