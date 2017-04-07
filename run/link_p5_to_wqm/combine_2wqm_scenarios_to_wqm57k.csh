#!/bin/csh

  if (${#argv} != 1 ) then
    echo ' '
    echo ' '
    echo 'usage: combine_2wqm_scenarios_to_wqm57k.csh RIVER_SCENARIO WQM_SCENARIO1 WQM_SCENARIO2 YEAR1 YEAR2'
    echo ' '
    echo '   will look for files in '
    echo '   ../tmp/wdm/river/RIVER_SCENARIO/stream'
    echo ' '
    echo '   RIVER_SCENARIO point to a directory under ./input/scenario/wqm/Combine2Scens/'
    echo ' '
    exit
  endif

  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates wqm scenario input '
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: combine_2wqm_scenario_to wqm57k.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../fragments/set_tree
  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

############ USER DEFINED VARIABLES, SET THESE BELOW APPROPIRATELY
  set rscen    = p53_180_12_and_113_712
  set wqmscen1 = p53_TSAA_p53_180_12
  set wqmscen2 = p53_E310Pbased_p53_113_712
  set year1 = 1991
  set year2 = 2000
############ END OF USER VARIABLES

######## make directories
  mkdir -p $tree/output/wqm_input/${rscen}/

  echo 'running'

  if (-e problem) then
    rm problem
  endif

  echo $rscen $wqmscen1 $wqmscen2 $year1 $year2 | $tree/code/bin/combine_2wqm_scenarios_ps_to_wqm57k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

  echo $rscen $wqmscen1 $wqmscen2 $year1 $year2 | $tree/code/bin/combine_2wqm_scenarios_nps_to_wqm57k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

