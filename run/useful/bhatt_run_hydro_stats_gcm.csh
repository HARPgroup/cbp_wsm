#!/bin/csh
# csh bhatt_run_hydro_stats_gcm.csh p532062211_H allBay 1985 1989 water.yr tempDIR
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 6) then
    if (${#argv} != 7) then
      echo ' '
      echo 'usage:  run_river_stats.csh scenario calscen basin year1 year2 '
      echo ' or     run_river_stats.csh scenario calscen basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]
  set postfix = $argv[5]
  set tempdir = $argv[6]

  if (${#argv} == 7) then
    set tree = $argv[7]
  else
    source ../fragments/set_tree
#    mkdir -p ../../tmp/scratch/temp$$/
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
#    cd ../../tmp/scratch/temp$$/
    pwd
  endif
  

  source $tree/config/seglists/${basin}.riv

  if (-e problem) then
    rm problem
  endif


####### DO RIVER CALCULATIONS NEXT
  rm -f $tree/tmp/wdm/river/$scenario/stream/*_water.yr_FLO.stats.csv
  rm -f $tree/tmp/wdm/river/$scenario/stream/*_Nov-Apr_FLO.stats.csv
  rm -f $tree/tmp/wdm/river/$scenario/stream/*_May-Oct_FLO.stats.csv
  rm -f $tree/tmp/wdm/river/$scenario/stream/*_7dMinCY_FLO.stats.csv
  rm -f $tree/tmp/wdm/river/$scenario/stream/*_1dMaxWY_FLO.stats.csv

#  echo $scenario, $year1, $year2, $postfix, 'Water.Year.Mean.Flow.as.(ac-ft-per-hr)' | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe  
#  echo $scenario, $year1, $year2,'Nov-Apr', 'Nov-Apr.Mean.Flow.as.(ac-ft-per-hr)' | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
#  echo $scenario, $year1, $year2,'May-Oct', 'May-Oct.Mean.Flow.as.(ac-ft-per-hr)'| $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
#  echo $scenario, $year1, $year2,'7dMinCY', '7DayMin.ClimateYr.Flow.as.(ac-ft-per-hr)'| $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
#  echo $scenario, $year1, $year2,'1dMaxWY', '1DayMax.WaterYr.Flow.as.(ac-ft-per-hr)'| $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe

  echo $scenario, $year1, $year2, $postfix, 'Water.Year.Mean.Flow.as.(cfs)'  | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
  echo $scenario, $year1, $year2,'Nov-Apr', 'Nov-Apr.Mean.Flow.as.(cfs)'     | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
  echo $scenario, $year1, $year2,'May-Oct', 'May-Oct.Mean.Flow.as.(cfs)'     | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
  echo $scenario, $year1, $year2,'7dMinCY', '7DayMin.ClimateYr.Flow.as.(cfs)'| $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe
  echo $scenario, $year1, $year2,'1dMaxWY', '1DayMax.WaterYr.Flow.as.(cfs)'  | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowHeader.exe

  foreach seg ($segments)

      echo $scenario $seg, $year1, $year2, $postfix | $tree/code/src/postproc/river/stats_bhatt/GCM_FlowStats.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  end

  mkdir -p $tree/sumout/gcm-stats/$scenario
  mv $tree/tmp/wdm/river/$scenario/stream/*_water.yr_FLO.stats.csv $tree/sumout/gcm-stats/$scenario
  mv $tree/tmp/wdm/river/$scenario/stream/*_Nov-Apr_FLO.stats.csv  $tree/sumout/gcm-stats/$scenario
  mv $tree/tmp/wdm/river/$scenario/stream/*_May-Oct_FLO.stats.csv  $tree/sumout/gcm-stats/$scenario
  mv $tree/tmp/wdm/river/$scenario/stream/*_7dMinCY_FLO.stats.csv  $tree/sumout/gcm-stats/$scenario
  mv $tree/tmp/wdm/river/$scenario/stream/*_1dMaxWY_FLO.stats.csv  $tree/sumout/gcm-stats/$scenario

  if (${#argv} == 6) then
    cd ../
#    rm -r temp$$/
    rm -r $tempdir
  endif

      
