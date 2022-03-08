#!/bin/csh

  if (${#argv} != 4) then
    echo ' '
    echo ' usage:  gy_etm_monthly_parallel.csh scenario basin year1 year2'
    echo ' to run all segments use allP6'
    echo ' '
    exit
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]

  source ../../config/seglists/${basin}.riv

  @ i = 1
  foreach seg ($segments)
    
    sbatch gy_etm_monthly.csh $scenario $seg $year1 $year2 `uuidgen`-`printf "%04d" $i`
    @ i += 1

  end
