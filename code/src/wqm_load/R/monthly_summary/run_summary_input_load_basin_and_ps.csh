#!/bin/csh

  if (${#argv} != 4) then
      echo ' '
      echo 'usage:  run_summary_input_load.csh scenario basin startyear endyear'
      echo ' '
      echo ' example scenario CH3D130513wq_s130712, GYp532cal '
      echo ' '
      echo ' Before you run this, you need (same # years):'
      echo ' ./out/input/*monthly*/loads/ Use: code/src/R/monthly_summary/run_input_monthly_load.csh'
      echo ' '
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set startyear = $argv[3]
  set endyear = $argv[4]


  Rscript summary_input_load_basin.r $scenario $basin $startyear $endyear

  Rscript summary_input_load_basin_ps.r $scenario $basin $startyear $endyear

