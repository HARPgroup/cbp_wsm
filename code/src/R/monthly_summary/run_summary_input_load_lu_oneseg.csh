#!/bin/csh

  if (${#argv} != 6) then
    echo ' '
    echo 'usage:  run_summary_input_load.csh scenario basin landuse startyear endyear input_type'
    echo ' '
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = $argv[3]
  set startyear = $argv[4]
  set endyear = $argv[5]
  set intype = $argv[6]


  Rscript summary_input_load_basin_lu.r $scenario $basin $lu $startyear $endyear $intype





