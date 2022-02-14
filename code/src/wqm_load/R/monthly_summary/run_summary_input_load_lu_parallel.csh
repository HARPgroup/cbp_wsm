#!/bin/csh

  if (${#argv} != 4) then

     echo ' '
     echo 'usage:  run_summary_input_load.csh scenario basin startyear endyear'
     echo ' '
     echo ' Before you run this, you need (same # years):'
     echo ' ./out/input/*monthly*/loads/ Use: code/src/R/monthly_summary/run_input_monthly_load.csh'
     echo ' '
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set startyear = $argv[3]
  set endyear = $argv[4]

  source ./../../../../run/fragments/set_landuse
  source ./../vars/set_inputtype

  Rscript directory2.r $scenario $basin

  foreach lu ( $perlnds $implnds )

    foreach intype ( $inputtype )
 
      sbatch run_summary_input_load_lu_oneseg.csh $scenario $basin $lu $startyear $endyear $intype

    end

  end





