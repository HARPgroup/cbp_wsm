#!/bin/csh

    if (${#argv} != 1) then
      echo ' '
      echo 'usage:  input_monthly_load.r scenario'
      echo ' example scenario CH3D130513wq_s130712, GYp532cal '
      echo ' '
      echo ' '
      exit
    endif

  set scenario = $argv[1]

  source ./../seglists/land_water.lrseg

  Rscript directory1.r $scenario

  foreach seg ($segments)

    sbatch run_input_monthly_load_oneseg.csh $scenario $seg 

    sbatch run_input_monthly_load_feed_oneseg.csh $scenario $seg

  end

