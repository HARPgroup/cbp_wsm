#!/bin/csh

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  input_monthly_load.r scenario segment '
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set seg = $argv[2]

  Rscript input_monthly_load_feedspace.r $scenario $seg



