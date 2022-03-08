#!/bin/csh

    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  input_monthly_load.r scenario segment'
      echo ' '
      exit
    endif

  set scenario = $argv[1]
  set seg = $argv[2]

  source ./../../../../run/fragments/set_landuse


  foreach lu ( $perlnds $implnds )

    Rscript input_monthly_load.r $scenario $seg $lu

  end


