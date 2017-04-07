#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' usage '
    echo 'check_hydrograph_of_WQ_data.csh basin'
    echo ' '
    exit
  endif

  source ../../fragments/set_tree

  set constituents = (CHLA )
  set constituents = (CHLA NO3X ORGN PO4X TOTN TSSX DOXX NH3X ORGP TOTP WTMP)

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

  source $tree/config/seglists/$argv[1].riv

######## SET TIME PERIOD TO SPLIT
  set year1 = 1985
  set year2 = 2005

  if (-e problem) then
    rm problem
  endif

  foreach const ($constituents)
    foreach seg ($segments)
      echo $seg $const $year1 $year2 | $tree/code/bin/check_hydrograph_of_WQ_data.exe
      if (-e problem) then
         cat problem
         exit
      endif
    end 
  end


  cd ../
  rm -r temp$$

