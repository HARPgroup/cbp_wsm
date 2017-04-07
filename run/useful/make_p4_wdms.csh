#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
########################################################################
##  This script creates phase 4 wdms from pltgen files                ##
########################################################################


  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates calibration point source wdms '
    echo '  It may replace wdms already in use. '
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_calib_ps_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates calibration point source wdms '
      echo '  It may replace wdms already in use. '
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_calib_ps_wdms.csh GO'
      echo ' '
      exit
    endif
  endif
  
  source ../fragments/set_tree
  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  source $tree/config/seglists/phase4.calib

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    cp $tree/config/blank_wdm/river.wdm $tree/tmp/wdm/river/phase4/stream/$seg.wdm

    echo $seg | $tree/code/bin/create_p4_wdms.exe

    if (-e problem) then
      echo ' '
      cat problem
      rm problem
      exit
    endif

  end

  cd ../
  rm -r temp$$
