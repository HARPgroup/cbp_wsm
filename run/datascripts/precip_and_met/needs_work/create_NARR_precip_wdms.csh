#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script creates new precipitation wdms.  It may replace wdms'
    echo '  already in use. Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: create_precip_wdms.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script creates new precipitation wdms.  It may replace wdms'
      echo '  already in use. Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_precip_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../fragments/set_tree

  set code = $tree/pp/src/calibration_utils/wdm/bin/rite1wdm_prec_NARR
  set pradscen = narr804
  set basin = 'justone'

  source $tree/config/seglists/${basin}.land

  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)
    cp $tree/config/blank_wdm/blank_prad.wdm prad_${seg}.wdm 
    echo $seg | $code
    mv prad_${seg}.wdm $tree/input/scenario/climate/prad/$pradscen/
    if (-e problem) then
      cat problem
      exit
    endif
  end

  cd ../
  rm -r temp$$
