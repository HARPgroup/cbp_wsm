#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script modifies the precipitation wdms.  It may replace wdms'
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
      echo ' running this script modifies the precipitation wdms.  It may replace wdms'
      echo '  already in use. Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: create_precip_wdms.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../fragments/set_tree

  set code = $tree/pp/src/calibration_utils/wdm/bin/flash10_from_uniform
  set baseprad = ns611a
  set uniformprad = a_10_ncar_M
  set f10prad = a_10_ncar_F10
  set basin = 'all'

  source $tree/config/seglists/${basin}.land

  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  if (-e problem) then
    rm problem
  endif

  mkdir -p $tree/input/scenario/climate/prad/$f10prad/
  foreach seg ($segments)

    cp $tree/input/scenario/climate/prad/$baseprad/prad_${seg}.wdm base_${seg}.wdm
    cp $tree/input/scenario/climate/prad/$uniformprad/prad_${seg}.wdm uniform_${seg}.wdm

    echo $seg | $code

    rm base_${seg}.wdm

    if (-e problem) then
      rm uniform_${seg}.wdm
      cat problem
      exit
    else
      mv uniform_${seg}.wdm $tree/input/scenario/climate/prad/$f10prad/prad_${seg}.wdm
    endif

  end

  cd ../
  rm -r temp$$
