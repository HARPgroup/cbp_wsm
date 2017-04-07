#!/bin/csh

  if (${#argv} != 1) then
    echo ' '
    echo ' running this script extracts diversions from wdms and writes a data file'
    echo '  It may replace existing data files'
    echo '  Check the script to make sure that the variables are correctly'
    echo '  set before continuing'
    echo ' '
    echo ' To make this script run, type: extract_diversions.csh GO'
    echo ' '
    exit
  endif
  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' running this script extracts diversions from wdms and writes a data file'
      echo '  It may replace existing data files'
      echo '  Check the script to make sure that the variables are correctly'
      echo '  set before continuing'
      echo ' '
      echo ' To make this script run, type: extract_diversions.csh GO'
      echo ' '
      exit
    endif
  endif

  source ../fragments/set_tree

  set code = $tree/pp/src/calibration_utils/wdm/bin/extract_diversions.exe
  set basin = allBay
  set year1 = 1984
  set year2 = 2005
  set divscen = ext05

  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

  if (-e problem) then
    rm problem
  endif

  echo $divscen $basin $year1 $year2 | $code

  if (-e problem) then
    cat problem
    exit
  endif

  cd ../
  rm -r temp$$

