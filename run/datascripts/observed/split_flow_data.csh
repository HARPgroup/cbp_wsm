#!/bin/csh

  if ($argv[1] != 'GO') then
    if ($argv[1] != 'go') then
      echo ' '
      echo ' run this script to split flow data for validation'
      echo ' '
      exit
    endif
  endif

  source ../fragments/set_tree

  mkdir -p ../../tmp/scratch/temp$$/
  cd ../../tmp/scratch/temp$$/

######## SET TIME PERIOD TO SPLIT
  set year1 = 1985
  set year2 = 2005

  echo $year1 $year2 | $tree/code/bin/split_flow_data.exe

   if (-e problem) then
      cat problem
      exit
   endif

  cd ../
  rm -r temp$$

