#!/bin/csh
#   GET SCENARIO and TREE

  if (${#argv} != 2) then
    if (${#argv} != 1) then
      echo ' ' >problem
      echo 'usage:  sumWTMP.csh scenario ' >> problem
      echo ' or     sumWTMP.csh scenario tree' >> problem
      echo ' '
      cat problem
      exit
    endif
  endif

  set scenario = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

 set year1 = 1985
 set year2 = 2005

 grep -H 'base' $tree/output/river/stats/${scenario}/*_${year1}_${year2}_monthbias.WTMP > $tree/output/river/summary/$scenario/WTMP_base.csv
 grep -H 'quick' $tree/output/river/stats/${scenario}/*_${year1}_${year2}_monthbias.WTMP > $tree/output/river/summary/$scenario/WTMP_quik.csv

 echo ${scenario} | $tree/code/bin/sumWTMPstats.exe
 
 if (-e problem) then
   cat problem
   if (${#agv} == 1) then
     rm problem
   endif
 endif

  if (${#argv} == 1) then
    cd ../
    rm -r temp$$
  endif

