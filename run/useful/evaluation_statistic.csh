#!/bin/csh
 
  if (${#argv} != 6) then
    if (${#argv} != 5) then
        echo ' '
        echo 'usage:  evaluation_statistic.csh calibration basin EarliestYear Latestyear Obsdata'
        echo 'or  evaluation_statistic.csh calibration basin EarliestYear Latestyear Obsdata tree'
        echo ' '
        echo 'Obsdata(calib, alldata, alldata-85-12)'
        echo ' '
        exit
     endif
  endif
  
  set calib = $argv[1]
  set basin = $argv[2]
  set year1 = $argv[3]
  set year2 = $argv[4]
  set odata = $argv[5]

  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

####### RIVER SEGMENTS ########
   source $tree/config/seglists/${basin}.riv

   foreach seg ($segments)
    if (-e problem) then
      rm problem
    endif

   mkdir -p $tree/output/river/evaluation_statistics/$calib/
   
   echo $calib,$seg,$year1,$year2,$odata|$tree/code/bin/evaluation_stat.exe

   end



