#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 3) then
    echo " "
    echo 'usage: make_PWATER_weights.csh scenario calscen basin'
    echo " "
    exit
  endif

  set scenario = $argv[1]
  set calscen =  $argv[2]
  set basin =    $argv[3]

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

######## SET YEARS FOR CALIBRATION
  set year1 = 1985
  set year2 = 2005
  set limit = 0.1

# SET SCENARIO FOR OBSERVED DATA
  source $tree/config/control/calib/PWATER/$calscen/set_obscen

########### rename .riv and .land files in seglists to a calscen and module specific name
    cp $tree/config/seglists/${basin}.riv $tree/config/seglists/${calscen}_PWATER.riv
    cp $tree/config/seglists/${basin}.land $tree/config/seglists/${calscen}_PWATER.land
  
################# MAKE WEIGHT FILE FROM BASIN BY INSPECTING DATA
    if (-e problem) then
      rm problem
    endif

  $tree/run/calibration/PWATER/compile_PWATER_weights.csh $calscen $tree
  
  echo $calscen $scenario $obscen ${calscen}_PWATER $year1 $year2 $limit | $tree/code/bin/make_PWATER_weights_${calscen}.exe
  
    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  cd ../
  rm -r temp$$
