#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 3) then
    echo 'usage: run_PWATER_optimization.csh scenario calscen'
    exit
  endif

  set scenario = $argv[1]
  set calscen =  $argv[2]
  set basin =    $argv[3]

  source ../../fragments/set_tree
  set nums = (7 8 9 10)

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

######## SET YEARS FOR CALIBRATION
  set year1 = 1985
  set year2 = 2005
  set limit = 0.1

# SET SCENARIO FOR OBSERVED DATA
  source $tree/config/control/calib/PWATER/$calscen/set_obscen

###### compile code for this scenario
  $tree/run/calibration/PWATER/compile_PWATER_opt.csh $calscen $tree

######### complete current iteration
   set num = 6

   $tree/run/calibration/PWATER/runsome_iter.csh $scenario $calscen ${calscen}_PWATER $tree

   if (-e problem) then
     echo 'problem in run number: ',$num
     cat problem
     rm problem
     exit
   endif

   echo $calscen, $scenario | $tree/code/bin/calib_iter_PWATER_params_${calscen}.exe

   if (-e problem) then
     echo 'problem in parameter change program, run number: ',$num
     cat problem
     rm problem
     exit
   endif

############# perform rest of the iterations
  foreach num ($nums)
    
     $tree/run/calibration/PWATER/run_PWATER_iter.csh $scenario $calscen ${calscen}_PWATER $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif

     echo $calscen, $scenario | $tree/code/bin/calib_iter_PWATER_params_${calscen}.exe

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/PWATER/run_PWATER_iter.csh $scenario $calscen ${calscen}_PWATER $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

  cd ../
  rm -r temp$$
