#!/bin/csh
#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_PSTEMP_one_station_river_optimization.csh scenario calscen calibration_segment '
      echo ' or     run_PSTEMP_one_station_river_optimization.csh scenario calscen calibration_segment tree'
      echo ' '
      exit
    endif
  endif
    
  set scenario = $argv[1]
  set calscen = $argv[2]
  set rseg = $argv[3]   

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  set year1 = 1985
  set year2 = 2005

######## rseg is the calibration station.  It can be a double

  set nums = (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50)

  echo $calscen PSTEMP $rseg $scenario | $tree/code/bin/make_incremental_calib_list.exe

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in make_incremental_calib_list.exe '
       cat problem
       exit
     endif

  if (-e $tree/run/calibration/PSTEMP/${calscen}_${scenario}_${rseg}.converge) then
    rm $tree/run/calibration/PSTEMP/${calscen}_${scenario}_${rseg}.converge
  endif

  foreach num ($nums)

     $tree/run/calibration/PSTEMP/run_PSTEMP_river_iter.csh $scenario $calscen ${calscen}_PSTEMP_${rseg} $num $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in run_PSTEMP_river_iter.csh '
       cat problem
       exit
     endif

     echo $calscen, $scenario, $rseg, $num $year1, $year2 | $tree/code/bin/calib_iter_PSTEMP_river_${calscen}.exe

     if (-e $tree/run/calibration/PSTEMP/${calscen}_${scenario}_${rseg}.converge) then
       rm $tree/run/calibration/PSTEMP/${calscen}_${scenario}_${rseg}.converge
       exit
     endif

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in calib_iter_PSTEMP_river_'${calscen}'.exe'
       cat problem
       exit
     endif


  end

  $tree/run/calibration/PSTEMP/run_PSTEMP_river_iter.csh $scenario $calscen ${calscen}_PSTEMP_${rseg} $num $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in run_PSTEMP_river_iter.csh '
    cat problem
    exit
  endif


  if (${$argv} == 3) then
    cd ../
    rm -r temp$$
  endif
