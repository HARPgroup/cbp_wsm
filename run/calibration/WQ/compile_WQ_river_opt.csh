#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_WQ_river_opt.csh calib_scenario '
      echo ' or     compile_WQ_river_opt.csh calib_scenario tree'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  if (${#argv} == 2) then
    source ../../../run/fragments/abs_tree
  else
    source ../../fragments/abs_tree
    mkdir -p ../../../tmp/scratch/temp$$
    cd ../../../tmp/scratch/temp$$
  endif

  set controldir = $tree/config/control/calib/WQ/${calscen}/
  set libdir = $tree/code/src/lib/

  cd $controldir
  if (-e callib.a) then
    rm callib.a
  endif
  compile

 echo 'linking'

  f77 -o $tree/code/bin/calib_iter_WQ_river_${calscen}.exe $controldir/main.o $controldir/callib.a $libdir/get_lib.a $libdir/util_lib.a
  f77 -o $tree/code/bin/set_defaults_WQ_river_${calscen}.exe $controldir/main_setdefaults.o $controldir/callib.a $libdir/get_lib.a $libdir/util_lib.a

  rm $controldir/*.o

