#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_IQUAL_opt.csh calib_scenario '
      echo ' or     compile_IQUAL_opt.csh calib_scenario tree'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$
    cd ../../../tmp/scratch/temp$$
  endif

  f77 -c -o $tree/config/control/calib/IQUAL/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_sensitivities.f
  f77 -c -o $tree/config/control/calib/IQUAL/$calscen/${calscen}_calibrated.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_calibrated.f
  f77 -c -o $tree/config/control/calib/IQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_initial.f

  f77 -o $tree/code/bin/calib_iter_IQUAL_params_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/main.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/calib_iter_IQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  f77 -o $tree/code/bin/calib_iter_IQUAL_init_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/main_assign_initial_params.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/calib_iter_IQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  f77 -o $tree/code/bin/calib_iter_IQUAL_check_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/main_check_calib.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/IQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/IQUAL/calib_iter_IQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  if (${#argv} == 1) then
    cd ../
    rm -r temp$$
  endif

