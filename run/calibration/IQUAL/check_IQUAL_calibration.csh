#!/bin/csh
#   GET SCENARIO AND BASIN (SEGMENT LISTS)

  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo 'usage:  check_IQUAL_optimization.csh scenario calib_scen basin landuse'
      echo ' or     check_IQUAL_optimization.csh scenario calib_scen basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set lu = $argv[4]
  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
  endif

  echo $scenario $basin $lu 1985 2005 | $tree/code/bin/pltgen_annave_summary.exe

  $tree/run/calibration/IQUAL/compile_IQUAL_opt.csh $calscen $tree
  echo $calscen, $scenario ${basin} $lu | $tree/code/bin/calib_iter_IQUAL_check_${calscen}.exe

  if (-e problem) then
    echo 'message from calibration check program'
    cat problem
    rm problem
    exit
  endif

