#!/bin/csh

  if (${#argv} != 2 ) then
    echo 'usage: run_p5_fall_line_with_temp.com BASE_DIRECTORY RIVER_SCENARIO'
    echo '   will look for files in '
    echo '   /model/BASE_DIRECTORY/wdm/RIVER_SCENARIO/stream'
    exit
  endif

  set base = $argv[1]
  set rscen = $argv[2]

  set tree = /model/$base/

  if (!(-e $tree/wdm/river/$rscen)) then
    echo 'could not find ' $tree/wdm/$rscen
    echo ' check input parameters'
    exit
  endif

  echo 'copying'
  cp  $tree/pp/src/lib/inc/standard.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/locations.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/wdm.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/modules.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/masslinks.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/rsegs.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/get_lib.a ../src/lib/
  cp  $tree/pp/src/lib/util_lib.a ../src/lib/
  cp  $tree/pp/src/lib/dsn/dsn_utils.o ../src/lib/dsn/
###  f77 -c -o ../src/lib/dsn/dsn_utils.o ../src/lib/dsn/dsn_utils.f
  echo 'compiling'

  cd ../src/p5_fall_line_with_temp/
  compile
  cd ../../run/

  if (!(-e ../out/$rscen/)) then
    mkdir ../out/$rscen/
  endif

  echo 'running'

  echo $rscen | ../bin/p5_fall_line_with_temp.exe

  if (-e problem) then
    type problem
    rm problem
    exit
  endif


