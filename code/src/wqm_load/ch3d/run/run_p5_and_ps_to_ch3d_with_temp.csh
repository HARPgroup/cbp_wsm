#!/bin/csh

  if (${#argv} != 3 ) then
    echo 'usage: run_p5_and_ps_to_ch3d_with_temp.com BASE_DIRECTORY RIVER_SCENARIO POINT_SOURCE_METHOD'
    echo '   will look for files in '
    echo '   /model/BASE_DIRECTORY/wdm/RIVER_SCENARIO/stream'
    echo '   POINT_SOURCE_METHOD must be hcell wcell or lrseg'
    exit
  endif

  set base = $argv[1]
  set rscen = $argv[2]
  set psmethod = $argv[3]  # must be 'hcell' 'wcell' or 'lrseg'

  set tree = $base

  if (!(-e $tree/tmp/wdm/river/$rscen)) then
    echo 'could not find ' $tree/wdm/$rscen
    echo ' check input parameters'
    exit
  endif

  echo 'copying'
  cp  $tree/code/src/lib/inc/standard.inc ../src/lib/inc/
#BHATT  cp  $tree/code/src/lib/inc/locations.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/land_use.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/bmp_constituents.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/land_constituents.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/wdm.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/modules.inc ../src/lib/inc/
#BHATT  cp  $tree/code/src/lib/inc/masslinks.inc ../src/lib/inc/
  cp  $tree/code/src/lib/inc/rsegs.inc ../src/lib/inc/
  cp  $tree/code/src/lib/get_lib.a ../src/lib/
  cp  $tree/code/src/lib/util_lib.a ../src/lib/
###  cp  $tree/code/src/lib/dsn/dsn_utils.o ../src/lib/dsn/
  f77 -c -o ../src/lib/dsn/dsn_utils.o ../src/lib/dsn/dsn_utils.f
  echo 'compiling'

  cd ../src/p5_and_ps_to_ch3d_with_temp/; pwd
  mkdir -p ../../bin
  ./compile
  cd ../../run/; pwd

  mkdir -p ../out/$rscen/

  echo 'running'

  echo $rscen $psmethod | ../bin/p5_and_ps_to_ch3d_with_temp.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif


