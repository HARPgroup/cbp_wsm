#!/bin/csh

  if (${#argv} != 5 ) then
    echo 'usage: run_nps_and_ps_to_wqm13k.csh BASE_DIRECTORY RIVER_SCENARIO POINT_SOURCE_METHOD YEAR1 YEAR2'
    echo '   will look for files in '
    echo '   /model/BASE_DIRECTORY/wdm/RIVER_SCENARIO/stream'
    echo '   POINT_SOURCE_METHOD must be hcell wcell or lrseg'
    exit
  endif

  set base = $argv[1]
  set rscen = $argv[2]
  set psmethod = $argv[3]  # must be 'hcell' 'wcell' or 'lrseg'
  set year1 = $argv[4]
  set year2 = $argv[5]

  set hotstart = 0   #  zero for no hotstart, 1 for write, 2 for read

  set tree = /model/$base/

  if (!(-e $tree/wdm/river/$rscen)) then
    echo 'could not find ' $tree/wdm/$rscen
    echo ' check input parameters'
    exit
  endif

  echo 'copying'
  cp  $tree/pp/src/lib/inc/standard.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/locations.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/land_use.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/bmp_constituents.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/land_constituents.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/wdm.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/modules.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/masslinks.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/inc/rsegs.inc ../src/lib/inc/
  cp  $tree/pp/src/lib/get_lib.a ../src/lib/
  cp  $tree/pp/src/lib/util_lib.a ../src/lib/
  cp  $tree/pp/src/lib/dsn/dsn_utils.f ../src/lib/dsn/

  echo 'writing date include file'
  set fnam = '../src/p5_and_ps_to_wqm13k/date.inc'
  echo "       parameter ( year1 = $year1 )" > $fnam
  echo "       parameter ( year2 = $year2 )" >> $fnam
  echo 'compiling'

  f77 -c -o ../src/lib/dsn/dsn_utils.o ../src/lib/dsn/dsn_utils.f
  cd ../src/p5_and_ps_to_wqm13k/
  compile
  cd ../../run/

  mkdir -p ../out/$rscen/

  echo 'running'

#  echo $rscen $psmethod $hotstart | ../bin/p5_ps_to_wqm13k.exe
#
#  if (-e problem) then
#    cat problem
#    rm problem
#    exit
#  endif

  echo $rscen $psmethod $hotstart | ../bin/p5_nps_to_wqm13k.exe

  if (-e problem) then
    cat problem
    rm problem
    exit
  endif

