#!/bin/csh

set DEBUG = 0 # 0, 1, 2,

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo 'Problem '
      echo 'usage:  run_river.csh scenario basin'
      echo ' or     run_river.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set rseg     = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    set tempdir = $argv[3]
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
    cd ../../tmp/${user}-scratch/$tempdir/
  endif

  if (-e problem) rm problem

  set DOWNSTREAM = `echo $rseg | awk '{print substr($0,10,4)}'`

  if ( $DOWNSTREAM != '0000' ) then
     cp -v ${tree}/tmp/wdm/river/${scenario}/stream/${rseg}.wdm .

     echo $scenario $rseg $DEBUG | ${tree}/code/bin/eotbmp_nonpour.exe

     if (! -e pourpoint) mv -v ${rseg}.wdm ${tree}/tmp/wdm/river/${scenario}/bay/
  endif

  pwd
  ls -al ./

  if (-e problem) then
    cat problem
    exit
  endif

  cd ..
  rm -r $tempdir
