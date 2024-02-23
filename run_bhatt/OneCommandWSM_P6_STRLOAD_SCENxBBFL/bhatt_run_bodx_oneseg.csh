#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_land_seg.csh scenario segment'
      echo ' or     run_land_seg.csh scenario segment tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
#    set tempdir = temp$$`../fragments/random.ksh`
    set tempdir = $argv[3] 
    mkdir -p ../../tmp/${user}-scratch/$tempdir/
#    wait
    cd ../../tmp/${user}-scratch/$tempdir/
    pwd
  endif

  source $tree/run_bhatt/fragments/set_landuse

set DEBUG0 = 0
set WDM    = 0 #WDM = 1 => work on WDM in tmp directory & WDM = 0 => overwrite the WDM in land directory

#*** COPY WDM FILE WITH HYDROLOGY etc.  
  if ( $WDM == 1) then
     foreach lu ($perlnds $implnds)
         cp -pv $tree/tmp/wdm/land/$lu/$scenario/$lu$seg'.wdm' .
     end
  endif

#*** RUN BODX SIMULATION
  echo $seg $scenario 1984 2000 $WDM $DEBUG0 | ../../../code/bin/bodx.exe

  if (-e problem) then
      cat problem
      exit
  endif

#*** (1) BACKUP EXISTING WDM FILES, AND (2) MOVE NEW WDM FILES
  if ( $WDM == 1 ) then
     foreach lu ($perlnds $implnds)
         mv -v $tree/tmp/wdm/land/$lu/$scenario/$lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/x$lu$seg'.wdm'
         mv -v $lu$seg'.wdm'                                  $tree/tmp/wdm/land/$lu/$scenario/$lu$seg'.wdm'
      end
  endif

  if (${#argv} == 3) then
    cd ../
    rm -r $tempdir
  endif
#  wait

exit 0
