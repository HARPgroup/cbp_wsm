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
  source $tree/run_bhatt/fragments/set_icprb_hspf
  
    foreach lu ($perlnds $implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land_Naomi.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'

      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest$tempdir
      diff $tree/run_bhatt/fragments/EOJ EOJtest$tempdir > diffeoj
      rm EOJtest$tempdir
#      wait
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo   check the file ../../tmp/${user}-scratch/$tempdir/$lu$seg.ech >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/ 
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/
#      wait
    end

  if (${#argv} == 3) then
    cd ../
    rm -r $tempdir
  endif
#  wait

exit 0
