#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_land_seg.csh scenario segment'
      echo ' or     run_land_seg.csh scenario segment tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set seg = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/scratch/$tempdir/
    cd ../../tmp/scratch/$tempdir/
#bhatt
    echo pwd=
    pwd
    echo @
  endif

  source $tree/run/fragments/set_landuse
  source $tree/run/fragments/set_icprb_hspf
  
    foreach lu ($perlnds $implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'
#bhatt
      echo $inp
      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest$$
      diff $tree/run/test/EOJ EOJtest$$ > diffeoj
      rm EOJtest$$
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo   check the file ../../tmp/scratch/$tempdir/$lu$seg.ech >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/

    end

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif

