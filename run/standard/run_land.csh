#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_land.csh scenario basin'
      echo ' or     run_land.csh scenario basin tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse
  source $tree/run/fragments/set_icprb_hspf
  
  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

    foreach lu ($perlnds $implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'
      echo $seg, $lu
      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest$$
      diff $tree/run/fragments/EOJ EOJtest$$ > diffeoj
      rm EOJtest$$
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo   check the file ../../tmp/scratch/temp$$/$lu$seg.ech >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/

    end

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

