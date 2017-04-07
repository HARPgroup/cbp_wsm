#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 4) then
    if (${#argv} != 3) then
      echo ' '
      echo 'usage:  run_1land.csh scenario basin landuse'
      echo ' or     run_1land.csh scenario basin landuse tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set basin = $argv[2]
  set landuse = $argv[3]
  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  source $tree/run/fragments/set_landuse
  
  source $tree/run/fragments/set_quiet_hspf

  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

    foreach lu ($perlnds $implnds)

      if ($lu == $landuse) then

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'

      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest
      diff $tree/run/test/EOJ EOJtest > diffeoj
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo '  check the file '${tree}'/run/temp/'temp$$'/'$lu$seg'.ech' >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/

      endif

    end

  end

  if (${#argv} == 3) then
    cd ../
    rm -r temp$$
  endif

