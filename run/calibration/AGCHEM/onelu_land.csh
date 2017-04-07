#!/bin/csh

#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = $argv[3]

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  source $tree/config/seglists/${basin}.land

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)
  source $tree/run/fragments/set_icprb_hspf

  foreach seg ($segments)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/config/blank_wdm/land.wdm $lu$seg'.wdm'

      set inp = $tree/tmp/uci/land/$lu/$scenario/$lu$seg'.uci'

      echo $inp | $hspf

      tail -1 $lu$seg'.ech' > EOJtest
      diff $tree/run/fragments/EOJ EOJtest > diffeoj
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo   check the file ../../tmp/scratch/temp$$/${lu}${seg}.ech >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/tmp/wdm/land/$lu/$scenario/

  end

  cd ../
  rm -r temp$$

