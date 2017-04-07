#!/bin/csh

#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../fragments/set_tree
  endif

  source $tree/run/fragments/set_landuse
  source $tree/config/seglists/${basin}.land

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)
  
  source $tree/run/fragments/set_quiet_hspf

  mkdir /working/temp/temp$$/
  cd /working/temp/temp$$/

  foreach seg ($segments)

    foreach lu ($implnds)

      echo running $lu for segment $seg land scenario $scenario

      cp -v $tree/wdm/blank/land.wdm $lu$seg'.wdm'

      set inp = $tree/uci/land/$lu/$scenario/$lu$seg'.uci'

      echo $inp | $hspf11

      tail -1 $lu$seg'.ech' > EOJtest
      diff $tree/run/EOJ EOJtest > diffeoj
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'land segment: ' $seg ' did not run for land use: ' $lu >problem
        echo '  input file ' $inp >>problem
        echo   check the file /working/temp/temp$$/${lu}${seg}.ech >>problem
        cat problem
        exit
      endif

      mv $lu$seg'.out' $tree/output/hspf/land/out/$lu/$scenario/
      mv $lu$seg'.ech' $tree/output/hspf/land/ech/$lu/$scenario/
      mv $lu$seg'.wdm' $tree/wdm/land/$lu/$scenario/

   end
  end

  cd $tree/run/
  rm -r /working/temp/temp$$

