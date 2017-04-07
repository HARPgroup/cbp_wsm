#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_lug.csh scenario basin'
      echo ' or     run_lug.csh scenario basin tree'
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

  set years = (84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 )

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)

  source $tree/run/fragments/set_landuse
  source $tree/config/seglists/${basin}.riv

  foreach year ($years)

    foreach lu ($perlnds)

    set fnam =  $tree/output/eos/summary/${scenario}/${basin}_19${year}_annual_EOS_tonsac.csv
    if (-e $fnam) then
      rm $fnam
    endif

    foreach seg ($segments)
    
      grep ^19${year} $tree/output/eos/annual/${scenario}/*$seg*ann >> $fnam
    end

#    end

#    foreach lu ($implnds)

#    set fnam =  $tree/output/eof/summary/${scenario}/${basin}_19${year}_${lu}_annual_EOF_tonsac.csv
#    if (-e $fnam) then
#      rm $fnam
#    endif

#    foreach seg ($segments)

#      grep ^19${year} $tree/output/eof/annual/${scenario}/*$seg*$lu*ann >> $fnam
#    end
#   end 

  end                


