#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if ($#argv != 1) then
      echo ' '
      echo ' run this script with the argument GO'
      echo '   after modification'
      echo ' '
      exit
  endif
  if ($argv[1] != 'go') then
    if ($argv[1] != 'GO') then
      echo ' '
      echo ' run this script with the argument GO'
      echo '   after modification'
      echo ' '
      exit
    endif
  endif

#  set scenario = 
  set basin = all
  set basintype = riv
  set scenario = somescen
#  set landuse = 

  source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/

#  source $tree/run/fragments/set_landuse
#  set landuse = 

  set code = $tree/code/bin/stand_alone_get_downstream_river_names_and_miles.exe

  source $tree/config/seglists/${basin}.${basintype}

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

#    foreach lu ($perlnds $implnds)

      echo $seg $scen | $code

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

      endif

#    end

  end
     
  cd ../useful/
  rm -r temp$$
 
