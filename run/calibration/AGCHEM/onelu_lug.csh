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

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

      echo making UCI for $lu  segment $seg   land scenario $scenario

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/lug.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  end
     
  cd ../
  rm -r temp$$
 
