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
  source $tree/run/seglists/${basin}.land

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)

  mkdir /working/temp/temp$$/
  cd /working/temp/temp$$/

  if (-e core) then
    rm core
  endif

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    foreach lu ($implnds)
   
      echo making UCI for $lu  segment $seg   land scenario $scenario

      if (-e core) then
        rm core
      endif

      if (-e problem) then
        rm problem
      endif

      echo $seg, $lu, $scenario | $tree/code/bin/lug.exe

      if (-e core) then
        echo 'core dump generating file ',$seg, $lu, $scenario
        exit
      endif
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

   end

  end
     
  cd $tree/run/
  rm -r /working/temp/temp$$
 
