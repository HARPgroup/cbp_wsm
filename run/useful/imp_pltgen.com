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

  foreach seg ($segments)

    foreach lu ($implnds)

      cd /working/temp/
      if (-e problem) then
        rm problem
      endif

      if (-e core) then
        rm core
      endif
 
    echo summarizing pltgens for $lu for segment $seg land scenario $scenario

      echo $seg,$lu,$scenario | $tree/code/bin/pltgen.exe

      if (-e core) then
        echo 'core dump generating file ',$seg, $lu, $scenario
        exit
      endif
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif


# summary land sediment results

     grep 'dets'           $tree/output/pltgen/land/${scenario}/${lu}*sums                > $tree/output/pltgen/summary/${scenario}/${lu}_DETS_tons.csv
 
     grep 'wssd'           $tree/output/pltgen/land/${scenario}/${lu}*sums                > $tree/output/pltgen/summary/${scenario}/${lu}_WSSD_tons.csv

    end 
  end


  if (-e problem) then
   cat problem
   rm problem
  endif

 

