#!/bin/csh

#   GET SCENARIO AND BASIN (SEGMENT LISTS)

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage: read_nitr_out.csh scenario basin '
      echo ' or    read_nitr_out.csh scenario basin tree'
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

  set clu = (npa hyo hvf urs trp nho nhi nlo nal nhy rpd npd cpd) #(for hwm hom lwm alf hyw pas)

  source $tree/config/seglists/${basin}.land
  
  if (-e problem) then
    rm problem
  endif

  #foreach seg ($segments)

  foreach lu ($clu)

    foreach seg ($segments)

      echo reading out file for $lu segment $seg land scenario $scenario

      echo $scenario, $lu, $seg | $tree/code/bin/read_nitr_output.exe >> $tree/output/hspf/land/out/${scenario}_${lu}.csv

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

    end

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

