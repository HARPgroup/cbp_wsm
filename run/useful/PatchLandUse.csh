#!/bin/csh

 if (${#argv} != 4) then
      echo ' '
      echo 'usage:  PatchLandUse.csh basin baseLUFile inLUFile outLUFile'
      echo ' '
      exit
    endif

set basin  = $argv[1]
set baseLUFile  = $argv[2]
set inLUFile = $argv[3]
set outLUFile = $argv[4]

source ../../config/seglists/${basin}.land

    cp ../../input/scenario/river/land_use/land_use_$baseLUFile.csv ../../input/scenario/river/land_use/land_use_$outLUFile.csv

foreach seg ($segments)

    grep $seg ../../input/scenario/river/land_use/land_use_$inLUFile.csv >> ../../input/scenario/river/land_use/land_use_$outLUFile.csv

end
