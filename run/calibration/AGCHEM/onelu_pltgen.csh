#!/bin/csh

#   GET SCENARIO AND BASIN

  set scenario = $argv[1]
  set basin = $argv[2]
  set lu = $argv[3]

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$
    cd ../../../tmp/scratch/temp$$
  endif

  source $tree/config/seglists/${basin}.land

  foreach seg ($segments)

      if (-e problem) then
        rm problem
      endif

    echo summarizing pltgens for $lu for segment $seg land scenario $scenario

      echo $seg,$lu,$scenario | $tree/code/bin/pltgen.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

  end


# summary land simulation results

  set types = (SNH4 INH4 DNH4 BNH4 SNO3 INO3 BNO3 SLON ILON DLON BLON SRON IRON BRON DRON SPO4 IPO4 DPO4 BPO4)

  foreach type ($types)

     set fnam = $tree/output/pltgen/summary/${scenario}/${lu}_${type}_sum.csv

     if (-e $fnam) then
       rm $fnam
     endif

     foreach seg ($segments)

       grep $type $tree/output/pltgen/land/${scenario}/${lu}_$seg.sums  >> $fnam

     end
  end

  if (-e problem) then
   cat problem
   rm problem
  endif



