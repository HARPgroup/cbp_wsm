#!/bin/csh

# GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set basin = $argv[2]
# csh bhatt_run_pltgen_lu.csh p532cal_062211 allBay

  source ../../fragments/set_tree
  source $tree/config/seglists/${basin}.land
#  set clu = (hwm hom lwm alf pas)
  set clu = (hwm)
  set types = (SNH4 INH4 DNH4 BNH4 SNO3 INO3 BNO3 SLON ILON DLON BLON SRON IRON BRON DRON SPO4 IPO4 DPO4 BPO4)

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$
 
     foreach lu ($clu)
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
	echo 'pltgen processing finished for '$lu $seg

        # Summary land simulation results
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
     end

     if (${#argv} == 2) then
        cd ../
       rm -r temp$$
     endif

     exit

