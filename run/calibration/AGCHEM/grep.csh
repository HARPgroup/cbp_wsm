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
    source ../../fragments/set_tree
  endif

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE SCENARIO, PERLND, IMPLND, AND BASIN (SEGMENT LISTS)

  source $tree/run/fragments/set_landuse
  source $tree/config/seglists/${basin}.land

#  foreach seg ($segments)

#    foreach lu ($perlnds)

     grep 'SNH4' $tree/output/pltgen/summary/${scenario}/*SNH4*.csv > $tree/output/pltgen/summary/SNH4_SUMMARY.csv
     grep 'INH4' $tree/output/pltgen/summary/${scenario}/*INH4*.csv > $tree/output/pltgen/summary/INH4_SUMMARY.csv
     grep 'DNH4' $tree/output/pltgen/summary/${scenario}/*DNH4*.csv > $tree/output/pltgen/summary/DNH4_SUMMARY.csv
     grep 'BNH4' $tree/output/pltgen/summary/${scenario}/*BNH4*.csv > $tree/output/pltgen/summary/BNH4_SUMMARY.csv
     grep 'SNO3' $tree/output/pltgen/summary/${scenario}/*SNO3*.csv > $tree/output/pltgen/summary/SNO3_SUMMARY.csv
     grep 'INO3' $tree/output/pltgen/summary/${scenario}/*INO3*.csv > $tree/output/pltgen/summary/INO3_SUMMARY.csv
     grep 'BNO3' $tree/output/pltgen/summary/${scenario}/*BNO3*.csv > $tree/output/pltgen/summary/BNO3_SUMMARY.csv
     grep 'SLON' $tree/output/pltgen/summary/${scenario}/*SLON*.csv > $tree/output/pltgen/summary/SLON_SUMMARY.csv
     grep 'ILON' $tree/output/pltgen/summary/${scenario}/*ILON*.csv > $tree/output/pltgen/summary/ILON_SUMMARY.csv
     grep 'DLON' $tree/output/pltgen/summary/${scenario}/*DLON*.csv > $tree/output/pltgen/summary/DLON_SUMMARY.csv
     grep 'BLON' $tree/output/pltgen/summary/${scenario}/*BLON*.csv > $tree/output/pltgen/summary/BLON_SUMMARY.csv
     grep 'SRON' $tree/output/pltgen/summary/${scenario}/*SRON*.csv > $tree/output/pltgen/summary/SRON_SUMMARY.csv
     grep 'IRON' $tree/output/pltgen/summary/${scenario}/*IRON*.csv > $tree/output/pltgen/summary/IRON_SUMMARY.csv
     grep 'DRON' $tree/output/pltgen/summary/${scenario}/*DRON*.csv > $tree/output/pltgen/summary/DRON_SUMMARY.csv
     grep 'BRON' $tree/output/pltgen/summary/${scenario}/*BRON*.csv > $tree/output/pltgen/summary/BRON_SUMMARY.csv
     grep 'TOTN' $tree/output/pltgen/summary/${scenario}/*TOTN*.csv > $tree/output/pltgen/summary/TOTN_SUMMARY.csv

     grep 'SPO4' $tree/output/pltgen/summary/${scenario}/*SPO4*.csv > $tree/output/pltgen/summary/SPO4_SUMMARY.csv
     grep 'IPO4' $tree/output/pltgen/summary/${scenario}/*IPO4*.csv > $tree/output/pltgen/summary/IPO4_SUMMARY.csv
     grep 'DPO4' $tree/output/pltgen/summary/${scenario}/*DPO4*.csv > $tree/output/pltgen/summary/DPO4_SUMMARY.csv
     grep 'BPO4' $tree/output/pltgen/summary/${scenario}/*BPO4*.csv > $tree/output/pltgen/summary/BPO4_SUMMARY.csv
     grep 'SROP' $tree/output/pltgen/summary/${scenario}/*SROP*.csv > $tree/output/pltgen/summary/SROP_SUMMARY.csv
     grep 'TOTP' $tree/output/pltgen/summary/${scenario}/*TOTP*.csv > $tree/output/pltgen/summary/TOTP_SUMMARY.csv

#    end                

#   end

