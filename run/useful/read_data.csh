#!/bin/csh

 set scenario = $argv[1]

 source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/

# summary runoff results
 grep 'suro'           $tree/output/pltgen/land/${scenario}/*sums                > $tree/output/pltgen/summary/${scenario}_SURO_inches.csv

# summary land sediment results
 grep 'dets'           $tree/output/pltgen/land/${scenario}/*sums                > $tree/output/pltgen/summary/${scenario}_DETS_tons_pacre.csv
 grep 'wssd'           $tree/output/pltgen/land/${scenario}/*sums                > $tree/output/pltgen/summary/${scenario}_WSSD_tons_pacre.csv

# summary river sediment results
 grep 'beddep'           $tree/output/pltgen/river/${scenario}/*sums             > $tree/output/pltgen/summary/${scenario}_BEDDEP_feet.csv
 grep 'depscr'           $tree/output/pltgen/river/${scenario}/*sums             > $tree/output/pltgen/summary/${scenario}_DEPSCR_tons.csv

 if (-e problem) then
   cat problem
   rm problem
 endif

