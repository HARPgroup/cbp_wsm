#!/bin/csh

  set scenario = $argv[1]

  source ../fragments/set_tree
  source ../../run/fragments/set_landuse
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/

  set landuse = ($perlnds $implnds)

  
########## UCIs
  foreach lu ($landuse)
    rm -r $tree/tmp/uci/land/$lu/$scenario
  end

############ temp space
  foreach lu ($landuse)
    rm -r $tree/run/land/$lu/$scenario
  end

########## wdms
  foreach lu ($landuse)
    rm -r $tree/tmp/wdm/land/$lu/$scenario
  end


