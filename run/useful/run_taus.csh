#!/bin/csh
#   GET SCENARIO, BASIN, and TREE
########################################################################
##  This script creates percentiles of shear stress (tau) in the      ##
##    river simulation.  It requires the shear stress to be produced  ##
##    as a pltgen.  The program will run with any temporal            ##
##    aggregation of shear stress, but it is important for accuracy   ##
##    to have the pltgens written as hourly values                    ##
##    once the file is created and appropriately modified (see        ##
##    previous versions for guidance), move it to                     ##
##    $tree/input/param/river/$scen/tau_percentiles.csv               ##
########################################################################


  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_taus.csh scenario basin'
      echo ' or     run_taus.csh scenario basin tree'
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

  source $tree/config/seglists/${basin}.riv

  set outfile = 'tau_percentiles.csv'

  if (-e $outfile) then
    rm $outfile
  endif

  if (-e problem) then
    rm problem
  endif

  foreach seg ($segments)

    echo finding shear stresses for segment $seg   River scenario $scenario

    echo $seg, $scenario | $tree/code/bin/readtau.exe >> $outfile

    if (-e problem) then
      echo ' '
      cat problem
      rm problem
      exit
    endif

  end

  mv $outfile $tree/input/param/river/${scenario}_${outfile}

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif
