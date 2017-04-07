#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm.csh scenario basin'
      echo ' or     run_etm.csh scenario basin tree'
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

  set loud = '1'           # zero for loud, non-zero for quiet

  source $tree/config/seglists/${basin}.riv

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########

  foreach seg ($segments)

########## make binary transfer files ******
    if (-e problem) then
      rm problem
    endif

    echo $scenario, $seg, `${tree}/run/useful/random.ksh` | $tree/code/bin/make_binary_transfer_coeffs.exe

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif

