#!/bin/csh

#   GET SCENARIO AND BASIN
  set scenario = $argv[1]
  set basin = $argv[2]
  set loud = 1           # zero for loud, non-zero for quiet
  set lu = 'iml'

#   SET VARIABLES  --  TO CHANGE WHAT RUNS, CHANGE THE PERLND, IMPLND, AND BASIN (SEGMENT LISTS)
  source ../../fragments/set_tree
  source seglists/${basin}.riv

  mkdir -p ../../../tmp/scratch/temp$$/
  cd ../../../tmp/scratch/temp$$/

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########

  foreach seg ($segments)

########## make binary transfer files ******
    if (-e problem) then
      rm problem
    endif

    echo $scenario, $seg | $tree/code/bin/make_binary_transfer_coeffs.exe

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif


    if (-e problem) then
      rm problem
    endif

    cp -v $tree/run/config/blank_wdm/river.wdm $seg'.wdm'

    echo $scenario, $seg, $lu, $loud | $tree/code/bin/onelu_etm.exe

    rm $seg'.wdm' 

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

  end

  cd ../
  rm -r temp$$


