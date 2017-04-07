#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

########################################################################
## runs the river with modifications to the parameters in the UCI     ##
##  specified in ./pp/catalog/iovars/scenario_parameter_modifications ##
## Make sure that the appropriate calibration scenario is specified   ##
##  in the control file for this scenario                             ##
## There is no need to run the rug separately                         ##
########################################################################


  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_river.csh scenario basin'
      echo ' or     run_river.csh scenario basin tree'
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
    set tempdir = temp$$`../fragments/random.ksh`
    mkdir -p ../../tmp/scratch/$tempdir/
    cd ../../tmp/scratch/$tempdir/
  endif

  source $tree/run/fragments/set_quiet_hspf

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########
  source $tree/config/seglists/${basin}.riv

  foreach seg ($segments)

    if (-e problem) then
      rm problem
    endif

#################### make the point source / diversions / septic wdm.
################  This has to be made from all land/river pairs that drain into the river

      cp -v $tree/config/blank_wdm/blank_ps_sep_div_ams.wdm ps_sep_div_ams_$scenario'_'$seg'.wdm'
      echo $scenario, $seg | $tree/code/bin/ross_combine_ps_sep_div_ams_from_landsegs.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

      mv ps_sep_div_ams_$scenario'_'$seg'.wdm' $tree/tmp/wdm/river/$scenario/data/

    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif
 
