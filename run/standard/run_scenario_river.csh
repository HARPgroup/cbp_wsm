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

    if (-e confluencefile) then
      rm confluencefile
    endif

    echo $seg $scenario | $tree/code/bin/getconfluence.exe

    if (-e confluencefile) then

      chmod 777 confluencefile
      ./confluencefile
      rm confluencefile

      echo $scenario, $seg | $tree/code/bin/confluence.exe
      if (-e problem) then
       echo 'Problem with confluence program'
       cat problem
       exit
      endif

      mv *_0003.wdm $tree/tmp/wdm/river/$scenario/stream/

    endif

############ check if a river
    echo $seg | $tree/code/bin/check_river.exe >temp$$

    if (!(-z temp$$)) then
      rm temp$$

########## make stream wdms: copy the eos wdm then add upstream to it. ******

      if (!(-e $tree/tmp/wdm/river/$scenario/eos/$seg'.wdm')) then
        echo 'Problem in river' > problem
        echo ' could not find eos wdm: ' >> problem
        echo  $tree/tmp/wdm/river/$scenario/eos/$seg'.wdm' >> problem
        echo ' check that etm ran for segment ' $seg >> problem
        cat problem
        exit
      endif

      cp -v $tree/tmp/wdm/river/$scenario/eos/$seg'.wdm' $seg'.wdm'
      echo $scenario, $seg | $tree/code/bin/stream_wdm.exe

      if (-e problem) then
        echo 'Problem adding upstream segments to wdm file, segment ',$seg
        cat problem
        exit
      endif


#################### make the point source / diversions / septic wdm.
################  This has to be made from all land/river pairs that drain into the river

      cp -v $tree/config/blank_wdm/blank_ps_sep_div_ams.wdm ps_sep_div_ams_$scenario'_'$seg'.wdm'
      echo $scenario, $seg | $tree/code/bin/combine_ps_sep_div_ams_from_landsegs.exe

      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

############### Compare calibration and scenario loads input to this river 
###############    modify comparison file
      echo $scenario, $seg | $tree/code/bin/compare_scen_and_calib.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif

################ Generate the scenario UCI
      echo making River UCI for segment $seg   River scenario $scenario
      echo $seg, $scenario | $tree/code/bin/scenario_rug.exe
      if (-e problem) then
        echo ' '
        cat problem
        exit
      endif
      
######## run the UCI
      set inp = $tree/tmp/uci/river/$scenario/$seg'.uci'

      if (!(-e $inp)) then
        echo 'HSPF UCI for segment ' $seg ' named'
        echo $inp 'does not exist'
        exit
      endif

      echo $inp | $hspf

      tail -1 $seg'.ech' > EOJtest$$
      diff $tree/run/fragments/EOJ EOJtest$$ > diffeoj
      rm EOJtest$$
      if (!(-z diffeoj)) then
        if (-e problem) then
          rm problem
        endif
        echo 'river segment: ' $seg ' did not run'  >problem
        echo '  input file ' $inp >>problem
        set fnam = $tree/tmp/scratch/temp$$/$seg'.ech '
        echo '  check the file ' $fnam >>problem
        cat problem
        exit
      endif

      mv $seg'.out' $tree/output/hspf/river/out/$scenario/
      mv $seg'.ech' $tree/output/hspf/river/ech/$scenario/
      mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/stream/
      rm ps_sep_div_ams_$scenario'_'$seg'.wdm' 

    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r $tempdir
  endif
 
