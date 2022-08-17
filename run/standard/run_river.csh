#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

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
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

  # load new scenario configuration script
  source $tree/config/control/script/${scenario}.con
  source $tree/run/fragments/set_icprb_hspf
  # Set up export dirs - this could be moved
  if (! -d "$CBP_EXPORT_DIR/river" ) mkdir  $CBP_EXPORT_DIR/river 
  if (! -d "$CBP_EXPORT_DIR/river/$scenario" ) mkdir  $CBP_EXPORT_DIR/river/$scenario 
  if (! -d "$CBP_EXPORT_DIR/river/$scenario/h5" ) mkdir  $CBP_EXPORT_DIR/river/$scenario/h5 
  if (! -d "$CBP_EXPORT_DIR/river/$scenario/stream" ) mkdir  $CBP_EXPORT_DIR/river/$scenario/stream

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########
  source $tree/config/seglists/${basin}.riv
  echo "********************************************"
  echo "********************************************"
  echo "********** HSPF Model Run Beginning ********"
  echo "********************************************"
  echo "********************************************"
  echo "Found segments: $segments"
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

      mv *_0003.wdm $tree/tmp/wdm/river/$scenario/stream/ -f

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
      
######## run the UCI
      set inp = $tree/tmp/uci/river/$scenario/$seg'.uci'

      if (!(-e $inp)) then
        echo 'HSPF UCI for segment ' $seg ' named'
        echo $inp 'does not exist'
        exit
      endif

      if ($HSP_VERSION == "hsp2") then
        hsp2 import_uci $inp $seg'.h5'
        set h5file = $seg'.h5'
        hsp2 run $h5file
        #csvfile =
        # Run post-process extract routine
        echo "Exporting HYDR data for $seg"
        set ds="/RESULTS/RCHRES_R001/HYDR/table"
        set mod="hydr"
        set csvfile = ${seg}_hydr'.csv'
        echo "Notice: Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds"
        Rscript $CBP_ROOT/run/export/export_hsp_h5.R $h5file $csvfile $ds
        Run conversion script to add Qout and other derived/alias cokumns
        echo "Notice (unit conversions): Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile"
        Rscript $CBP_ROOT/run/export/hsp_hydr_conversion.R $csvfile 
        # Prep outflow data for export to river wdm
        set wdmcsv = ${seg}_rovol'.csv'
        echo "Notice: Rscript $CBP_ROOT/run/export/csv_export_wdm_format.R $csvfile $wdmcsv ROVOL"
        Rscript $CBP_ROOT/run/export/csv_export_wdm_format.R $csvfile $wdmcsv ROVOL
        # Push ROVOL into wdm
        # copy here cause it is hardto send these paths to wdm_insert_one, need escape?
        cp /usr/local/lib/hspf/message.wdm ./
        echo ${seg}.wdm $wdmcsv 111 1 w message.wdm | wdm_insert_one
        # move all files to the model data archive
        mv $csvfile $CBP_EXPORT_DIR/river/$scenario/$mod/ -f
        chgrp $MODEL_FILES_GROUP $CBP_EXPORT_DIR/river/$scenario/$mod/$csvfile
        chmod 664 $csvfile
        mv $wdmcsv $CBP_EXPORT_DIR/river/$scenario/$mod/ -f
        chgrp $MODEL_FILES_GROUP $CBP_EXPORT_DIR/river/$scenario/$mod/$wdmcsv
        chmod 664 $wdmcsv
        # Remove message and h5 file to save space
        echo "Cleaning up $h5file"
        rm $h5file
        rm message.wdm
        # todo: add the call to run the river hsp2 summary script here
        echo "Notice(analyze): Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/"
        Rscript $CBP_ROOT/run/export/hsp_hydr_analysis.R $seg $scenario $CBP_EXPORT_DIR/river/$scenario/$mod/
      else
        echo $inp | $hspf

        # Check the output to see if its OK
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
        mv $seg'.out' $tree/output/hspf/river/out/$scenario/ -f
        mv $seg'.ech' $tree/output/hspf/river/ech/$scenario/ -f
        # export the flow data
        if ( $?START_YEAR ) then
          echo "echo ${seg}.wdm,$START_YEAR,$END_YEAR,111 | wdm2text"
          echo "${seg}.wdm,$START_YEAR,$END_YEAR,111" | wdm2text
          mv $seg'_0111.csv' $CBP_EXPORT_DIR/river/$scenario/stream/ -f
        endif
        mv ps_sep_div_ams_$scenario'_'$seg'.wdm' $tree/tmp/wdm/river/$scenario/eos/ -f
        # todo: add the call to run the river hspf summary script here
      endif
      # move the WDM to stream folder so next watershed can access it's upstream inflows
      # we curently do thisfor both hspf and hsp2, but later may move this into the 
      # hspf specific code block
      mv $seg'.wdm' $tree/tmp/wdm/river/$scenario/stream/ -f
    endif

  end

  if (${#argv} == 2) then
    cd ../
    rm -r temp$$
  endif
 
