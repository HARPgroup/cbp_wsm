#!/bin/csh

# Written by GOPAL BHATT (gopal.bhatt@psu.edu)
# Script does "baby-sitting [TM - Gary Shenk]" of CBP-WSM Simulation
# Script Distributes the Jobs Based on the Specified Number of the CPUs

set SCENARIO       = $argv[1]
set SCENARIO_SLURM = $argv[2]
set SCENARIO_LOG   = $argv[3]


source ../../config/control/script/$SCENARIO.con

set iSLURM_JOBID    = $SLURM_JOBID
set iSLURM_JOB_NAME = $SLURM_JOB_NAME

if ( ! ($?INFEXP) ) then
     set INFEXP = 0
endif

if ( ! ($?LAND_QUIET) ) then
     set LAND_QUIET = 0
endif

if ( ! ($?SKIP_ETMPP) ) then
     set SKIP_ETMPP = 0
endif

printf "\nINFEXP = $INFEXP & LAND_QUIET = $LAND_QUIET & SKIP_ETMPP = $SKIP_ETMPP\n" >> $SCENARIO_LOG


# ****************************** CONFIGURE USER SCRATCH DIRECTORY ON EACH COMPUTE NODE ******************************* #
echo "Running Compute Node Setup Script"
srun --ntasks-per-node=1 --ntasks=$NUM_NODES aws_wsm_script_init.sh $PROJECT_HOME

# **************************************** PRE-PROCESS SCENARIO BUILDER DATA  **************************************** #
@ PRN = $DISTRIBUTE_SB_DATA + $POINT_SOURCE_WDMS + $RIB_WDMS + $RPA_WDMS + $SEPTIC_WDMS
if ( $PRN > 0 ) then
     echo '' >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo 'Input Preprocessing (CSV to WDM) Started  :: ' $SCENARIO ' ' `date` >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo '' >> $SCENARIO_LOG
endif

if ($DISTRIBUTE_SB_DATA == 1) then

     echo 'Distribute Scenario Builder Data :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=SB_DATA_$iSLURM_JOB_NAME  bhatt_distribute_sb_data.csh $SB_FOLDER $SB_VERSION $SB_SCENARIO $WSM_SCENARIO OVERWRITE &
     wait
     echo 'Distribute Scenario Builder Data :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     #if( $SEPTIC_WDMS == 1) then
     #     echo 'Septic CSV files to WDMs :: Started  ' `date` >> $SCENARIO_LOG
     #     srun --nodes=1 --ntasks=1 --exclusive --job-name=SB_DATA_$iSLURM_JOB_NAME  bhatt_create_scenario_septic_wdms_from_scenario_csv.csh $SCENARIO $user-$iSLURM_JOBID-SEPTIC_WDMS &
     #     wait
     #     echo 'Septic CSV files to WDMs :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     #endif

     echo 'CHECK SLURM ' $CHECK_SLURM
     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_DISTRIBUTE_SB_DATA :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_DISTRIBUTE_SB_DATA &
          wait
          echo 'Checking Slurm STEP_DISTRIBUTE_SB_DATA :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif


if ($POINT_SOURCE_WDMS == 1) then

     echo 'Point Source CSV files to WDMs :: Started  ' `date` >> $SCENARIO_LOG
     #srun --nodes=1 --ntasks=1 --exclusive --job-name=PS_DATA_$iSLURM_JOB_NAME  Clean_PS_TextFiles.csh $PS_DATAVERSION $WWTP tempWWTP &
     #srun --nodes=1 --ntasks=1 --exclusive --job-name=PS_DATA_$iSLURM_JOB_NAME  Clean_PS_TextFiles.csh $PS_DATAVERSION $IND  tempIND  &
     #srun --nodes=1 --ntasks=1 --exclusive --job-name=PS_DATA_$iSLURM_JOB_NAME  Clean_PS_TextFiles.csh $PS_DATAVERSION $CSO  tempCSO  &
     #wait

     srun --nodes=1 --ntasks=1 --exclusive --job-name=PS_DATA_$iSLURM_JOB_NAME  bhatt_create_scenario_ps_wdms_from_csv.csh $SCENARIO $user-$iSLURM_JOBID-PS_WDMS &
     wait
     echo 'Point Source CSV files to WDMs :: Finishded ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_POINT_SOURCE_WDMS :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_POINT_SOURCE_WDMS &
          wait
          echo 'Checking Slurm STEP_POINT_SOURCE_WDMS :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif


if ( $RIB_WDMS == 1 ) then
     echo 'RIB CSV files to WDMs :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=RIB_$iSLURM_JOB_NAME bhatt_create_scenario_rib.csh $SCENARIO $user-$iSLURM_JOBID-RIB_WDMS &
     wait
     echo 'RIB CSV files to WDMs :: Finishded ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RIB_WDMS :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RIB_WDMS &
          wait
          echo 'Checking Slurm STEP_RIB_WDMS :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ( $RPA_WDMS == 1 ) then
     echo 'RPA CSV files to WDMs :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=RPA_$iSLURM_JOB_NAME bhatt_create_scenario_rpa.csh $SCENARIO $user-$iSLURM_JOBID-RPA_WDMS &
     wait
     echo 'RPA CSV files to WDMs :: Finishded ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RPA_WDMS :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RPA_WDMS &
          wait
          echo 'Checking Slurm STEP_RPA_WDMS :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ( $SEPTIC_WDMS == 1 ) then
     echo 'Septic CSV files to WDMs :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=SEP_$iSLURM_JOB_NAME bhatt_create_scenario_sep.csh $SCENARIO $user-$iSLURM_JOBID-SEP_WDMS &
     wait
     echo 'Septic CSV files to WDMs :: Finishded ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_SEP_WDMS :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_SEP_WDMS &
          wait
          echo 'Checking Slurm STEP_SEP_WDMS :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ( $PRN > 0 ) then
     echo '' >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo 'Input Preprocessing (CSV to WDM) Finished :: ' $SCENARIO ' ' `date` >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo '' >> $SCENARIO_LOG
endif


# **************************************** WATERSHED MODEL SIMULATION **************************************** #
@ PRN = $RUN_MAKE_LAND_DIR + $RUN_MAKE_RIVER_DIR + $RUN_LUG + $RUN_LAND + $RUN_ETM1 + $RUN_ETM2 + $RUN_RIVER + $RUN_POSTPROC + $RUN_WQM_INPUT + $RUN_CH3D_INPUT + $RUN_REMOVE_DIRS
if ( $PRN > 0 ) then
     echo '' >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo 'Watershed Model Simlation Started for Scenario  :: ' $SCENARIO ' ' `date` >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo '' >> $SCENARIO_LOG
endif


if ($RUN_MAKE_LAND_DIR == 1) then

     echo 'Create Land Directories :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=LAND_DIR_$iSLURM_JOB_NAME  make_land_directories.csh $SCENARIO $user-$iSLURM_JOBID-LAND &
     wait
     echo 'Create Land Directories :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG


     echo 'CHECK SLURM ' $CHECK_SLURM
     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_MAKE_LAND_DIR :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_MAKE_LAND_DIR &
          wait
          echo 'Checking Slurm STEP_RUN_MAKE_LAND_DIR :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif


if ($RUN_MAKE_RIVER_DIR == 1) then

     echo 'Create River Directories :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=RIV_DIR_$iSLURM_JOB_NAME  make_river_directories.csh $SCENARIO $user-$iSLURM_JOBID-RIVER &
     wait
     echo 'Create River Directories :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG


     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_MAKE_RIVER_DIR :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_MAKE_RIVER_DIR &
          wait
          echo 'Checking Slurm STEP_RUN_MAKE_RIVER_DIR :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif


if ($RUN_LUG == 1) then

     echo 'Land UCI Generator :: Started  ' `date` >> $SCENARIO_LOG

     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.land

     @ i = 1
     foreach segment ($segments)

#          if ( $INFEXP == 0 ) then
#               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_lug_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
#          else
#               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_lug_INFEXP_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
#          endif
        if ( $LAND_QUIET == 1 ) then
           if ( $INFEXP == 0 ) then
              srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=LUG_`printf "%03d" $i`_$SCENARIO bhatt_run_lug_quiet_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &
           else
              srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=LUG_`printf "%03d" $i`_$SCENARIO bhatt_run_lug_INFEXP_quiet_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &
           endif
        else
           if ( $INFEXP == 0 ) then
              srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=LUG_`printf "%03d" $i`_$SCENARIO bhatt_run_lug_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &
           else
              srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=LUG_`printf "%03d" $i`_$SCENARIO bhatt_run_lug_INFEXP_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &
           endif
        endif

        @ i += 1

     end

     wait
     echo 'Land UCI Generator :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG


     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_LUG :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_LUG &
          wait
          echo 'Checking Slurm STEP_RUN_LUG :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif



if ($RUN_LAND == 1) then
     
     echo 'Land Simulation :: Started  ' `date` >> $SCENARIO_LOG

     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.land

     @ i = 1
     foreach segment ($segments)

          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_land_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &

          @ i += 1

     end

     wait
     echo 'Land Simulation :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
          
     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_LAND :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_LAND &
          wait
          echo 'Checking Slurm STEP_RUN_LAND :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif


if ($RUN_LUGX == 1) then
     echo 'LUGX Simulation :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.land

     @ i = 1
     foreach segment ($segments)
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_lugx_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
          @ i += 1
     end

     wait
     echo 'LUGX Simulation :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then
          echo 'Checking Slurm STEP_RUN_LUGX :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_LUGX &
          wait
          echo 'Checking Slurm STEP_RUN_LUGX :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ($RUN_UNEC == 1) then
     echo 'UNEC Simulation :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS
     
     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES
     
     source ../../config/seglists/${basin}.land

     @ i = 1
     foreach segment ($segments)
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_unec_oneseg.csh $scenario $segment $N_SPIN_MONTHS $user-$iSLURM_JOBID-`printf "%04d" $i` &
          @ i += 1
     end

     wait
     echo 'UNEC Simulation :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then
          echo 'Checking Slurm STEP_RUN_UNEC :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_UNEC &
          wait
          echo 'Checking Slurm STEP_RUN_UNEC :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ($RUN_BODX == 1) then
     echo 'BODX Simulation :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.land

     @ i = 1
     foreach segment ($segments)
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_bodx_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
          @ i += 1
     end

     wait
     echo 'BODX Simulation :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then
          echo 'Checking Slurm STEP_RUN_BODX :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_BODX &
          wait
          echo 'Checking Slurm STEP_RUN_BODX :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif

if ($RUN_ANNLOAD == 1) then
     echo 'RUN ANN LOAD :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.riv

     @ i = 1
     foreach segment ( $segments )

        srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=ANNLOAD_`printf "%03d" $i`_$SCENARIO bhatt_run_annload_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &

        @ i += 1
     end
     wait

     echo 'RUN ANN LOAD :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then
        echo 'Checking Slurm STEP RUN ANN LOAD :: Started  ' `date` >> $SCENARIO_LOG
        srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=CHK_ANNLOAD_$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_ANNLOAD &
        wait
        echo 'Checking Slurm STEP RUN ANN LOAD :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ($RUN_AVGLOAD == 1) then
     echo 'RUN AVG LOAD :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.riv

     @ i = 1
     foreach segment ( $segments )

        srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=AVGLOAD_`printf "%03d" $i`_$SCENARIO bhatt_run_avgload_oneseg.csh $scenario $segment $user-${iSLURM_JOBID}-`printf "%04d" $i` &

        @ i += 1
     end
     wait

     echo 'RUN AVG LOAD :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     if($CHECK_SLURM == 1) then
        echo 'Checking Slurm STEP RUN AVG LOAD :: Started  ' `date` >> $SCENARIO_LOG
        srun --slurmd-debug=0 --nodes=1 --ntasks=1 --exclusive --job-name=CHK_AVGLOAD_$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_AVGLOAD &
        wait
        echo 'Checking Slurm STEP RUN AVG LOAD :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
endif


if ($RUN_ETM1 == 1) then
          
     echo 'External Transfer Module 1 :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS
     set year1    = $AVG_YEAR1
     set year2    = $AVG_YEAR2

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.riv

     @ i = 1
     foreach segment ($segments)

       if ( $SKIP_ETMPP == 0 ) then
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &
       else
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_oneseg_SKIP.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &
       endif

          @ i += 1
     end

     if ( $SKIP_ETMPP == 1 ) then
        #cp -pr ../../output/eop/aveann/$ETMSCEN/ ../../output/eop/aveann/$scenario/
        #cp -pr ../../output/eof/aveann/$ETMSCEN/ ../../output/eof/aveann/$scenario/
        #cp -pr ../../output/eos/aveann/$ETMSCEN/ ../../output/eos/aveann/$scenario/
        #cp -pr ../../output/eor/aveann/$ETMSCEN/ ../../output/eor/aveann/$scenario/
        #cp -pr ../../output/bay/aveann/$ETMSCEN/ ../../output/bay/aveann/$scenario/

        #cp -pr ../../tmp/wdm/river/$ETMSCEN/eos/* ../../tmp/wdm/river/$scenario/eos/
     endif

     wait
     echo 'External Transfer Module 1 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG


     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_ETM1 :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_ETM1 &
          wait
          echo 'Checking Slurm STEP_RUN_ETM1 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif



if ($RUN_ETM2 == 1) then
           
     echo 'ETM 2 :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS
     set year1    = $AVG_YEAR1
     set year2    = $AVG_YEAR2

     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES

     source ../../config/seglists/${basin}.riv

     @ i = 1
     foreach segment ($segments)

          srun -l --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_etm_and_land_and_dat_simultaneously_oneseg_ETM2.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &

          @ i += 1
     end

     wait 
     echo 'ETM 2 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
               
          
     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_RUN_ETM2 :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_ETM2 &
          wait
          echo 'Checking Slurm STEP_RUN_ETM2 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif



if ($RUN_RIVER == 1 ) then
     if( $CDF == 0) then
          echo 'RUN RIVER SIMULATION :: Started  ' `date` >> $SCENARIO_LOG
          set scenario = $SCENARIO
          set basin    = $BASINS

          set numnodes = $NUM_NODES
          set numcores = $NUM_CORES

          source ../../config/seglists/${basin}_order.riv

          foreach odr ($order)
	       source ../../config/seglists/${basin}_order/${odr}.riv

               @ i = 1
               echo RIV_MSG_STARTED $odr AT `date`
               foreach segment ( $segments )

                    if($RUN_RIVER_MODE == 'SCENARIO') then
                         srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_river_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
                    endif
                    if($RUN_RIVER_MODE == 'CALIB') then
                         srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_river_oneseg_calib.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
                    endif

                    @ i += 1
               end
               wait
               echo RIV_MSG_COMPLETED $odr AT `date`
          end

          wait 
          echo 'RUN RIVER SIMULATION :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     else
          echo >> $SCENARIO_LOG
          echo 'CRITICAL ERROR -- ABORTING' >> $SCENARIO_LOG
          echo 'RUN_RIVER :: Set CDF=0 if Using River Simulation' >> $SCENARIO_LOG
          echo 'RUN_RIVER :: Set RUN_RIVER=0 if Using CDF Mode' >> $SCENARIO_LOG
          echo '' >> $SCENARIO_LOG
          exit
     endif


     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_RUN_RIVER :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_RIVER &
          wait
          echo 'Checking Slurm STEP_RUN_RIVER :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif



if ($RUN_POSTPROC == 1) then

     if( $CDF == 1 ) then 
          echo 'RUN POST PROCESSING CDF = 1 :: Started  ' `date` >> $SCENARIO_LOG

          set scenario = $SCENARIO
          set basin    = $BASINS
          set year1    = $AVG_YEAR1
          set year2    = $AVG_YEAR2

          set numnodes = $NUM_NODES
          set numcores = $NUM_CORES

          set CDF_Sce  = $CDF_SCENARIO

          source ../../config/seglists/${basin}_order.riv

          echo 'LOG-MESSAGE POST OTHER DEL START ' `date`
          # *** DEL
          foreach odr ($order)
               source ../../config/seglists/${basin}_order/${odr}.riv

               @ i = 1

               echo DEL_MSG_STARTED $odr AT `date`
               foreach segment ( $segments )

                    srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_PostProcOtherDF_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` $CDF_Sce DEL &

                    @ i += 1
               end
               wait
               echo DEL_MSG_COMPLETED $odr AT `date`
          end
          wait
          echo 'LOG-MESSAGE POST OTHER DEL END ' `date`



          echo 'LOG-MESSAGE POST OTHER SUM_ALL_BASINS START ' `date`
          set scenario = $SCENARIO
          set basin    = $BASINS
          set year1    = $AVG_YEAR1
          set year2    = $AVG_YEAR2

          set numnodes = $NUM_NODES
          set numcores = $NUM_CORES

          set CDF_Sce  = $CDF_SCENARIO

          set basins   = ( $SUMALL_BASINS )

          mkdir -p ../../sumout/aveann/${scenario}_DF_${CDF_Sce}

          @ i = 1
          while ( $i <= ${#basins} )

               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_summarize_OtherDF_aveann.csh $scenario $CDF_Sce $basins[$i] $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &

               @ i += 1
          end

          wait
          echo 'LOG-MESSAGE POST OTHER SUM_ALL_BASINS END ' `date`
          echo 'RUN POST PROCESSING CDF = 1 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif

     if( $CDF == 0 ) then
          echo 'RUN POST PROCESSING CDF = 0 :: Started  ' `date` >> $SCENARIO_LOG
          echo 'LOG-MESSAGE postproc_river_aveann START ' `date`

          set scenario = $SCENARIO
          set basin    = $BASINS
          set year1    = $AVG_YEAR1
          set year2    = $AVG_YEAR2

          set numnodes = $NUM_NODES
          set numcores = $NUM_CORES

          source ../../config/seglists/${basin}.riv

          @ i = 1

          foreach segment ( $segments )

               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_postproc_river_aveann_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &

               @ i += 1

          end

          wait
          echo 'LOG-MESSAGE postproc_river_aveann END ' `date`


          set scenario = $SCENARIO
          set basin    = $BASINS
          set year1    = $AVG_YEAR1
          set year2    = $AVG_YEAR2

          set numnodes = $NUM_NODES
          set numcores = $NUM_CORES

          source ../../config/seglists/${basin}.riv
          echo 'LOG-MESSAGE TFS_DFS_DEL TFS START ' `date`
          # *** TFS
               @ i = 1

               echo TFS_MSG_STARTED AT `date`
               foreach segment ( $segments )

                    srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_tf_df_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` TFS &

                    @ i += 1
               end
               wait
               echo TFS_MSG_COMPLETED AT `date`
          echo 'LOG-MESSAGE TFS_DFS_DEL TFS END ' `date`


          echo 'LOG-MESSAGE TFS_DFS_DEL DFS START ' `date`
          # *** DFS
          source ../../config/seglists/${basin}_order.riv
          foreach odr ($order)
               source ../../config/seglists/${basin}_order/${odr}.riv

               @ i = 1

               echo DFS_MSG_STARTED $odr AT `date`
               foreach segment ( $segments )

                    srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_tf_df_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` DFS &

                    @ i += 1

               end
               wait
               echo DFS_MSG_COMPLETED $odr AT `date`
          end
          wait
          echo 'LOG-MESSAGE TFS_DFS_DEL DFS END ' `date`


          echo 'LOG-MESSAGE TFS_DFS_DEL DEL START ' `date`
          source ../../config/seglists/${basin}.riv
          # *** DEL
               @ i = 1

               echo DEL_MSG_STARTED AT `date`
               foreach segment ( $segments )

                    srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_tf_df_oneseg.csh $scenario $segment $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` DEL &

                    @ i += 1

               end
               wait
               echo DEL_MSG_COMPLETED AT `date`
          echo 'LOG-MESSAGE TFS_DFS_DEL DEL END ' `date`


          echo 'LOG-MESSAGE SUM_ALL_BASINS START ' `date`

          set scenario = $SCENARIO
          set basin    = $BASINS
          set year1    = $AVG_YEAR1
          set year2    = $AVG_YEAR2

          set numnodes = $AVG_YEAR1
          set numcores = $AVG_YEAR2

          set basins   = ( $SUMALL_BASINS )


          @ i = 1
          while ( $i <= ${#basins} )

               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_summarize_output_aveann.csh $scenario $basins[$i] $year1 $year2 $user-$iSLURM_JOBID-`printf "%04d" $i` &

               @ i += 1
          end

          wait
          echo 'LOG-MESSAGE SUM_ALL_BASINS END ' `date`
          echo 'RUN POST PROCESSING CDF = 0 :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     endif
          
     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_RUN_POSTPROC :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_POSTPROC &
          wait
          echo 'Checking Slurm STEP_RUN_POSTPROC :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif




if ($RUN_EOT == 1 ) then
     echo 'RUN EOT SIMULATION :: Started  ' `date` >> $SCENARIO_LOG
     set scenario = $SCENARIO
     set basin    = $BASINS
          
     set numnodes = $NUM_NODES
     set numcores = $NUM_CORES
          
     source ../../config/seglists/${basin}.riv
               
     @ i = 1
     foreach segment ( $segments )
               
          srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_$iSLURM_JOB_NAME bhatt_run_eotbmp_oneseg.csh $scenario $segment $user-$iSLURM_JOBID-`printf "%04d" $i` &
               
          @ i += 1
     end  
     wait
     echo 'RUN EOT SIMULATION :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
     

     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_RUN_EOT :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_EOT &
          wait 
          echo 'Checking Slurm STEP_RUN_EOT :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif



if ($RUN_WQM_INPUT == 1) then

     echo 'Generate WQM Input :: Started  ' `date` >> $SCENARIO_LOG

     set scenario = $SCENARIO
     set Request  = $WQM_REQUEST
     set year1    = $WQM_YEAR1
     set year2    = $WQM_YEAR2

     if ( $RUN_CH3D_INPUT == 0 ) then
          set CH3D     = 0

          @ i  = 1
          @ yyyy = $year1
          while ( $yyyy <= $year2 )

               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_PS_$iSLURM_JOB_NAME bhatt_run_ps_to_request.csh $scenario $Request $PSMETHOD $yyyy $yyyy $user-$iSLURM_JOBID-PS-`printf "%04d" $i` $CH3D &
               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_NPS_$iSLURM_JOB_NAME bhatt_run_nps_to_request.csh $scenario $Request $yyyy $yyyy $user-$iSLURM_JOBID-NPS-`printf "%04d" $i` $CH3D &
               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_SHO_$iSLURM_JOB_NAME bhatt_run_sho_to_request.csh $scenario $Request $yyyy $yyyy $user-$iSLURM_JOBID-SHO-`printf "%04d" $i` $CH3D &

               @ yyyy += 1
               @ i += 1
          end
     endif

     if ( $RUN_CH3D_INPUT == 1 ) then
          set CH3D     = 1

          @ i = 1
          @ y2 = $year1
          while ( $y2 <= $year2 )

               @ y1 = $y2 - 1
               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_PS_$iSLURM_JOB_NAME bhatt_run_ps_to_request.csh $scenario $Request $PSMETHOD $y1 $y2 $user-$iSLURM_JOBID-PS-`printf "%04d" $i` $CH3D &
               srun --nodes=1 --ntasks=1 --exclusive --job-name=`printf "%03d" $i`_NPS_$iSLURM_JOB_NAME bhatt_run_nps_to_request.csh $scenario $Request $y1 $y2 $user-$iSLURM_JOBID-NPS-`printf "%04d" $i` $CH3D &

               @ y2 += 1
               @ i += 1
          end
     endif

     wait
     echo 'Generate WQM Input :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG


     if($CHECK_SLURM == 1) then

          echo 'Checking Slurm STEP_RUN_WQM_INPUT :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_WQM_INPUT &
          wait
          echo 'Checking Slurm STEP_RUN_WQM_INPUT :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG

     endif

     if ( $PSMETHOD == 'wcell' ) then
        cp -vp ../../output/wqm_input/${Request}_W/$SCENARIO/wsm57k_wsm_ps.*  ../../output/wqm_input/${Request}/$SCENARIO/
     else if ( $PSMETHOD == 'lrseg' ) then
        cp -vp ../../output/wqm_input/${Request}_L/$SCENARIO/wsm57k_wsm_ps.*  ../../output/wqm_input/${Request}/$SCENARIO/
     else
        echo "Method $PSMETHOD for processing PS WQM inputs not implemented"
     endif

     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG

endif





if ($RUN_REMOVE_DIRS > 0) then
           
     echo 'Remove Land + River Dirs :: Started  ' `date` >> $SCENARIO_LOG
     srun --nodes=1 --ntasks=1 --exclusive --job-name=RM_LAND_DIR_$iSLURM_JOB_NAME remove_land_directories.csh $SCENARIO $user-$iSLURM_JOBID-RM_LAND &
     srun --nodes=1 --ntasks=1 --exclusive --job-name=RM_RIV_DIR_$iSLURM_JOB_NAME remove_river_directories.csh $SCENARIO $user-$iSLURM_JOBID-RM_RIV &
     if ($RUN_REMOVE_DIRS == 2) then
          srun --nodes=1 --ntasks=1 --exclusive --job-name=RM_ALL_DIR_$iSLURM_JOB_NAME remove_everything_else.csh $SCENARIO $CDF_SCENARIO $user-$iSLURM_JOBID-RM_ALL &
     endif
     wait 
     echo 'Remove Land + River Dirs :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
               
          
     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_RUN_REMOVE_DIRS :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_RUN_REMOVE_DIRS &
          wait
          echo 'Checking Slurm STEP_RUN_REMOVE_DIRS :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif


set ABC = 0
if ($ABC == 1) then
           
     echo '???? :: Started  ' `date` >> $SCENARIO_LOG
     ???? srun --nodes=1 --ntasks=1 --exclusive --job-name=LAND_DIR_$iSLURM_JOB_NAME  ????.csh $SCENARIO $user-$iSLURM_JOBID-LAND &
     wait 
     echo '???? :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
               
          
     if($CHECK_SLURM == 1) then
          
          echo 'Checking Slurm STEP_???? :: Started  ' `date` >> $SCENARIO_LOG
          srun --nodes=1 --ntasks=1 --exclusive --job-name=$SCENARIO bhatt_check_slurm.csh $SCENARIO_SLURM $SCENARIO_LOG STEP_???? &
          wait
          echo 'Checking Slurm STEP_???? :: Finished ' `date` >> $SCENARIO_LOG; echo >> $SCENARIO_LOG
          
     endif
     echo '---------------------------------------------------------------------------------------------' >> $SCENARIO_LOG
     
endif









./sendmail.csh FINISHED $SCENARIO $SCENARIO_LOG

     
if ($REMOVE_SLURM == 1) then
     rm $SCENARIO_SLURM
endif


if ( $PRN > 0 ) then
     echo '' >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo 'Watershed Model Simlation Finished for Scenario :: ' $SCENARIO ' ' `date` >> $SCENARIO_LOG
     echo '*********************************************************************************************' >> $SCENARIO_LOG
     echo '' >> $SCENARIO_LOG
     echo '\o/' >> $SCENARIO_LOG
     echo '' >> $SCENARIO_LOG

endif

