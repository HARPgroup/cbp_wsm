#!/bin/csh
# Script attempts to run entire Watershed Model Simulation
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)
# This script also uses existing scripts to run vaious WSM steps

set myHome = /bluefish/archive/modeling/gb533/run_bhatt

set RUN_MAKE_LAND_DIR  = 0
set RUN_MAKE_RIVER_DIR = 0
set RUN_LUG            = 0
set RUN_LAND           = 0
set RUN_ETM            = 1
set RUN_RIVER          = 0
set RUN_POSTPROC_GCM   = 0
set RUN_POSTPROC       = 0
set RUN_WQM_INPUT      = 0

# YAY or NAY INPUTS
#set CONSTANT_DELIVERY_FACTORS   = YAY
set CONSTANT_DELIVERY_FACTORS    = NAY
#set WQM_INPUT                   = YAY
set WQM_INPUT                    = NAY

#SSSSSSSSSSSSSSSSSSSSSSS ASSIGN CMD LINE INPUTS TO VARIABLES : START SSSSSSSSSSSSSSSSSSSSSSS
set SceName  = $argv[1]
set Basin    = $argv[2]
set AveYear1 = $argv[3]
set AveYear2 = $argv[4]

set NumNodes = $argv[5]
set NumCores = $argv[6]
set ProcPool =
if ( ${#argv} > 6 ) then
        set ProcPool = -$argv[7]
endif

#Directory to store Slurms from WSM & ErrorCheck
mkdir -p ../tmp/${user}-SlurmOutputs
mkdir -p ../tmp/${user}-CheckSlurms
mkdir -p ../tmp/${user}-scratch

set CheckDir = ../../tmp/${user}-CheckSlurms
#TODO: 1.0 Implement Slurm Dir based on User Names
#EEEEEEEEEEEEEEEEEEEEEEE ASSIGN CMD LINE INPUT TO VARIABLES : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS CREATE LAND DIRECTORIES : START SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_MAKE_LAND_DIR == 1) then
        cd ../run_bhatt/standard/
        pwd
        
        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_LDIR_$$
        mkdir -p $SlurmDir
        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton $ProcPool make_land_directories.csh $SceName

        #CHECK SUCCESS
        cd ../../run_bhatt/useful/
	#sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_LDIR

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE CREATE LAND DIRECTORIES : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS CREATE RIVER DIRECTORIES : START SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_MAKE_RIVER_DIR == 1) then
        cd ../run_bhatt/standard
        pwd

        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_RDIR_$$
        mkdir -p $SlurmDir
        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton $ProcPool make_river_directories.csh $SceName

        #CHECK SUCCESS
        cd ../../run_bhatt/useful/
        #sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_RDIR

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE CREATE RIVER DIRECTORIES : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS RUN LUG [LAND USER-CONTROL-INPUT (UCI) GENERATOR]  : START SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_LUG == 1) then
        cd ../run_bhatt/useful/
        pwd

        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_LUG_$$
        mkdir -p $SlurmDir

        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton --nodes=$NumNodes --ntasks-per-node=$NumCores $ProcPool bhatt_run_lug_parallel_srun.csh $SceName $Basin $NumNodes $NumCores

        #CHECK SUCCESS
	cd ../../run_bhatt/useful/
        #sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_LUG

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE RUN LUC [LAND USER-CONTROL-INPUT (UCI) GENERATOR]  : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS RUN LAND SIMULATION : START SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_LAND == 1) then
        cd ../run_bhatt/useful/
        pwd

        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_LAND_$$
        mkdir -p $SlurmDir

        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton --nodes=$NumNodes --ntasks-per-node=$NumCores $ProcPool bhatt_run_land_parallel_srun.csh $SceName $Basin $NumNodes $NumCores

        #CHECK SUCCESS
	cd ../../run_bhatt/useful/
        #sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_LAND

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE RUN LAND SIMULATION : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS RUN ETM LAND DAT : START SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_ETM == 1) then
        cd ../run_bhatt/useful/
        pwd
        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_ETM_$$
        mkdir -p $SlurmDir
        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton --nodes=$NumNodes --ntasks-per-node=$NumCores $ProcPool bhatt_run_etm_and_land_and_dat_simultaneously_parallel_srun.csh $SceName $Basin $AveYear1 $AveYear2 $NumNodes $NumCores
        #-vvvv -d4 -x bluefish4       #wait

        #CHECK SUCCESS
	cd ../../run_bhatt/useful/
        #sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_ETM

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE RUN ETM LAND DAT : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS RUN RIVER : START SSSSSSSSSSSSSSSSSSSSSSS
if($CONSTANT_DELIVERY_FACTORS == 'NAY') then
    if($RUN_RIVER == 1) then
        cd ../run_bhatt/useful/
        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_RIVER_$$
        mkdir -p $SlurmDir

	sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton --nodes=$NumNodes --ntasks-per-node=$NumCores $ProcPool bhatt_run_river_parallel_srun.csh $SceName $Basin $NumNodes $NumCores

	#CHECK SUCCESS
	cd ../../run_bhatt/useful/
        #sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_RIVER

	cd $myHome
        pwd

    endif
endif
#EEEEEEEEEEEEEEEEEEEEEEE RUN RIVER : END EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_POSTPROC_GCM == 1) then
        cd ../run_bhatt/useful/
        set SlurmDir = ../../tmp/${user}-SlurmOutputs/${SceName}_POST_$$
        mkdir -p $SlurmDir

        sbatch --output=$SlurmDir/slurm-%j.out --job-name=$SceName --dependency=singleton --share --nodes=$NumNodes --ntasks-per-node=$NumCores $ProcPool run_postproc_rflow_stats_bhatt_gcm.csh $SceName $Basin 1988 1999 water.yr

        #CHECK SUCCESS
        cd ../parallel/
        sbatch --output=$CheckDir/$SceName-%j.out --job-name=$SceName --dependency=singleton --nodes=1 --ntasks-per-node=1 $ProcPool bhatt_check_slurm.csh $SlurmDir STEP_POST_GCM

        cd $myHome
        pwd
endif
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_POSTPROC == 1) then
     if($CONSTANT_DELIVERY_FACTORS == 'NAY') then

#DO POST PROC CALCULATIONS
        #/bluefish/archive/modeling/p532/run/useful>run_postproc_river_aveann_parallel.csh scenario allBay 1991 2000
        #/bluefish/archive/modeling/p532/run/useful>run_tf_df_parallel.csh scenario 1991 2000
        #/bluefish/archive/modeling/p532/run/standard>sum_all_basins.csh scenario 1991 2000

     endif
endif
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_POSTPROC == 1) then
     if($CONSTANT_DELIVERY_FACTORS == 'YAY') then

#DO POST PROC CALCULATIONS alternate
        #/run/useful/run_postproc_otherDF.csh scenario DFscenario basin aveyear1 aveyear2
        #/run/standard/sum_otherDF_all_basins.csh scenario DFscenario aveyear1 aveyear2

     endif
endif
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE



#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
if($RUN_WQM_INPUT == 1) then

        #/bluefish/archive/modeling/p532/run/link_p5_to_wqm>sbatch run_nps_and_ps_to_wqm57k.csh scenario wcell 1991 2000

endif
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE


#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE
#SSSSSSSSSSSSSSSSSSSSSSS  SSSSSSSSSSSSSSSSSSSSSSS
#EEEEEEEEEEEEEEEEEEEEEEE  EEEEEEEEEEEEEEEEEEEEEEE
