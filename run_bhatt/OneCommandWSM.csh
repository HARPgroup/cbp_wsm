#!/bin/csh

# Script is provided on an "AS IS" basis and any warranties, either expressed 
# or implied, including but not limited to implied warranties of noninfringement, 
# originality, merchantability and fitness for a particular purpose, are disclaimed.

# Written by GOPAL BHATT (gopal.bhatt@psu.edu)
# Script attempts to run Chesapeake Bay Program - Watershed Model Simulation (CBP-WSM)

# Existing scripts were modified for the purpose
# Execution of this script requires a "script" configuration file for a scenario

# Usage:
# csh OneCommandWSM.csh <scenario name>
# ** e.g. csh OneCommandWSM.csh p532cal_062211

# PS: Actual simulation Start and End dates are specified in the Land/River control files
if( ${#argv} > 0 ) then
     set SCENARIO = $argv[1]
else
     echo 'Usage: csh OneCommandWSM.csh <scenario name>'; echo
     echo 'Enter Name of the WSM Scenario: '
     set SCENARIO = $<
     set INTERACTIVE = 1
endif

echo ''
echo 'Locating USER Configuration File...'
if ( ! -e ./.config/${user}_CONFIG ) then
     echo 'Error'; echo 'Please execute configure.csh first!'; exit;
endif

echo ''
echo 'Locating LAND Configuration File...'
if ( ! -e ../config/control/land/$SCENARIO.con ) then
     echo 'Error'; exit;
endif
echo 'Locating RIVER Configuration File...'
if ( ! -e ../config/control/river/$SCENARIO.con ) then
     echo 'Error'; exit;
endif
echo 'Locating SCRIPT Configuration File...'
if ( ! -e ../config/control/script/$SCENARIO.con ) then
     echo 'Error'; exit;
endif
echo ''

source ./.config/${user}_CONFIG
source ../config/control/script/$SCENARIO.con

if ( ($?EMAIL_TO_WSM) ) then
   set EMAIL_TO = $EMAIL_TO_WSM
endif


if( ${#argv} == 2 ) then
     if ( "$argv[2]" == 'pdebug' ) then
          set ProcPool = -$argv[2]
     else if ( "$argv[2]" == '-pdebug' ) then
          set ProcPool = $argv[2]
     else
          set ProcPool = 
     endif
endif

set TIME_STAMP     = `date +%Y-%m-%d-%H-%M-%S`


# Directories to store Slurms, Processing, and Logs
mkdir -p $SLURM_OUT_DIR; mkdir -p $SCRATCH; mkdir -p $LOGDIR
if ( ! ($?PRIORITY) ) set PRIORITY = 5000

echo 'WSM Scenario: **' $SCENARIO '** in '$PROJECT_HOME
echo 'Using **' $NUM_NODES '** processor with **' $NUM_CORES '** cores on each'
echo 'User Name: '$user' ('$EMAIL_TO')'
echo "Job Priority = $PRIORITY"
echo; echo 'Modules: *' `csh ${SCRIPT_DIR}/modules.csh $SCENARIO`
echo ''
echo 'Continue? [Y/N]'
set response = $<

if ( $response == 'Y' || $response == 'y') then
     # Define (a) Slurm Output File (b) Log File
     set SCENARIO_SLURM = ${SLURM_OUT_DIR}/${SCENARIO}_${TIME_STAMP}.out
     set SCENARIO_LOG   = ${LOGDIR}/${SCENARIO}_${TIME_STAMP}.log
     #mkdir -p $SCENARIO_SLURM

     # Submit CBP-WSM Job to the HPC Slurm Queue
     cd ../${MY_HOME}/${SCRIPT_DIR}; echo
#     sbatch --mem-per-cpu=1500 --nice=$PRIORITY --output=$SCENARIO_SLURM --error=$SCENARIO_SLURM --job-name=$SCENARIO --dependency=singleton --nodes=$NUM_NODES --ntasks-per-node=$NUM_CORES $ProcPool --mail-type=BEGIN --mail-user=$EMAIL_TO bhatt_one_command_wsm.csh $SCENARIO $SCENARIO_SLURM $SCENARIO_LOG
#     sbatch --mem-per-cpu=1500 --nice=$PRIORITY --output=$SCENARIO_SLURM --error=$SCENARIO_SLURM --job-name=$SCENARIO --dependency=singleton --nodes=$NUM_NODES --ntasks-per-node=$NUM_CORES $ProcPool --mail-type=FAIL --mail-user=$EMAIL_TO bhatt_one_command_wsm.csh $SCENARIO $SCENARIO_SLURM $SCENARIO_LOG
#     sbatch --mem-per-cpu=1000 --nice=$PRIORITY --output=$SCENARIO_SLURM --error=$SCENARIO_SLURM --job-name=$SCENARIO --dependency=singleton --nodes=$NUM_NODES --ntasks=$NUM_TASKS $ProcPool --mail-type=FAIL --mail-user=$EMAIL_TO bhatt_one_command_wsm.csh $SCENARIO $SCENARIO_SLURM $SCENARIO_LOG
     echo "Running: sbatch --nice=$PRIORITY --output=$SCENARIO_SLURM --error=$SCENARIO_SLURM --job-name=$SCENARIO --dependency=singleton --nodes=$NUM_NODES --ntasks=$NUM_TASKS $ProcPool --mail-type=FAIL --mail-user=$EMAIL_TO bhatt_one_command_wsm.csh $SCENARIO $SCENARIO_SLURM $SCENARIO_LOG"
     sbatch --nice=$PRIORITY --output=$SCENARIO_SLURM --error=$SCENARIO_SLURM --job-name=$SCENARIO --dependency=singleton --nodes=$NUM_NODES --ntasks=$NUM_TASKS $ProcPool --mail-type=FAIL --mail-user=$EMAIL_TO bhatt_one_command_wsm.csh $SCENARIO $SCENARIO_SLURM $SCENARIO_LOG
     set iRETURN = $?

     if ( $iRETURN == 1 ) then
          #cd ${SCRIPT_DIR}
          #./sendmail.csh SUBMITTED $SCENARIO /dev/null 
          echo 'SLURM Unable to Queue'
     else
          #cd ${SCRIPT_DIR}
          ./sendmail.csh SUBMITTED $SCENARIO /dev/null
          echo ''
          echo 'Data Processing direcoties is:   '$SCRATCH
          echo 'Slurm Output will be written to: '$SCENARIO_SLURM
          echo 'Log Messages will be written to: '$SCENARIO_LOG
          echo ''
     endif
endif

# Exit

