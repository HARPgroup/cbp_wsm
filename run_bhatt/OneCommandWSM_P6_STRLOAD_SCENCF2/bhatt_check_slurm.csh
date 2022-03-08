#!/bin/csh
# Script finds specific keyword in the slurm file to check success
# If success delete slurms :: If failed cancel all the jobs
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

if( ${#argv} > 1 ) then
   set SCENARIO_SLURM = $argv[1]
   set SCENARIO_LOG   = $argv[2]
else
   echo 'ERROR'
   echo 'Please provide input : Slurm & Log File Names'
endif

if( ${#argv} > 2 ) then
   set FLAG_STEP    = $argv[3]
else
   set FLAG_STEP = 'PROCESS_NA'
endif

#set user = `whoami`
set jobs = $SLURM_JOB_NAME
#set jobs = abc
set error_exclude = '/modeling/p532/run_bhatt/OneCommandWSM_20141117/slurm_error_exclude.txt'
set perm_exclude = '/modeling/p532/run_bhatt/OneCommandWSM_20141117/slurm_permission_exclude.txt'

set a = `grep -il problem    $SCENARIO_SLURM | wc -l`
set b = `grep -il ech        $SCENARIO_SLURM | wc -l`
set c = `grep -il abort      $SCENARIO_SLURM | wc -l`
#set m = `grep -il permission $SCENARIO_SLURM | wc -l`
set m = `grep -v -f $perm_exclude $SCENARIO_SLURM | grep -i permission | wc -l`
#set n = `grep -il error       $SCENARIO_SLURM | wc -l`
set n = `grep -v -f $error_exclude $SCENARIO_SLURM | grep -i error | wc -l`
#set o = `grep -il command    $SCENARIO_SLURM | wc -l`

echo a= $a
echo b= $b
echo c= $c
echo m= $m
echo n= $n
#echo o= $o
#@ d += $a + $b + $c + $m + $n + $o
@ d += $a + $b + $c + $m + $n
#@ d += $a + $b + $c + $m
#echo $d
#echo Dir= $SlurmDir
#echo `pwd`

if ($d > 0) then
	echo; echo;
	echo ERROR in Processing Step $FLAG_STEP >> $SCENARIO_LOG
	echo 
	echo Canceling jobs of the $jobs scenario... >> $SCENARIO_LOG
	echo 

        #./sendmail.csh FAILED $SCENARIO $SCENARIO_LOG
        ./sendmail.csh FAILED $jobs $SCENARIO_LOG
	scancel --state=PENDING --user=$user --name=$jobs
	scancel --state=RUNNING --user=$user --name=$jobs
else
#	echo rm  = $SlurmDir
#	echo frm = `pwd`
#	cd $SlurmDir; cd ..
	echo 'Cleaning ' $SCENARIO_SLURM
	#??./CleanSlurmFile.exe $SCENARIO_SLURM
endif

exit 0
