#!/bin/csh
# Script finds specific keyword in the slurm file to check success
# If success delete slurms :: If failed cancel all the jobs
# Created By Gopal Bhatt (gopal.bhatt@psu.edu)

if( ${#argv} > 0 ) then
   set SlurmDir = $argv[1]
else
   echo 'ERROR'
   echo 'Please provide input : Slurm Directory'
endif

if( ${#argv} > 1 ) then
   set FLAG_STEP = $argv[2]
else
   set FLAG_STEP = 'PROCESS_NA'
endif

#set user = `whoami`
set jobs = $SLURM_JOB_NAME
#set jobs = abc

set a = `grep -il problem    $SlurmDir/slurm* | wc -l`
set b = `grep -il ech        $SlurmDir/slurm* | wc -l`
set c = `grep -il abort      $SlurmDir/slurm* | wc -l`
set m = `grep -il Permission $SlurmDir/slurm* | wc -l`

echo a= $a
echo b= $b
echo c= $c
echo m= $m
@ d += $a + $b + $c + $m
#echo $d
echo Dir= $SlurmDir
#echo `pwd`

if ($d > 0) then
	echo 
	echo ERROR in Processing Step $FLAG_STEP
	echo 
	echo Canceling jobs of the $jobs scenario...
	echo 
	scancel --state=PENDING --user=$user --name=$jobs
	scancel --state=RUNNING --user=$user --name=$jobs
else
#	echo rm  = $SlurmDir
#	echo frm = `pwd`
#	cd $SlurmDir; cd ..
	rm -rf $SlurmDir
endif

exit 0
