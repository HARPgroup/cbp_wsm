#!/bin/bash
if( ${#argv} > 1 ) then
   set SlurmDir = $argv[1]
else
   set SlurmDir = .
endif

set var=`grep -il problem $SlurmDir/slurm* | wc -l`
echo $var
#a=${(grep -il problem $SlurmDir/slurm* | wc -l)}
#eacho b= $a
