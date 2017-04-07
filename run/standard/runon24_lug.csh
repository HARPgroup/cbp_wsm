#!/bin/csh

 set scenario = $argv[1]
 set basin = $argv[2]

 sbatch run_lug.csh $scenario ${basin}01
 sbatch run_lug.csh $scenario ${basin}02
 sbatch run_lug.csh $scenario ${basin}03
 sbatch run_lug.csh $scenario ${basin}04
 sbatch run_lug.csh $scenario ${basin}05
 sbatch run_lug.csh $scenario ${basin}06
 sbatch run_lug.csh $scenario ${basin}07
 sbatch run_lug.csh $scenario ${basin}08
 sbatch run_lug.csh $scenario ${basin}09
 sbatch run_lug.csh $scenario ${basin}10
 sbatch run_lug.csh $scenario ${basin}11
 sbatch run_lug.csh $scenario ${basin}12
 sbatch run_lug.csh $scenario ${basin}13
 sbatch run_lug.csh $scenario ${basin}14
 sbatch run_lug.csh $scenario ${basin}15
 sbatch run_lug.csh $scenario ${basin}16
 sbatch run_lug.csh $scenario ${basin}17
 sbatch run_lug.csh $scenario ${basin}18
 sbatch run_lug.csh $scenario ${basin}19
 sbatch run_lug.csh $scenario ${basin}20
 sbatch run_lug.csh $scenario ${basin}21
 sbatch run_lug.csh $scenario ${basin}22
 sbatch run_lug.csh $scenario ${basin}23
 sbatch run_lug.csh $scenario ${basin}24
