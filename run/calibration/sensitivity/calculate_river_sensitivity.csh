#!/bin/csh
###############################################################################
## To start a sensitivity run do the following:
##   make sure the years in this script match the years 
##       in the river control file
##   delete everything in the ./output/river/cfd/$scen/ directory
##   reset the param list in the ./pp/param/river/$paramscen/ to all -9s
##   remove the ./pp/param/river/$paramscen/npnv.prn file
##
## when this run is finished run the program 
##  ../../../code/bin/read_river_sens_matrix.exe
###############################################################################

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = $argv[1]
  set basin = $argv[2]
  source ../../fragments/set_tree

  set year1 = 1996
  set year2 = 1997

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

    if (-e problem) then
      rm problem
    endif

########## RUN TO GET INITIAL CONDITIONS
##### first make sure initial conditions match param_list.csv
    echo $scenario | $tree/code/bin/initialize_river_sens_param_files.exe
    $tree/run/run_river_sens_iter.csh $scenario $basin $year1 $year2 $tree
     if (-e problem) then
       echo 'problem in run_river_sens_iter'
       cat problem
       rm problem
       exit
     endif

########### SPECIAL PROGRAM TO STORE INITIAL CONDITIONS
    echo $scenario $year1 $year2 | $tree/code/bin/write_WQ_start_matrix.exe
     if (-e problem) then
       echo 'problem in write_WQ_start_matrix.exe'
       cat problem
       rm problem
       exit
     endif

############# CALL SCRIPT TO CREATE JACOBIAN RECURSIVELY
       if (-e finished.sens) then
         rm finished.sens
       endif

     set nums = (1 2 3 4 5 6 7 8 9)

     foreach num1 ($nums)           ### 9
       foreach num2 ($nums)         ### 81
         foreach num3 ($nums)       ### 729  will not need that many, exits when finished
       
           $tree/run/calc_river_sens_one_param.csh $scenario $basin $year1 $year2 $tree
             if (-e problem) then
               echo 'problem in calc_river_sens_one_param, run number: ',$num1,' ',$num2,' ',$num3
               cat problem
               rm problem
               exit
             endif

           if (-e finished.sens) then
             rm finished.sens
             exit
           endif

         end
       end
     end

  cd ../
  rm -r temp$$

