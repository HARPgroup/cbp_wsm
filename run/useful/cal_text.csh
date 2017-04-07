#!/bin/csh
 
#  if (${#argv} != 3) then
#    if (${#argv} != 2) then
#        echo ' '
#        echo 'usage:  cal_text.csh calibration '
#        echo 'or  cal_text.csh calibration tree'
#        echo ' '
#        exit
#     endif
#  endif
  
#  set calib = $argv[1]
#  set basin = $argv[2]
#  if (${#argv} == 2) then
#    set tree = $argv[2]
#  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
#  endif

####### RIVER SEGMENTS ########
#   source $tree/config/seglists/${basin}.riv

#   foreach seg ($segments)
#    if (-e problem) then
#      rm problem
#    endif

#   mkdir -p $tree/output/river/evaluation_statistics/$calib/
   set code = $tree/code/bin/caltext.exe
   $code 





