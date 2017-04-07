#!/bin/csh
########################################################################
## this script makes subgrid transport factors to convert EOF to EOS  ##  
##  for nitrogen and phosphorous.                                     ##
##                                                                    ##
## This is based on the following:                                    ##
##  The flux at any point in the watershed can be though of as the    ##
##    sum of the upstream edge of stream loads times the transport    ##
##    within the rivers down to the flux point.  The EOS loads are    ##
##    edge of field loads time the sub-grid transport factor          ##
##  The fluxes are known at certain points through estimator          ##
##  The EOF loads are known through targets                           ##
##  The riverine transport is known through sparrow                   ##
##  Other EOS (point source, septic) is known                         ##
##  The only unknown value is the sub-grid transport factor, which    ##
##    will be assumed to be equal for all segments upstream of a flux ##
##    point and downstream of any other flux point.                   ##
##                                                                    ##
##  The subgrid transport factor can be calculated as follows:        ##
##   FluxOut - sum(FluxIn*(pi(TFs))) - sum(EOS*(pi(TFs)))             ##
##   ---------------------------------------------------              ##
##                 sum(EOF*(pi(TFs)))                                 ##
##                                                                    ##
##  The user specifies which data sets are used in the calculation    ##
##    in the ./run/control/calib/subgridtf/$calscen.con  file         ##
##    which references file in the ./pp/data/transport/calc_factors/  ##
##    directory.                                                      ##
##                                                                    ##
##  this always runs on the whole model domain because the chance of  ##
##    errors is too great when passing a segment list                 ##
########################################################################
#   GET SCENARIO 
  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_subgrid_transport_factors.csh cal_scenario '
      echo 'usage:  run_subgrid_transport_factors.csh cal_scenario tree'
      echo ' '
      echo '   cal_scenario refers to a file in ./run/control/calib/subgridTF/'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
  endif

  echo $calscen | $tree/code/bin/EOF_to_EOS.exe

  if (-e problem) then
    echo ' '
    echo ' PROBLEM EOF_to_EOS program'
    cat problem
    if (${#argv} == 1) then
      rm problem
    endif
    exit
  endif

