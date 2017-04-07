#!/bin/csh
########################################################################
## this script makes subgrid transport factors to convert EOF to EOS  ##
##  for nitrogen and phosphorous.                                     ##
##                                                                    ##
## This method is based on the following:                             ##
##  To correct a load bias at a station, assuming the delivery        ##
##    factors do not change, the total loads input to the rivers must ##
##    change by the inverse of the bias.  This ignores the relative   ##
##    placement in the upstream watershed of the different load       ##
##    sources, but this assumption probably does not cause large      ##
##    problems with the overall analysis.                             ##
##  Bias is defined as simulated/observed                             ##
##                                                                    ##
##  A mass balance is performed around a sub-watershed, which is      ##
##   defined as the watershed between a calibration site and any      ##
##   upstream calibration sites                                       ##
##                                                                    ##
## Variables:                                                         ##
##   Si  = Simulated load at any upstream points                      ##
##   Bsi = Bias of instantaneous load at upstream points              ##
##   Bso = Bias of outlet point                                       ##
##   P   = Point sources in the sub-watershed                         ##
##   A   = Direct atmospheric deposition to water in the sub-watershed##
##   S   = Septic in the sub-watershed                                ##
##   E   = Sum of existing edge-of-stream loads in the sub-watershed  ##
##   T   = Subgrid transport factor (output of program)               ##
##                                                                    ##
## Logic:                                                             ##
##   If observed bias at the outlet is Bso, then, assuming equal      ##
##     delivery factors, we would need to divide all inputs by Bso to ##
##     acheive a balance.  Since the existing EOS loads (E) are the   ##
##     only modified item, the math works out like so:                ##
##                                                                    ##
##   Needed inputs = (Si + P + A + S + E) / Bso                       ##
##   Inputs        = Si/Bsi + P + A + S + E*T                         ##
##   Equating and solving for T:                                      ##
##    T = (Si*(1-Bso/Bsi) + (P+A+S)(1-Bso) + E) / (E * Bso)           ##
##                                                                    ##
##  logic test: with no Si,P,A,S, this reduces to 1/Bso               ##
##   The best representation of bias was found to be the average bias ##
##   of the top four quintiles of the cfd for TN and the top quintile ##
##   for TP.                                                          ##
########################################################################
########################################################################
#   GET SCENARIO 
  set rscen = SL
  set basin = SLbigsgtf
  set year1 = 1986
  set year2 = 2005
  source ../../fragments/set_tree
  if (-e problem) then
    rm problem
  endif

  echo $rscen $basin $year1 $year2 | $tree/code/bin/EOF_to_EOS_KLUGE_for_SLbig.exe

  if (-e problem) then
    echo ' '
    echo ' PROBLEM EOF_to_EOS program'
    cat problem
    rm problem
    exit
  endif

