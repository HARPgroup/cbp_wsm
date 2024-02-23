#!/bin/csh

#set SYEAR = 1985
#set EYEAR = 2014

set SYEAR = 2001
set EYEAR = 2014

@ YEAR  = $SYEAR

while ( $YEAR <= $EYEAR )
   echo $YEAR
   csh bhatt_run_ps_to_request.csh P620171001WQg all wcell $YEAR $YEAR tempWQM1 0
   @ YEAR = $YEAR + 1
end
