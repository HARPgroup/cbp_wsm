************************************************************************
**  Include file for the river uci generator                          **
************************************************************************

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/pltgen.inc'

      integer uci                ! file numbers
      parameter (uci=12)

      integer Nexits,lakeflag    ! river flags
				 ! number of exits and flag for
				 ! lake simulation.  
			! 0 = regular river
			! 1 = reservoir 
      character*1 resflag  ! S = standard
                           ! V = variable ftable
                           ! C = Conowingo special case
                           ! W = Conanesque (6 month transition period)
                           ! R = Curwensville (variable to static)
                           ! B = Sayer/Blanchard (2 separate variable ftables)

      integer startmonth  ! used in global and reservoir ftables

      integer timestep    ! used in op seq, ext sources, ext targets, and pltgen


