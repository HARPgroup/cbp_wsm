************************************************************************
** finds column numbers for the calibrated variables                  **
**  for temperature, just finds the column for the January value since**
**  all variables are monthly except for LGTMP
************************************************************************
      subroutine allcolumns( 
     I       tkeep,vkeep,module,
     O       vcKATRAD)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers

      character*2000 vkeep,tkeep,varline,tableline

      integer vcKATRAD


*************** END DECLARATIONS ***************************************
      table = 'HEAT-PARM'
      variable = 'KATRAD'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                  vcKATRAD,err)
      if (err.ne.0) go to 992

      return
********************* ERROR SPACE **************************************
992   report(1) = 'problem reading parameter file'
      report(2) = ' for river'
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //table//variable
      if (err.eq.3) report(3) = 
     .                      'file has line longer than 2000 characters'
      go to 999

999   call stopreport(report)

      end
