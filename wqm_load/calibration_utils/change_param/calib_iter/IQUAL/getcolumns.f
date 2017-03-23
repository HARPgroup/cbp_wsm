************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumns(
     I                      tkeep,vkeep,cnq,clu,
     O                      vcPOTFW,vcSQO,vcACQOP,
     O                      vcSQOLIM,vcWSQOP)
      implicit none
      include 'iqual.inc'

      character*12 table,variable  ! variable specifiers

      integer i,nl      ! index

      character*2000 vkeep,tkeep,varline,tableline
      character*(*) cnq
      character*(*) clu

*************** END DECLARATIONS ***************************************

********* FIND THE COLUMNs OF EACH VARIABLE
      table = 'QUAL-INPUT#'//cnq
      variable = 'POTFW'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcPOTFW,err)
      if (err.ne.0) go to 992

      table = 'QUAL-INPUT#'//cnq
      variable = 'SQO'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcSQO,err)
      if (err.ne.0) go to 992

      table = 'QUAL-INPUT#'//cnq
      variable = 'ACQOP'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcACQOP,err)
      if (err.ne.0) go to 992

      table = 'QUAL-INPUT#'//cnq
      variable = 'SQOLIM'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcSQOLIM,err)
      if (err.ne.0) go to 992

      table = 'QUAL-INPUT#'//cnq
      variable = 'WSQOP'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcWSQOP,err)
      if (err.ne.0) go to 992

      return

********************* ERROR SPACE **************************************
992   report(1) = 'problem reading parameter file'
      report(2) = ' for land use '//clu
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //table//variable
      if (err.eq.3) report(3) = 
     .                      'file has line longer than 2000 characters'
      go to 999

999   call stopreport(report)

      end
