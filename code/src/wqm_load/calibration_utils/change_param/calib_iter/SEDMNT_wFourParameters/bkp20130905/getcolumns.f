************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumns( 
     I                      tkeep,vkeep,module,clu,
     O                      vcKRER,vcAFFIX,vcNVSI,vcKSER,vcJSER,vcJRER)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers

      character*2000 vkeep,tkeep,varline,tableline

      character*(*) clu

      integer vcAFFIX
      integer vcNVSI
      integer vcKRER
      integer vcKSER

      integer vcJSER
      integer vcJRER

*************** END DECLARATIONS ***************************************
      table = 'SED-PARM2'
      variable = 'KRER'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcKRER,err)
      if (err.ne.0) go to 992

      table = 'SED-PARM2'
      variable = 'AFFIX'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcAFFIX,err)
      if (err.ne.0) go to 992

      table = 'SED-PARM2'
      variable = 'NVSI'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcNVSI,err)
      if (err.ne.0) go to 992

      table = 'SED-PARM3'
      variable = 'KSER'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcKSER,err)
      if (err.ne.0) go to 992

      table = 'SED-PARM3'
      variable = 'JSER'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcJSER,err)
      if (err.ne.0) go to 992

      table = 'SED-PARM2'
      variable = 'JRER'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcJRER,err)
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
