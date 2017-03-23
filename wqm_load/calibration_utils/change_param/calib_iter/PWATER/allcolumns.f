************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine allcolumns( 
     I       tkeep,vkeep,lus,numlu,module,
     O        vcLZSN,vcAGWR,vcIRC,vcINTFW,vcINFILT,vcAGWETP,vcKVARY,
     O        vcUZSN,vcUZSNJAN)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers

      integer numlu,nl  ! number of land uses to act on
      integer lus(numlu) ! index of land uses to act on

      character*2000 vkeep(nlu),tkeep(nlu),varline,tableline

      integer vcLZSN(nlu)
      integer vcAGWR(nlu)
      integer vcIRC(nlu)
      integer vcINTFW(nlu)
      integer vcINFILT(nlu)
      integer vcAGWETP(nlu)
      integer vcKVARY(nlu)
      integer vcUZSN(nlu)
      integer vcUZSNJAN(nlu)


*************** END DECLARATIONS ***************************************


      table = 'PWAT-PARM2'
      variable = 'LZSN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcLZSN(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM2'
      variable = 'AGWR'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcAGWR(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM4'
      variable = 'IRC'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcIRC(lus(nl)),err)
        if (err.ne.0) go to 992
      end do
      
      table = 'PWAT-PARM4'
      variable = 'INTFW'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcINTFW(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM2'
      variable = 'INFILT'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcINFILT(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM3'
      variable = 'AGWETP'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcAGWETP(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM2'
      variable = 'KVARY'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcKVARY(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PWAT-PARM4'
      variable = 'UZSN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcUZSN(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'MON-UZSN'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcUZSNJAN(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      return
********************* ERROR SPACE **************************************
992   report(1) = 'problem reading parameter file'
      report(2) = ' for land use '//luname(lus(nl))
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //table//variable
      if (err.eq.3) report(3) = 
     .                      'file has line longer than 2000 characters'
      go to 999

999   call stopreport(report)

      end
