************************************************************************
** finds column numbers for the calibrated variables                  **
**  for temperature, just finds the column for the January value since**
**  all variables are monthly except for LGTMP
************************************************************************
      subroutine allcolumns( 
     I       tkeep,vkeep,lus,numlu,module,
     O       vcASLT,vcULTP1,vcLGTP1,vcBSLT,vcULTP2,vcLGTMP)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers

      integer numlu,nl  ! number of land uses to act on
      integer lus(numlu) ! index of land uses to act on

      character*2000 vkeep(nlu),tkeep(nlu),varline,tableline

      integer vcASLT(nlu),vcULTP1(nlu),vcLGTP1(nlu),
     .        vcBSLT(nlu),vcULTP2(nlu),vcLGTMP(nlu)


*************** END DECLARATIONS ***************************************


      table = 'MON-ASLT'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcASLT(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'MON-BSLT'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcBSLT(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'MON-ULTP1'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcULTP1(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'MON-ULTP2'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcULTP2(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'MON-LGTP1'
      variable = 'JAN'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcLGTP1(lus(nl)),err)
        if (err.ne.0) go to 992
      end do

      table = 'PSTEMP-TEMPS'
      variable = 'LGTMP'
      do nl = 1,numlu
        varline = vkeep(lus(nl))
        tableline = tkeep(lus(nl))
        call findcolumn(tableline,varline,table,variable,
     O                  vcLGTMP(lus(nl)),err)
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
