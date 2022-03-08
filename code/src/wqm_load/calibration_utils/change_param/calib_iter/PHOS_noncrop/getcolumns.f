************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumn1(
     I                      tkeep,vkeep,module,clu,
     O                      vcKIMP,vcKMP)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers
      character*2000 vkeep,tkeep,varline,tableline
      character*3 clu

      integer nly        ! number of soil layers
      integer vcKIMP(4)
      integer vcKMP(4)        
      integer KIMP1,KMP1   ! column of surface variable 
*************** END DECLARATIONS ***************************************

********* FIND THE COLUMEs OF EACH VARIABLE AT ALL FOUR LAYERS
      table = 'PHOS-FSTPM#1'
      variable = 'KIMP'
       
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                KIMP1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKIMP(nly) = KIMP1 + (nly-1)*4         ! get the column for all four layers                 !
      end do

      table = 'PHOS-FSTPM#1'
      variable = 'KMP'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                KMP1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKMP(nly) = KMP1 + (nly-1)*4         ! get the column for all four layers                 ! 
      end do 
       
        
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


************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumn2(
     I                      tkeep,vkeep,module,clu,
     O                      vcXFIX,vcK1)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers
      character*2000 vkeep,tkeep,varline,tableline
      character*3 clu     

      integer nly        ! number of soil layers
      integer vcXFIX(4)
      integer vcK1(4)
      integer XFIX1,K11   ! column of surface variable

*************** END DECLARATIONS ***************************************
      table = 'PHOS-SVALPM#1'
      variable = 'XFIX'

      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                XFIX1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcXFIX(nly) = XFIX1 + (nly-1)*3         ! get the column for all four layers                 !
      end do

      table = 'PHOS-SVALPM#1'
      variable = 'K1'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                K11,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcK1(nly) = K11 + (nly-1)*3         ! get the column for all four layers                 !
      end do


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


************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumn5(
     I                      tkeep,vkeep,module,clu,
     O                      vcORGP,vcP4AD)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*12 module,table,variable  ! variable specifiers
      character*2000 vkeep,tkeep,varline,tableline
      character*3 clu

      integer nly        ! number of soil layers
      integer vcORGP(4)
      integer vcP4AD(4)
      integer orp1,p4a1   ! column of surface variable

*************** END DECLARATIONS ***************************************
      table = 'PHOS-STOR1#1'
      variable = 'ORGP'

      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                orp1,err)
      if (err.ne.0) go to 992
      
      do nly = 1, 4
        vcORGP(nly) = orp1 + (nly-1)*4         ! get the column for all four layers                 !
      end do

      table = 'PHOS-STOR1#1'
      variable = 'P4AD'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                p4a1,err)
      if (err.ne.0) go to 992
      
      do nly = 1, 4
        vcP4AD(nly) = p4a1 + (nly-1)*4         ! get the column for all four layers                 !
      end do


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

