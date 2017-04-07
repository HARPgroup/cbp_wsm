************************************************************************
** finds column numbers for the calibrated variables                  **
************************************************************************
      subroutine getcolumns1(
     I                       tkeep,vkeep,module,clu,
     O                       vcKIMNI,vcKAM,vcKDNI,vcKNI,vcKIMAM,
     O                       vcKLON,vcKRON,vcSKVOL,vcUKVOL)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*3 clu  ! character land use
      character*12 module,table,variable  ! variable specifiers
      character*2000 vkeep,tkeep,varline,tableline

      integer nly        ! number of soil layers
      integer vcKIMNI(4),vcKAM(4),vcKDNI(4),vcKNI(4)
      integer vcKIMAM(4),vcKLON(4),vcKRON(4)
      integer vcSKVOL,vcUKVOL

      integer KIMNI1,KAM1,KDNI1,KNI1   ! column of surface variable 
      integer KIMAM1,KLON1,KRON1
    
*************** END DECLARATIONS ***************************************

********* FIND THE COLUMEs OF EACH PARAMETER AT ALL FOUR LAYERS
      table = 'NIT-FSTPM#1'
      variable = 'KIMNI'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                KIMNI1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKIMNI(nly) = KIMNI1 + (nly-1)*7         ! get the column for all four layers                 !
      end do

      varline = vkeep
      tableline = tkeep
      table = 'NIT-FSTPM#1'
      variable = 'KAM'
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                KAM1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKAM(nly) = KAM1 + (nly-1)*7         ! get the column for all four layers                 ! 
      end do 
 
      varline = vkeep
      tableline = tkeep
      table = 'NIT-FSTPM#1'
      variable = 'KDNI'
      call findcolumn(tableline,varline,table,variable,
     O                KDNI1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKDNI(nly) = KDNI1 + (nly-1)*7         ! get the column for all four layers                 !
      end do
  
      varline = vkeep
      tableline = tkeep
      table = 'NIT-FSTPM#1'
      variable = 'KNI'
      call findcolumn(tableline,varline,table,variable,
     O                KNI1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKNI(nly) = KNI1 + (nly-1)*7         ! get the column for all four layers                 !
      end do
 
      varline = vkeep
      tableline = tkeep
      table = 'NIT-FSTPM#1'
      variable = 'KIMAM'
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                KIMAM1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKIMAM(nly) = KIMAM1 + (nly-1)*7         ! get the column for all four layers                 !
      end do

      varline = vkeep
      tableline = tkeep
      table = 'NIT-ORGPM#1'
      variable = 'KLON'
      call findcolumn(tableline,varline,table,variable,
     O                KLON1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKLON(nly) = KLON1 + (nly-1)*4         ! get the column for all four layers                 !
      end do
 
      varline = vkeep
      tableline = tkeep
      table = 'NIT-ORGPM#1'
      variable = 'KRON'
      call findcolumn(tableline,varline,table,variable,
     O                KRON1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcKRON(nly) = KRON1 + (nly-1)*4         ! get the column for all four layers                 !
      end do

      varline = vkeep
      tableline = tkeep
      table = 'NIT-AMVOLAT'
      variable = 'SKVOL'
      call findcolumn(tableline,varline,table,variable,
     O                vcSKVOL,err)
      if (err.ne.0) go to 992
 
      varline = vkeep
      tableline = tkeep  
      table = 'NIT-AMVOLAT'
      variable = 'UKVOL'
      call findcolumn(tableline,varline,table,variable,
     O                vcUKVOL,err)
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


********************************************************************
** finds column numbers for uptake rate                         ****
********************************************************************
       subroutine getcolumns3(
     I                        tkeep,vkeep,module,clu,
     O                        vcNITUPT1,vcNITUPT2,vcNITUPT3)

       implicit none
       include '../../../../lib/inc/standard.inc'
       include '../../../../lib/inc/land_use.inc'

       character*3 clu  ! character land use
       character*12 module,table,variable  ! variable specifiers
       character*2000 vkeep,tkeep,varline,tableline

       integer nm         ! month
       integer vcNITUPT1(12),vcNITUPT2(12),vcNITUPT3(12)
       integer jan1,jan2,jan3

*************** END DECLARATIONS ***************************************

       varline = vkeep
       tableline = tkeep
       table = 'MON-NITUPT#1'
       variable = 'JAN'
       call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                  jan1,err)
       if (err.ne.0) go to 992
       do nm = 1, 12
         vcNITUPT1(nm) = jan1 + (nm-1)         ! get the column for all four layers                 !
       end do

       varline = vkeep
       tableline = tkeep
       table = 'MON-NITUPT#2'
       variable = 'JAN'
       call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                 jan2,err)
       if (err.ne.0) go to 992
       do nm = 1, 12
         vcNITUPT2(nm) = jan2 + (nm-1)         ! get the column for all four layers                 !
       end do

       varline = vkeep
       tableline = tkeep
       table = 'MON-NITUPT#3'
       variable = 'JAN'
       call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                 jan3,err)
       if (err.ne.0) go to 992
       do nm = 1, 12
         vcNITUPT3(nm) = jan3 + (nm-1)         ! get the column for all four layers                 !
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


********************************************************************
** finds column numbers for the initial stoarge                 ****
********************************************************************
      subroutine getcolumns8(
     I                       tkeep,vkeep,module,clu,
     O                       vcORGN,vcRORGN)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*3 clu
      character*12 module,table,variable  ! variable specifiers
      character*2000 vkeep,tkeep,varline,tableline

      integer nly        ! number of soil layers
      integer vcORGN(4),vcRORGN(4)
      integer ORGN1,RORGN1

*************** END DECLARATIONS ***************************************

      varline = vkeep
      tableline = tkeep
      table = 'NIT-STOR1#1'
      variable = 'ORGN'
      call findcolumn(tableline,varline,table,variable,        ! find the column of surface layer
     O                ORGN1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcORGN(nly) = ORGN1 + (nly-1)*6         ! get the column for all four layers                 !
      end do

      varline = vkeep
      tableline = tkeep
      table = 'NIT-STOR1#1'
      variable = 'RORGN'
      call findcolumn(tableline,varline,table,variable,
     O                RORGN1,err)
      if (err.ne.0) go to 992
      do nly = 1, 4
        vcRORGN(nly) = RORGN1 + (nly-1)*6         ! get the column for all four layers                 !
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

