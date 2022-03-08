************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nAvar, Adsn, Aname    **
********** using the files in the pp/lib/catalogs/ directory          **
******** no need to check for active modules since this does not go   **
*********  through the watershed model                                **
************************************************************************
      subroutine masslink(
     O                    nRvar,Rdsn,Rname,nAvar,Adsn,Aname,Afactor)
      implicit none

      include 'transfer_wdm.inc'

      integer i,l,n,nR,nm     ! indices

      integer dsnR,dsnA                   ! temp variables for reading
      character*1 cl                      ! temp character variable
      character*4 name,nameR,nameA            !     catalog files
      character*6 Tmod                  ! temp variable for modules
      character*300 lin3                  ! reading variable
      real factor

      logical found,scompare,scompcase,comment
      external scompare,scompcase,comment

******** populate nRvar, Rdsn, Rname from 'rchres_in'
      fnam = catdir//'iovars/rchres_in'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvar = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          nRvar = nRvar + 1
          Rdsn(nRvar) = dsn
          Rname(nRvar) = name
        end if
        read(dfile,'(a100)') line
      end do

      close (dfile)

******* populate nAvar, Adsn, Aname, Afactor from 'land_to_river'
      fnam = catdir//'iovars/atdep_to_river'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,maxRvar     ! initialize number of land variables
        nAvar(i) = 0
      end do

      read(dfile,'(a300)') lin3
      if (lin3(296:300).ne.'      ') go to 992

      do while (lin3(:3).ne.'end')
        read(lin3,1234,iostat=err)Tmod,dsnR,nameR,dsnA,nameA,factor
        if (lin3(35:44).eq.'          ') factor = 1.0
        if (err.eq.0.and.dsnR.ne.0.and..not.comment(lin3)) then
          do nR = 1,nRvar
            if (Rdsn(nR).eq.dsnR) then
              nAvar(nR) = nAvar(nR) + 1
              Adsn(nR,nAvar(nR)) = dsnA
              Aname(nR,nAvar(nR)) = nameA
              Afactor(nR,nAvar(nR)) = factor
            end if
          end do
        end if
        read(dfile,'(a300)') lin3
      end do
              
      close (dfile)

      return

1234  format(a6,3x,i4,2x,a4,3x,i4,2x,a4,2x,f10.0)

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1)='Characters near the far right of file: land_to_river'
      report(2)=' indicate that recoding may be necessary to allow for'
      report(3)=' more land uses ./pp/etm/src/transfer_wdm/masslink.f'
      go to 999

999   call stopreport(report)

      end 
