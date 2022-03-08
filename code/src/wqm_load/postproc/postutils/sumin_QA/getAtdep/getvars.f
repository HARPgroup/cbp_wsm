************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nvars, Ldsn, Lname    **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
**  if recoding for more land uses is necessary, you should only have **
**     to change the length of the variable 'lin3', the format of any **
**     statement that reads 'lin3', and the test line going to 992    **
************************************************************************
      subroutine getvars(
     I                   ioscen,nRvar,Rname,type,
     O                   nvars,dsns,names,facs)
      implicit none

      include 'data.inc'

      character*(*) type  ! pointsource, atdep, or septic
      integer lentype

********** output vars
      integer nvars(maxRvar)           ! number of vars for each Rvar
      integer dsns(maxRvar,maxDat2Rv)  ! dsns of vars
      character*4 names(maxRvar,maxDat2Rv) ! name of vars
      real facs(maxRvar,maxDat2Rv)      ! conversion factor

      integer i,l,n,nR,nm     ! indices

      integer dsnR,dsnL                   ! temp variables for reading
      character*1 cl                      ! temp character variable
      character*4 name,nameR,nameL            !     catalog files
      character*6 Tmod                  ! temp variable for modules
      character*300 lin3                  ! reading variable
      real factor

      logical found,scompare,scompcase
      external scompare,scompcase

******* END DECLARATIONS
      call lencl(type,lentype)
      call lencl(ioscen,lenioscen)
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/'//type(:lentype)
     .       //'_to_river'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nR = 1,maxRvar     ! initialize number of land variables
        nvars(nR) = 0
      end do

      read(dfile,'(a300)') lin3
      if (lin3(296:300).ne.'      ') go to 992

      do while (lin3(:3).ne.'end')
        read(lin3,1234,iostat=err)Tmod,dsnR,nameR,dsnL,nameL,factor
        if (factor.eq.0) factor = 1.0
        if (err.eq.0.and.dsnR.ne.0) then
          do nR = 1,nRvar
            if (Rname(nR).eq.nameR) then
              nvars(nR) = nvars(nR) + 1
              dsns(nR,nvars(nR)) = dsnL
              names(nR,nvars(nR)) = nameL
              facs(nR,nvars(nR)) = factor
            end if
          end do
        end if
        read(dfile,'(a300)',err=993,end=994) lin3
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

992   report(1)='Characters near the far right of file: '//fnam
      report(2)=' indicate that recoding may be necessary to allow for'
      report(3)=' more land uses ./code/src/etm/transfer_wdm/masslink.f'
      go to 999

993   report(1) = '  Problem reading file:  near line:'
      report(2) = fnam
      report(3) = lin3
      go to 999

994   report(1) = '  Problem reading file:'
      report(2) = fnam
      report(3)='file ended before literal '//char(39)//'end'//char(39)
      go to 999

999   call stopreport(report)

      end 
