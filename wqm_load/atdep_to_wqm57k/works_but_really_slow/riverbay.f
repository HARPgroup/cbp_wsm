************************************************************************
******* Routine to populate nBvar, Bname, nRv2Bv, Rname2Bv, Rfactor   **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine riverbay(
     I                    nRvar,Rname,
     O                    nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivRvar)
      implicit none

      include 'transfer_wdm.inc'

      integer i,l,n,nR,nB,nm     ! indices

      character*4 nameR,nameB,Rdiv        ! temp variables 
      real factor

      logical found,scompare,scompcase,comment
      external scompare,scompcase,comment

******* populate nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor from 'river_to_bay'
      fnam = catdir//'iovars/river_to_bay'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,maxBvar     ! initialize number of river variables
        nRv2Bv(i) = 0
      end do
      nBvar = 0

      read(dfile,'(a100)') line

      do while (line(:3).ne.'end')
        read(line,1234,iostat=err)nameB,nameR,factor,Rdiv
        if (err.eq.0.and..not.comment(line).and.line(:4).ne.'    ') then
          if (line(34:43).eq.'          ') factor = 1.0

          found = .false.    ! make sure this Rvar name is known
          do nR = 1,nRvar
            if (nameR.eq.Rname(nR)) then
              found = .true.
            end if
          end do
          if (.not.found) go to 995

          found = .false.
          do nB = 1,nBvar    ! find nB for this line
            if (nameB.eq.Bname(nB)) then
              found = .true.
              exit
            end if
          end do
          if (.not.found) then
            nBvar = nBvar + 1
            if (nBvar.gt.maxBvar) go to 993
            Bname(nBvar) = nameB
            nB = nBvar
          end if

          nRv2Bv(nB) = nRv2Bv(nB) + 1
          if (nRv2Bv(nB).gt.maxRv2Bv) go to 994
          Rname2Bv(nB,nRv2Bv(nB)) = nameR
          Rfactor(nB,nRv2Bv(nB)) = factor
          DivRvar(nB) = Rdiv
        end if
        read(dfile,'(a100)') line
      end do
              
      close (dfile)

      return

1234  format(a4,14x,a4,11x,f10.0,2x,a4)

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

993   report(1) = 'more bay variables found than expected in '
      report(2) = './pp/catalog/iovars/river_to_bay file, recode in'
      report(3) = '/wqm/p5wqm/wqm/src/atdep_to_wqm50k_calib'
      go to 999

994   report(1) = 'more river variables going to a single bay variable'
      report(2) = 'than expected, recode maxRv2Bv in'
      report(3) = '/wqm/p5wqm/wqm/src/atdep_to_wqm50k_calib'
      go to 999

995   report(1) = 'river variable '//nameR//' specified in the file'
      report(2) = './pp/catalog/iovars/river_to_bay was not found in'
      report(3) = './pp/catalog/iovars/land_to_river'
      go to 999

999   call stopreport(report)

      end 
