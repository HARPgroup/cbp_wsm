      subroutine getSPARROWfacCOMID(sparrowfnam,tfactors)

      implicit none

      include 'SPARROW.inc'
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*50  sparrowfnam
      character*200 longline
      integer tcomidcb,tcomidnhd,tcomidorg
      real    tflo,ttnx,ttpx,tsed
      real    tfactors(SCOMIDCB:ECOMIDCB,4)

      integer i
      do i = SCOMIDCB,ECOMIDCB
         tfactors(i,1) = 1.0
         tfactors(i,2) = 1.0
         tfactors(i,3) = 1.0
         tfactors(i,4) = 1.0
      end do

      open(dfile+2,file=sparrowfnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile+2,'(a200)')longline  ! skip header row
      do
        read(dfile+2,'(a200)',end=224,err=996) longline
        call d2x(longline,last)
        read(longline,*,end=992,err=992)
     .      tcomidcb,tcomidnhd,tcomidorg,tflo,ttnx,ttpx,tsed
        tfactors(tcomidcb,1) = tflo
        tfactors(tcomidcb,2) = ttnx
        tfactors(tcomidcb,3) = ttpx
        tfactors(tcomidcb,4) = tsed
c        print*,tcomidcb,tflo,ttnx,ttpx,tsed
      end do

224   close(dfile+2)

      return

991   report(1) = 'could not open file'
      report(2) = sparrowfnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem reading file'
      report(2) = sparrowfnam
      report(3) = '>'//longline//'<'
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = sparrowfnam
      report(3) = longline
      go to 999

999   call stopreport(report)

      end
