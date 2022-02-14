      subroutine getCSOPassThru(passthrufnam,tfactors)

      implicit none

      include 'CSOPassThru.inc'
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*50  passthrufnam
      character*200 longline

      integer       tcbcsoid
      character*13  tlrseg
      character*25  tnpdes
      real    tpassthru1,tpassthru2,tpassthru3,tholdthresh
      real    tfactors(SCBCSOID:ECBCSOID,4)

      integer i
      do i = SCBCSOID,ECBCSOID
         tfactors(i,1) = 1.0
         tfactors(i,2) = 1.0
         tfactors(i,3) = 1.0
         tfactors(i,4) = 0.0
      end do

      open(dfile+2,file=passthrufnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile+2,'(a200)')longline  ! skip header row
      do
        read(dfile+2,'(a200)',end=224,err=996) longline
        call d2x(longline,last)
        read(longline,*,end=992,err=992)
     .      tcbcsoid,tlrseg,tnpdes,
     .      tpassthru1,tpassthru2,tpassthru3,tholdthresh
        tfactors(tcbcsoid,1) = tpassthru1
        tfactors(tcbcsoid,2) = tpassthru2
        tfactors(tcbcsoid,3) = tpassthru3
        tfactors(tcbcsoid,4) = tholdthresh
c        print*,tcbcsoid,tlrseg,tnpdes,tpassthru1,tpassthru2,tholdthresh
      end do

224   close(dfile+2)

      return

991   report(1) = 'could not open file'
      report(2) = passthrufnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem reading file'
      report(2) = passthrufnam
      report(3) = '>'//longline//'<'
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = passthrufnam
      report(3) = longline
      go to 999

999   call stopreport(report)

      end
