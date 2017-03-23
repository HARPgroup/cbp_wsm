************************************************************************
** gets facility information from a standard file                     **
************************************************************************
      subroutine getCSOfacs(
     I                      maxfacs,psscen,lenpsscen,
     O                      facility,CSOfac,nfacs)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*35 psscen            ! ps data scenario for wdm location
      integer lenpsscen
      integer maxfacs,nfacs

      character*10 facility(maxfacs)
      real CSOfac(maxfacs)

      logical found

***************** END DECLARATIONS *************************************
      fnam = tree//'input/unformatted/point_source/'//
     .             psscen(:lenpsscen)//'/CSO_reduction_factors.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nfacs = 1
      read(dfile,*,err=992,end=992) facility(nfacs)  ! ditch header
      do
        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
        read(line,*,err=992,end=992) facility(nfacs),CSOfac(nfacs)
        nfacs = nfacs + 1
        if (nfacs.gt.maxfacs) go to 993
      end do
111   nfacs = nfacs - 1
      close(dfile)

      return
************************* ERROR SPACE **********************************
991   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file:  near line:'
      report(2) = fnam
      report(3) = facility(nfacs-1)
      go to 999

993   report(1) = 'too many facilities specified in file:'
      report(2) = fnam
      report(3) = 'adjust maxfacs variable in point source program'
      go to 999

999   call stopreport(report)

      end

