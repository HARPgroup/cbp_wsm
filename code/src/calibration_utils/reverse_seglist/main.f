************************************************************************
** Reverses the order of segments in a segment list                   **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/rsegs.inc'
      include '../../lib/inc/lsegs.inc'

      integer ns,lenfnam

      character*13 brsegs(maxrsegs)  ! backwards segments
      character*6 blsegs(maxlsegs)  ! backwards segments

***************** END DECLARATION **************************************
      print*,'enter the name of the file that you want to reverse'
      read*,fnam
      call lencl(fnam,lenfnam)
      if (fnam(lenfnam-3:lenfnam).eq.'land') then
        fnam = fnam(:lenfnam-5)
        call readLandSeglistCurrentDir(
     I                       fnam,
     O                       lsegs,nlsegs)
        do ns = 1,nlsegs
          blsegs(ns) = lsegs(nlsegs+1-ns)
        end do
        fnam = fnam(:lenfnam-5)//'_back'
        call writeLandSeglistCurrentDir(
     I                        fnam,blsegs,nlsegs)
      else if (fnam(lenfnam-2:lenfnam).eq.'riv') then
        fnam = fnam(:lenfnam-4)
        call readRiverSeglistCurrentDir(
     I                       fnam,
     O                       rsegs,nrsegs)
        do ns = 1,nrsegs
          brsegs(ns) = rsegs(nrsegs+1-ns)
        end do
        fnam = fnam(:lenfnam-4)//'_back'
        call writeRiverSeglistCurrentDir(
     I                        fnam,brsegs,nrsegs)
      else
        print*,'problem with segment list file ',fnam(:50)
        print*,' this program only works with .land and .riv files'
      end if
      end
