************************************************************************
**  program to read in a segment list and split into as many equal    **
**   parts as requested                                               **
************************************************************************
      subroutine splitriv(fnam,last,splits)
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/rsegs.inc'

      character*100 basin  ! basin name without the suffix
      character*100 splitfnam ! name for the split file

      integer splits  ! number of splits
      integer basenumber  ! number of segments in all splits
      integer extras    ! number of extras
      integer insplit  ! number in this split

      integer nseg,ns        ! indices for segments
      integer nsplit      ! index for splits

      character*2 csplit  ! charactere representation of nsplits

      character*13 splitsegs(maxrsegs) ! list of segments in split file

************ END DECLARATIONS ******************************************

      basin = fnam(:last-4)
      call readRiverSeglist(
     I                     basin,
     O                     rsegs,nrsegs)

      basenumber = nrsegs/splits
      extras = mod(nrsegs,splits)
      nseg = 0
      do nsplit = 1,splits

        insplit = basenumber    ! determine number in file
        if (nsplit.le.extras) insplit = insplit + 1

        do ns = 1,insplit   ! populate splitsegs
          nseg  = nseg + 1
          splitsegs(ns) = rsegs(nseg)
        end do

        write(csplit,'(i2)') nsplit  ! create filename
        if (csplit(1:1).eq.' ') csplit(1:1) = '0'
        splitfnam = basin(:last-4)//csplit

        call writeRiverSeglist(
     I                        splitfnam,
     O                        splitsegs,insplit)
      end do

      stop

******************* ERROR SPACE ****************************************
999   call stopreport(report)
      end
