************************************************************************
** fuction to find downtream transport factor between two stations    **
************************************************************************
      function stationdf(upstation,downstation,
     I                   attenuation,rsegs,maxrsegs,
     I                   upsegs,nupsegs)
      implicit none
      include '../../lib/inc/standard.inc'
      real stationdf

      integer maxrsegs
      character*13 upstation,downstation,rsegs(maxrsegs)
      character*13 currentseg,lastseg
      real attenuation(maxrsegs)
      integer upsegs(maxrsegs),nupsegs

      integer nr

      logical done,found

************* set first seg
      currentseg = 'noseg'
      if (upstation(10:13).eq.'0003') then ! current seg is upseg
        do nr = 1,nupsegs
          if (upstation(5:8).eq.rsegs(upsegs(nr))(5:8)) then
            currentseg = rsegs(upsegs(nr))
            stationdf = attenuation(upsegs(nr))
            exit
          end if
        end do
      else ! current seg is next downstream seg
        do nr = 1,nupsegs
          if (upstation(10:13).eq.rsegs(upsegs(nr))(5:8)) then
            currentseg = rsegs(upsegs(nr))
            stationdf = attenuation(upsegs(nr))
            exit
          end if
        end do
      end if
      if (currentseg.eq.'noseg') go to 991

************ march downstream
      done = .false.
      if (currentseg(5:8).eq.downstation(5:8)) done = .true.
      do while (.not.done)
        found = .false.
        if (currentseg(10:13).eq.downstation(5:8) .and.
     .      downstation(10:13).eq.'0003') then
          done = .true.
        else
          do nr = 1,nupsegs
            if (currentseg(10:13).eq.rsegs(upsegs(nr))(5:8)) then
              currentseg = rsegs(upsegs(nr))
              stationdf = stationdf*attenuation(upsegs(nr))
              found = .true.
              if (currentseg(5:8).eq.downstation(5:8)) done = .true.
              exit 
            end if
          end do
          if (.not.found) go to 992
        end if
      end do

      return
******************* ERROR SPACE ****************************************
991   report(1) = 'calibration station not found in seglist'
      report(2) = 'calibration station = '//upstation
      report(3) = ' function stationdf'
      go to 999

992   report(1) = 'downstream segment not found in seglist'
      report(2) = 'current segment = '//currentseg
      report(3) = ' function stationdf'

999   call stopreport(report)

      end


      
      

