 
      implicit none
      include 'sumsumstats.inc'

      logical found

      integer j,lenscen

************ END DECLARATIONS ******************

*********** OPEN AND READ FILE ************************
      read*,scenario
      call lencl(scenario,lenscen)

      i = 0
      do 
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
        fnam = scenario(:lenscen)//'_sum_stats_'//version//'.csv'
        inquire(file=fnam,exist=found)
        if (.not.found) exit
        call onefile(fnam,version,scenario)
        i = i + 1
      end do

      write(version,'(i4)') i
      do j = 1,4
        if (version(j:j).eq.' ') version(j:j) = '0'
      end do
      fnam = scenario(:lenscen)//'_sum_stats.csv'
      call onefile(fnam,version,scenario)

      stop
 
      end
