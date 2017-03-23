************************************************************************
** subroutine to read observed quick flow files and derive interflow  **
**  recession constants for each station                              **
** interflow is quick flow that comes more than a day after a storm   **
**  peak, so we should regress day t+1 vs day t on days more than one **
**  day after a peak                                                  **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      integer file1          ! file number

      integer year,month,day,hour,zero
      real q,qp1,qm1  ! flow, flow tomorrow, flow yesterday

      integer maxobs,nobs
      parameter (maxobs=5000)
      real qt(maxobs),qtp1(maxobs)

      real slope,correl

********** END DECLARATIONS  *******************************************
      read*,rseg

      call findopen(file1)           ! open pltgen

      call lencl(rseg,lenrseg)

      fnam = calibdir//'observed/qflw/'//rseg(:lenrseg)//'.OQFLW'

      open(file1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(file1,*,err=994,end=994) year,month,day,hour,zero,qp1 ! day1

      q = qp1
      read(file1,*,err=994,end=994) year,month,day,hour,zero,qp1 ! day2

      nobs = 0
      do 
        qm1 = q
        q = qp1
        read(file1,*,err=994,end=111) year,month,day,hour,zero,qp1

        if (q.lt.qm1) then  ! not peak
          if (qp1.lt.q) then  ! still decending
            if (qp1.gt. 0.1) then  ! not at bottom
              nobs = nobs + 1
              if (nobs.gt.maxobs) go to 992
              qt(nobs) = q
              qtp1(nobs) = qp1
            end if
          end if
        end if

      end do

111   close(file1)

      call noIregress(qt,qtp1,nobs,maxobs,
     O                slope,correl,err)

      print*,rseg,slope,correl
 
      stop

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'too many observations in file'
      report(2) = 'increase parameter maxobs in'
      report(3) = './pp/src/calibration_utils/a_priori_irc'
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,zero,qp1
      go to 999

999   call stopreport(report)
      end

