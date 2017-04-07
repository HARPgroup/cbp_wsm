************************************************************************
** finds the value of tau at 'value' percentile                       **
************************************************************************
      function findtaupercent(Tseg,paramscen,value)
      implicit none

      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'

      real findtaupercent   ! value of tau at 'value' percentile

      real value     ! percentile to find

      integer upindex,downindex  ! indices

      integer maxpcnt,npcnt,np    ! number of percentiles in file
      parameter (maxpcnt=100)
      real percentile(maxpcnt)    ! percentiles
      real pval(maxpcnt)          ! values of tau at that percentile

      real interpolate
      external interpolate

      integer ifl

*************** END DECLARATIONS **************************************

******** OPEN FILE AND READ HEADERS
      call findopen(ifl)
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/tau_percentiles.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(ifl,*,err=992,end=992) npcnt
      read(ifl,*,err=992,end=992) rseg,(percentile(np),np=1,npcnt)
      do np = 1,npcnt-1
        if (percentile(np).le.percentile(np+1)) go to 995
      end do

********* LOOP OVER SEGEMENTS UNTIL YOU FIND THE ONE YOU WANT
      do 
        read(ifl,*,err=993,end=994)rseg
        if (Tseg.eq.rseg) then
          backspace ifl
          read(ifl,*,err=993,end=994)rseg,(pval(np),np=1,npcnt)
          exit
        end if
      end do

      close(ifl)

******** INTERPOLATE TO FIND THE CORRECT PERCENTILE
      upindex = -1
      downindex = -1
      do np = 1,npcnt   ! get upper and lower point
        if (value.le.percentile(np)) upindex = np
      end do
      do np = npcnt,1,-1
        if (value.ge.percentile(np)) downindex = np
      end do

      if (upindex.eq.downindex) then    ! value is in the table no interp
        if (upindex.eq.-1) go to 996
        findtaupercent = pval(upindex)
      else 
        if (upindex.eq.-1) then        ! asking for above max, OK
          upindex = 1
          downindex = 2
        else if (downindex.eq.-1) then  ! asking for below min, OK
          upindex = npcnt-1
          downindex = npcnt
        else                           ! regular interpolation
           if (downindex-upindex.ne.1) then  !  should be 1
             if (downindex-upindex.eq.2) then !  2 because of inexact math
               if (abs(percentile(upindex)-value).lt.
     .             abs(percentile(downindex)-value)) then
                 downindex = downindex - 1
               else
                 upindex = upindex + 1
               end if
             else
               go to 996
             end if
           end if  ! end error checking
         end if
         findtaupercent=interpolate(value,
     .                            percentile(downindex),pval(downindex),
     .                            percentile(upindex),pval(upindex))
       end if

       if (findtaupercent.lt.1e-9) findtaupercent=1e-9

       return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading first two lines in file:'
      report(2) = fnam
      report(3) = 'line 1: number of percentiles, line2: percentiles'
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error near segment '//Tseg
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'found EOF before finding segment '//Tseg
      go to 999

995   report(1) = 'problem with file'
      report(2) = fnam
      report(3) = 'percentiles must be sorted in decreasing order '//
     .            'left to right'
      go to 999

996   report(1) = Tseg//' '//fnam
      report(2) = 'problem calculating percentiles check code'
      report(3) = './code/src/calibration_utils/change_param/'//
     .            'calib_iter_WQ_simulataneous_river/shear_stress.f'
      go to 999

999   call stopreport(report)
      end
************************************************************************
** finds the percentile of a particular value of tau                  **
************************************************************************
      function findpercenttau(Tseg,paramscen,tau)
      implicit none

      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'

      real findpercenttau   ! value of tau at 'value' percentile

      real tau     ! tau to get percentile of

      integer upindex,downindex  ! indices

      integer maxpcnt,npcnt,np    ! number of percentiles in file
      parameter (maxpcnt=100)
      real percentile(maxpcnt)    ! percentiles
      real pval(maxpcnt)          ! values of tau at that percentile

      real interpolate
      external interpolate

      integer ifl

*************** END DECLARATIONS **************************************

******** OPEN FILE AND READ HEADERS
      call findopen(ifl)
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/tau_percentiles.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(ifl,*,err=992,end=992) npcnt
      read(ifl,*,err=992,end=992) rseg,(percentile(np),np=1,npcnt)
      do np = 1,npcnt-1
        if (percentile(np).le.percentile(np+1)) go to 995
      end do

********* LOOP OVER SEGEMENTS UNTIL YOU FIND THE ONE YOU WANT
      do 
        read(ifl,*,err=993,end=994)rseg
        if (Tseg.eq.rseg) then
          backspace ifl
          read(ifl,*,err=993,end=994)rseg,(pval(np),np=1,npcnt)
          exit
        end if
      end do

      close(ifl)

******** INTERPOLATE TO FIND THE CORRECT PERCENTILE
      upindex = -1
      downindex = -1
      do np = 1,npcnt   ! get upper and lower point
        if (tau.le.pval(np)) upindex = np
      end do
      do np = npcnt,1,-1
        if (tau.ge.pval(np)) downindex = np
      end do

      if (upindex.eq.downindex) then    ! tau is in the table no interp
        if (upindex.eq.-1) go to 996
        findpercenttau = percentile(upindex)
      else 
        if (upindex.eq.-1) then        ! asking for above max, OK
          upindex = 1
          downindex = 2
        else if (downindex.eq.-1) then  ! asking for below min, OK
          upindex = npcnt-1
          downindex = npcnt
        else                           ! regular interpolation
           if (downindex-upindex.ne.1) then  !  should be 1
             if (downindex-upindex.eq.2) then !  2 because of inexact math
               if (abs(pval(upindex)-tau).lt.
     .             abs(pval(downindex)-tau)) then
                 downindex = downindex - 1
               else
                 upindex = upindex + 1
               end if
             else if (downindex-upindex.lt.0) then ! same value
               if (abs(pval(upindex)-pval(downindex)).gt.0.0001) then
                 go to 996
               end if
             else
               go to 996
             end if
           end if  ! end error checking
         end if
         findpercenttau=interpolate(tau,
     .                            pval(downindex),percentile(downindex),
     .                            pval(upindex),percentile(upindex))
       end if

       if (findpercenttau.lt.1e-9) findpercenttau=1e-9

       return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading first two lines in file:'
      report(2) = fnam
      report(3) = 'line 1: number of percentiles, line2: percentiles'
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error near segment '//Tseg
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'found EOF before finding segment '//Tseg
      go to 999

995   report(1) = 'problem with file'
      report(2) = fnam
      report(3) = 'percentiles must be sorted in decreasing order '//
     .            'left to right'
      go to 999

996   report(1) = Tseg//' '//fnam
      report(2) = 'problem calculating percentiles check code'
      report(3) = './code/src/calibration_utils/change_param/'//
     .            'calib_iter_WQ_simulataneous_river/shear_stress.f'
      go to 999

999   call stopreport(report)
      end
