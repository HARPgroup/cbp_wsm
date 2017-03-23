************************************************************************
**  creates a new rainfall wdm from two existing wdms                 **
**   finds the multiplier between a base wdm and a uniform multipler  **
**   wdm and writes a 'flash 10' time series back to the uniform wdm  **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/wdm.inc'
      integer newwdm,basewdm,uniformwdm
      parameter (basewdm=dfile+11)
      parameter (uniformwdm=dfile+12)

      character*64 bfnam,ufnam

      real baserain(ndaymax*24)
      real uniformrain(ndaymax*24)
      real newrain(ndaymax*24)

      integer sdate(ndate),edate(ndate)
      data sdate /1984,01,01,00,0,0/
c     data edate /2005,12,31,24,0,0/
      data edate /2000,12,31,24,0,0/

c      real raindyB(366*24), raindyU(366*24), raindyN(366*24) !if yearly modify
       real raindyB(ndaymax*24), raindyU(ndaymax*24) ! ,raindyN(ndaymax*24)
         integer iy, idinyr, iyh, N10, N30 ! idyiny
         integer ord(ndaymax*24)    ! , nval 
         real    sumB, sumU, difF, sumN10, ratio
         real  TraindyB(ndaymax*24) 

************* open new, base, and uniform wdms
      read(*,'(a)') lseg

      print*,lseg

      bfnam = 'base_'//lseg(:6)//'.wdm'
      call wdbopn(basewdm,bfnam,0,err)
      if (err.ne.0) go to 991

      ufnam = 'uniform_'//lseg(:6)//'.wdm'
      call wdbopn(uniformwdm,ufnam,0,err)
      if (err.ne.0) go to 992

      tcode = 3    ! hourly data

      dsn = 2000   ! precip dsn

************ get base and uniform time series
      call gethourdsn(basewdm,sdate,edate,dsn,nvals,baserain)

      call gethourdsn(uniformwdm,sdate,edate,dsn,nvals,uniformrain)

*********** create new time series
******** create newrain from baserain and uniformrain
****** all three variables above are indexed 1 to nvals
******** PING, PUT THE CODE IN HERE
******* USE THE FUNCTION QSORTR TO SORT AN INDEX
******** VIEW THE SUBROUTINE IN /model/p517/pp/src/lib/util/sort.f
c            nvals = 0 
c         do iyh = edate(1), sdate(1)
c           idinyr = 365
c           if(mod(iyh, 4).eq.0) idinyr=366
c            nvals = nvals + idinyr     !! total ndays 
c         enddo   
c            nvals = nvals * 24         !! hourly data 
!c          N10 = int( float(nvals) *0.10) 
!c          N30 = int( float(nvals) *0.30) 
!         do iyr = sdate(1), edate(1)   ! 1984 to 2000  ! 2005
!           idinyr = 365
!           if(mod(iyr, 4).eq.0) idinyr=366
!           iyhr = idinyr*24     ! hours in A year, diff in leap&noleap
!           do iyh = 1, iyhr     ! individual year 
            sumB = 0.
            sumU = 0.
!           do iyh = 1, iyhr     ! individual year 
               N10 = 0 
            do iyh=1, nvals
               raindyB(iyh) = baserain(iyh)
               if(raindyB(iyh).lt.0.000001)raindyB(iyh)=0.
               sumB = sumB + raindyB(iyh)                            
                  if(raindyB(iyh).gt. 0.00001) then  ! doesn't matter the order
                    N10 = N10 + 1                    ! only non zero
                  endif 
               raindyU(iyh) = uniformrain(iyh)
               if(raindyU(iyh).lt.0.000001)raindyU(iyh)=0.
               sumU = sumU + raindyU(iyh)
            enddo
              N10 = int(N10 / 10)  
            diff = (sumU - sumB)
            call QSORTR(ord, nvals, raindyB)   ! only for base 
                 
                sumN10 = 0.0
             do iyh=1, nvals
                TraindyB(iyh) = raindyB(ord(nvals-iyh+1))
                if(iyh .le. N10) then   ! upper 10% from the bottom, first 
                 sumN10 = sumN10 + TraindyB(iyh)
                else                    ! others      
                endif
             enddo
!               now for upper 10%    ! change with a factor
                ratio = 1. + diff / sumN10    !!!!!!
               do iyh = 1, N10
                TraindyB(iyh) = TraindyB(iyh) * ratio
               enddo
             do iyh = 1, nvals   ! convert to new data  ! reverse order. 
               newrain(ord(nvals-iyh+1)) = TraindyB(iyh)
             enddo
c            do iyd = 1, idinyr
c               write(*,*) iyd, baserain(iyd), raindyN(iyd)
c            enddo

!!!         enddo  !!  iyr = sdate(1), edate(1)   ! 1984 to 2000 ! 2005 


************** write to wdm
      call puthourdsn(uniformwdm,sdate,edate,dsn,nvals,newrain)

      return

********************************* ERROR SPACE **************************
991   report(1) = 'could not open file'
      report(2) = bfnam
      report(3) = ' '
      go to 999

992   report(1) = 'could not open file'
      report(2) = ufnam
      report(3) = ' '
      go to 999

999   call stopreport(report)


      end

