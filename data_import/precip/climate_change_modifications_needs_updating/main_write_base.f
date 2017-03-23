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

      double precision ddiff,dsumU,dsumB
      integer year,month,day,hour,ny,high10(1980:2010),nv
        
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

C      call gethourdsn(uniformwdm,sdate,edate,dsn,nvals,uniformrain)

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
C          N10 = int( float(nvals) *0.10) 
C          N30 = int( float(nvals) *0.30) 
!         do iyr = sdate(1), edate(1)   ! 1984 to 2005
!           idinyr = 365
!           if(mod(iyr, 4).eq.0) idinyr=366
C            sumB = 0.
C            sumU = 0.
C            do iyh=1, nvals
C               raindyB(iyh) = baserain(iyh)
C               if(raindyB(iyh).lt.0.000001)raindyB(iyh)=0.
C               sumB = sumB + raindyB(iyh)
C               dsumB = dsumB + raindyB(iyh)
C                                                                                
C               raindyU(iyh) = uniformrain(iyh)
C               if(raindyU(iyh).lt.0.000001)raindyU(iyh)=0.
C               sumU = sumU + raindyU(iyh)
C               dsumU = dsumU + raindyU(iyh)
C            enddo
C            diff = (sumU - sumB)
C            ddiff = (dsumU - dsumB)
C            print*,lseg,',',diff,',',ddiff
C            call QSORTR(ord, nvals, raindyB)   ! only for base 
CC                sumN10 = 0.0
CC             do iyh=1, nvals
CC                if(iyh .le. N10) then   ! upper 10% from the bottom, first 
CC                TraindyB(iyh) = raindyB(ord(nvals-iyh+1))
CC                sumN10 = sumN10 + TraindyB(iyh)
CC                else                    ! others      
CC                TraindyB(iyh) = raindyB(ord(nvals-iyh+1))
CC                endif
CC             enddo
CC!               now for upper 10%    ! change with a factor
CC                ratio = 1. + diff / sumN10    !!!!!!
CC               do iyh = 1, N10
CC                TraindyB(iyh) = TraindyB(iyh) * ratio
CC               enddo
CC             do iyh = 1, nvals   ! convert to new data  ! reverse order. 
CC               newrain(ord(nvals-iyh+1)) = TraindyB(iyh)
CC             enddo
Cc            do iyd = 1, idinyr
Cc               write(*,*) iyd, baserain(iyd), raindyN(iyd)
Cc            enddo
C
C!!!         enddo  !!  iyr = sdate(1), edate(1)   ! 1984 to 2005

      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      hour = sdate(4)
C      do ny = sdate(1),edate(1)
C        high10(ny) = 0
C      end do
      do nv = 1,nvals
C        if (ord(nv).ge.nvals-N10) high10(year) = high10(year) + 1
        print*,year,',',month,',',day,',',hour,',',baserain(nv)
        call onehour(year,month,day,hour)
      end do
C      do ny = sdate(1),edate(1)
C        print*,lseg,',',ny,',',high10(ny)
C      end do



************** write to wdm
C      call puthourdsn(uniformwdm,sdate,edate,dsn,nvals,newrain)

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

