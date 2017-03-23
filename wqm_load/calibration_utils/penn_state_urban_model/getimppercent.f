************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine getimpperncent(lseg,
     .                 LBJday,LBdata,
     .                 T1year,T1month,T1day,
     .                 T2year,T2month,T2day,
     .                 nLB,
     .                 imppercent)
      
      implicit none

      include 'psu.inc'

      real lufac(nlu)
      logical lastindx
      integer i,k,l,j,indx1,indx2
      integer nLB,ndays
      real denom,lfactor(nlu)

      integer julian
      external julian

      ndays = julian(T1year,T1month,T1day,T2year,T2month,T2day)

********* interpolate land use
********** formula for interpolation is break1 + (break2-break1)
**********                       /(day2-day1) * (currentday - day1)
***********   this holds true up to day2
      if (nLB.eq.1) then      ! create second point for interpolation
        nLB = 2
        LBJday(2) = ndays
        if (ndays-LBJday(1).le.300) LBJday(2) = LBJday(1) + 1000
        do l = 1,nlu
          do i = 1,numsegs
            LBdata(2,i,l) = LBdata(1,i,l)
          end do
        end do
      end if

      j = 1                        ! interpolate land use
      indx1 = 1
      indx2 = 2
      lastindx = .false.
      do while (j.le.ndays)

        denom = real(LBJday(indx2) - LBJday(indx1))
        do l = 1,nlu
          lfactor(l) = (LBdata(indx2,i,l)-LBdata(indx1,i,l))/denom
        end do
        do while ((j.le.LBJday(indx2).or.lastindx).and.j.le.ndays)
          do l = 1,nlu
            lufac(l) = LBdata(indx1,i,l) + 
     .                   lfactor(l)*real(j-LBJday(indx1))
            if (lufac(l).lt. -0.1) go to 991
          end do
          if (lufac(lpur).gt.0.1) then
            imppercent(j) = lufac(limp)/lufac(lpur)
          else 
            imppercent(j) = -9
          end if
          j = j + 1
        end do

        if (indx2.eq.nLB) then
          lastindx = .true.
        else
          indx2 = indx2+1
          indx1 = indx1+1
        end if

      end do

      return
*********** ERROR SPACE
991   report(1) = 'problem with Land use data, '
      report(2) = '   land segment '//l2r(i)//' River '//rseg
      report(3) = '   day 99999, land use '//luname(l)//'  value '
      write(report(3)(8:12),'(i5)') j
      write(report(3)(35:44),'(f10.1)') lufac(j,l)
      go to 999

999   call stopreport(report)

      end
