************************************************************************
**  gets the water quality output for a river                         **
**   strategy:  read the whole wdm, then assemble into bay variables  **
************************************************************************
      subroutine readriverY1Y2(rscen,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I                     nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                     nRvarOut,RdsnOut,RnameOut,
     O                     pairwq)  ! get the data
      implicit none

      include '../wqmlib/transfer_wdm.inc'

      integer year1,month1,day1,year2,month2,day2
      character*13 trseg
C      real pairwq(366,1990:2010,maxBvar)
      real pairwq(366,1985:2005,maxBvar)
      integer ny              ! year of infomation to get

      real dvaltemp(ndaymax,maxRvar)   ! daily values from river

      integer jday,jdaytot,j1,j2    ! julian day determined from hours
      real fac1,fac2

      integer i, Rvar, nv, hour, nB, nR2B  ! indices
      integer wdmfil
       
      parameter (wdmfil=dfile+3)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      integer ndaysinyear,ndays,nd
      external ndaysinyear

      integer julian
      external julian

************ END DECLARATION ******************************************
      print*,'readriver.f year1 year2 = ',year1, year2
********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = year1
      sdate(2) = month1
      sdate(3) = day1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = year2
      edate(2) = month2
      edate(3) = day2
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

C********** wind back sdate to -5   ! not for wqm, just ch3d
C      do nd = 0,-5,-1
C        call yesterday(sdate(1),sdate(2),sdate(3))
C        ndays = ndays + 1
C      end do

      do nv = 1,ndays     ! initialize temp variables
        do Rvar = 1,maxRvar  
          dvaltemp(nv,Rvar) = 0.0
        end do
      end do

************ READY TO OPEN WDM FILE
      call lencl(rscen,lenrscen)
      call lencl(trseg,lenrseg)
      wdmfnam = outwdmdir//'/river/'//
     .          rscen(:lenrscen)//'/stream/'//
     .          trseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open land read only
      if (err .ne. 0) go to 998

      do Rvar = 1,nRvarOut  ! loop over river variables

        call gethourdsn(wdmfil,sdate,edate,
     I                  RdsnOut(Rvar),
     O                  nvals,hval)                   
        jdaytot = nvals/24
        if (jdaytot.ne.ndays) go to 992
        nv = 0
        do nd = 1,jdaytot
          do i = 1,24
            nv = nv + 1
            dvaltemp(nd,Rvar) = dvaltemp(nd,Rvar) + hval(nv)
          end do
        end do    

      end do            ! end loop over river variables in wdm

      call wdflcl (wdmfil,err) 
C      if (err.ne.0) call wdflc1(wdmfil,err)  ! causes problems
      if (err.ne.0) go to 996

********** done with wdm file arrange data in pairwq
      do nB = 1,nBvar  ! initialize
        do ny = year1,year2
          do i = 1,366
            pairwq(i,ny,nB) = 0.0
          end do
        end do
      end do

      do nB = 1,nBvar  ! loop over Bvars
        do nR2B = 1,nRv2Bv(nB)  ! loop over Rvars in this Bvar
          do Rvar = 1,nRvarOut  ! look for matches
            if (Rname2Bv(nB,nR2B).eq.RnameOut(Rvar)) then ! Match!!
              print*,Rname2Bv(nB,nR2B),RnameOut(Rvar)
              nd = 0
              do ny = year1,year2
                j1 = 1
                j2 = ndaysinyear(ny)
                if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                do i = j1,j2
                  nd = nd + 1
                  pairwq(i,ny,nB) = pairwq(i,ny,nB) 
     .                            + dvaltemp(nd,Rvar) * Rfactor(nb,nR2B)
                end do
              end do

            end if
          end do
        end do
      end do

      return

********************************* ERROR SPACE **************************
992   report(1) = 'problem in wdm, difference between '
      report(2) = 'expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',jdaytot
      go to 999

996   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

998   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

