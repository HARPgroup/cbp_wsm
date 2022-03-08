************************************************************************
** Program to populate eos wdms with river input variables            **
**  wdms are stored in ./wdm/river/$scen/eos/$seg.wdm                 **
**    general strategy:                                               **
**      Run the program once for each river or bfl subseg             **
**      For each land segment in the river                            **
**         for each land use in the segment                           **
**           determine the time series of river input by:             **
**             extracting relevant dsns                               **
**             multiply by the etm binary files                       **
**           add to the appropriate river variable                    **
**      sum over all segments                                         **
**      store in river wdm                                            **
**      write appropriate output files (eos, wqm)                     **
************************************************************************
      subroutine readdailydat(
     I                   year1,month1,day1,year2,month2,day2,
     I                   wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I                   nDvar,Ddsn,Dname,Dfac,
     O                   pairq,pairH)
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/masslinks.inc'
      include '../lib/inc/wdm.inc'

********* input variables
      integer maxDat2Rv

************ input variables specifiers
      integer nDvar(maxRvar),Dvar      ! number of PSvars for each Rvar
      integer Ddsn(maxRvar,maxDat2Rv)  ! dsns of PSvars
      character*4 Dname(maxRvar,maxDat2Rv) ! name of PSvars
      real Dfac(maxRvar,maxDat2Rv)      ! conversion factor


      integer year1,month1,day1,year2,month2,day2
      integer ny              ! year of infomation to get

      real pairq(-5:366,year1:year2)
      real pairH(-5:366,year1:year2)

      real dvaltemp(ndaymax,maxRvar)   ! hourly values to river

      character*25 LStest             ! test the etm file

      integer jday,jdaytot,j1,j2   ! julian day determined from hours
      integer jdcorrect  ! correction between start time of etm
                         ! file and time series from wdm

      real fac1,fac2

      integer i, l, Rvar, Lvar, nv, nlconi, nb, nbi, hour  ! indices
      integer wdmlnd
       
      parameter (wdmlnd=dfile+1)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      logical dothislu                ! decide to use this land use

      integer ndaysinyear,ndays,nd
      external ndaysinyear

      integer julian
      external julian

      logical found

      integer temp1y,temp1m,temp1d  ! start time of etm file
      integer temp2y,temp2m,temp2d  ! stop time of etm file


************ END DECLARATION ******************************************


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

********** wind back sdate to -5
      do nd = 0,-5,-1
        call yesterday(sdate(1),sdate(2),sdate(3))
        ndays = ndays + 1
      end do


      do Rvar = 1,maxRvar     ! initialize temp variable
        do nv = 1,ndays
          dvaltemp(nv,Rvar) = 0.
        end do
      end do

************ READY TO OPEN WDM FILE
      call wdbopn(wdmlnd,wdmfnam,1,err)     ! open land read only
      if (err .ne. 0) go to 998

      do Rvar = 1,nRvar  ! loop over river variables

        if (Rname(Rvar).ne.'WATR'.and.Rname(Rvar).ne.'HEAT') cycle
        if (Rname(Rvar).ne.'WATR') cycle

        if (nDvar(Rvar).le.0) cycle

        do Dvar = 1,nDvar(Rvar)  

          call getdailydsn(wdmlnd,sdate,edate,
     I                       Ddsn(Rvar,Dvar),
     O                       nvals,dval)                     !(1)

          nvals = nvals - 1
          if (nvals.ne.ndays) go to 992
          do nv = 1,ndays
            dvaltemp(nv,Rvar)=dvaltemp(nv,Rvar)+dval(nv)*Dfac(Rvar,Dvar)
          end do          ! end loop over days

        end do           ! end loop over land variables

            
      end do            ! end loop over river variables in land use

      call wdflcl (wdmlnd,err)
      if (err.ne.0) call wdflc1(wdmlnd,err)
      if (err.ne.0) go to 996


      do ny = year1,year2
        do jday = -5,ndaysinyear(ny)
          pairq(jday,ny) = 0.0
          pairH(jday,ny) = 0.0
        end do
      end do

      do Rvar = 1,nRvar  ! store water flows
        if (Rname(Rvar).eq.'WATR') then
          nv = 0
          do ny = year1,year2
            j1 = 1
            j2 = ndaysinyear(ny)
            if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1) - 6
            if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
            do jday = j1,j2
              nv = nv + 1
              pairq(jday,ny) = pairq(jday,ny) + dvaltemp(nv,Rvar) 
            end do
          end do
          do ny = year1+1,year2  ! go back and get days -5,0
            do jday = -5,0
              pairq(jday,ny) = pairq(ndaysinyear(ny-1)+jday,ny-1)
            end do
          end do
        end if

        if (Rname(Rvar).eq.'HEAT') then
          nv = 0
          do ny = year1,year2
            j1 = 1
            j2 = ndaysinyear(ny)
            if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1) - 6
            if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
            do jday = j1,j2
              nv = nv + 1
              pairH(jday,ny) = pairH(jday,ny) + dvaltemp(nv,Rvar) 
            end do
          end do
          do ny = year1+1,year2  ! go back and get days -5,0
            do jday = -5,0
              pairH(jday,ny) = pairH(ndaysinyear(ny-1)+jday,ny-1)
            end do
          end do
        end if
      end do
          
      return

********************************* ERROR SPACE **********************************
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

