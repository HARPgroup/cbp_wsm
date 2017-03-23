************************************************************************
** this program gets all the non-land associated loads                **
**  point source, septic, and atmospheric deposition.                 **
**  They are associated with a specific land seg / water seg pair     **
************************************************************************
      subroutine getAtdepAveann(
     I                          rscen,lseg,year1,year2,nls,lname,
     O                          atdepload)
      implicit none

      include 'data.inc'

******** input data
      integer year1,year2
      integer nls ! number of loads to actually report
      character*4 lname(nls)
      real atdepload(nls)  ! annual average lbs per acre

****************  daily load
      real dATDEPrvar(ndaymax,maxRvar) ! daily by river var
      real dATDEPload(ndaymax,nloadmax)     ! daily by load var

      integer outfil,datwdm,wdmrch
      parameter (outfil=dfile+1)    ! file number for output
      parameter (datwdm=dfile+2)     ! file number for point source
      parameter (wdmrch=dfile+5)    ! file number for fake wdm

      character*5 atdep         ! atdep, pointsource, or septic
      data atdep /'atdep'/

      character*25 pradscen
      integer lenpradscen

      integer hour,jday,ndays
      integer nv,nvar,Rvar,nc,ns,nl,nl2

      integer julian
      external julian

      logical doatdep
 
      data sdate /0,1,1,0,0,0/
      data edate /0,12,31,24,0,0/

***************** END DECLARATIONS *************************************
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

********** set number of days and wdm date variable
      ndays = julian(year1,1,1,year2,12,31)
      sdate(1) = year1
      edate(1) = year2

*********** READ CONTROL FILE FOR SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,
     O                     doatdep)
      if (.not.doatdep) then
        do nl = 1,nls
          atdepload(nl) = 0.0
        end do
        return
      end if
      call lencl(pradscen,lenpradscen)

********** READ CONTROL FOR IOSCEN
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)

******* POPULATE variables
      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)

      call getRvars(              ! get active Rvars
     I              ioscen,modules,nmod,
     O              nRvar,Rname)

      call loadinfo(            ! get loads for active Rvars
     I              ioscen,nRvar,Rname,
     O              nloads,loadname,unit,ncons,con,confactor)

      if (doatdep) then
        call getvars(
     I               ioscen,nRvar,Rname,atdep,
     O               nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)
      end if


      do Rvar = 1,nRvar    ! initialize
        do nv = 1,ndaymax
          dATDEPrvar(nv,Rvar) = 0.0
        end do
      end do

***************** ATMOSPHERIC DEPOSITION SECTION
      wdmfnam = ScenDatDir//'climate/prad/'//pradscen(:lenpradscen)//
     .   '/prad_'//lseg//'.wdm'
      call wdbopnlong(datwdm,wdmfnam,0,err)
      if (err .ne. 0) go to 998                  ! open atdep wdm

      do Rvar = 1,nRvar
        if (nATDEPvar(Rvar).le.0) cycle
        do nvar = 1,nATDEPvar(Rvar)
          call getdailydsn(datwdm,sdate,edate,ATDEPdsn(Rvar,nvar),
     O                    nvals,dval)
          if (nvals.ne.ndays) go to 990

          nv = 0
          do jday = 1,ndays
            dATDEPrvar(jday,Rvar) = dATDEPrvar(jday,Rvar)
     .                            + dval(jday) * ATDEPfac(Rvar,nvar)
          end do
        end do
      end do

      call wdflcl(datwdm,err)
      if (err.ne.0) go to 997

*********** convert to daily loads
      do nl = 1,nloads

        do nv = 1,ndays  ! initialize
          dATDEPload(nv,nl) = 0.0
        end do

        do nc = 1,ncons(nl)
          do Rvar = 1,nRvar        ! get the right dsn
            if (con(nl,nc).eq.Rname(Rvar)) then
              do nv = 1,ndays
                dATDEPload(nv,nl) = dATDEPload(nv,nl)
     .                       + dATDEPrvar(nv,Rvar) * confactor(nl,nc)
              end do
            end if
          end do
        end do
      end do

***************** convert to annual loads
      do nl2 = 1,nls   ! match with intended loads
        do nl = 1,nloads  ! loop over available loads
          if (lname(nl2).eq.loadname(nl)) then ! match
            atdepload(nl2) = 0.0
            do nv = 1,ndays
              atdepload(nl2) = atdepload(nl2) + dATDEPload(nv,nl)
            end do
            exit
          end if
        end do
        atdepload(nl2) = atdepload(nl2) / real(year2-year1+1)
      end do
        

      return

************************ ERROR SPACE *************************
990   report(1) = 'problem in wdm '//wdmfnam
      report(2) = 'difference between expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',nvals
      go to 999


995   report(1) = 'Error initializing data postprocessor'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and run_postproc.csh'
      go to 999

996   report(1) = 'Error initializing data postprocessor'
      report(2) = 'not enough input data in run_postproc.csh'
      report(3) = ' '
      go to 999

997   report(1) = 'Error:  closing wdm'
      report(2) = wdmfnam
      report(3) = 'Error = '
      write(report(3)(9:11),'(i3)') err
      go to 999

998   report(1) = 'Error: opening wdm= '
      report(2) = wdmfnam
      if (err.lt.0) then
        report(3) = 'Error = '
        write(report(3)(9:11),'(i3)') err
      else
        report(3) = 'Not a wdm file'
      end if
      go to 999

999   call stopreport(report)

      end


