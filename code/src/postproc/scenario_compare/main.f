************************************************************************
** this program gets all the non-land associated loads                **
**  point source, septic, and atmospheric deposition.                 **
**  They are associated with a specific land seg / water seg pair     **
************************************************************************
      implicit none

      include 'scencompare.inc'

****************  Total loads by river variable and load
      double precision ScenSTREAMrvar(maxRvar) 
      double precision ScenPSrvar(maxRvar) 
      double precision ScenATDEPrvar(maxRvar) 
      double precision ScenSEPrvar(maxRvar) 
      double precision ScenSTREAMload(nloadmax)     
      double precision ScenPSload(nloadmax)     
      double precision ScenATDEPload(nloadmax)     
      double precision ScenSEPload(nloadmax)     
      double precision CalSTREAMrvar(maxRvar) 
      double precision CalPSrvar(maxRvar) 
      double precision CalATDEPrvar(maxRvar) 
      double precision CalSEPrvar(maxRvar) 
      double precision CalSTREAMload(nloadmax)     
      double precision CalPSload(nloadmax)     
      double precision CalATDEPload(nloadmax)     
      double precision CalSEPload(nloadmax)     

*********** total loads
      real totalCal(nloadmax),totalScen(nloadmax)

      integer outfil,datwdm,wdmrch
      parameter (outfil=dfile+1)    ! file number for output
      parameter (datwdm=dfile+2)     ! file number for point source
      parameter (wdmrch=dfile+5)    ! file number for fake wdm

      real wateracres(ndaymax)   ! acre to multiply atdep

      integer numsegs          ! number of land segments with this river

      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/

************ scenario and calibration data specifiers
      character*25 Scenpradscen, Scenpsscen, Scensepscen
      character*25 Calpradscen, Calpsscen, Calsepscen
      integer lenpradscen, lenpsscen, lensepscen

************* calibration scenario to compare current loads against
      character*25 calscen
      integer lencalscen

************* output flags
      integer idaily,imonthly,iannual,iaveann
      logical daily,monthly,annual,aveann

************* variables to store output
      include '../../lib/inc/rsegs.inc'
      real ts(maxrsegs),sstr(maxrsegs),sp(maxrsegs)
      real ss(maxrsegs),sa(maxrsegs)
      real tc(maxrsegs),cstr(maxrsegs),cp(maxrsegs)
      real cs(maxrsegs),ca(maxrsegs)

************ utility variables
      integer year1,year2,hour,jday,ndays
      integer nv,nvar,Rvar,nc,ns,nl

      integer julian
      external julian

      logical doatdep,dops,dosep
      logical fileexists

      character*1 dummy

*********** PS vaiables
      integer nps,np
      parameter (nps=3)
      character*5 psnam(nps)         ! pointsource
      data psnam /'wwtp','indus','cso'/
      integer lenpsnam(nps)
 
***************** END DECLARATIONS *************************************
      lenlseg = 6

      read(*,*,err=995,end=996) rscen,rseg

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      print*,'comparing river loads for ',rseg,' ',rscen(:lenrscen)

********  open FAKE WDM file, used ONLY to solve the problem of
*******       opening and closing WDM file, never close this file
      call lencl(rseg,lenrseg)
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmrch,wdmfnam,0,err)
      if (err .ne. 0) go to 998

************ read control file to get the calibration scenario
      call readcontrol_Rcalscen(
     I                          rscen,lenrscen,
     O                          calscen)
      call lencl(calscen,lencalscen)

******** READ THE CONTROL FILE FOR START AND STOP TIME
      call readcontrol_time(rscen,lenrscen,
     O                      sdate,edate)
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

******* POPULATE variables
      call getl2r(rseg,rscen,lenrscen,
     O            numsegs,l2r)

      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call getparloads(
     I                 ioscen,lenioscen,
     O                 nparloads,parloadname)
     
      call getRvars(              ! get active Rvars from rchres_in
     I              ioscen,lenioscen,
     I              modules,nmod,
     O              nRvar,Rname,Rdsn)

      call loadinfo(            ! get loads for active Rvars
     I              ioscen,lenioscen,
     I              nRvar,Rname,
     I              nparloads,parloadname,
     O              nloads,loadname,unit,ncons,con,confactor)

      call getvars(
     I             ioscen,lenioscen,
     I             nRvar,Rname,ps,
     O             nPSvar,PSdsn,PSname,PSfac)

      call getvars(
     I             ioscen,lenioscen,
     I             nRvar,Rname,sep,
     O             nSEPvar,SEPdsn,SEPname,SEPfac)

      call getvars(
     I             ioscen,lenioscen,
     I             nRvar,Rname,atdep,
     O             nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)

*********** READ CONTROL FILES FOR DATA SPECIFIERS
      call readcontrol_wdm(rscen,lenrscen,
     O                     scenpradscen,scenpsscen,scensepscen,
     O                     doatdep,dops,dosep)  ! names of wdms

      call readcontrol_wdm(calscen,lencalscen,
     O                     calpradscen,calpsscen,calsepscen,
     O                     doatdep,dops,dosep)  ! names of wdms

      do Rvar = 1,nRvar    ! initialize
        ScenPSrvar(Rvar) = 0.0
        ScenATDEPrvar(Rvar) = 0.0
        ScenSEPrvar(Rvar) = 0.0
        CalPSrvar(Rvar) = 0.0
        CalATDEPrvar(Rvar) = 0.0
        CalSEPrvar(Rvar) = 0.0
      end do

***************** POINT SOURCE SECTION
      if (dops) then
        call ttyput('  Point Source,  ')

        do ns = 1,numsegs

********** get scenario PS data
          do np = 1, nps              ! add three types of PS
            foundwdm = .false.
            call lencl(Scenpsscen,lenpsscen)
            call lencl(psnam(np),lenpsnam(np)) 
            wdmfnam = ScenDatDir//'river/ps/'//Scenpsscen(:lenpsscen)//
     .                '/'//psnam(np)(:lenpsnam(np))//'_'//
     .                l2r(ns)(:lenlseg)//'_to_'//rseg(:lenrseg)//'.wdm'
            inquire (file=wdmfnam,exist=foundwdm)

            if (foundwdm) then
              call wdbopnlong(datwdm,wdmfnam,0,err)
              if (err .ne. 0) go to 998     ! open ps wdm

              do Rvar = 1,nRvar
                if (nPSvar(Rvar).le.0) cycle
                do nvar = 1,nPSvar(Rvar)
                  call getdailydsn(datwdm,sdate,edate,PSdsn(Rvar,nvar),
     O                             nvals,dval)
                  if (nvals.ne.ndays) go to 990
  
                  do nv = 1,ndays
                    ScenPSrvar(Rvar) = ScenPSrvar(Rvar) 
     .                               + dval(nv) * PSfac(Rvar,nvar)
                  end do
                end do
              end do

              call wdflcl(datwdm,err)
              if (err.ne.0) go to 997
            end if
          end do          ! end loop over PS data type

************ get calib PS data
          do np = 1, nps              ! add three types of PS
            foundwdm = .false.
            call lencl(Calpsscen,lenpsscen)
            wdmfnam = ScenDatDir//'river/ps/'//Calpsscen(:lenpsscen)//
     .                '/'//psnam(np)(:lenpsnam(np))//'_'//
     .                l2r(ns)(:lenlseg)//'_to_'//rseg(:lenrseg)//'.wdm'
            inquire (file=wdmfnam,exist=foundwdm)

            if (foundwdm) then
              call wdbopnlong(datwdm,wdmfnam,0,err)
              if (err .ne. 0) go to 998     ! open ps wdm

              do Rvar = 1,nRvar
                if (nPSvar(Rvar).le.0) cycle
                do nvar = 1,nPSvar(Rvar)
                  call getdailydsn(datwdm,sdate,edate,PSdsn(Rvar,nvar),
     O                             nvals,dval)
                  if (nvals.ne.ndays) go to 990

                  do nv = 1,ndays
                    CalPSrvar(Rvar) = CalPSrvar(Rvar) 
     .                              + dval(nv) * PSfac(Rvar,nvar)
                  end do
                end do
              end do
           
              call wdflcl(datwdm,err)
              if (err.ne.0) go to 997
            end if
          end do   !end loop over ps data types

        end do  ! end loop over lrsegs in rseg

      end if

***************** SEPTIC SECTION
      if (dosep) then
        call ttyput('Septic,  ')

        do ns = 1,numsegs
          call lencl(Scensepscen,lensepscen)
          wdmfnam = ScenDatDir//'river/septic/'//
     .              Scensepscen(:lensepscen)//
     .              '/septic_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          call wdbopnlong(datwdm,wdmfnam,0,err)
          if (err .ne. 0) go to 998     ! open septic wdm

          do Rvar = 1,nRvar
            if (nSEPvar(Rvar).le.0) cycle
            do nvar = 1,nSEPvar(Rvar)
              call getdailydsn(datwdm,sdate,edate,SEPdsn(Rvar,nvar),
     O                         nvals,dval)
              if (nvals.ne.ndays) go to 990
  
              do nv = 1,ndays
                ScenSEPrvar(Rvar) = ScenSEPrvar(Rvar)
     .                            + dval(nv) * SEPfac(Rvar,nvar)
              end do
            end do
          end do

          call wdflcl(datwdm,err)
          if (err.ne.0) go to 997

          call lencl(Calsepscen,lensepscen)
          wdmfnam = ScenDatDir//'river/septic/'//
     .              Calsepscen(:lensepscen)//
     .              '/septic_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          call wdbopnlong(datwdm,wdmfnam,0,err)
          if (err .ne. 0) go to 998     ! open septic wdm

          do Rvar = 1,nRvar
            if (nSEPvar(Rvar).le.0) cycle
            do nvar = 1,nSEPvar(Rvar)
              call getdailydsn(datwdm,sdate,edate,SEPdsn(Rvar,nvar),
     O                         nvals,dval)
              if (nvals.ne.ndays) go to 990
  
              do nv = 1,ndays
                CalSEPrvar(Rvar) = CalSEPrvar(Rvar)
     .                          + dval(nv) * SEPfac(Rvar,nvar)
              end do
            end do
          end do

          call wdflcl(datwdm,err)
          if (err.ne.0) go to 997

        end do

      end if

***************** ATMOSPHERIC DEPOSITION SECTION
      if (doatdep) then
        call ttyput('Atmospheric Deposition,  ')

        do ns = 1,numsegs

          call lencl(Scenpradscen,lenpradscen)
          wdmfnam = ScenDatDir//'climate/prad/'//
     .              Scenpradscen(:lenpradscen)//
     .       '/prad_'//l2r(ns)(:lenlseg)//'.wdm'
          call wdbopnlong(datwdm,wdmfnam,0,err)
          if (err .ne. 0) go to 998                  ! open atdep wdm

          call getwateracres(rscen,lenrscen,rseg,l2r(ns),
     I                       sdate,edate,
     O                       wateracres)

          do Rvar = 1,nRvar
            if (nATDEPvar(Rvar).le.0) cycle
            do nvar = 1,nATDEPvar(Rvar)
              call gethourdsn(datwdm,sdate,edate,ATDEPdsn(Rvar,nvar),
     O                        nvals,hval)
              nvals = nvals/24
              if (nvals.ne.ndays) go to 990

              nv = 0
              do jday = 1,ndays
                do hour = 1,24
                  nv = nv + 1
                  ScenATDEPrvar(Rvar) = ScenATDEPrvar(Rvar)
     .              + hval(nv) * ATDEPfac(Rvar,nvar) * wateracres(jday)
                end do
              end do
            end do
          end do

          call wdflcl(datwdm,err)
          if (err.ne.0) go to 997

          call lencl(Calpradscen,lenpradscen)
          wdmfnam = ScenDatDir//'climate/prad/'//
     .              Calpradscen(:lenpradscen)//
     .       '/prad_'//l2r(ns)(:lenlseg)//'.wdm'
          call wdbopnlong(datwdm,wdmfnam,0,err)
          if (err .ne. 0) go to 998                  ! open atdep wdm

          call getwateracres(calscen,lencalscen,rseg,l2r(ns),
     I                       sdate,edate,
     O                       wateracres)

          do Rvar = 1,nRvar
            if (nATDEPvar(Rvar).le.0) cycle
            do nvar = 1,nATDEPvar(Rvar)
              call gethourdsn(datwdm,sdate,edate,ATDEPdsn(Rvar,nvar),
     O                        nvals,hval)
              nvals = nvals/24
              if (nvals.ne.ndays) go to 990

              nv = 0
              do jday = 1,ndays
                do hour = 1,24
                  nv = nv + 1
                  CalATDEPrvar(Rvar) = CalATDEPrvar(Rvar)
     .              + hval(nv) * ATDEPfac(Rvar,nvar) * wateracres(jday)
                end do
              end do
            end do
          end do

          call wdflcl(datwdm,err)
          if (err.ne.0) go to 997

        end do

      end if

**************** got the ps, atdep, sep loads


************** get the upstream loads
      
      call ttyput('  Stream Loads  ')

      do Rvar = 1,nRvar
        CalSTREAMrvar(Rvar) = 0.0
        ScenSTREAMrvar(Rvar) = 0.0
      end do

      wdmfnam = rseg(:lenrseg)//'.wdm'
      call wdbopnlong(datwdm,wdmfnam,0,err)
      if (err .ne. 0) go to 998     ! open ps wdm

      do Rvar = 1,nRvar
        call gethourdsn(datwdm,sdate,edate,Rdsn(Rvar),
     O                  nvals,hval)
        if (nvals/24.ne.ndays) go to 990

        do nv = 1,nvals
          ScenSTREAMrvar(Rvar) = ScenSTREAMrvar(Rvar) + hval(nv) 
        end do
      end do

      call wdflcl(datwdm,err)
      if (err.ne.0) go to 997

      call lencl(calscen,lencalscen)
      wdmfnam = outwdmdir//'river/'//calscen(:lencalscen)//
     .          '/stream/'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(datwdm,wdmfnam,0,err)
      if (err .ne. 0) go to 998     ! open ps wdm

      do Rvar = 1,nRvar
        call gethourdsn(datwdm,sdate,edate,Rdsn(Rvar),
     O                  nvals,hval)
        if (nvals/24.ne.ndays) go to 990

        do nv = 1,nvals
          CalSTREAMrvar(Rvar) = CalSTREAMrvar(Rvar) + hval(nv) 
        end do
      end do

      call wdflcl(datwdm,err)
      if (err.ne.0) go to 997

*********** convert all to loads
      do nl = 1,nloads

        ScenPSload(nl) = 0.0
        ScenATDEPload(nl) = 0.0
        ScenSEPload(nl) = 0.0
        ScenSTREAMload(nl) = 0.0
        CalPSload(nl) = 0.0
        CalATDEPload(nl) = 0.0
        CalSEPload(nl) = 0.0
        CalSTREAMload(nl) = 0.0

        do nc = 1,ncons(nl)
          do Rvar = 1,nRvar        ! get the right dsn
            if (con(nl,nc).eq.Rname(Rvar)) then
              ScenPSload(nl) = ScenPSload(nl)
     .                       + ScenPSrvar(Rvar) * confactor(nl,nc)
              ScenSEPload(nl) = ScenSEPload(nl)
     .                        + ScenSEPrvar(Rvar) * confactor(nl,nc)
              ScenATDEPload(nl) = ScenATDEPload(nl)
     .                          + ScenATDEPrvar(Rvar) * confactor(nl,nc)
              ScenSTREAMload(nl) = ScenSTREAMload(nl)
     .                           + ScenSTREAMrvar(Rvar)*confactor(nl,nc)
              CalPSload(nl) = CalPSload(nl)
     .                      + CalPSrvar(Rvar) * confactor(nl,nc)
              CalSEPload(nl) = CalSEPload(nl)
     .                       + CalSEPrvar(Rvar) * confactor(nl,nc)
              CalATDEPload(nl) = CalATDEPload(nl)
     .                         + CalATDEPrvar(Rvar) * confactor(nl,nc)
              CalSTREAMload(nl) = CalSTREAMload(nl)
     .                          + CalSTREAMrvar(Rvar) * confactor(nl,nc)
            end if
          end do
        end do
      end do

************ find total loads
      do nl = 1,nloads
        totalScen(nl) = ScenPSload(nl) + ScenSepload(nl) 
     .                + ScenATDEPload(nl) + ScenSTREAMload(nl)
        totalCal(nl) = CalPSload(nl) + CalSepload(nl) 
     .               + CalATDEPload(nl) + CalSTREAMload(nl)
      end do

*********** open and write output file for each rseg
      fnam = outdir//'river/scenario_compare/'//
     .       rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.csv'
      open(outfil,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(outfil,*,err=951) 'Load,Scen,Total,Stream,PS,Sep,Atdep,',
     .                        'CalScen,Total,Stream,PS,Sep,Atdep'

      do nl = 1,nloads
        write(outfil,1234,err=951) loadname(nl),
     .                     rscen,totalScen(nl),
     .                     ScenSTREAMload(nl),ScenPSload(nl),
     .                     ScenSepload(nl),ScenATDEPload(nl),
     .                     calscen,totalCal(nl),
     .                     CalSTREAMload(nl),CalPSload(nl),
     .                     CalSepload(nl),CalATDEPload(nl)
      end do

      close(outfil)

      stop

1234  format(a4,2(',',a25,5(',',e12.5)))

************************ ERROR SPACE *************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

990   report(1) = 'problem in wdm '//wdmfnam
      report(2) = 'difference between expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',nvals
      go to 999

991   report(1) = 'Error opening file:'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Error Reading File:'
      report(2) = fnam
      report(3) = ' '
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


