************************************************************************
** write out the atmospheric desposition for this land seg/scenario   **
************************************************************************
      subroutine writeatdep(
     I                      lscen,lenlscen,lseg,modules,nmod,
     I                      pradscen,clu,species,nspecies,
     I                      startY,startM,startD,endY,endM,endD)
      implicit none
      include 'lug.inc'
      include '../lib/inc/wdm.inc'
      include 'acts.inc'

      logical doatdep,found
      integer wdmfil
      parameter (wdmfil=dfile+30)

      integer year,month,day

      integer nm,nd,ndays

      integer spno3,spnh3,sporn,sppo4,sporp

      integer lenpradscen

      integer sdate(ndate),edate(ndate)

      double precision annapp(startY:endY,nspecies) !ind annual
      double precision monapp(startY:endY,12,nspecies)
      character*5 atdep
      data atdep /'atdep'/

********************* END DECLARATIONS *********************************

********* skip if no nitrogen simulated
CBHATT      doatdep = .false.
CBHATT      do nm = 1,nmod
CBHATT        if (modules(nm).eq.'NITR') doatdep = .true.
CBHATT        if (modules(nm).eq.'PQUAL') doatdep = .true.
CBHATT        if (modules(nm).eq.'IQUAL') doatdep = .true.
CBHATT      end do
CBHATT      if (.not.doatdep) return

********** initialize
      do sp = 1,nspecies
        do year = startY,endY
          annapp(year,sp) = 0.0
          do month = 1,12
            monapp(year,month,sp) = 0.0
          end do
        end do
      end do

********** find indices of no3 and nh3
      found = .false.
      do sp = 1,nspecies  ! find no3 species
        if (species(sp).eq.'no3n') then
          found = .true.
          spno3 = sp
          exit
        end if
      end do
      if (.not.found) go to 992
      found = .false.
      do sp = 1,nspecies  ! find nh3 species
        if (species(sp).eq.'nh3n') then
          found = .true.
          spnh3 = sp
          exit
        end if
      end do
      if (.not.found) go to 992
      do sp = 1,nspecies  ! find nh3 species
        if (species(sp).eq.'orgn') then
          found = .true.
          sporn = sp
          exit
        end if
      end do
      if (.not.found) go to 992
      do sp = 1,nspecies  ! find nh3 species
        if (species(sp).eq.'po4p') then
          found = .true.
          sppo4 = sp
          exit
        end if
      end do
      if (.not.found) go to 992
      do sp = 1,nspecies  ! find nh3 species
        if (species(sp).eq.'orgp') then
          found = .true.
          sporp = sp
          exit
        end if
      end do
      if (.not.found) go to 992

*********** get the dates in wdm format
      sdate(1) = startY
      sdate(2) = startM
      sdate(3) = startD
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(1) = endY
      edate(2) = endM
      edate(3) = endD
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0
      

      call lencl(pradscen,lenpradscen)
      wdmfnam = ScenDatDir//'climate/prad/'//pradscen(:lenpradscen)//
     .          '/prad_'//lseg//'.wdm'
      call wdbopnlong(dfile+30,wdmfnam,1,err)  ! open read only
      if (err.ne.0) go to 991

*************** read each type from wdm and store in variable
      dsn = 2001  ! wet no3
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,spno3) = annapp(year,spno3) + dval(nd)
        monapp(year,month,spno3) = monapp(year,month,spno3) + dval(nd)
        call tomorrow(year,month,day)
      end do

      dsn = 2002  ! wet nh3
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,spnh3) = annapp(year,spnh3) + dval(nd)
        monapp(year,month,spnh3) = monapp(year,month,spnh3) + dval(nd)
        call tomorrow(year,month,day)
      end do

      dsn = 2003  ! dry no3
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,spno3) = annapp(year,spno3) + dval(nd)
        monapp(year,month,spno3) = monapp(year,month,spno3) + dval(nd)
        call tomorrow(year,month,day)
      end do

      dsn = 2004  ! dry nh3
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,spnh3) = annapp(year,spnh3) + dval(nd)
        monapp(year,month,spnh3) = monapp(year,month,spnh3) + dval(nd)
        call tomorrow(year,month,day)
      end do

*********** do not process the following for land simulation
      dsn = 2005  ! wet orn
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,sporn) = annapp(year,sporn) + dval(nd)
        monapp(year,month,sporn) = monapp(year,month,sporn) + dval(nd)
        call tomorrow(year,month,day)
      end do

      dsn = 2006  ! wet po4
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,sppo4) = annapp(year,sppo4) + dval(nd)
        monapp(year,month,sppo4) = monapp(year,month,sppo4) + dval(nd)
        call tomorrow(year,month,day)
      end do

      dsn = 2007  ! wet orp
      call getdailydsn(wdmfil,sdate,edate,dsn,
     O                 ndays,dval)
      year = startY
      month = startM
      day = startD
      do nd = 1,ndays
        annapp(year,sporp) = annapp(year,sporp) + dval(nd)
        monapp(year,month,sporp) = monapp(year,month,sporp) + dval(nd)
        call tomorrow(year,month,day)
      end do

      call writeapp(
     I              lscen,lenlscen,lseg,clu,species,nspecies,
     I              startY,endY,atdep,annapp,monapp)

      return

991   report(1) = 'could not open file'
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

992   report(1) = 'did not find expected qual species'
      report(2) = 'check program logic'
      report(3) = ' '
      go to 999
      
999   call stopreport(report)
      end
