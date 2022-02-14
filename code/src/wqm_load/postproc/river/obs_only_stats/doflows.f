************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine doflows(
     I                   obscen,rseg,year1,year2,rscen,lenrscen,
     O                   obfl,bofl,qofl,FloObsExist,
     O                   obflow,ndays)
      implicit none
      include 'Rstats.inc'

      integer ndays       ! test dates for checking wdm
      integer ifl

      character(100) pfname,obfnam 

      integer i,ny,nm,nd,nh,nmin  ! indices
      integer year1,year2             ! first and last year to average

      real flowvals(ndaymax*24)          ! flow
      real tflo            ! daily flow                        

*******  regression variables
      integer npeaks                          ! peaks to investigate

      integer ndaysinmonth
      external ndaysinmonth
 
******************* END DECLARATIONS ***********************************

************ check for flow obs.  If available, send to flowstats
      call lencl(rseg,lenrseg)
      call lencl(obscen,lenobscen)

      FloObsExist = .false.
      obfnam = calibdir//'observed/'//obscen(:lenobscen)//'/FLOW/'
     .         //rseg(:lenrseg)//'.OFLOW'
      call findopen(ifl)
      open(ifl,file=obfnam,status='old',iostat=err)
      if (err .ne. 0) return

********** initialize
      do ny = year1,year2
        do nm = 1,12
          do nd = 1,ndaysinmonth(ny,nm)
            obfl(ny,nm,nd) = -9.0
          end do
        end do
      end do

*********** read observed flow
      ndays=0
      do
        read(ifl,*, end=111) ny,nm,nd,nh,nmin,tflo
          
        if (ny.lt.year1.or.ny.gt.year2) cycle 

        ndays=ndays+1
        if (tflo.lt.0.1) tflo = 0.1
        obflow(ndays) = tflo            ! unit: cfs
        obyear(ndays) = ny
        obmonth(ndays) = nm
        obday(ndays) = nd
        obfl(ny,nm,nd) = tflo 
         
      end do
        
111   close(ifl)                        ! close observed flow file

      if (ndays.eq.0) return

      FloObsExist = .true.
      nm = 1
      nd = 1
      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/watershed_area.csv'

      call PART(obflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,
     O          bobs,qobs,err)
      if (err.ne.0) go to 993

      do nd = 1,ndays  ! change format
        bofl(obyear(nd),obmonth(nd),obday(nd)) = bobs(nd)
        qofl(obyear(nd),obmonth(nd),obday(nd)) = qobs(nd)
      end do

      return
  
************************ ERROR REPORTING 

993   report(1) = 'Error in PART program'
      if (err.eq.993) then
        report(2) = 'could not find segment '//rseg//' in file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else if (err.eq.994) then
        report(2) = 'trouble reading file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else if (err.eq.996) then
        report(2) = 'could not find segment '//rseg//' in file:'
        report(3) = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
      else
        report(2) = 'unspecified error, check file'
        report(3) = './pp/src/postproc/river/part/part_sub.f'
      end if
      go to 999

999   call stopreport(report)

      end

