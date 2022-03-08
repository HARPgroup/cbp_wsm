************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine doflows(
     I                   rscen,obscen,rseg,year1,year2,nvals,flowvals,
     I                   sdate,npeaks,
     O                   simfl,obfl,bofl,bsfl,qofl,qsfl,FloObsExist)
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
      logical writeFlowDaily,writeSFlowDaily,writeBFlowDaily
      writeFlowDaily = .false.
      writeSFlowDaily = .true.
      writeBFlowDaily = .true.
 
******************* END DECLARATIONS ***********************************

********************Calculate flow statistics 
*********** code to re-arrange the flow to get hourly 
      ny = sdate(1)
      nm = sdate(2)
      nd = sdate(3)
      nh = max(sdate(4),1)
      i = 1
      do while (ny.le.year2)
        simfl(ny,nm,nd,nh) = flowvals(i)   ! unit: ac-ft/hr
        i = i + 1
        call onehour(ny,nm,nd,nh)
      end do

************ check for flow obs.  If available, send to flowstats
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(obscen,lenobscen)

      FloObsExist = .false.
      obfnam = calibdir//'observed/'//obscen(:lenobscen)//'/FLOW/'
     .         //rseg(:lenrseg)//'.OFLOW'
      call findopen(ifl)
      open(ifl,file=obfnam,status='old',iostat=err)
      if (err .eq. 0) then

*********** read observed flow
        ndays=0
        do
          read(ifl,*, end=111) ny,nm,nd,nh,nmin,tflo
          
          if (ny.ge.year1.and.ny.le.year2) then
            ndays=ndays+1
            if (tflo.lt.0.1) tflo = 0.1
            obflow(ndays) = tflo            ! unit: cfs
            simflow(ndays) = 0.0
            obyear(ndays) = ny
            obmonth(ndays) = nm
            obday(ndays) = nd
            obfl(ny,nm,nd) = tflo 
           
            do nh = 1,24   ! re-arrange to get daily flow in cfs
              if (simfl(ny,nm,nd,nh).lt.0.1) simfl(ny,nm,nd,nh)=0.1
              simflow(ndays)=simflow(ndays)+simfl(ny,nm,nd,nh)!ac-ft/day
            end do
            simflow(ndays)=simflow(ndays)*43560.0/3600.0/24.0 ! cfs
          end if
       
        end do
        
111     close(ifl)                        ! close observed flow file

cbhatt        print*,obflow(4036),',',simflow(4036)
        if (writeFlowDaily) then
          fnam = outdir//'/river/daily/'//rscen(:lenrscen)//'/'//
     .                   rseg(:lenrseg)//'_FLOW.OBSI'
          open(11,file=fnam,status='unknown',iostat = err)
          if (err.ne.0) go to 991
          write(11,'(A4,A,A2,A,A2,A,A12,A,A12)'),
     .       'YEAR',',','MM',',','DY',',',
     .       'OBSFLOW(cfs)',',','SIMFLOW(cfs)'
          do ny=year1,year2
             do nm=1,12
                do nd=1,ndaysinmonth(ny,nm)
                   if (obfl(ny,nm,nd).gt.0) then
                      tflo = 0.0
                      do nh = 1,24
                         tflo = tflo + simfl(ny,nm,nd,nh)
                      end do
                      write(11,'(I4,A,I2,A,I2,A,F12.2,A,F12.2)'),
     .                   ny,',',nm,',',nd,',',obfl(ny,nm,nd),
     .                      ',',tflo*43560.0/3600.0/24.0
                   end if
                end do
             end do
          end do
          close(11)
        end if

        if (ndays.gt.50) then
          FloObsExist = .true.
          nm = 1
          nd = 1
          call readcontrol_Rgeoscen(rscen,lenrscen,
     O                              geoscen)
          call lencl(geoscen,lengeoscen)

          fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'

          call PART(simflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,
     O              bsim,qsim,err)
          if (err.ne.0) go to 993
          call PART(obflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,
     O              bobs,qobs,err)
          if (err.ne.0) go to 993

C          open(98,file='../bflw',status='unknown')
C          write(98,*) year1
          do nd = 1,ndays  ! change format
C            write(98,*) nd,bobs(nd)
            bofl(obyear(nd),obmonth(nd),obday(nd)) = bobs(nd)
            bsfl(obyear(nd),obmonth(nd),obday(nd)) = bsim(nd)
            qofl(obyear(nd),obmonth(nd),obday(nd)) = qobs(nd)
            qsfl(obyear(nd),obmonth(nd),obday(nd)) = qsim(nd)
          end do
C          close(98)

        if (writeSFlowDaily) then
          fnam = outdir//'/river/daily/'//rscen(:lenrscen)//'/'//
     .                   rseg(:lenrseg)//'_FLOW.SPART'
          open(11,file=fnam,status='unknown',iostat = err)
          if (err.ne.0) go to 991
          write(11,'(A4,A,A2,A,A2,A,A12,A,A12)'),
     .       'YEAR',',','MM',',','DY',',',
     .       'OBSFLOW(cfs)',',','SIMFLOW(cfs)'
          do nd = 1,ndays
             write(11,'(I4,A,I2,A,I2,A,F12.2,A,F12.2)'),
     .         obyear(nd),',',obmonth(nd),',',obday(nd),',',
     .         qobs(nd),',',qsim(nd)
          end do
          close(11)
        end if

        if (writeBFlowDaily) then
          fnam = outdir//'/river/daily/'//rscen(:lenrscen)//'/'//
     .                   rseg(:lenrseg)//'_FLOW.BPART'
          open(11,file=fnam,status='unknown',iostat = err)
          if (err.ne.0) go to 991
          write(11,'(A4,A,A2,A,A2,A,A12,A,A12)'),
     .       'YEAR',',','MM',',','DY',',',
     .       'OBSFLOW(cfs)',',','SIMFLOW(cfs)'
          do nd = 1,ndays
             write(11,'(I4,A,I2,A,I2,A,F12.2,A,F12.2)'),
     .         obyear(nd),',',obmonth(nd),',',obday(nd),',',
     .         bobs(nd),',',bsim(nd)
          end do
          close(11)
        end if

          call flowstats(obflow,simflow,ndays,rscen,rseg,year1,year2,
     .                   'FLOW',obyear,obmonth,obday)
          call flowstats(qobs,qsim,ndays,rscen,rseg,year1,year2,'QFLW',
     .                   obyear,obmonth,obday)
          call flowstats(bobs,bsim,ndays,rscen,rseg,year1,year2,'BFLW',
     .                   obyear,obmonth,obday)

          if (npeaks.gt.0)
     .    call peaks(obflow,simflow,qobs,qsim,ndays,rscen,rseg,npeaks,
     .                 year1,year2,obyear,obmonth,obday)

          call VAstats(obflow,simflow,qobs,qsim,bobs,bsim,
     I                 ndays,rscen,rseg,year1,year2,
     I                 obyear,obmonth,obday,
     I                 npeaks)

        end if          ! end if any observations in the time frame

      end if           ! end if observed file opens

      return
  
************************ ERROR REPORTING 
991   report(1) = 'PROBLEM Unable to open file: '
      report(2) = fnam
      report(3) = ''
      go to 999

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
        write(report(2),*) 'unspecified error ',err,' check file'
        report(3) = './pp/src/postproc/river/part/part_sub.f'
      end if
      go to 999

999   call stopreport(report)

      end

