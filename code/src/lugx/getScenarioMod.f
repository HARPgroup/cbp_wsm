************************************************************************
** PQUAL and IQUAL are not sensitive to inputs                        **
**                                                                    **
** To simulate sensitivity, the inputs for a partilcular scenario are **
**  compared to the inputs for the calibration scenario and factors   **
**  are determined.  The factors are used to modify PQUAL parameters  **
**                                                                    **
** In addition, if there is a trend in inputs over time, the factors  **
**  will vary with the trend in inputs on an annual basis             **
** This trend will be part of the calibration as well.                **
**                                                                    **
** both relationships are such that the parameter is halved when the  **
**  balance is equal to zero and equal to the nominal value when the  **
**  balance is equal to the average or calibration value              **
**                                                                    **
** Scenario Modification                                              **
** Pnew = Pold/2 ( 1 + scen_application / cal_application)            **
**   mult = Pnew/Pold = ( 1 + scen_application / cal_application)/2   **
**                                                                    **
** Time Modification                                                  **
** Pnew = Pold/2 ( 1 + annual_application / average_application)      **
**   mult = Pnew/Pold = (1+annual_application/average_application)/2  **
************************************************************************
************************************************************************
** gets modification of PQUAL scenario parameters from the difference **
** between the calibration and scenraio inputs                        **
************************************************************************
      subroutine getScenarioMod(
     I                          lseg,lscen,lenlscen,clu,modules,nmod,
     I                          startY,endY,species,nspecies,
     I                          doV,doP,doF,doM,doL,doT,doU,doR,
     O                          ScenarioMod,TimeMod)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      character*50 calscen
      integer lencalscen

      real scentotapp(nspecies) ! total application per year
      real caltotapp(nspecies) ! total application per year
      real scenannapp(startY:endY,nspecies) !ind annual
      real calannapp(startY:endY,nspecies) !ind annual

      character*6 apptype

      integer s2q     ! index
      real num,denom     ! temp vars to calc average
          
      integer ny,nm

      character*1 operation  ! operation to be performed
                             ! allowable +, -, =

      logical doV,doP,doF,doM,doL,doT,doU,doR

      logical doScenarioMod

**************** END DECLARATIONS **************************************
********* skip if no PQUAL
      doScenarioMod = .false.
      do nm = 1,nmod
        if (modules(nm).eq.'PQUAL') doScenarioMod = .true.
        if (modules(nm).eq.'IQUAL') doScenarioMod = .true.
      end do
      if (.not.doScenarioMod) return

************
      do sp = 1,nspecies
        ScenarioMod(sp) = 1.0   ! no change is the default
      end do

      do sp = 1,nspecies
        do ny = startY,endY
          calannapp(ny,sp) = 0.0
          scenannapp(ny,sp) = 0.0
        end do
        caltotapp(sp) = 0.0
        scentotapp(sp) = 0.0
      end do
         
      call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          calscen)
      call lencl(calscen,lencalscen)

********** read annual fertilizer files
      if (doF) then
        apptype = 'fert'
        operation = '+'
        call readannfile(
     I                   apptype,calscen,lencalscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   calannapp)
        call readannfile(
     I                   apptype,lscen,lenlscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   scenannapp)
      end if

********** read annual manure files
      if (doM) then
        apptype = 'manure'
        operation = '+'
        call readannfile(
     I                   apptype,calscen,lencalscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   calannapp)
        call readannfile(
     I                   apptype,lscen,lenlscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   scenannapp)
      end if

********** read annual legume files
      if (doL) then
        apptype = 'legume'
        operation = '+'
        call readannfile(
     I                   apptype,calscen,lencalscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   calannapp)
        call readannfile(
     I                   apptype,lscen,lenlscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   scenannapp)
      end if

********** read annual atdep files
      apptype = 'atdep'
      operation = '+'
      call readannfile(
     I                 apptype,calscen,lencalscen,lseg,clu,
     I                 startY,endY,nspecies,operation,
     M                 calannapp)
      call readannfile(
     I                 apptype,lscen,lenlscen,lseg,clu,
     I                 startY,endY,nspecies,operation,
     M                 scenannapp)

********** read annual uptake files
      if (doU) then
        apptype = 'uptake'
        operation = '-'
        call readannfile(
     I                   apptype,calscen,lencalscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   calannapp)
        call readannfile(
     I                   apptype,lscen,lenlscen,lseg,clu,
     I                   startY,endY,nspecies,operation,
     M                   scenannapp)
      end if

*************** got calibration and scenario annual applications

*********** get the correspondence between species and QUALs
      call getSpecies2Qual(
     I                     lscen,lenlscen,nspecies,species,
     O                     nquals,nspecies2qual,species2qual)

************ create scenario modification
      do sp = 1,nspecies
        caltotapp(sp) = 0.0
        scentotapp(sp) = 0.0
        do ny = startY,endY
          caltotapp(sp) = caltotapp(sp) + calannapp(ny,sp)
          scentotapp(sp)= scentotapp(sp) + scenannapp(ny,sp)
        end do
        caltotapp(sp) = caltotapp(sp) / real(endY-startY+1)
        scentotapp(sp)= scentotapp(sp) / real(endY-startY+1)
      end do

      do nq = 1,nquals
        num = 0.0
        denom = 0.0
        do s2q = 1,nspecies2qual(nq)
          sp = species2qual(nq,s2q) 
          if (caltotapp(sp).gt.0.01) then
            num = num + (1.0 + scentotapp(sp) / caltotapp(sp)) / 2.0
            denom = denom + 1.0
          end if
        end do
        if (denom.gt.0.5) then
          ScenarioMod(nq) = num / denom
          ScenarioMod(nq) = max(ScenarioMod(nq),0.01) ! positive
          ScenarioMod(nq) = min(ScenarioMod(nq),10.0) !  not too big
        else
          ScenarioMod(nq) = 1.0
        end if
      end do

*************** get temporal modification, annual time series
      do nq = 1,nquals
        do ny = startY,endY
          num = 0.0
          denom = 0.0
          do s2q = 1,nspecies2qual(nq)
            sp = species2qual(nq,s2q)
            if (scentotapp(sp).gt.0.01) then
              num = num + (1.0+scenannapp(ny,sp)/scentotapp(sp)) /2.0
              denom = denom + 1.0
            end if
          end do
          if (denom.gt.0.5) then
            TimeMod(ny,nq) = num / denom
            TimeMod(ny,nq) = max(TimeMod(ny,nq),0.01) ! positive
          else
            TimeMod(ny,nq) = 1.0
          end if
        end do
      end do

********** output the modifications
C      print*,','
C      print*,(scenariomod(nq),nq=1,nquals)
C      do ny = startY,endY
C        print*,(timemod(ny,nq),nq=1,nquals)
C      end do
C      print*,','
C      print*,(scenariomod(nq),nq=1,nquals)

      return

      end


************************************************************************
** reads an annual file and INCREMENTS the aggannapp variable         **
************************************************************************
      subroutine readannfile(
     I                       apptype,lscen,lenlscen,lseg,clu,
     I                       startY,endY,nspecies,operation,
     M                       aggannapp)
      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      integer startY,endY,nspecies
      real aggannapp(startY:endY,nspecies)
      character*(*) apptype
      character*(*) clu
      logical foundyear(startY:endY)
      integer lenapptype
      integer ny,sp
      real tempapp(nspecies)
      character*1 dummy
      character*1 operation

      call lencl(apptype,lenapptype)
      fnam = outdir//'input/'//lscen(:lenlscen)//'/annual_'//
     .       apptype(:lenapptype)//'_'//lseg//'_'//clu//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = startY,endY
        foundyear(ny) = .false.
      end do

      read(11,*,err=992,end=992) dummy  ! header
      do 
        read(11,*,err=992,end=111) ny,(tempapp(sp),sp=1,nspecies)
        if (ny.ge.startY.and.ny.le.endY) then
          foundyear(ny) = .true.
          do sp = 1,nspecies
            if (operation.eq.'+') then
              aggannapp(ny,sp) = aggannapp(ny,sp) + tempapp(sp)
            else if (operation.eq.'-') then
              aggannapp(ny,sp) = aggannapp(ny,sp) - tempapp(sp)
            else if (operation.eq.'=') then
              aggannapp(ny,sp) = tempapp(sp)
            else 
              go to 994
            end if
          end do
        end if
      end do

111   close(11)
      do ny = startY,endY
        if (.not.foundyear(ny)) go to 993 
      end do

      return

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'not all expected years in file'
      report(2) = fnam
      write(report(3),*) 'expected ',startY,'-',endY
      go to 999

994   report(1) = 'operation sent from calling program to readannfile'
      report(2) = ' is not allowed'
      report(3) = ' check code'
      go to 999

999   call stopreport(report)

      end
