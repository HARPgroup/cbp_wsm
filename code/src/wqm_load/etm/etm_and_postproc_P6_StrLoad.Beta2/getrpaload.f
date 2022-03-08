************************************************************************
** The program reads RPA monthly loads from a file generated by       **
**    Scenario Builder, and then covert the loads to hourly values    **
**    and put into land WDM                                           **
************************************************************************
      subroutine getrpaload(rscen,rseg,lscen,lseg,
     I                      clu,sdate,edate,Lvname,
     O                      nvals,hval)

      implicit none
      include 'land.inc'

      integer nrpaloads
      parameter (nrpaloads=6)                                 ! number of rpa load
      character*4 loadnm(nrpaloads)
      data loadnm /'no3n','nh3n','orgn','po4p','orgp','sedm'/ !these names must match exaxtly load name in files from SB

      character*4 Lvname,Tload
      character*3 clu,Tlu
      character*6 Tlseg     ! land segment
      character*13 Trseg    ! river segment
      character*300 dline

      integer year1,year2,oldyear
      integer year,month,day,hour
      integer mon
      integer nl,nLB,n,i

      integer indx1,indx2
      logical lastindx

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 35)
      integer LByear(maxTimeBreaks)             ! year of break
      integer LBmonth(maxTimeBreaks)            ! month
      integer LBday(maxTimeBreaks)              ! day
      character*40 LBfile(maxTimeBreaks)

*************** load related variables
      real NH3N(EarliestYear:LatestYear,12)
      real NO3N(EarliestYear:LatestYear,12)
      real ORGN(EarliestYear:LatestYear,12)
      real PO4P(EarliestYear:LatestYear,12)
      real ORGP(EarliestYear:LatestYear,12)
      real SEDM(EarliestYear:LatestYear,12) 

      real SLONload(EarliestYear:LatestYear,12)
      real SNH3load(EarliestYear:LatestYear,12)
      real SNO3load(EarliestYear:LatestYear,12)
      real SPO4load(EarliestYear:LatestYear,12) 
      real SEDMload(EarliestYear:LatestYear,12)

      real SLONcon(EarliestYear:LatestYear,12)
      real SNH3con(EarliestYear:LatestYear,12)
      real SNO3con(EarliestYear:LatestYear,12)
      real SPO4con(EarliestYear:LatestYear,12)
      real SEDMcon(EarliestYear:LatestYear,12)

      real NPratio          ! N to P ratio
      parameter (NPratio = 0.01384)

      real load(maxTimeBreaks,12,nrpaloads),tmon
      real monload(EarliestYear:LatestYear,12,nrpaloads)

*************** WDM related variables
      integer wdmfilrpd
      parameter (wdmfilrpd=dfile+61)

      integer sdate(ndate),edate(ndate)

      integer flowdsn
      data    flowdsn /111/

      real flowvals(ndaymax*24)       ! flow
      double precision monflow(EarliestYear:LatestYear,12)

      real hSLON(ndaymax*24),hSNH3(ndaymax*24)
      real hSNO3(ndaymax*24),hSPO4(ndaymax*24)
      real hSEDM(ndaymax*24)

      logical foundload(nrpaloads)

      logical writeout
      writeout = .false.
c      writeout = .true.
***************** END DECLARATION ************************************

      call lencl(rscen,lenrscen)
      call lencl(lscen,lenlscen)
      call lencl(rseg,lenrseg)
      call lencl(lseg,lenlseg)

      print*,'From Get RPA load ',
     .   ' rscen= ',rscen(:lenrscen),
     .   ' lscen= ',lscen(:lenlscen),
     .   ' rseg = ',rseg(:lenrseg),
     .   ' lseg = ',lseg(:lenlseg),
     .   ' lu   = ',clu,
     .   ' Lvname = ',Lvname


********** open land WDM to get flow info
      wdmfnam = outwdmdir//'land/'//clu//'/'//lscen(:lenlscen)//
     .          '/'//clu//lseg(:lenlseg)//'.wdm'
      call wdbopnlong(wdmfilrpd,wdmfnam,1,err)     ! open land read only
      if (err .ne. 0) go to 996

      call gethourdsn(wdmfilrpd,sdate,edate,flowdsn,
     O                nvals,flowvals)

      do i = 1,nvals
        hval(i) = 0.
      end do

      return

      if ( Lvname.eq.'SNO3' .or.
     .     Lvname.eq.'SBOD' .or.
     .     Lvname.eq.'SNH3' .or.
     .     Lvname.eq.'SPO4' .or.
     .     Lvname.eq.'SEDM' ) then
         print*,'processing... ',Lvname
      else
         return
      end if
 
***** GET THE START YEAR
      year1 = sdate(1)
      year2 = edate(1)
   
***** initilize variable
      do n = 1, maxTimeBreaks               
        do mon = 1,12 
          do nl = 1, nrpaloads
            load(n,mon,nl) = 0.0
          end do
        end do
      end do

      do year = EarliestYear,LatestYear 
        do mon = 1,12
          monflow(year,mon) = 0.0
          do nl = 1, nrpaloads
            monload(year,mon,nl) = 0.0
          end do
        end do
      end do

******** READ THE CONTROL FILE FOR RPA LOAD FILE
      call readcontrol_load(rscen,lenrscen,
     .                      'RPA LOADS',9,
     .                      'END RPA LOADS',13,
     O                      nLB,LByear,LBfile)

      if (nLB.eq.1) then
        nLB = 2
        LByear(2) = LByear(1) +  1
        LBfile(2) = LBfile(1)
      end if
c      print*,'nLB = ',nLB

********read in rpa loads            
      do n = 1,nLB
        
        call trims(LBfile(n),last)
        
        fnam = ScenDatDir//'river/loads/rpaload_'//
     .       LBfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
         
        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        do nl = 1,nrpaloads
          foundload(nl) = .false.
        end do

        do
          read(dfile,'(a300)',err=1001,end=992) dline
          call d2x(dline,last)
          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993
          call findcomma(dline,last)
          Trseg = dline(:last-1)
          if (Trseg .eq. rseg) then         ! get river segment
            call shift(dline)
            call findcomma(dline,last)
            Tlseg = dline(:last-1)
            if (Tlseg.eq.lseg) then          ! get land segment
              call shift(dline)
              call findcomma(dline,last)
              Tlu = dline(:last-1)           
              call lowercase(Tlu)
              if (Tlu.eq.clu) then           ! get land use
c                print*,Trseg,Tlseg,Tlu
                call shift(dline)
                call findcomma(dline,last)
                Tload = dline(:last-1)         ! find load species
                call lowercase(Tload)
                do nl = 1,nrpaloads+1
                  if (nl.eq.nrpaloads+1) go to 994
                  if (Tload.eq.loadnm(nl)) exit
                end do
                foundload(nl) = .true.
                do mon = 1,12
                  call shift(dline)
                  call fread(dline,load(n,mon,nl))
                end do
              end if
            end if
          end if
          
        end do

992     close (dfile)

      end do         ! finish reading all load files

C      do nl = 1,nafoloads
C        if (.not.foundload(nl)) go to 995
C      end do
     
********** interperate to get annual loads for each constitutes
      do nl = 1,nrpaloads
      !{
        if (nLB.gt.0) then  ! if all missing, then don't do this species
        !{
          do mon = 1,12
          !{
            year = year1
            indx1 = 1
            indx2 = 2
            lastindx = .false.
            do while (year.le.year2)
            !{
              do while ((year.le.LByear(indx2).or.lastindx)
     .                     .and.year.le.year2)
              !{
                if (load(indx1,mon,nl).gt.-1) then
                !{
                  tmon = load(indx1,mon,nl) +
     .                (load(indx2,mon,nl)-load(indx1,mon,nl))/
     .                 real(LByear(indx2)-LByear(indx1))*
     .                 real(year-LByear(indx1))
                  monload(year,mon,nl) = max(tmon,0.0)
                !}
                end if
                year = year + 1
              !}
              end do
              if (indx2.eq.nLB) then
                lastindx = .true.
              else
                indx2 = indx2 + 1
                indx1 = indx1 + 1
              end if
            !}
            end do
          !}
          end do
        !}
        end if  ! if this species exists
      !}
      end do  ! loop over species

********** rearrange variables for each species
      if (writeout .eqv. .true.) then
        fnam = outdir//'input/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'_'//lseg(:lenlseg)//'_'//
     .         clu//'_pounds'//Lvname//'.csv'
        open(99,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(99,*) 'RPA pounds'
      end if
      do year = year1, year2
        do mon = 1,12
          NO3N(year,mon) = monload(year,mon,1)
          NH3N(year,mon) = monload(year,mon,2)
          ORGN(year,mon) = monload(year,mon,3)
          PO4P(year,mon) = monload(year,mon,4)
          ORGP(year,mon) = monload(year,mon,5)
          SEDM(year,mon) = monload(year,mon,6)
          if (writeout .eqv. .true.)
     .       write(99,*) year,',',mon,',',
     .           monload(year,mon,1),monload(year,mon,2),
     .           monload(year,mon,3),monload(year,mon,4),
     .           monload(year,mon,5),monload(year,mon,6)
        end do
      end do

********** create WDM variable from loads
      do year = year1, year2
        do mon = 1,12
         SNO3load(year,mon) = NO3N(year,mon)
         SLONload(year,mon) = min(ORGN(year,mon),ORGP(year,mon)/NPratio)
         SNH3load(year,mon) = NH3N(year,mon)
     .                        + ORGN(year,mon)
     .                        - SLONload(year,mon)
         SPO4load(year,mon) = PO4P(year,mon)
     .                        + ORGP(year,mon)
     .                        - SLONload(year,mon) * NPratio
         SEDMload(year,mon) = SEDM(year,mon)
        end do
      end do

********** open land WDM to get flow info
c      wdmfnam = outwdmdir//'land/'//clu//'/'//lscen(:lenlscen)//
c     .          '/'//clu//lseg(:lenlseg)//'.wdm'
c      call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open land read only
c      if (err .ne. 0) go to 996

c      call gethourdsn(wdmfil,sdate,edate,flowdsn,
c     O                nvals,flowvals)
       
********** get annual flow
      hour  = 0
      year  = sdate(1)
      month = sdate(2)
      day   = sdate(3)

      do i = 1,nvals
        hour = hour + 1
        monflow(year,month) = monflow(year,month) + flowvals(i)
        if (hour.eq.24) then
          hour = 0
          call tomorrow(year,month,day)
        end if
      end do

      do year = year1,year2
        do mon = 1,12
          SNO3con(year,mon) = SNO3load(year,mon)/monflow(year,mon)
          SLONcon(year,mon) = SLONload(year,mon)/monflow(year,mon)
          SNH3con(year,mon) = SNH3load(year,mon)/monflow(year,mon)
          SPO4con(year,mon) = SPO4load(year,mon)/monflow(year,mon)
          SEDMcon(year,mon) = SEDMload(year,mon)/monflow(year,mon)
        end do
      end do

********** get hourly loads
      hour  = 0
      year  = sdate(1)
      month = sdate(2)
      day   = sdate(3)
      oldyear = year


      do i = 1,nvals
        hSNO3(i) = SNO3con(year,month)*flowvals(i)
        hSLON(i) = SLONcon(year,month)*flowvals(i)
        hSNH3(i) = SNH3con(year,month)*flowvals(i)
        hSPO4(i) = SPO4con(year,month)*flowvals(i)
        hSEDM(i) = SEDMcon(year,month)*flowvals(i)

        hour = hour + 1
        if (hour.eq.24) then
          hour = 0
          call tomorrow(year,month,day)
        end if
      end do

******** find the loads for each land varible in WDM
      do i = 1,nvals
c        hval(i) = 0.
        if(Lvname .eq. 'SNO3') hval(i) = hSNO3(i)
        if(Lvname .eq. 'SBOD') hval(i) = hSLON(i)  
        if(Lvname .eq. 'SNH3') hval(i) = hSNH3(i) 
        if(Lvname .eq. 'SPO4') hval(i) = hSPO4(i) 
        if(Lvname .eq. 'SEDM') hval(i) = hSEDM(i)
      end do

      call wdflcl(wdmfilrpd,err)
      if (err.ne.0) go to 997
       
      return

****************** ERROR SPACE ****************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = ' do not recognize nutrient loads '//Tload
      report(2) = ' found in file:  must match varible declared'
      report(3) = fnam
      go to 999

995   report(1) = ' load species not found in file'//loadnm(nl)
      report(2) = ' check load file'
      report(3) = fnam
      go to 999

996   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

997   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

1000  report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end


