************************************************************************
** The program reads AVERAGE loads from a file generated by           **
**    Scenario Builder, and then covert the loads to hourly values    **
**    and put into land WDM                                           **
************************************************************************

      implicit none
      include 'land.inc'

      real minavgann
      parameter (minavgann = 1E-4)
      integer nloads
      parameter (nloads=6)                                 ! number of afo/cafo load
      character*4 loadnm(nloads)
      data loadnm /'no3n','nh3n','orgn','po4p','orgp','sedm'/ !these names must match exaxtly load name in files from SB
      integer iloads(nloads)

      integer     NLVAR,NLVARMAX
      parameter   ( NLVARMAX = 9 )
      !TODO Populate using iovar file afocfo_loads
      character*4 Lvname(NLVARMAX)
      data Lvname /'SNO3','SLON','SRON','SNH3',
     .             'DPO4','SPO4',
     .             'SLOP','SROP',
     .             'SEDM'/
      integer     Ldsn(NLVARMAX)
      !TODO Polulate from iovar/implnd file
      data Ldsn   /145,146,147,144,152,153,155,157,121/

      character*4 Tload
      character*3 clu,Tlu,CLUUC
      character*6 Tlseg     ! land segment
      character*13 Trseg    ! river segment
      character*300 dline

      integer ns
      character*300 command

      integer year1,year2,oldyear
      integer year,month,day,hour
      integer nl,nLB,n,i,j

      integer indx1,indx2
      logical lastindx

      integer SAVGYEAR, EAVGYEAR
      character*50 LBfile

*************** load related variables
      real NH3N
      real NO3N
      real ORGN
      real PO4P
      real ORGP
      real SEDM

      real SLONload
      real SRONload
      real SNH3load
      real SNO3load
      real DPO4load
      real SPO4load 
      real SLOPload
      real SROPload
      real SEDMload

      real SLONcon
      real SRONcon
      real SNH3con
      real SNO3con
      real DPO4con
      real SPO4con
      real SLOPcon
      real SROPcon
      real SEDMcon

      real ySEDM
      real NPratio          ! N to P ratio
      parameter (NPratio = 0.01384)

      real CVN2BOD

      real load(nloads),avg
      real avgload(nloads)

*************** WDM related variables
      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)

      integer flowdsn
      data    flowdsn /111/

      integer sedmdsn
      data    sedmdsn /121/

      real flowvals(ndaymax*24)       ! flow
      double precision avgflow

      real sedmvals(ndaymax*24)       ! sedm
      double precision avgsedm
      

      real hSLON(ndaymax*24),hSNH3(ndaymax*24),hSNO3(ndaymax*24)
      real hSLOP(ndaymax*24),hDPO4(ndaymax*24),hSPO4(ndaymax*24)
      real hSRON(ndaymax*24),hSROP(ndaymax*24)
      real hSEDM(ndaymax*24)

      real fLORN,fLORP,fPIPX

      logical foundload(nloads)

      logical debug0
c      data debug0 / .false. /
      data debug0 / .true. /
      
***************** END DECLARATION ************************************


      read*,lscen,rseg,clu

      call lencl(lscen,lenlscen)
      call lencl(rseg,lenrseg)
c      call lencl(lseg,lenlseg)
      print*,lscen
      print*,lscen(:lenlscen)

      call readcontrol_tmsce(lscen,lenlscen,clu,
     O                      sdate(1),sdate(2),sdate(3),
     O                      edate(1),edate(2),edate(3))
c     O                      lscen)
      print*,lscen,lscen
      call lencl(lscen,lenlscen)

******** READ THE CONTROL FILE FOR AFO LOAD FILE
      CLUUC = clu
      call uppercase(CLUUC)
      call readcontrol_load(lscen,lenlscen,
     .                      CLUUC//' LOADS',9,
     .                      'END '//CLUUC//' LOADS',13,
     O                      SAVGYEAR,EAVGYEAR,LBfile)

c      sdate(1) = reqSRTy
c      sdate(2) = reqSRTm
c      sdate(3) = reqSRTd
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

c      edate(1) = reqENDy
c      edate(2) = reqENDm
c      edate(3) = reqENDd
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      call readcontrol_Lioscen(
     I                          lscen,lenlscen,
     O                          ioscen)
      call lencl(ioscen,lenioscen)
      print*,ioscen(:lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/species_cvfactor'
      !SPECIES1 = 'NITR'
      !SPECIES2 = 'BODX'
      call SPECIES_CVFACTOR(fnam,'NITR','BODX',CVN2BOD)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lus_orgx'
      call getorgx(fnam,clu,fLORN,fLORP,fPIPX)
      print*,clu,' LORN fraction = ',fLORN
      print*,clu,' LORP fraction = ',fLORP
      print*,clu,' PIPX fraction = ',fPIPX

      ! TODO read afocfo_load table
      NLVAR = NLVARMAX

***** GET THE START YEAR
      year1 = sdate(1)
      year2 = edate(1)


      call getl2r(rseg,lscen,lenlscen,
     O            numsegs,l2r)

********** open dummy wdm
        wdmfnam = dummyWDMname
        call wdbopnlong(wdmfil+1,wdmfnam,0,err)

***** process land segments
      do ns = 1,numsegs
      !{

        call lencl(l2r(ns),lenlseg)
c      do l = 1,nlu
c        if ( luname(l).eq.clu .and. isloadlu(l) ) then
        wdmfnam = outwdmdir//'land/'//clu//'/'//
     .          lscen(:lenlscen)//'/'//clu//'_'//
     .          l2r(ns)(:lenlseg)//'_'//rseg(:lenrseg)//'.wdm'
        command =
     .      'cp -v '//
     .          outwdmdir//'land/'//clu//'/'//
     .          lscen(:lenlscen)//'/'//clu//
     .          l2r(ns)(:lenlseg)//'.wdm'//
     .      ' '//
c     .          outwdmdir//'river/'//rscen(:lenrscen)//'/loads/'//
c     .          lscen(:lenlscen)//'_'//clu//'_'//
c     .          l2r(ns)(:lenlseg)//'_'//rseg(:lenrseg)//'.wdm'
c     .          outwdmdir//'land/'//clu//'/'//
c     .          lscen(:lenlscen)//'/'//clu//'_'//
c     .          l2r(ns)(:lenlseg)//'_'//rseg(:lenrseg)//'.wdm'
     .          wdmfnam
              if (command(300-4:300).ne.'     ') go to 998
              print*, command
        call system(command)
c        end if
c      end do

***** initilize variable
        do nl = 1, nloads
            load(nl) = 0.0
        end do

        avgflow = 0.0
        avgsedm = 0.0
        do nl = 1, nloads
            avgload(nl) = 0.0
        end do

********read in loads            
        !{
          call trims(LBfile,last)
        
          fnam = ScenDatDir//'land/loads/'//clu//'_'//
     .       LBfile(:last)//'.csv'
          print*,fnam
          if(debug0)print*,'... reading ',fnam
          open (dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) go to 991
         
c          read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
c          do nl = 1,nloads
c            foundload(nl) = .false.
c          end do

          read(dfile,'(a300)',err=1000,end=992) dline
          call d2x(dline,last)
          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993

          call findcomma(dline,last)
          if (dline(:last-1).ne.'riverseg') go to 801

          call shift(dline)
          call findcomma(dline,last)
          if (dline(:last-1).ne.'landseg') go to 802

          do nl = 1,nloads
            foundload(nl) = .false.
            call shift(dline)
            call findcomma(dline,last)
            do i = 1,nloads
              if (dline(:last-1).eq.loadnm(i)) then
                 if ( debug0 ) 
     .             print*,'... ... found ',dline(:last-1),' ',
     .               loadnm(i),' at col ',i
                 iloads(nl) = i
                 foundload(nl) = .true.
              end if
            end do
            if (foundload(nl) .eqv. .false.) go to 803
          end do
        

          do
            read(dfile,'(a300)',err=1001,end=992) dline
            call d2x(dline,last)
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993
            call findcomma(dline,last)
            Trseg = dline(:last-1)
c            print*,'>',Trseg,'<'
            if (Trseg .eq. rseg) then         ! get river segment
              call shift(dline)
              call findcomma(dline,last)
              Tlseg = dline(:last-1)
              if (Tlseg.eq.l2r(ns)(:lenlseg)) then          ! get land segment
                if(debug0)print*,'... ... LRsegs: ',Trseg,' ',Tlseg
                do nl = 1,nloads
                  call shift(dline)
                  call fread(dline,load(iloads(nl)))
                  if(debug0)print*,'... ... ',nl,' ',loadnm(iloads(nl)),
     .                   load(iloads(nl))
                end do
              end if
            end if
          end do
c              call shift(dline)
c              call findcomma(dline,last)
c              Tlu = dline(:last-1)           
c              call lowercase(Tlu)
c              if (Tlu.eq.clu) then           ! get land use
c                call shift(dline)
c                call findcomma(dline,last)
c                Tload = dline(:last-1)         ! find load species
c                call lowercase(Tload)
c                do nl = 1,nloads+1
c                  if (nl.eq.nloads+1) go to 994
c                  if (Tload.eq.loadnm(nl)) exit
c                end do
c                foundload(nl) = .true.
c                call shift(dline)
c                call fread(dline,load(n,nl))
c              end if
c            end if
c          end if
          
c        end do

992       close (dfile)
        !}

C      do nl = 1,nloads
C        if (.not.foundload(nl)) go to 995
C      end do
     
********** rearrange variables for each species
          NO3N = load(1)+minavgann ! +1E-8 ensures non-zero EOR loads for lbsBMP factor calculation
          NH3N = load(2)+minavgann
          ORGN = load(3)+minavgann
          PO4P = load(4)+minavgann
          ORGP = load(5)+minavgann
          SEDM = load(6)+minavgann

********** create WDM variable from loads
        if(debug0)print*,'... ... ... Average Loads ',rseg,' ',
     .    l2r(ns)(:lenlseg)
          SNO3load = NO3N
          if(debug0)print*,'... ... ... SNO3 ',SNO3load
c          SLONload(year) = min(ORGN(year),ORGP(year)/NPratio)
          SLONload = ORGN * fLORN
          SRONload = ORGN * (1-fLORN)
          if(debug0)print*,'... ... ... LORN ',SLONload
          if(debug0)print*,'... ... ... RORN ',SRONload
          SNH3load = NH3N !+ORGN-SLONload
c          SPO4load(year) = PO4P(year)+ORGP(year)-SLONload(year)*NPratio
          if(debug0)print*,'... ... ... SNH3 ',SNH3load
          DPO4load = PO4P * fPIPX
          SPO4load = PO4P * (1-fPIPX)
          if(debug0)print*,'... ... ... DPO4 ',DPO4load
          if(debug0)print*,'... ... ... SPO4 ',SPO4load
          SLOPload = ORGP * fLORP
          SROPload = ORGP * (1-fLORP)
          if(debug0)print*,'... ... ... SLOP ',SLOPload
          if(debug0)print*,'... ... ... SROP ',SROPload
          SEDMload = SEDM
          if(debug0)print*,'... ... ... SEDM ',SEDMload

c      wdmfnam = outwdmdir//'land/'//clu//'/'//lscen(:lenlscen)//
c     .          '/'//clu//lseg(:lenlseg)//rseg(:lenrseg)//'.wdm'
c        wdmfnam = clu//lseg(:lenlseg)//rseg(:lenrseg)//'.wdm'
c        wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//'/loads/'//
c     .          lscen(:lenlscen)//'_'//clu//'_'//
c     .          l2r(ns)(:lenlseg)//'_'//rseg(:lenrseg)//'.wdm'
        call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open land read only
        if (err .ne. 0) go to 996

        call gethourdsn(wdmfil,sdate,edate,flowdsn,
     O                nvals,flowvals)
        call gethourdsn(wdmfil,sdate,edate,sedmdsn,
     O                nvals,sedmvals)       
********** get annual flow
        hour = 0
        year = sdate(1)
        month = sdate(2)
        day = sdate(3)
        oldyear = year

        do i = 1,nvals
          hour = hour + 1
          if ( year.ge.SAVGYEAR .and. year.le.EAVGYEAR ) then
            avgflow = avgflow + flowvals(i)
            avgsedm = avgsedm + sedmvals(i)
          end if
            
          if (hour.eq.24) then
            hour = 0
            call tomorrow(year,month,day)
          end if
        end do
        avgflow = avgflow / (EAVGYEAR-SAVGYEAR+1)
        avgsedm = avgsedm / (EAVGYEAR-SAVGYEAR+1)
          
        SNO3con= SNO3load/avgflow
        SLONcon= SLONload/avgflow
        SRONcon= SRONload/avgflow
        SNH3con= SNH3load/avgflow
        DPO4con= DPO4load/avgflow
c        DPO4con= DPO4load/avgsedm
        SPO4con= SPO4load/avgflow
        SLOPcon= SLOPload/avgflow
        SROPcon= SROPload/avgflow
        SEDMcon= SEDMload/avgflow
        print*,"SEDMcon = ",SEDMcon,SEDMload,avgflow

********** get hourly loads
        hour = 0
        year = sdate(1)
        month = sdate(2)
        day = sdate(3)
        oldyear = year

c        ySEDM = 0
        do i = 1,nvals
          hSNO3(i) = SNO3con*flowvals(i)
          hSLON(i) = SLONcon*flowvals(i)
          hSRON(i) = SRONcon*flowvals(i)
          hSNH3(i) = SNH3con*flowvals(i)
          hDPO4(i) = DPO4con*flowvals(i)
          hSPO4(i) = SPO4con*flowvals(i)
          hSLOP(i) = SLOPcon*flowvals(i)
          hSROP(i) = SROPcon*flowvals(i)
          hSEDM(i) = SEDMcon*flowvals(i)

c          ySEDM = ySEDM + hSEDM(i)

          hour = hour + 1
          if (hour.eq.24) then
            hour = 0
            call tomorrow(year,month,day)
          end if
          if (year.ne.oldyear .and. year.le.year2) then
            oldyear = year
c            print*,ySEDM
c            ySEDM = 0
          end if
        end do

******** find the loads for each land varible in WDM
        do j = 1,NLVAR
          if(Lvname(j) .eq. 'SNO3')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSNO3)
          if(Lvname(j) .eq. 'SLON') then
            if(debug0)print*,'writing SLON ',Ldsn(j)
            call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSLON)
          end if
          if(Lvname(j) .eq. 'SRON')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSRON)
          if(Lvname(j) .eq. 'SNH3')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSNH3)
          if(Lvname(j) .eq. 'DPO4')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hDPO4)
          if(Lvname(j) .eq. 'SPO4')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSPO4)
          if(Lvname(j) .eq. 'SLOP')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSLOP)
          if(Lvname(j) .eq. 'SROP')
     .      call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSROP)
          if(Lvname(j) .eq. 'SEDM') then
            if(debug0)print*,'writing SEDM ',Ldsn(j)
            call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hSEDM)
          end if
c        do i = 1,nvals
c          hval(i) = 0.
c          if(Lvname(j) .eq. 'SNO3') hval(i) = hSNO3(i)
c          if(Lvname .eq. 'SBOD') hval(i) = hSLON(i) * CVN2BOD
c          if(Lvname(j) .eq. 'SLON') hval(i) = hSLON(i)
c          if(Lvname(j) .eq. 'SNH3') hval(i) = hSNH3(i) 
c          if(Lvname(j) .eq. 'SPO4') hval(i) = hSPO4(i)
c          if(Lvname(j) .eq. 'SLOP') hval(i) = hSLOP(i) 
c          if(Lvname(j) .eq. 'SEDM') hval(i) = hSEDM(i)
c        end do
c        call puthourdsn(wdmfil,sdate,edate,Ldsn(j),nvals,hval)
        end do

        call wdflcl(wdmfil,err)
        if (err.ne.0) go to 997
      !}
      end do

      return

****************** ERROR SPACE ****************************************
801   report(1) = 'problem parsing header variables'
      report(2) = fnam
      report(3) = dline(:last-1)//' vs '//'riverseg'
      go to 999

802   report(1) = 'problem parsing header variables'
      report(2) = fnam
      report(3) = dline(:last-1)//' vs '//'landseg'
      go to 999

803   report(1) = 'problem parsing header variables'
      report(2) = fnam
      report(3) = dline(:last-1)//' vs '//loadnm(i)
      go to 999

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

998   report(1) = 'Problem with command'
      report(2) = ''
      report(2) = ''
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


