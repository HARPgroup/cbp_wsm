************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/wqm_load.inc'

********* date variables
      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (month1=1,day1=1)
      parameter (month2=12,day2=31)
      character*4 cy1,cy2,cyear
      integer doCH3D

C      include 'date.inc'
      integer Y1, Y2
c      parameter (Y1=1990, Y2=2010)
      parameter (Y1=1985, Y2=2005)
      integer I1, I2, I3, I4

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=2400)
      character*11  cell(maxcells),Tcell
      integer       intcell  ! character version of cell
      integer lencell

********* lrseg variables
      character*13 Trseg
      character*6 Tlseg
      integer maxlrsegs,nlrsegs,nlrseg
      parameter(maxlrsegs=2000)
      character*19 lrseg(maxlrsegs)

************ variables to match cells to lrsegs
      integer maxCellsPerLrseg,ncLR
      parameter(maxCellsPerLrseg=50)
      integer CellsPerLrseg(maxlrsegs)
      integer lrsegCellIndex(maxlrsegs,maxCellsPerLrseg)
      real lrsegWeight(maxlrsegs,maxCellsPerLrseg)
      real Tweight

********** rseg variables
      integer maxrsegs,nrsegs,nrseg
      parameter (maxrsegs=200)
      character*13 rsegs(maxrsegs)
     
************ variables to match cells to rsegs
      integer maxCellsPerRseg,ncR
      parameter(maxCellsPerRseg=100)
      integer CellsPerRseg(maxrsegs)
      integer rsegCellIndex(maxrsegs,maxCellsPerRseg)
      real rsegWeight(maxrsegs,maxCellsPerRseg)

*********** output load variables
      real wq(366,Y1:Y2,maxcells,maxBvar)
      real wq_m(maxBvar)
      integer nd,ny,nm,nq,Divnq,itm
      real pairwq(366,Y1:Y2,maxBvar)
      double precision AverageCell(maxBvar)

      double precision DayTotal(maxBvar),DayTotalDiv(maxBvar)
      double precision DayCell(maxBvar),DayCellDiv(maxBvar)
      double precision MonthTotal(maxBvar),MonthTotalDiv(maxBvar)
      double precision MonthCell(maxBvar),MonthCellDiv(maxBvar)
      double precision YearTotal(maxBvar),YearTotalDiv(maxBvar)
      double precision YearCell(maxBvar),YearCellDiv(maxBvar)

      integer Div_NQ(maxBvar)

      integer icell1, jcell1
      parameter ( icell1 =178,  jcell1 = 282 )
      real q(icell1,jcell1)
      integer i,j

********** ps, atdep, septic variables
      character*25 pradscen, psscen, sepscen
      integer lenpradscen, lenpsscen, lensepscen
      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/
      logical doatdep,dops,dosep
!      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'
      real wateracres(ndaymax)

************* utility variables
      integer ndaysinyear,ndaysinmonth
      external ndaysinyear,ndaysinmonth
      integer julian,jday
      external julian
      logical found,comment
      external comment
      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output
      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

******** hotstart variables  0 = do nothing, 1 = write , 2 = read
      integer hotstart
      character*19 lastlrseg
      integer hotfile  ! file number for hotstart
      parameter (hotfile = dfile + 80)  ! = 91
      integer wdmfil
      parameter (wdmfil = dfile + 9)
      character*200 Request!,Lrsegfnam,Rsegfnam
      integer lenRequest!,lenLrsegfnam,lenRsegfnam

******** ps variables
      integer nps,np
      parameter (nps=3)
      character*5 psnam(nps)         ! pointsource
      data psnam /'wwtp','indus','cso'/
      integer lenpsnam(nps)
      logical foundfile

************** PS only variables
      integer npssegs,numsegs,ns
      character*6 psl2r(maxL2R)

************ END DECLARATIONS ******************************************
      read*,rscen,hotstart,Request,year1,year2,doCH3D
********* stupid wdm thing
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open dummy read only
      if (err .ne. 0) go to 998             ! and never close

********** initialize
      do nq = 1,maxBvar ! initialize
        do nc = 1,maxcells
          do ny = year1,year2
            do nd = 1,366
              wq(nd,ny,nc,nq) = 0.0
            end do
          end do
        end do
      end do
      do nq = 1,maxBvar
          YearTotal(nq)     = 0.0
          YearTotalDiv(nq)  = 1.0
          YearCell(nq)      = 0.0
          YearCellDiv(nq)   = 1.0
          MonthTotal(nq)    = 0.0
          MonthTotalDiv(nq) = 1.0
          MonthCell(nq)     = 0.0
          MonthCellDiv(nq)  = 1.0
          DayTotal(nq)      = 0.0
          DayTotalDiv(nq)   = 1.0
          DayCell(nq)       = 0.0
          DayCellDiv(nq)    = 1.0
      end do

C      read*,rscen,hotstart,Request!,Lrsegfnam,Rsegfnam  ! get river scenario
      call lencl(rscen,lenrscen)
      call lencl(Request,lenRequest)

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_lscen(rscen,
     .                       LandScen)

********* READ THE CONTROL FILE FOR DATA SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,psscen,sepscen,
     O                     doatdep,dops,dosep)
      call lencl(psscen,lenpsscen)

********* read control file for I/O, geo, and param scenario
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call readcontrol_Rgeoscen(
     I                          rscen,lenrscen,
     O                          geoscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(ioscen,lenioscen)
      call lencl(geoscen,lengeoscen)
      call lencl(paramscen,lenparamscen)

******* POPULATE nRv2Bv, Rname2Bv, Rfactor, nRvar, nBvar, Bname,
********         Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(
     I                         rscen,lenrscen,
     O                         modules,nmod)
      call masslink(
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rdsn,Rname,RvarBMP,
     O              nLvar,Ldsn,Lname,Lfactor)

******* POPULATE similar variables for the data types
      if (dops) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,ps,
     O               nPSvar,PSdsn,PSname,PSfac)
      end if

************** POPULATE variables relating river vars to bay vars
      rbfnam = 'river_to_wqm57kPS'
      call riverbayY1Y2(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,Uname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** END OF SETUP, OPEN AND READ LRSEG LINK FILE
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/request/p532_'//Request(:lenRequest)//'_lrsegs_PS.csv'

      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading ',fnam

      ncells = 0  ! initialize variables
      nlrsegs = 0
      do nlrseg = 1,maxlrsegs
        CellsPerLrseg(nlrseg) = 0
      end do

      do 
        read(dfile-1,'(a100)',end=111,err=992) line 
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,end=992,err=992) Tcell,Tlseg,Trseg,Tweight
C        print*,'newline ',Tcell,' ',Tlseg,' ',Trseg,' ',Tweight

        found = .false.   ! find cell index
        do nc = 1,ncells
          if (Tcell.eq.cell(nc)) then
            found = .true.
            exit    ! nc preserved
          end if
        end do

        if (.not.found) then
          ncells = ncells + 1
          if (ncells.gt.maxcells) go to 981
          nc = ncells
          cell(nc) = Tcell
        end if

        found = .false.  ! find lrseg index
        do nlrseg = 1,nlrsegs
          if (lrseg(nlrseg).eq.Tlseg//Trseg) then
            found = .true.
            exit     ! nlrseg preserved
          end if
        end do

        if (.not.found) then
          nlrsegs = nlrsegs + 1
          if (nlrsegs.gt.maxlrsegs) go to 982
          nlrseg = nlrsegs
          lrseg(nlrseg) = Tlseg//Trseg
        end if
C        print*,'cell ',nc,'   lrseg ',nlrseg
        
        CellsPerLrseg(nlrseg) = CellsPerLrseg(nlrseg) + 1
C        print*,' cells per ',CellsPerLrseg(nlrseg)
        if (CellsPerLrseg(nlrseg).gt.maxCellsPerLrseg) go to 983
        lrsegCellIndex(nlrseg,CellsPerLrseg(nlrseg)) = nc
        lrsegWeight(nlrseg,CellsPerLrseg(nlrseg)) = Tweight

      end do
111   close(dfile-1)

****************** loop over all lrsegs
      if (dops)then
        
        do np = 1, nps
          call lencl(psnam(np),lenpsnam(np))

          do nlrseg = 1,nlrsegs
            Tlseg = lrseg(nlrseg)(:6)
            Trseg = lrseg(nlrseg)(7:)

          print*,'Processing LRseg ',nlrseg,' of ',nlrsegs,' LRsegs ',
     .          Tlseg,' ',Trseg

            wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .              '/'//psnam(np)(:lenpsnam(np))//'_'//
     .              Tlseg//'_to_'//Trseg//'.wdm'

            inquire(file=wdmfnam,exist=foundfile)
            if (foundfile) then

            print*,'file exists'

              call readdailydatY1Y2(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nPSvar,PSdsn,PSname,PSfac,
     O                      pairwq)
              do ncLR = 1,CellsPerLrseg(nlrseg)
                nc = lrsegCellIndex(nlrseg,ncLR)

              print*,'adding lrseg ps: ',wdmfnam,' to cell:',cell(nc)

                do nq = 1,nBvar  ! add to big storage variables
                  do ny = year1,year2
                    do nd = 1,ndaysinyear(ny)
                      wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq)
     .                            * lrsegWeight(nlrseg,ncLR)
                    end do
                  end do
                end do
              end do

            end if

          end do  ! end loop over lrsegs

        end do  ! end loop ps names 

      end if

******** DEAL WITH POINT SOURCE ONLY SEGMENTS

c          call getl2r(Trseg,rscen,lenrscen,
c     O                numsegs,l2r)
c          call getpsonlyl2r(
c     I                      Trseg,rscen,lenrscen,numsegs,l2r,
c     O                      npssegs,psl2r)
c            print*,'non physical rseg: ',npssegs,psl2r

****************** ALL DATA PROCESSED, NOW DO OUTPUTS


*********** Make Concentrations from Loads for some variables
      call ttyput(' making concentrations ')
      do nq = 1,nBvar  

        if (DivBvar(nq).eq.'    ') then
             Div_NQ(nq) = 0
             cycle
        end if

        call ttyput(Bname(nq))
        call ttyput(' ')
        found = .false.
        do Divnq = 1,nBvar  ! find divisor variable
          if (Bname(Divnq).eq.DivBvar(nq)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) go to 995

        Div_NQ(nq) = Divnq

      end do
      print*,' '

********** write out DAILY DATA
      do ny = year1,year2

        fnam = 'wsm57k_wsm_ps.YY'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        fnam = 'wsm57k_wsm_ps.YY_DT'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

c        write(dfile,1233)
c     .     '           ','year','mnth',(Bname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     '           ','year','mnth',(Bname(nq),nq=1,nBvar)

c        write(dfile,1233)
c     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          if (day.eq.1) then
           do nq=1,nBvar
             DayTotal(nq)    = 0
             DayTotalDiv(nq) = 0
           end do
           do nc = 1,ncells
              do nq=1,nBvar
                 DayCell(nq)    = 0
                 DayCellDiv(nq) = 0
              end do
C              if (writecell(nc)) then
              do nq=1,nBvar
                 DayCell(nq) = wq(nd,ny,nc,nq)
                 DayTotal(nq) = DayTotal(nq) + wq(nd,ny,nc,nq)
                 if ( Div_NQ(nq) > 0 ) then
                    DayCellDiv(nq) = wq(nd,ny,nc,Div_NQ(nq))
                    DayTotalDiv(nq) = DayTotalDiv(nq) 
     .                                 + wq(nd,ny,nc,Div_NQ(nq))
                 else
                    DayCellDiv(nq) = 1
                    DayTotalDiv(nq) = 1
                 end if
              end do

              do nq=1,nBvar
                 if ( DayCellDiv(nq) .lt. 0.000001 ) then
                       DayCell(nq)    = 0
                       DayCellDiv(nq) = 1
                 end if
              end do
              write(dfile,1234)
     .          cell(nc),year,month,
     .          (DayCell(nq)/DayCellDiv(nq),nq=1,nBvar)
C              end if
           end do


           do nq=1,nBvar
              if ( DayTotalDiv(nq) .lt. 0.000001 ) then
                    DayTotal(nq)    = 0
                    DayTotalDiv(nq) = 1
              end if
           end do
           write(dfile+1,1234)
     .          'TOTAL      ',year,month,
     .          (DayTotal(nq)/DayTotalDiv(nq),nq=1,nBvar)
          end if
          call tomorrow(year,month,day)
        end do
        close(dfile)
        close(dfile+1)
      end do

********** write out MONTHLY DATA
      do ny = year1,year2

        fnam = 'wsm57k_wsm_ps.YY_MC'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        fnam = 'wsm57k_wsm_ps.YY_MT'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        write(dfile,1233)
     .     'CB Segment ','year','mnth',(Bname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     'CB Segment ','year','mnth',(Bname(nq),nq=1,nBvar)

        write(dfile,1233)
     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)
        year = ny
        month = 1
        day = 1
        nd = 1
        do nm = 1,12
           do nq=1,nBvar
             MonthTotal(nq)    = 0
             MonthTotalDiv(nq) = 0
           end do
           do nc = 1,ncells
              do nq=1,nBvar
                 MonthCell(nq)    = 0
                 MonthCellDiv(nq) = 0
              end do
C              if (writecell(nc)) then
              do nq=1,nBvar
                 MonthCell(nq) = wq(nd,ny,nc,nq) * ndaysinmonth(ny,nm)
                 MonthTotal(nq) = MonthTotal(nq) 
     .                           + wq(nd,ny,nc,nq) * ndaysinmonth(ny,nm)
                 if ( Div_NQ(nq) > 0 ) then
                    MonthCellDiv(nq) = wq(nd,ny,nc,Div_NQ(nq))  
     .                                 * ndaysinmonth(ny,nm)
                    MonthTotalDiv(nq) = MonthTotalDiv(nq) 
     .                   + wq(nd,ny,nc,Div_NQ(nq)) * ndaysinmonth(ny,nm)
                 else
                    MonthCellDiv(nq) = 1
                    MonthTotalDiv(nq) = 1
                 end if
              end do

              do nq=1,nBvar
                 if ( MonthCellDiv(nq) .lt. 0.000001 ) then
                       MonthCell(nq)    = 0
                       MonthCellDiv(nq) = 1
                 end if
              end do
              write(dfile,1234)
     .          cell(nc),ny,nm,
     .          (MonthCell(nq)/MonthCellDiv(nq),nq=1,nBvar)
C              end if
           end do
           nd = nd + ndaysinmonth(ny,nm)

           do nq=1,nBvar
              if ( MonthTotalDiv(nq) .lt. 0.000001 ) then
                    MonthTotal(nq)    = 0
                    MonthTotalDiv(nq) = 1
              end if
           end do
           write(dfile+1,1234)
     .          'TOTAL      ',ny,nm,
     .          (MonthTotal(nq)/MonthTotalDiv(nq),nq=1,nBvar)
         end do

         close(dfile)
         close(dfile+1)
        end do

********** write out ANNUAL DATA
      do ny = year1,year2

        fnam = 'wsm57k_wsm_ps.YY_YC'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        fnam = 'wsm57k_wsm_ps.YY_YT'           ! open file
        write(fnam(13:16),'(i4)') ny
        fnam(13:14) = 's.'
        fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .     '/'//rscen(:lenrscen)//'/'//fnam
        open(dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:80)

        write(dfile,1233)
     .     'CB Segment ','year','mnth',(Bname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     'CB segment ','year','mnth',(Bname(nq),nq=1,nBvar)

        write(dfile,1233)
     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)
        write(dfile+1,1233)
     .     '           ','    ','    ',(Uname(nq),nq=1,nBvar)
        year = ny
        month = 1
        day = 1

        do nq=1,nBvar
           YearTotal(nq)    = 0
           YearTotalDiv(nq) = 0
        end do
        do nc = 1,ncells
           do nq=1,nBvar
              YearCell(nq)    = 0
              YearCellDiv(nq) = 0
           end do
           nd = 1
           do nm = 1,12
C              if (writecell(nc)) then
              do nq=1,nBvar
                 YearCell(nq) = YearCell(nq) 
     .                          + wq(nd,ny,nc,nq) * ndaysinmonth(ny,nm)
                 YearTotal(nq) = YearTotal(nq)
     .                          + wq(nd,ny,nc,nq) * ndaysinmonth(ny,nm)
                 if ( Div_NQ(nq) > 0 ) then
                    YearCellDiv(nq) = YearCellDiv(nq)
     .                   + wq(nd,ny,nc,Div_NQ(nq)) * ndaysinmonth(ny,nm)
                    YearTotalDiv(nq) = YearTotalDiv(nq)
     .                   + wq(nd,ny,nc,Div_NQ(nq)) * ndaysinmonth(ny,nm)
                 else
                    YearCellDiv(nq)  = 1
                    YearTotalDiv(nq) = 1
                 end if
              end do
              nd = nd + ndaysinmonth(ny,nm)
           end do

           do nq=1,nBvar
               if ( YearCellDiv(nq) .lt. 0.000001 ) then
                    YearCell(nq)    = 0
                    YearCellDiv(nq) = 1
               end if
           end do
           write(dfile,1234)
     .          cell(nc),ny,1,
     .          (YearCell(nq)/YearCellDiv(nq),nq=1,nBvar)
C              end if
        end do

        do nq=1,nBvar
            if ( YearTotalDiv(nq) .lt. 0.000001 ) then
                 YearTotal(nq)    = 0
                 YearTotalDiv(nq) = 1
            end if
        end do
        write(dfile+1,1234)
     .          'TOTAL      ',ny,1,
     .          (YearTotal(nq)/YearTotalDiv(nq),nq=1,nBvar)

        close(dfile)
        close(dfile+1)
      end do


********** CH3D OUTPUT **********
      if ( doCH3D.eq.1 ) then
       itm = 0
       do ny = year2,year2
           write(cyear,'(i4)') ny
           fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .            '/'//rscen(:lenrscen)//'/'//
     .            '/fortPS.33_new_'//cyear
C           print*,'BHATT $$',fnam
           open(33,file=fnam,status='unknown')
           fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .            '/'//rscen(:lenrscen)//'/'//
     .            '/fortPS.34_new_'//cyear
           open(34,file=fnam,status='unknown')
           fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .            '/'//rscen(:lenrscen)//'/'//
     .            '/fortPS.13_new_'//cyear
           open(13,file=fnam,status='unknown')
           fnam = outdir//'wqm_input/'//Request(:lenRequest)//
     .            '/'//rscen(:lenrscen)//'/'//
     .            '/fortPS.78_new_'//cyear
           open(78,file=fnam,status='unknown')

c          if (dops)then
c              do np = 1, nps
c                   do nlrseg = 1,nlrsegs
c                       do ncLR = 1,CellsPerLrseg(nlrseg)
c                           nc = lrsegCellIndex(nlrseg,ncLR)
c                           do nq = 1,nBvar  ! add to big storage variables
c                               do ny = year1,year2
c                                   do nd = 1,ndaysinyear(ny)
c                                       wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq) 
c     .                                        + pairwq(nd,ny,nq)
c     .                                        * lrsegWeight(nlrseg,ncLR) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ! FILE 13 START
           do nq = 1,nBvar
                if(Bname(nq).eq.'flow') then
                     print*,'nq = ',nq
                     exit
                end if
           end do
          ! LAST 5 DAYS OF THE (YEAR2 - 1)
           do nd = ndaysinyear(ny-1)-4,ndaysinyear(ny-1)
                write(13,301) nd-ndaysinyear(ny-1)-1,itm
C                write(33,201) nd-ndaysinyear(ny-1)-1,itm
                do nc = 1,ncells
                   read(cell(nc),*) intcell
                   write(13,302) INT(intcell/1000),MOD(intcell,1000),
     .                         (35.314666*wq(nd,ny-1,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! ONE YEAR OF DATA FOR THE YEAR2
           do nd = 1,ndaysinyear(ny)
                write(13,301) nd-1,itm

                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    write(13,302) INT(intcell/1000),MOD(intcell,1000),
     .                          (35.314666*wq(nd,ny,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! REPEAT LAST DAY OF THE PREVIOUS YEAR2
           do nd = ndaysinyear(ny),ndaysinyear(ny)
                write(13,301) nd,itm

                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    write(13,302) INT(intcell/1000),MOD(intcell,1000),
     .                          (35.314666*wq(nd,ny,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! FILE 13 END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ! FILE 33 START
          ! LAST 5 DAYS OF THE (YEAR2 - 1)
           do nd = ndaysinyear(ny-1)-4,ndaysinyear(ny-1)
                write(33,201) nd-ndaysinyear(ny-1)-1,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 0.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      35.314666*(wq(nd,ny-1,nc,1))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(33,211)(q(i,j),i=1,icell1)
                end do
           end do
          ! ONE YEAR OF DATA FOR THE YEAR2
           do nd = 1,ndaysinyear(ny)
                write(33,201) nd-1,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 0.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      35.314666*(wq(nd,ny-1,nc,1))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(33,211)(q(i,j),i=1,icell1)
                end do
           end do
          ! REPEAT LAST DAY OF THE PREVIOUS YEAR2
           do nd = ndaysinyear(ny),ndaysinyear(ny)
                write(33,201) nd,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 0.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      35.314666*(wq(nd,ny-1,nc,1))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(33,211)(q(i,j),i=1,icell1)
                end do
           end do
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          ! FILE 78 START
           do nq = 1,nBvar
                if(Bname(nq).eq.'temp') then
                     print*,'nq = ',nq
                     exit
                end if
           end do
          ! LAST 5 DAYS OF THE (YEAR2 - 1)
           do nd = ndaysinyear(ny-1)-4,ndaysinyear(ny-1)
                write(78,304) nd-ndaysinyear(ny-1)-1,itm
C                write(33,201) nd-ndaysinyear(ny-1)-1,itm
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    write(78,303) INT(intcell/1000),MOD(intcell,1000),
     .                          (1*wq(nd,ny-1,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! ONE YEAR OF DATA FOR THE YEAR2
           do nd = 1,ndaysinyear(ny)
                write(78,304) nd-1,itm

                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    write(78,303) INT(intcell/1000),MOD(intcell,1000),
     .                          (1*wq(nd,ny,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! REPEAT LAST DAY OF THE PREVIOUS YEAR2
           do nd = ndaysinyear(ny),ndaysinyear(ny)
                write(78,304) nd,itm

                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    write(78,303) INT(intcell/1000),MOD(intcell,1000),
     .                          (1*wq(nd,ny,nc,nq))
                                ! 1 cmd = 35.314666 cfs
c cell(nc)  imon(ic(j)),jmon(ic(j)),
                end do
C                write(13,302) 134,282,750.
           end do
          ! FILE 13 END
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          ! FILE 34 START
          ! LAST 5 DAYS OF THE (YEAR2 - 1)
           do nd = ndaysinyear(ny-1)-4,ndaysinyear(ny-1)
                write(34,201) nd-ndaysinyear(ny-1)-1,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 20.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      1*(wq(nd,ny-1,nc,nq))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(34,211)(q(i,j),i=1,icell1)
                end do
           end do
          ! ONE YEAR OF DATA FOR THE YEAR2
           do nd = 1,ndaysinyear(ny)
                write(34,201) nd-1,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 20.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      1*(wq(nd,ny-1,nc,nq))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(34,211)(q(i,j),i=1,icell1)
                end do
           end do
          ! REPEAT LAST DAY OF THE PREVIOUS YEAR2
           do nd = ndaysinyear(ny),ndaysinyear(ny)
                write(34,201) nd,itm
                do i=1,icell1
                   do j=1,jcell1
                      q(i,j) = 20.0
                   end do
                end do
                do nc = 1,ncells
                    read(cell(nc),*) intcell
                    q(INT(intcell/1000),MOD(intcell,1000))= 0.0 +
     .                      1*(wq(nd,ny-1,nc,nq))
                            ! 1 cmd = 35.314666 cfs
                end do
                do j=1,jcell1
                    write(34,211)(q(i,j),i=1,icell1)
                end do
           end do
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

           close(13)
           close(33)
           close(34)
           close(78)
       end do

      end if

      stop
1233  format(A11,2(',',A4),18(',',A10))
1234  format(A11,2(',',i4),18(',',e10.4))


 201  format(2i8)
 211  format(22f7.1)
 301  FORMAT(2I8)
 302  FORMAT(2I8,F8.0)
 303  FORMAT(2I5,4f6.2)
 304  FORMAT(2I5)

********************************* ERROR SPACE **************************
981   report(1) = 'too many cells in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxcells to greater than ',
     .                  maxcells
      go to 999

982   report(1) = 'too many lrsegs in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxlrsegs to greater than ',
     .                  maxlrsegs
      go to 999

983   report(1) = 'too many lrsegs-cell linkages in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxCellsPerLrseg ',
     .                  'to greater than ',maxCellsPerLrseg
      go to 999

984   report(1) = 'too many rsegs in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxrsegs to greater than ',
     .                  maxrsegs
      go to 999

985   report(1) = 'too many rsegs-cell linkages in file '
      report(2) = fnam
      write(report(3),*)'increase variable maxCellsPerRseg ',
     .                  'to greater than ',maxCellsPerRseg
      go to 999

989   report(1) = 'error closing wdm file'
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Variable psmethod specifed as input must be:'
      report(2) = 'hcell, wcell, or lrseg'
      report(3) = ' for point sources by ch3d cell, wqm cell, or lrseg'
      go to 999

994   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'Attempting to make concentrations'
      report(2) = 'can not find time series for variable '//DivBvar(nq)
      report(3) = 'requested for bay variable '//Bname(nq)
      go to 999

996   report(1) = 'should not reach this point in the program'
      report(2) = ' ps and nps are done separately '
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

