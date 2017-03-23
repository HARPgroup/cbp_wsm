************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/transfer_wdm.inc'

********* date variables
      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (month1=1,day1=1)
      parameter (month2=12,day2=31)
      character*4 cy1,cy2

      include 'date.inc'

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=2400)
      integer cell(maxcells)
      character*6 ccell  ! character version of cell
      integer lencell

********* lrseg variables
      character*13 trseg
      character*6 tlseg

      integer npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=10)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      real pairweight  ! fraction of this LRseg or rseg going to cell

*********** output load variables
      real wq(366,year1:year2,maxcells,maxBvar)
      integer nd,ny,nq,Divnq
      real pairwq(366,year1:year2,maxBvar)
      double precision AnnualTotal(year1:year2,maxBvar)  ! print out annual totals
      double precision AnnualCell(year1:year2,maxcells,maxBvar)

********** ps, atdep, septic variables
      character*25 pradscen, psscen, sepscen
      integer lenpradscen, lenpsscen, lensepscen

      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/
      logical doatdep,dops,dosep

      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'

      real wateracres(ndaymax)

************* utility variables
      integer ndaysinyear
      external ndaysinyear

      integer julian,jday
      external julian

      logical found

      character*300 bigline  ! file reading variable

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

******** hotstart variables  0 = do nothing, 1 = write , 2 = read
      integer hotstart, lastcell
      integer hotfile  ! file number for hotstart
      parameter (hotfile = dfile + 80)  ! = 91

      integer wdmfil
      parameter (wdmfil = dfile + 9)

      integer nfalllines,nf
      parameter (nfalllines = 17)
      integer falllines(nfalllines)
      data falllines /4820,6030,7070,7480,6750,4650,6620,2720,4590,
     .                4562,5570,4100,3520,3830,5270,5430,3980/

************ END DECLARATIONS ******************************************

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
        do ny = year1,year2
          AnnualTotal(ny,nq) = 0.0
        end do
      end do
      do nq = 1,maxBvar
        do nc = 1,maxcells
          do ny = year1,year2
            AnnualCell(ny,nc,nq) = 0.0
          end do
        end do
      end do

      read*,rscen,psmethod,hotstart    ! get river scenario
      call lencl(rscen,lenrscen)
      if (.not.(psmethod.eq.'hcell'.or.psmethod.eq.'wcell'.or.
     .          psmethod.eq.'lrseg')) go to 993

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_twdm(rscen,lenrscen,
     .                      LandScen)

********* READ THE CONTROL FILE FOR DATA SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,psscen,sepscen,
     O                     doatdep,dops,dosep)
      call lencl(psscen,lenpsscen)
      call lencl(sepscen,lensepscen)
      call lencl(pradscen,lenpradscen)
      dops = .false.  ! done in separate program

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

      if (dosep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,sep,
     O               nSEPvar,SEPdsn,SEPname,SEPfac)
      end if

      if (doatdep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,atdep,
     O               nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)
      end if

************** POPULATE variables relating river vars to bay vars
      rbfnam = 'river_to_wqm57kNPS'
      call riverbay(
     I              ioscen,lenioscen,rbfnam,
     I              nRvar,Rname,
     O              nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivBvar)

********** get river information
      call getriver(
     I              geoscen,lengeoscen,
     O              rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)

      tlseg(1:1) = 'A'

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/p5_to_57k_wqm.prn'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile-1,'(a300)') bigline    ! get rid of header lines

      read(dfile-1,'(a300)',err = 992,end=992) bigline 
      call d2x(bigline,last)   

************ CHECK FOR HOT START FILE
      if (hotstart.eq.2) then
        print*,'getting output for hotstart'
        fnam= outdir//'wqm_input/kluge_'//rscen(:lenrscen)//'/'//
     .  'hotstart_wqm57_nps.bin'
        open(hotfile,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 991
        read(hotfile)wq,lastcell
        close(hotfile)
      end if
        

****************** loop over cells in file
      nc = 0
      do while (bigline(:3).ne.'end')
        nc = nc + 1  ! increment number of cells
        read(bigline,*,err=992,end=992)
     .    cell(nc),npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        if (npairs.gt.maxpairs) go to 994
        print*,' '
        print*,'cell ',cell(nc)
        if (hotstart.eq.2) then
          if (cell(nc).ne.lastcell) then
            read(dfile-1,'(a300)',err = 992,end=992) bigline
            call d2x(bigline,last)   
            cycle
          else
            hotstart = 1
            read(dfile-1,'(a300)',err = 992,end=992) bigline
            call d2x(bigline,last)   

            cycle
          end if
        end if

*********************** loop over Land-river pairs in cell
        do np = 1,npairs

          trseg = rsegs(uniqindex(riverid(np)))  ! get river name

          call ttyput(trseg)

          if (fips(np).eq.0) then  ! river 
            call getrflags(
     I                     paramscen,lenparamscen,trseg,
     O                     Nexits,lakeflag,resflag,timestep)

            call Rmasslink(
     I                     ioscen,lenioscen,
     I                     Nexits,modules,nmod,
     O                     nRvarOut,RdsnOut,RnameOut)

            call readriver(
     I                     rscen,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I                     nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                     nRvarOut,RdsnOut,RnameOut,
     O                     pairwq) ! get the data
            do nq = 1,nBvar  ! add to big storage variables
              if (Bname(nq).eq.'tssx'.or.Bname(nq).eq.'sand'.or.
     .            Bname(nq).eq.'silt'.or.Bname(nq).eq.'clay') then
                found = .false.
                do nf = 1,nfalllines
                  if (riverid(np).eq.falllines(nf)) found = .true.
                end do
                if (found) then
                  print*,'skipping ',riverid(np),' ',Bname(nq)
                  cycle
                end if
              end if
              do ny = year1,year2
                do nd = 1,ndaysinyear(ny)
                  wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq) * weight(np)
                  AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                               + pairwq(nd,ny,nq) * weight(np)
                  AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                               + pairwq(nd,ny,nq) * weight(np)
                end do
              end do
            end do

            print*,'   ' 
          else  ! land
            write(tlseg(2:6),'(i5)')fips(np)  ! convert to character
            call ttyput(' ')
            call ttyput(tlseg)
            call readpair(rscen,tlseg,trseg,np,
     I                    ioscen,lenioscen,
     I                    year1,month1,day1,year2,month2,day2,
     I                    nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                    nRvar,Rname,RvarBMP,
     I                    nLvar,Ldsn,Lname,Lfactor,LandScen,
     O                    pairwq)  ! get the data
            do nq = 1,nBvar  ! add to big storage variables
              do ny = year1,year2
                do nd = 1,ndaysinyear(ny)
                  wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq) * weight(np)
                  AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                               + pairwq(nd,ny,nq) * weight(np)
                  AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                               + pairwq(nd,ny,nq) * weight(np)
                end do
              end do
            end do

            if (dosep) then
              call ttyput(' septic')
              wdmfnam = ScenDatDir//'river/septic/'//
     .                  sepscen(:lensepscen)//
     .                  '/septic_'//tlseg//'_to_'//trseg//'.wdm'
              call readdailydat(
     I                          year1,month1,day1,year2,month2,day2,
     I                          nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                          wdmfnam,nRvar,Rname,Rdsn,
     I                          nSEPvar,SEPdsn,SEPname,SEPfac,
     O                          pairwq)
              do nq = 1,nBvar  ! add to big storage variables
                do ny = year1,year2
                  do nd = 1,ndaysinyear(ny)
                    wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                              + pairwq(nd,ny,nq) * weight(np)
                    AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                                 + pairwq(nd,ny,nq) * weight(np)
                    AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                                 + pairwq(nd,ny,nq) * weight(np)
                  end do
                end do
              end do
            end if

            if (dops.and.psmethod.eq.'lrseg') then
              call ttyput(' ps')
              wdmfnam = ScenDatDir//'river/ps/'//
     .                  psscen(:lenpsscen)//'/ps_'//
     .                    tlseg//'_to_'//trseg//'.wdm'
              call readdailydat(
     I                          year1,month1,day1,year2,month2,day2,
     I                          nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                          wdmfnam,nRvar,Rname,Rdsn,
     I                          nPSvar,PSdsn,PSname,PSfac,
     O                          pairwq)
              do nq = 1,nBvar  ! add to big storage variables
                do ny = year1,year2
                  do nd = 1,ndaysinyear(ny)
                    wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                              + pairwq(nd,ny,nq) * weight(np)
                    AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                                 + pairwq(nd,ny,nq) * weight(np)
                    AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                                 + pairwq(nd,ny,nq) * weight(np)
                  end do
                end do
              end do
            end if

            if (doatdep) then
              call ttyput(' atdep')
              wdmfnam = ScenDatDir//'climate/prad/'//
     .                  pradscen(:lenpradscen)//
     .                  '/prad_'//tlseg//'.wdm'

              call getwateracres(
     I                           rscen,lenrscen,trseg,tlseg,
     I                           year1,month1,day1,
     I                           year2,month2,day2,
     O                           wateracres)
              call readdailydat(
     I                           year1,month1,day1,year2,month2,day2,
     I                           nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                           wdmfnam,nRvar,Rname,Rdsn,
     I                           nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac,
     O                           pairwq)
              do nq = 1,nBvar  ! add to big storage variables
                jday = 1
                do ny = year1,year2
                  do nd = 1,ndaysinyear(ny)
                    wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)+ pairwq(nd,ny,nq) 
     .                              * weight(np) * wateracres(jday)
                    AnnualTotal(ny,nq) = AnnualTotal(ny,nq) 
     .                                 + pairwq(nd,ny,nq) * weight(np) 
     .                                 * wateracres(jday)
                    AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                                 + pairwq(nd,ny,nq) * weight(np)
     .                                 * wateracres(jday)
                    jday = jday + 1
                  end do
                end do
              end do
            end if

            print*,'   ' 

          end if  ! end switch river or lrseg

        end do  ! end loop over pairs

        if (dops.and.psmethod.ne.'lrseg') then
          call ttyput(' ps')
          write(ccell,'(i6)') cell(nc)
          call trims(ccell,lencell)
          if (psmethod.eq.'hcell') then
            wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .                '/bay_models/ps_ch3d57k_'//ccell(:lencell)//'.wdm'
          else if (psmethod.eq.'wcell') then
            wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .                '/bay_models/ps_wqm57k_'//ccell(:lencell)//'.wdm'
          end if
          call readdailydat(
     I                      year1,month1,day1,year2,month2,day2,
     I                      nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                      wdmfnam,nRvar,Rname,Rdsn,
     I                      nPSvar,PSdsn,PSname,PSfac,
     O                      pairwq)
          do nq = 1,nBvar  ! add to big storage variables
            do ny = year1,year2
              do nd = 1,ndaysinyear(ny)
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)+ pairwq(nd,ny,nq) 
                AnnualTotal(ny,nq) = AnnualTotal(ny,nq)+pairwq(nd,ny,nq)
                AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq)
     .                               + pairwq(nd,ny,nq)
              end do
            end do
          end do
        end if
          
**************** store output for hot start
        if (hotstart.eq.1) then
          print*,'storing output for hotstart'
          fnam = outdir//'wqm_input/kluge_'//rscen(:lenrscen)//'/'//
     .           'hotstart_wqm57_nps.bin'
          open(hotfile,file=fnam,form='unformatted',
     .         status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(hotfile)wq,cell(nc)
          close(hotfile)
        end if

********* read next line
        read(dfile-1,'(a300)',err = 992,end=992) bigline    
        call d2x(bigline,last)   

      end do      ! end loop over cells
      close(dfile-1)

********** store total number of cells
      ncells = nc


*********** Make Concentrations from Loads for some variables
      call ttyput(' making concentrations ')
      do nq = 1,nBvar  

        if (DivBvar(nq).eq.'    ') cycle  ! no division necessary

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

        do nc = 1,ncells  ! divide
          do ny = year1,year2
            do nd = 1,ndaysinyear(ny)
              if (wq(nd,ny,nc,Divnq).gt.0.000001) then
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)/wq(nd,ny,nc,Divnq)
              else
                wq(nd,ny,nc,nq) = 0.0
              end if
            end do
          end do
        end do
        do nc = 1,ncells
          do ny = year1,year2
            AnnualCell(ny,nc,nq) = AnnualCell(ny,nc,nq) 
     .                           / AnnualCell(ny,nc,Divnq)
          end do
        end do
        do ny = year1,year2
          AnnualTotal(ny,nq) = AnnualTotal(ny,nq)/AnnualTotal(ny,Divnq)
        end do
      end do
      print*,' '

******************** got all data, now write to file
      do ny = year1,year2

        fnam = 'wsm57k_wsm_nps.YY'           ! open file
        write(fnam(14:17),'(i4)') ny
        fnam(14:15) = 's.'
        fnam = outdir//'wqm_input/kluge_'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        print*,'writing file ',fnam(:50)

        year = ny
        month = 1
        day = 1
        do nd = 1,ndaysinyear(ny)
          do nc = 1,ncells
            write(dfile,1234)
     .            cell(nc),year,month,day,(wq(nd,ny,nc,nq),nq=1,nBvar)
          end do
          call tomorrow(year,month,day)
        end do
        close(dfile)
      end do

********* write out annual totals
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      fnam = outdir//'wqm_input/kluge_'//rscen(:lenrscen)//
     .       '/annual_out_wqm57_nps_'//cy1//'_'//cy2//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:60)

      write(dfile,'(a4,22(a1,a4))')'year',(',',Bname(nq),nq=1,nBvar)
      do ny = year1,year2
        write(dfile,'(i4,22(a1,e10.3))')
     .               ny,(',',AnnualTotal(ny,nq),nq=1,nBvar)
      end do
      close(dfile)
      
********* write out annual totals by cell
      fnam = outdir//'wqm_input/kluge_'//rscen(:lenrscen)//
     .       '/annual_out_wqm57_nps_'//cy1//'_'//cy2//'_by_cell.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing file ',fnam(:60)

      write(dfile,'(a9,22(a1,a4))')
     .        'year,cell',(',',Bname(nq),nq=1,nBvar)
      do nc = 1,ncells
        do ny = year1,year2
          write(dfile,'(i4,a1,i6,22(a1,e10.3))')
     .          ny,',',cell(nc),(',',AnnualCell(ny,nc,nq),nq=1,nBvar)
        end do
      end do
      close(dfile)
      

      stop
1234  format(i5,3(',',i4),18(',',e10.4))

********************************* ERROR SPACE **************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

993   report(1) = 'Variable psmethod specifed as input must be:'
      report(2) = 'hcell, wcell, or lrseg'
      report(3) = ' for point sources by ch3d cell, wqm cell, or lrseg'
      go to 999

994   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

995   report(1) = 'Attempting to make concentrations'
      report(2) = 'can not find time series for variable '//DivBvar(nq)
      report(3) = 'requested for bay variable '//Bname(nq)
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

