************************************************************************
**  program to generate the input file for the 57k Bay model          **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all constituents                  **
************************************************************************

      implicit none

      include '../wqmlib/transfer_wdm.inc'

********* date variables
      integer month,day,oldmonth,oldny
      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1985,month1=1,day1=1)
      parameter (year2=2005,month2=12,day2=31)

********** wqm cell variables
      integer maxcells,ncells,nc
      parameter (maxcells=26)
      character*15 cell(maxcells),lastcell
      character*6 ccell  ! character version of cell
      integer lencell

********* lrseg variables
      character*13 trseg
      character*6 tlseg

      integer npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=90)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      real pairweight  ! fraction of this LRseg or rseg going to cell

*********** output load variables
      real wq(366,year1:year2,maxcells,maxBvar)
      integer nd,ny,nq,Divnq
      real pairwq(366,year1:year2,maxBvar)
      double precision AnnualTotal(year1:year2,maxBvar)  ! print out annual totals

********** ps, atdep, septic variables
      character*25 pradscen, psscen, sepscen
      integer lenpradscen, lenpsscen, lensepscen

      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/
      logical doatdep,dops,dosep

      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'

      real wateracres(ndaymax)

************* utility variables
      integer ndaysinyear,ndaysinmonth
      external ndaysinyear,ndaysinmonth

      integer julian,jday
      external julian

      logical found

      character*1200 bigline  ! file reading variable

      character*100 linkfile

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

      double precision monthtot(maxBvar)
      integer nsummer,issummer
      integer nlags,nl
      parameter (nlags = 7)
      real lag(0:nlags)

      integer hotstart

************ END DECLARATIONS ******************************************
      print*,'running boynton loads'

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
      rbfnam = 'river_to_boynton'
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
      linkfile = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/p5_to_boynton.prn'
      open(dfile-1,file=linkfile,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile-1,'(a1200)') bigline    ! get rid of header lines

      read(dfile-1,'(a1200)',err = 992,end=992) bigline    

************ CHECK FOR HOT START FILE
      if (hotstart.ne.0) then
        print*,'getting output for hotstart'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//
     .         'hotstart_bps.bin'
        open(dfile+30,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 990
        read(dfile+30)wq,lastcell
        close(dfile+30)
      end if
        

****************** loop over cells in file
      nc = 0
      do while (bigline(:3).ne.'end')
        nc = nc + 1  ! increment number of cells
        read(bigline,*,err=992,end=992)
     .    cell(nc),npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        if (npairs.gt.maxpairs) go to 994
        print*,' '
        print*,cell(nc)
        if (hotstart.ne.0) then
          if (cell(nc).ne.lastcell) then
            read(dfile-1,'(a1200)',err = 992,end=992) bigline
            cycle
          else
            hotstart = 0
            read(dfile-1,'(a1200)',err = 992,end=992) bigline

            cycle
          end if
        end if

*********************** loop over Land-river pairs in cell
        do np = 1,npairs

          trseg = rsegs(uniqindex(riverid(np)))  ! get river name

          print*,'   ' 
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
     O                     pairwq)  ! get the data
            do nq = 1,nBvar  ! add to big storage variables
              do ny = year1,year2
                do nd = 1,ndaysinyear(ny)
                  wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                            + pairwq(nd,ny,nq) * weight(np)
                  AnnualTotal(ny,nq) = AnnualTotal(ny,nq)
     .                               + pairwq(nd,ny,nq) * weight(np)
                end do
              end do
            end do

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
                  end do
                end do
              end do
            end if

            if (doatdep) then
              call ttyput(' atdep')
              wdmfnam = ScenDatDir//'river/ps/'//
     .                  psscen(:lenpsscen)//'/ps_'//
     .                    tlseg//'_to_'//trseg//'.wdm'

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
              end do
            end do
          end do
        end if
          
**************** store output for hot start
        print*,'storing output for hotstart'
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//
     .         'hotstart_bps.bin'
        open(dfile+30,file=fnam,form='unformatted',
     .       status='unknown',iostat=err)
        if (err.ne.0) go to 990
        write(dfile+30)wq,cell(nc)
        close(dfile+30)

        read(dfile-1,'(a1200)',err = 992,end=992) bigline    

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
        do ny = year1,year2
          AnnualTotal(ny,nq) = AnnualTotal(ny,nq)/AnnualTotal(ny,Divnq)
        end do
      end do

******************** got all data, now write to file
      do nc = 1,ncells
        call lencl(cell(nc),lencell)
        fnam = outdir//'wqm_input/'//rscen(:lenrscen)//'/'//
     .         'boynton_with_ps_'//cell(nc)(:lencell)//'.csv'! open file

        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 990
        print*,'writing file ',fnam(:50)

        write(dfile,'(a)')'year,month,flow,nh4,no23,tn,dip,tp'

        ny = year1
        month = 1
        day = 1  ! day of month
        nd = 1  ! day of year
        oldmonth = month
        oldny = ny
        do nq = 1,nBvar
          monthtot(nq) = 0.0
        end do
        do while (ny.le.year2)

          if (oldmonth.ne.month) then
            do nq = 1,nBvar
              if (Bname(nq).eq.'flow') then
                monthtot(nq) = monthtot(nq)/ndaysinmonth(oldny,oldmonth)
              end if
            end do
            write(dfile,1234) oldny,oldmonth,(monthtot(nq),nq=1,nBvar)
            oldmonth = month
            oldny = ny
            do nq = 1,nBvar
              monthtot(nq) = 0.0
            end do
          end if

          do nq = 1,nBvar
            monthtot(nq) = monthtot(nq) + wq(nd,ny,nc,nq)
          end do

          call tomorrow(ny,month,day)
          nd = nd + 1
          if (month.eq.1.and.day.eq.1) nd = 1

        end do
        write(dfile,1234) oldny,oldmonth,(monthtot(nq),nq=1,nBvar)

        close(dfile)
      end do

      stop
1234  format(i4,',',i2,6(',',e10.4))

********************************* ERROR SPACE **************************
990   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = linkfile
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file near line'
      report(2) = linkfile
      report(3) = bigline(:64)
      go to 999

993   report(1) = 'Variable psmethod specifed as input must be:'
      report(2) = 'hcell, wcell, or lrseg'
      report(3) = ' for point sources by ch3d cell, wqm cell, or lrseg'
      go to 999

994   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = linkfile
      report(3) = bigline(:64)
      go to 999

995   report(1) = 'Attempting to make concentrations'
      report(2) = 'can not find time series for variable '//DivBvar(nq)
      report(3) = 'requested for bay variable '//Bname(nq)
      go to 999

999   call stopreport(report)

      end

