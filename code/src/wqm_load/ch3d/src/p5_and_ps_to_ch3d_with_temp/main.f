************************************************************************
**  program to generate the input file for ch3d                       **
**    loops over all cells in water quality model                     **
**      loops over all inputs                                         **
**        creates one variable with all q's                           **
************************************************************************

      implicit none

      include 'transfer_wdm.inc'
      include '../lib/inc/rsegs.inc'

      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1996,month1=1,day1=1)
      parameter (year2=1996,month2=12,day2=31)
      integer icell1,jcell1
      parameter (icell1=178,jcell1=282)

      character*13 trseg
      character*6 tlseg

      integer cell,npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=20)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      character*300 bigline

      character*6 ccell  ! character version of cell
      integer lencell

      real q(-5:366,year1:year2,icell1,jcell1)  ! flow
      real H(-5:366,year1:year2,icell1,jcell1)  ! heat
      integer i,j,nd,ny
      real fac1  ! combination of weight and conversion

      real pairq(-5:366,year1:year2)
      real pairH(-5:366,year1:year2)  ! heat

      real acftday2cfs
      parameter (acftday2cfs = 43560./3600./24.)

      integer ndaysinyear
      external ndaysinyear
      integer julian,jday
      external julian

      real wateracres(ndaymax)

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

      real afl(-5:366,year1:year2),bfl(-5:366,year1:year2)  ! check flows

      real heat2c
      parameter (heat2c = 2.043e-7)  ! conversion to temp celcius

      character*25 pradscen, psscen, sepscen
      integer lenpradscen, lenpsscen, lensepscen

      character*11 ps,sep,atdep         ! atdep, pointsource, or septic
      data ps,sep,atdep /'pointsource','septic','atdep'/
      logical doatdep,dops,dosep

      character*5 psmethod ! must be either 'hcell, wcell, or lrseg'

      integer wdmfil
      parameter (wdmfil = dfile + nlu + 10)

************ END DECLARATIONS ******************************************

********* stupid wdm thing
      print*,'main.f start'
      wdmfnam = '../src/lib/dummy.wdm'
      call wdbopn(wdmfil,wdmfnam,1,err)     ! open dummy read only
      if (err .ne. 0) go to 998             ! and never close

      do j = 1,jcell1  ! initialize
        do i = 1,icell1
          do ny = year1,year2
            do nd = -5,366
              q(nd,ny,i,j) = 0.0
              H(nd,ny,i,j) = 0.0
            end do
          end do
        end do
      end do
      do ny = year1,year2
        do nd = -5,366
          afl(nd,ny) = 0.0
          bfl(nd,ny) = 0.0
        end do
      end do

      read*,rscen,psmethod    ! get river scenario
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

******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)
      call masslink(modules,nmod,
     O              nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor)

******** POPULATE similar variables for the data types
      if (dops) then
        call getvars(
     I               nRvar,Rname,ps,
     O               nPSvar,PSdsn,PSname,PSfac)
      end if

      if (dosep) then
        call getvars(
     I               nRvar,Rname,sep,
     O               nSEPvar,SEPdsn,SEPname,SEPfac)
      end if

      if (doatdep) then
        call getvars(
     I               nRvar,Rname,atdep,
     O               nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)
      end if


************** the preceding section is more useful for the wqm

      call getriver(
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)

      tlseg(1:1) = 'A'

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = '../link/distributed_flows.prn'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile-1,'(a200)') bigline    ! get rid of header lines

      read(dfile-1,'(a200)',err = 992,end=992) bigline    
      do while (bigline(:3).ne.'end')
        read(bigline,*,err=992,end=992)
     .    cell,npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        print*,' '
        print*,cell
        i = int(cell/1000)
        j = mod(cell,1000)


*********** loop over all pairs
        do np = 1,npairs

          trseg = rsegs(uniqindex(riverid(np)))  ! get river name

          fac1 = weight(np)*acftday2cfs

          print*,'   ' 
          call ttyput(trseg)

*************** river section
          if (fips(np).eq.0) then  ! river 
            call getrflags(trseg,Nexits,lakeflag,resflag,timestep)

            call Rmasslink(Nexits,modules,nmod,
     O                     nRvarOut,RdsnOut,RnameOut)

            call readriver(rscen,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I                     nRvarOut,RdsnOut,RnameOut,
     O                     pairq,pairH)  ! get the data
            do ny = year1,year2  ! add to big storage variable
              do nd = -5,ndaysinyear(ny)
                afl(nd,ny) = afl(nd,ny) + pairq(nd,ny) * fac1
                q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * fac1
                H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * fac1
              end do
            end do

****************** land section
          else  ! land
            write(tlseg(2:6),'(i5)')fips(np)  ! convert to character
            call ttyput(' ')
            call ttyput(tlseg)
            call readpair(rscen,tlseg,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I              LandScen,nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor,
     O              pairq,pairH)  ! get the data
            do ny = year1,year2  ! add to big storage variable
              do nd = -5,ndaysinyear(ny)
                bfl(nd,ny) = bfl(nd,ny) + pairq(nd,ny) * fac1
                q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * fac1
                H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * fac1
              end do
            end do

            if (dops.and.psmethod.eq.'lrseg') then
              call ttyput(' ps')
              wdmfnam = tree//'wdm/ps/'//psscen(:lenpsscen)//'/ps_'//
     .                    tlseg//'_to_'//trseg//'.wdm'
              call readdailydat(
     I                     year1,month1,day1,year2,month2,day2,
     I                     wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I                     nPSvar,PSdsn,PSname,PSfac,
     O                     pairq,pairH)
              do ny = year1,year2  ! add to big storage variable
                do nd = -5,ndaysinyear(ny)
                  bfl(nd,ny) = bfl(nd,ny) + pairq(nd,ny) * fac1
                  q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * fac1
                  H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * fac1
                end do
              end do
            end if

            if (dosep) then
              call ttyput(' septic')
              wdmfnam = tree//'wdm/septic/'//sepscen(:lensepscen)//
     .                  '/septic_'//tlseg//'_to_'//trseg//'.wdm'
              call readdailydat(
     I                     year1,month1,day1,year2,month2,day2,
     I                     wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I                     nSEPvar,SEPdsn,SEPname,SEPfac,
     O                     pairq,pairH)
              do ny = year1,year2  ! add to big storage variable
                do nd = -5,ndaysinyear(ny)
                  bfl(nd,ny) = bfl(nd,ny) + pairq(nd,ny) * fac1
                  q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * fac1
                  H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * fac1
                end do
              end do
            end if

            if (doatdep) then
              call ttyput(' atdep')
              wdmfnam = tree//'wdm/ps/'//psscen(:lenpsscen)//'/ps_'//
     .                    tlseg//'_to_'//trseg//'.wdm'

              call getwateracres(
     I                           rscen,lenrscen,trseg,tlseg,
     I                           year1,month1,day1,
     I                           year2,month2,day2,
     O                           wateracres)
              call readhourlydat(
     I                     year1,month1,day1,year2,month2,day2,
     I                     wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I                     nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac,
     O                     pairq,pairH)
              jday = 1
              do ny = year1,year2  ! add to big storage variable
                do nd = -5,ndaysinyear(ny)
                  bfl(nd,ny) = bfl(nd,ny) + pairq(nd,ny) * fac1
     .                         * wateracres(jday)
                  q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * fac1
     .                         * wateracres(jday)
                  H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * fac1
     .                         * wateracres(jday)
                  if (nd.gt.0) jday = jday + 1
                end do
              end do
            end if

            print*,'   ' 

          end if  ! land or water

        end do  ! loop over pairs for this cell

        if (dops.and.psmethod.ne.'lrseg') then
          call ttyput(' ps')
          write(ccell,'(i6)') cell
          call trims(ccell,lencell)
          if (psmethod.eq.'hcell') then
            wdmfnam = tree//'wdm/ps/'//psscen(:lenpsscen)//
     .                '/bay_models/ps_ch3d57k_'//ccell(:lencell)//'.wdm'
          else if (psmethod.eq.'wcell') then
            wdmfnam = tree//'wdm/ps/'//psscen(:lenpsscen)//
     .                '/bay_models/ps_wqm57k_'//ccell(:lencell)//'.wdm'
          end if
          call readdailydat(
     I                 year1,month1,day1,year2,month2,day2,
     I                 wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I                 nPSvar,PSdsn,PSname,PSfac,
     O                 pairq,pairH)
          do ny = year1,year2  ! add to big storage variable
            do nd = -5,ndaysinyear(ny)
              bfl(nd,ny) = bfl(nd,ny) + pairq(nd,ny) * acftday2cfs
              q(nd,ny,i,j) = q(nd,ny,i,j) + pairq(nd,ny) * acftday2cfs 
              H(nd,ny,i,j) = H(nd,ny,i,j) + pairH(nd,ny) * acftday2cfs
            end do
          end do
        end if

        read(dfile-1,'(a200)',err = 992,end=992) bigline    
      end do
      close(dfile-1)

      do ny = year1,year2
        fnam = 'flows_year_XXXX.bin'
        write(fnam(12:15),'(i4)')ny
        fnam = '../out/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',form='unformatted',
     .       iostat=err)
        if (err.ne.0) go to 991
        do nd = -5,ndaysinyear(ny)
          write(dfile) nd
          write(dfile)((q(nd,ny,i,j),i=1,icell1),j=1,jcell1)
        end do
        close(dfile)
      end do

      do ny = year1,year2
        fnam = 'temps_year_XXXX.bin'
        write(fnam(12:15),'(i4)')ny
        fnam = '../out/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',form='unformatted',
     .       iostat=err)
        if (err.ne.0) go to 991
        do nd = -5,ndaysinyear(ny)
          do i = 1,icell1  ! convert to temperature
            do j = 1,jcell1
              if (q(nd,ny,i,j).gt.0.0001) then
                H(nd,ny,i,j) = H(nd,ny,i,j)/q(nd,ny,i,j)*heat2c
              else
                H(nd,ny,i,j) = 20
              end if
            end do
          end do
          write(dfile) nd
          write(dfile) ((H(nd,ny,i,j),i=1,icell1),j=1,jcell1)
        end do
        close(dfile)
      end do

      do ny = year1,year2  ! check flows for negatives
        do nd = -5,ndaysinyear(ny)
          do i = 1,icell1
            do j = 1,jcell1
              if (q(nd,ny,i,j).lt.0.0) then
                if  (.not.((i.eq. 46.and.j.eq. 24)
     .                 .or.(i.eq. 54.and.j.eq. 40)
     .                 .or.(i.eq. 60.and.j.eq. 44)
     .                 .or.(i.eq.100.and.j.eq.215))) then
                  print*,'CELL ',i,j,' is negative on ',nd,' ',ny
                  print*,'   value ',q(nd,ny,i,j)
                end if
              end if
            end do
          end do
        end do
      end do

      fnam = 'check_flows_XXXX_YYYY.csv'
      write(fnam(13:16),'(i4)') year1
      write(fnam(18:21),'(i4)') year2
      fnam = '../out/'//rscen(:lenrscen)//'/'//fnam
      open(dfile,file=fnam,status='unknown')
      write(dfile,*)'year, day, 0001s (cfs), 0000s (cfs)'
      do ny = year1,year2
        do nd = 1,ndaysinyear(ny)
          write(dfile,*) ny,',',nd,',',afl(nd,ny),',',bfl(nd,ny)
        end do
      end do
      close(dfile)

      close(dfile-1)

      print*,'main.f end'
      stop

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

