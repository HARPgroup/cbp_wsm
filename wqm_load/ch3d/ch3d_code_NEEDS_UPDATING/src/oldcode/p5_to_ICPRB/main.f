************************************************************************
**  program to generate the input file for ch3d                       **
**    loops over all cells in water quality model                     **
**      loops over all inputs to that cell                            **
**        creates one variable with all q's                           **
************************************************************************

      implicit none

      include 'transfer_wdm.inc'
      include '../lib/inc/rsegs.inc'

      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1994,month1=1,day1=1)
      parameter (year2=2005,month2=8,day2=30)

      character*13 trseg
      character*6 tlseg

      integer cell,npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=20)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      character*300 bigline

      integer maxcells
      parameter (maxcells=345)   ! number of wqm cells to load
      integer cells(maxcells),ncells,nc

      real q(366,year1:year2,maxcells)
      integer i,j,nd,ny
      real pairq(-5:366,year1:year2)
      real fac1  ! combination of weight and conversion

      real acftday2cfs
      parameter (acftday2cfs = 43560./3600./24)

      integer ndaysinyear
      external ndaysinyear


      integer julian
      external julian

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

************ END DECLARATION ******************************************

      do nc = 1,maxcells
        do ny = year1,year2
          do nd = 1,366
            q(nd,ny,nc) = 0.0
          end do
        end do
      end do

      read*,rscen    ! get river scenario
      call lencl(rscen,lenrscen)

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_twdm(rscen,lenrscen,
     .                      LandScen)


******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)
      call masslink(modules,nmod,
     O              nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor)

************** the preceding section is more useful for the wqm

      call getriver(
     O              rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)

      tlseg(1:1) = 'A'

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = '../link/potom_dist.prn'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile-1,'(a200)') bigline    ! get rid of header lines

      ncells = 0
      read(dfile-1,'(a200)',err = 992,end=992) bigline    
      do while (bigline(:3).ne.'end')
        read(bigline,*,err=992,end=992)
     .    cell,npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        print*,' '
        print*,cell
        ncells = ncells + 1
        cells(ncells) = cell  ! record cell name

        do np = 1,npairs

          trseg = rsegs(uniqindex(riverid(np)))  ! get river name

          fac1 = weight(np)*acftday2cfs

          print*,'   ' 
          call ttyput(trseg)

          if (fips(np).eq.0) then  ! river 
            call getrflags(trseg,Nexits,lakeflag,resflag,timestep)

            call Rmasslink(Nexits,modules,nmod,
     O                     nRvarOut,RdsnOut,RnameOut)

            call readriver(
     I                     rscen,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I                     nRvarOut,RdsnOut,RnameOut,
     O                     pairq)  ! get the data

          else  ! land
            write(tlseg(2:6),'(i5)')fips(np)  ! convert to character
            call ttyput(' ')
            call ttyput(tlseg)
            call readpair(rscen,tlseg,trseg,
     I                    year1,month1,day1,year2,month2,day2,
     I              LandScen,nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor,
     O              pairq)  ! get the data

            print*,'   ' 

          end if

          
          do ny = year1,year2  ! add to big storage variable
            do nd =  1,ndaysinyear(ny)
              q(nd,ny,ncells) = q(nd,ny,ncells) + pairq(nd,ny) * fac1
            end do
          end do

        end do
        read(dfile-1,'(a200)',err = 992,end=992) bigline    
      end do
      close(dfile-1)

      fnam = 'LTI_flows.prn'
      fnam = '../out/ICPRB/'//rscen(:lenrscen)//'/'//fnam
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile,'(a10,343(i6,a1))') 
     .        'year,day,',(cells(nc),',',nc=1,ncells)
      do ny = year1,year2
        do nd = 1,ndaysinyear(ny)
          write(dfile,'(i4,a1,i3,343(a1,f10.2))') 
     .           ny,',',nd,(',',q(nd,ny,nc),nc=1,ncells)
        end do
      end do
      close(dfile)

      do ny = year1,year2  ! check flows for negatives
        do nd = 1,ndaysinyear(ny)
          do nc = 1,ncells
            if (q(nd,ny,nc).lt.0.0) then
              print*,'CELL ',cells(nc),' is negative on ',nd,' ',ny
              print*,'   value ',q(nd,ny,nc)
            end if
          end do
        end do
      end do

      fnam = 'ICPRB_flows.prn'
      fnam = '../out/ICPRB/'//rscen(:lenrscen)//'/'//fnam
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile,*)'year,day,cell,flow'
      do nc = 1,ncells
        do ny = year1,year2
          do nd = 1,ndaysinyear(ny)
            write(dfile,*)ny,',',nd,',',cells(nc),',',q(nd,ny,nc)
          end do
        end do
      end do
      close(dfile)


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

999   call stopreport(report)

      end

