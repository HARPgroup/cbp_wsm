************************************************************************
**  program to generate the input file for ch3d                       **
**    loops over all cells in water quality model                     ****      loops over all inputs                                         **
**        creates one variable with all q's                           **
************************************************************************

      implicit none

      include 'transfer_wdm.inc'
      include '../lib/inc/rsegs.inc'

      integer year1,year2
      parameter (year1=1992,year2=2000)
      integer icell1,jcell1
      parameter (icell1=178,jcell1=282)

      character*13 trseg
      character*6 tlseg

      integer cell,npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=20)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)
      character*300 bigline

      real q(-5:366,year1:year2,icell1,jcell1)
      integer i,j,nd,ny
      real annualq(-5:366)

      real acftday2cfs
      parameter (acftday2cfs = 43560./3600./24)

      integer ndaysinyear
      external ndaysinyear
************ END DECLARATION ******************************************

      do j = 1,jcell1  ! initialize
        do i = 1,icell1
          do ny = year1,year2
            do nd = -5,366
              q(nd,ny,i,j) = 0.0
            end do
          end do
        end do
      end do

      read*,rscen    ! get river scenario
      call lencl(rscen,lenrscen)

******** READ THE CONTROL FILE FOR LAND SCENARIOS
      call readcontrol_landscen(rscen,lenrscen,LandScen)

******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)
      call masslink(modules,nmod,
     O              nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor)
************** the preceding section is more useful for the wqm

      call getriver(
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)


      tlseg(1:1) = 'A'

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = '../link/distributed_flows.prn'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a200)') bigline    ! get rid of header lines

      read(dfile,'(a200)',err = 992,end=992) bigline    
      do while (bigline(:3).ne.'end')
        read(bigline,*,err=992,end=992)
     .    cell,npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        print*,cell
        i = int(cell/1000)
        j = mod(cell,1000)

        do np = 1,npairs
          write(tlseg(2:6),'(i5)')fips(np)
          trseg = rsegs(uniqindex(riverid(np)))

          do ny = year1,year2

	print*,1, rscen,tlseg,trseg,ny
            call readpairyear(rscen,tlseg,trseg,ny,
     I              LandScen,nRvar,Rdsn,Rname,nLvar,Ldsn,Lname,Lfactor,
     O              annualq)  ! get the data
	print*,2

            do nd = -5,ndaysinyear(ny)
              q(nd,ny,i,j) = weight(np)*annualq(nd)*acftday2cfs
            end do
          end do
        end do
        read(dfile,'(a200)',err = 992,end=992) bigline    
      end do
      close(dfile)

      do ny = year1,year2
        fnam = 'flows_year_XXXX.prn'
        write(fnam(12:15),'(i4)')ny
        open(dfile,file=fnam,status='unknown')
        do nd = -5,ndaysinyear(ny)
          write(dfile,'(i4)') nd
          do j=1,jcell1
            write(dfile,'(178F6.1)')(q(nd,ny,i,j),i=1,icell1)
          end do
        end do
        close(dfile)
      end do
      stop


********************************* ERROR SPACE *****************************
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

