************************************************************************
**  program to generate the input file for ch3d                       **
**    loops over all cells in water quality model                     ****      loops over all inputs                                         **
**        creates one variable with all q's                           **
************************************************************************

      implicit none

      include 'transfer_wdm.inc'
      include '../lib/inc/rsegs.inc'

      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1993,month1=1,day1=1)
      parameter (year2=2000,month2=12,day2=31)

      integer ncellmax,ncells,nc,nc1
      parameter (ncellmax=15)

      real q(-5:366,year1:year2,ncellmax)
      integer i(ncellmax),j(ncellmax)

      integer cell(ncellmax),tcell

      character*13 trseg

      real weight
      integer riverid
      character*300 bigline

      integer nd,ny
      real pairq(-5:366,year1:year2)
      real fac1  ! combination of weight and conversion

      real acftday2cfs
      parameter (acftday2cfs = 43560./3600./24.)

      integer ndaysinyear
      external ndaysinyear

      logical comment,found
      external comment

      integer Nexits  ! number of exits in this river
                      !  affects the river dsns to output

      integer lakeflag,resflag,timestep  ! variables to send to 
                             ! getflags routine, not used

      real afl(366,year1:year2)  ! check flows

      integer stupidWDMthing

************ END DECLARATION ******************************************

      do nc = 1,ncellmax ! initialize
        do ny = year1,year2
          do nd = -5,366
            q(nd,ny,nc) = 0.0
          end do
        end do
      end do
      do ny = year1,year2
        do nd = 1,366
          afl(nd,ny) = 0.0
        end do
      end do

      read*,rscen    ! get river scenario
      call lencl(rscen,lenrscen)

******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)

************** the preceding section is more useful for the wqm

      call getriver(
     O           rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)

********** END OF SETUP, OPEN FILE AND LOOP OVER CELLS
      fnam = '../link/gaged_rivers.prn'
      open(dfile-1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      ncells = 0

      bigline = 'GO HOOS'    ! get rid of header lines

      stupidWDMthing = 0

      do while (bigline(:3).ne.'end')
        read(dfile-1,'(a200)',err = 992,end=101) bigline    
        if (bigline(:3).eq.'end') exit
        if (comment(bigline)) cycle
        if (bigline(:10).eq.'          ') cycle
        read(bigline,*,err=992,end=992) tcell,weight,riverid
        print*,tcell

        found = .false.
        do nc1 = 1,ncells
          if (tcell.eq.cell(nc1)) then
            found = .true.
            nc = nc1
          end if
        end do

        if (.not.found) then
          ncells = ncells + 1
          nc = ncells
          cell(nc) = tcell
          i(nc) = int(tcell/1000)
          j(nc) = mod(tcell,1000)
        end if


        trseg = rsegs(uniqindex(riverid))  ! get river name

        fac1 = weight*acftday2cfs

        call getrflags(trseg,Nexits,lakeflag,resflag,timestep)

        call Rmasslink(Nexits,modules,nmod,
     O                   nRvarOut,RdsnOut,RnameOut)

        stupidWDMthing = stupidWDMthing + 1
        call readriver(rscen,trseg,stupidWDMthing,
     I                     year1,month1,day1,year2,month2,day2,
     I                   nRvarOut,RdsnOut,RnameOut,
     O                   pairq)  ! get the data

        do ny = year1,year2
          do nd = 1,ndaysinyear(ny)
            afl(nd,ny) = afl(nd,ny) + pairq(nd,ny) * fac1
          end do
        end do

        do ny = year1,year2  ! add to big storage variable
          do nd = -5,ndaysinyear(ny)
            q(nd,ny,nc) = q(nd,ny,nc) + pairq(nd,ny) * fac1
          end do
        end do

      end do
101   close(dfile-1)

      do ny = year1,year2
        fnam = 'modeled_gages_XXXX.prn'
        write(fnam(15:18),'(i4)')ny
        fnam = '../out/'//rscen(:lenrscen)//'/'//fnam
        open(dfile,file=fnam,status='unknown',form='unformatted',
     .       iostat=err)
        if (err.ne.0) go to 991
        write(dfile) ncells,i,j
        do nd = -5,ndaysinyear(ny)
          write(dfile) nd
          write(dfile) (q(nd,ny,nc),nc=1,ncells)
        end do
        close(dfile)
      end do

      fnam = 'check_gage_flows.prn'
      fnam = '../out/'//rscen(:lenrscen)//'/'//fnam
      open(dfile,file=fnam,status='unknown')
      write(dfile,*)'year, day, 0001s (cfs), 0000s (cfs)'
      do ny = year1,year2
        do nd = 1,ndaysinyear(ny)
          write(dfile,*) ny,',',nd,',',afl(nd,ny)
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

