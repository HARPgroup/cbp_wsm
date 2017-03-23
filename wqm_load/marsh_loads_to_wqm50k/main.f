************************************************************************
**  program to generate the marsh load input file for the 50k Bay model*
**    loops over all cells in water quality model with marsh loads    **
************************************************************************

      implicit none

********* commonly used variables
      character*100 fnam      ! file name variable 
      character*100 line      ! variable to read one line

      integer err                   ! error in read and open statements

      character*64 report(3)    ! string to report errors in execution
      
********* parameter to use when one datafile is open at a time
      integer dfile
      parameter(dfile=11)

*********** definition of bay variables for marsh loads
      integer nBvar      ! actual number of bay variables
      parameter (nBvar=4)
      character*4 Bname(nBvar)  ! names of bay variables
      data Bname /'sand','silt','clay','orgm'/

      character*25 marshscen  ! marsh load scenario directory
      integer lenscen     ! under /wqm/p5wqm/wqm/data/marsh/

      integer year,month,day
      integer year1,year2,month1,month2,day1,day2
      parameter (year1=1985,month1=1,day1=1)
      parameter (year2=2005,month2=12,day2=31)
      integer maxcells,ncells,nc
      parameter (maxcells=400)
      integer cell(maxcells)

***********  the pair variables are treated differently in this program
******** npairs and maxpairs are the same as the others
******* while allpairs, nallpairs, allfips, alluniqid are global
      integer npairs,np,maxpairs ! variables to read line
      parameter(maxpairs=5)
      real weight(maxpairs)
      integer fips(maxpairs), riverid(maxpairs)

      integer nallpairs, nap, maxallpairs ! variables to store input
      parameter(maxallpairs = 40)
      integer allfips(maxallpairs),alluniqid(maxallpairs)
      real pairwq(nBvar,maxallpairs)

      real wq(366,year1:year2,maxcells,nBvar)
      integer nd,ny,nq

      real pairweight  ! fraction of this LRseg or rseg going to cell

      character*200 bigline

      integer ndaysinyear
      external ndaysinyear

      logical found

************ END DECLARATIONS ******************************************
      read*,marshscen    ! get river scenario
      call lencl(marshscen,lenscen)

      do nq = 1,nBvar ! initialize big variable
        do nc = 1,maxcells
          do ny = year1,year2
            do nd = 1,366
              wq(nd,ny,nc,nq) = 0.0
            end do
          end do
        end do
      end do

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

********** READ DATA
      fnam = '../data/marsh/'//marshscen(:lenscen)//'/marsh_loads.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,*) line  ! ditch header row
      nallpairs = 1
      do
        read(dfile,*,err=994,end=111) 
     .       allfips(nallpairs),alluniqid(nallpairs),
     .       (pairwq(nq,nallpairs),nq=1,nBvar)
        nallpairs = nallpairs + 1
        if (nallpairs.gt.maxallpairs) go to 995
      end do
111   nallpairs = nallpairs - 1
      close(dfile)

********** OPEN LINK FILE AND LOOP OVER CELLS
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/marsh_loss_to_50k.prn'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a200)') bigline    ! get rid of header lines

      read(dfile,'(a200)',err = 992,end=992) bigline    

****************** loop over cells in file
      nc = 0
      do while (bigline(:3).ne.'end')
        nc = nc + 1  ! increment number of cells
        read(bigline,*,err=992,end=992)
     .    cell(nc),npairs,(weight(np),fips(np),riverid(np),np=1,npairs)
        if (npairs.gt.maxpairs) go to 993

*********************** loop over Land-river pairs in cell
        do np = 1,npairs

          found = .false.   ! found lrseg?
          do nap = 1,nallpairs
            if (allfips(nap).eq.fips(np).and.
     .          alluniqid(nap).eq.riverid(np)) then
              found = .true.
              exit
            end if
          end do

          if (.not.found) go to 996

          do nq = 1,nBvar
            do ny = year1,year2  ! add to big storage variable
              do nd = 1,ndaysinyear(ny)
                wq(nd,ny,nc,nq) = wq(nd,ny,nc,nq)
     .                          + pairwq(nq,nap) * weight(np)
              end do
            end do
          end do

        end do   ! end loop over pairs

        read(dfile,'(a200)',err = 992,end=992) bigline    

      end do      ! end loop over cells

      close(dfile)

      ncells = nc

      do ny = year1,year2

        fnam = 'marsh_loads.YY'           ! open file
        write(fnam(11:14),'(i4)') ny
        fnam(11:12) = 's.'
        fnam = outdir//'wqm_input/'//marshscen(:lenscen)//'/'//fnam
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

993   report(1) = 'Problem with linkage file:  Too many pairs on line:'
      report(2) = fnam
      report(3) = bigline(:64)
      go to 999

994   report(1) = 'error reading file:  near data:'
      report(2) = fnam
      write(report(3),*) allfips(nallpairs-1),' ',alluniqid(nallpairs-1)
      go to 999

995   report(1) = 'number of pairs in file:'
      report(2) = fnam
      report(3) = 'exceeds max specified in program'
      go to 999

996   report(1) = 'could not find lrseg'
      write(report(2),*) fips(np),' ',riverid(np)
      report(3) = 'in '//marshscen(:lenscen)//' loading file'
      go to 999
999   call stopreport(report)

      end

