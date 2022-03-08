************************************************************************
**  reads the inputs                                                  **
************************************************************************
      subroutine readInAveann(
     I                        lscen,lseg,
     I                        lu,year1,year2,
     I                        nloads,loadname,
     O                        fert,manure,legume,uptake)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character*3 type  ! eos,eof,del
      character*(*) lu   ! land use name
      integer nloads
      character*4 loadname(nloads)

      integer year1,year2,y1,y2
      character*4 cyear1,cyear2

      character*25 header
      real fert(nloads),manure(nloads),legume(nloads),uptake(nloads)

      integer nlmax  ! number of loads to read
      parameter(nlmax=5)
      real value(nlmax)  ! klugy, needs to be in order 
                         ! no3n,nh3n,orgn,po4p,orgp
                         ! should fix output/input program to write
                         ! in output variable format (rchres_out...)

      integer nl   ! index

************** END DECLARATION **************************************
      if (lu.eq.'wat') then
        do nl = 1,nloads
          fert(nl) = 0.0
          manure(nl) = 0.0
          legume(nl) = 0.0
          uptake(nl) = 0.0
        end do
        return
      end if

      call lencl(lscen,lenlscen)
      call lencl(lseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************ fertilizer
      fert(1) = 0.0
      fert(2) = 0.0
      fnam = outdir//'input/'//lscen(:lenlscen)//
     .           '/fert_'//lseg(:lenlseg)//'_'//lu//'_'//
     .           cyear1//'_'//cyear2//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.eq.2) go to 111  ! missing file skip down
      if (err.ne.0) go to 991

      read(11,'(a25)',err=994,end=994) header
      if (header.ne.' no3n,nh3n,orgn,po4p,orgp') go to 993
      read(11,*,err=992) (value(nl),nl=1,nlmax)
      fert(1) = value(1) + value(2) + value(3)
      fert(2) = value(4) + value(5)

      close (11)

************ manure
111   manure(1) = 0.0
      manure(2) = 0.0
      fnam = outdir//'input/'//lscen(:lenlscen)//
     .           '/manure_'//lseg(:lenlseg)//'_'//lu//'_'//
     .           cyear1//'_'//cyear2//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.eq.2) go to 222  ! missing file skip down
      if (err.ne.0) go to 991

      read(11,'(a25)',err=994,end=994) header
      if (header.ne.' no3n,nh3n,orgn,po4p,orgp') go to 993
      read(11,*,err=992) (value(nl),nl=1,nlmax)
      manure(1) = value(1) + value(2) + value(3)
      manure(2) = value(4) + value(5)

      close (11)

************ legume fixation
222   legume(1) = 0.0
      legume(2) = 0.0
      fnam = outdir//'input/'//lscen(:lenlscen)//
     .           '/legume_'//lseg(:lenlseg)//'_'//lu//'_'//
     .           cyear1//'_'//cyear2//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.eq.2) go to 333  ! missing file skip down
      if (err.ne.0) go to 991

      read(11,'(a25)',err=994,end=994) header
      if (header.ne.' no3n,nh3n,orgn,po4p,orgp') go to 993
      read(11,*,err=992) (value(nl),nl=1,nlmax)
      legume(1) = value(1) + value(2) + value(3)
      legume(2) = value(4) + value(5)

      close (11)

************ uptake
333   uptake(1) = 0.0
      uptake(2) = 0.0
      fnam = outdir//'input/'//lscen(:lenlscen)//
     .           '/uptake_'//lseg(:lenlseg)//'_'//lu//'_'//
     .           cyear1//'_'//cyear2//'.csv'
      open(11,file=fnam,status='old',iostat=err)
      if (err.eq.2) go to 444  ! missing file skip down
      if (err.ne.0) go to 991

      read(11,'(a25)',err=994,end=994) header
      if (header.ne.' no3n,nh3n,orgn,po4p,orgp') go to 993
      read(11,*,err=992) (value(nl),nl=1,nlmax)
      uptake(1) = value(1) + value(2) + value(3)
      uptake(2) = value(4) + value(5)

      close (11)

444   return

**************ERROR SPACE ******************************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      go to 999

993   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = 'unexpected header line, reprogram necessary'
      go to 999

994   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading header line' 
      go to 999

999   call stopreport(report)

      end
