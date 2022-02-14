************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readDATannual
     I                         (rscen,onelseg,rseg,
     I                          type,year1,year2,
     I                          nloads,loadname,nloadmax,
     O                          dwwtp,dindus,dcso,dsep,datdep)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'
      include '../../../lib/inc/land_use.inc'

      integer nloadmax

      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos,eof,del
c      character*(*) lu   ! land use name
      character*3 lu(nlu)
      character*4 loadname(nloadmax)
      integer nloads

      integer year1,year2,nm,ny
      character*4 cyear1,cyear2

      real dwwtp(nloadmax,year1:year2)
      real dindus(nloadmax,year1:year2)
      real dcso(nloadmax,year1:year2)
      real dsep(nloadmax,year1:year2)
      real datdep(nloadmax,year1:year2)

      integer nl   ! index

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************* eof = eos for these constituents

************ pointsource -wwtp
      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'.ann'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'.ann'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nl = 1,nloads
          dwwtp(nl,ny) = 0.0
        end do
      end do

      read(dfile,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(dfile,*,err=992,end=111) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace dfile
            read(dfile,*,err=992) ny,(dwwtp(nl,ny),nl=1,nloads)
          end if
        end do
      end if
111   close (dfile)

************ pointsource -industrial
      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'.ann'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'.ann'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nl = 1,nloads
          dindus(nl,ny) = 0.0
        end do
      end do

      read(dfile,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=222) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace dfile
            read(dfile,*,err=992) ny,(dindus(nl,ny),nl=1,nloads)
          end if
        end do
      end if
222   close (dfile)

************ pointsource -cso
      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'.ann'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'.ann'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nl = 1,nloads
          dcso(nl,ny) = 0.0
        end do
      end do

      read(dfile,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(dfile,*,err=992,end=333) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace dfile
            read(dfile,*,err=992) ny,(dcso(nl,ny),nl=1,nloads)
          end if
        end do
      end if
333   close (dfile)

************ septic
      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'.ann'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'.ann'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nl = 1,nloads
          dsep(nl,ny) = 0.0
        end do
      end do

      read(dfile,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(dfile,*,err=992,end=444) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace dfile
            read(dfile,*,err=992) ny,(dsep(nl,ny),nl=1,nloads)
          end if
        end do
      end if
444   close (dfile)

************ atdep
      fnam = outdir//'eos/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'.ann'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/annual/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'.ann'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nl = 1,nloads
          datdep(nl,ny) = 0.0
        end do
      end do

      read(dfile,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(dfile,*,err=992,end=555) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace dfile
            read(dfile,*,err=992) ny,(datdep(nl,ny),nl=1,nloads)
          end if
        end do
      end if
555   close (dfile)

      return

1001  format(a15,',',60(5x,a4,','))
1234  format(i5,1x,i4,60(1x,e14.7))

**************ERROR SPACE ******************************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      go to 999

999   call stopreport(report)

      end
