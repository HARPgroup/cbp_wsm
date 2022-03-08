************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readDATmonthly
     I                         (rscen,onelseg,rseg,
     I                          type,year1,year2,
     I                          nloads,loadname,nloadmax,
     O                          dwwtp,dindus,dcso,dsep,datdep)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'

      integer nloadmax

      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos,eof,del
      character*(*) lu   ! land use name
      character*4 loadname(nloadmax)
      integer nloads

      integer year1,year2,nm,ny
      character*4 cyear1,cyear2

      real dwwtp(nloadmax,year1:year2,12)
      real dindus(nloadmax,year1:year2,12)
      real dcso(nloadmax,year1:year2,12)
      real dsep(nloadmax,year1:year2,12)
      real datdep(nloadmax,year1:year2,12)

      integer nl   ! index

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************* eof = eos for these constituents

************ pointsource -wwtp
      fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            dwwtp(nl,ny,nm) = 0.0
          end do
        end do
      end do

      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=111) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace 11
            read(11,*,err=992) ny,nm,(dwwtp(nl,ny,nm),nl=1,nloads)
          end if
        end do
      end if
111   close (11)

************ pointsource -industrial
      fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            dindus(nl,ny,nm) = 0.0
          end do
        end do
      end do

      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=222) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace 11
            read(11,*,err=992) ny,nm,(dindus(nl,ny,nm),nl=1,nloads)
          end if
        end do
      end if
222   close (11)

************ pointsource -cso
      fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            dcso(nl,ny,nm) = 0.0
          end do
        end do
      end do

      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=333) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace 11
            read(11,*,err=992) ny,nm,(dcso(nl,ny,nm),nl=1,nloads)
          end if
        end do
      end if
333   close (11)

************ septic
      fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            dsep(nl,ny,nm) = 0.0
          end do
        end do
      end do
      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=444) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace 11
            read(11,*,err=992) ny,nm,(dsep(nl,ny,nm),nl=1,nloads)
          end if
        end do
      end if
444   close (11)

************ atdep
      fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            datdep(nl,ny,nm) = 0.0
          end do
        end do
      end do
      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then
        do
          read(11,*,err=992,end=555) ny
          if (ny.ge.year1.and.ny.le.year2) then
            backspace 11
            read(11,*,err=992) ny,nm,(datdep(nl,ny,nm),nl=1,nloads)
          end if
        end do
      end if
555   close (11)

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
