************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readPSSEPaveann
     I                         (rscen,onelseg,rseg,
     I                          type,year1,year2,
     I                          nloads,loadname,nloadmax,
     O                          dwwtp,dindus,dcso,dsep)
     
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

      integer year1,year2,y1,y2
      character*4 cyear1,cyear2

      real dwwtp(nloadmax)
      real dindus(nloadmax)
      real dcso(nloadmax)
      real dsep(nloadmax)

      integer nl   ! index

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************* eof = eos for these constituents

************ pointsource -wwtp
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dwwtp(nl) = 0.0
        end do
      else      
        read(11,1234,err=992) y1,y2,(dwwtp(nl),nl=1,nloads)
      end if
      close (11)

************ pointsource -industrial
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dindus(nl) = 0.0
        end do
      else
        read(11,1234,err=992) y1,y2,(dindus(nl),nl=1,nloads)
      end if
      close (11)

************ pointsource -cso
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dcso(nl) = 0.0
        end do
      else
        read(11,1234,err=992) y1,y2,(dcso(nl),nl=1,nloads)
      end if
      close (11)

************ septic
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dsep(nl) = 0.0
        end do
      else 
        read(11,1234,err=992) y1,y2,(dsep(nl),nl=1,nloads)
      end if
      close (11)

      return

1001  format(a15,',',60(5x,a4,','))
1234  format(i5,1x,i4,60(1x,e14.7))

**************ERROR SPACE ******************************************
991   report(1) = 'readPSSEPaveann Problem opening following file'
      write(report(2),*) fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      go to 999

999   call stopreport(report)

      end
