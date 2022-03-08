************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readEOaveann
     I                         (LongScen,onelseg,rseg,
     I                          lu,type,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          load,acres)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer nloadmax

      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos,eof,del
      character*(*) lu   ! land use name
      character*4 loadname(nloadmax)
      integer nloads

      integer year1,year2,y1,y2
      character*4 cyear1,cyear2

      real acres,load(nloadmax)

      integer nl   ! index

      character*300 lin3

      character*(*) DFmethod(nloadmax)        ! Basin, segment, or reservoir DFs
      real resLoad(nloadmax),segLoad(nloadmax),basLoad(nloadmax)

      character*(*) LongScen
      integer lenLongScen

************** END DECLARATION **************************************

      call lencl(LongScen,lenLongScen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

      fnam = outdir//type//'/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//lu//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//lu//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then

        do nl = 1,nloads
          load(nl) = 0.0
        end do
        acres = 0.0

      else

        if (type.eq.'del') then
          read(11,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                 acres

          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              load(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              load(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              load(nl) = resLoad(nl)
            else
              go to 993
            end if
          end do

        else
          read(11,1234,err=992) y1,y2,(load(nl),nl=1,nloads),acres
        end if

        if (type.eq.'eof' .or. type.eq.'eop') then
          do nl = 1,nloads
            load(nl) = load(nl) * acres
          end do
        end if

      end if

      close (11)

      return

1001  format(a15,',',60(5x,a4,','))
1234  format(i5,1x,i4,60(1x,e14.7))

**************ERROR SPACE ******************************************
991   report(1) = 'readEOaveann Problem opening following file'
      write(report(2),*) fnam
      report(3) = ' '
      print*,fnam
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      print*,fnam
      go to 999

993   report(1) = 'Delivery Factor method must be'
      report(2) = 'res - reservoir, seg - segment, bas - basin'
      report(3) = 'method read was '//DFmethod(nl)
      go to 999

999   call stopreport(report)

      end
