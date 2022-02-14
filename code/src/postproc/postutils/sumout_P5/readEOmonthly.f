************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readEOmonthly
     I                         (rscen,onelseg,rseg,
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

      integer year1,year2,ny,nm
      character*4 cyear1,cyear2

      real acres(year1:year2,12),load(nloadmax,year1:year2,12)

      integer nl   ! index

      character*300 lin3

      character*(*) DFmethod(nloadmax)! Basin, segment, or reservoir DFs

      real resLoad(nloadmax,year1:year2,12)
      real segLoad(nloadmax,year1:year2,12)
      real basLoad(nloadmax,year1:year2,12)

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

      fnam = outdir//type//'/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//lu//'.mon'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/monthly/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//lu//'.mon'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nm = 1,12
        do ny = year1,year2
          do nl = 1,nloads
            load(nl,ny,nm) = 0.0
          end do
        end do
      end do

      read(11,'(a8)') line
      if (line(:8).ne.'NO LOADS') then

        if (type.eq.'del') then 
          do
            read(11,*,err=992,end=111) ny
            if (ny.ge.year1.and.ny.le.year2) then
              backspace 11
              read(11,*,err=992) ny,nm,(segLoad(nl,ny,nm),nl=1,nloads),
     .                                 (basLoad(nl,ny,nm),nl=1,nloads),
     .                                 (resLoad(nl,ny,nm),nl=1,nloads),
     .                                 acres(ny,nm)
              do nl = 1,nloads
                if (DFmethod(nl) .eq. 'seg') then
                  load(nl,ny,nm) = segLoad(nl,ny,nm)
                else if (DFmethod(nl) .eq. 'bas') then
                  load(nl,ny,nm) = basLoad(nl,ny,nm)
                else if (DFmethod(nl) .eq. 'res') then
                  load(nl,ny,nm) = resLoad(nl,ny,nm)
                else
                  go to 993
                end if
              end do
            end if
          end do
        else
          read(11,*,err=992) ny,nm,(load(nl,ny,nm),nl=1,nloads)
        end if
 
        if (type.eq.'eof') then
          do nl = 1,nloads
            load(nl,ny,nm) = load(nl,ny,nm) * acres(ny,nm)
          end do
        end if

      end if
111   close (11)

      return

1001  format(a15,',',60(5x,a4,','))
1234  format(i5,1x,i4,60(1x,e14.7))

**************ERROR SPACE ******************************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
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
