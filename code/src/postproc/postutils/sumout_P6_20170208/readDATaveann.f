************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readDATaveann
     I                        (LongScen,onelseg,rseg,
     I                         type,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         dwwtp,dindus,dcso,dsep,drib,drpa,datdep,
     O                         atdepacres)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'

      integer nloadmax

      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos,eof,del
      character*4 loadname(nloadmax)
      integer nloads

      integer year1,year2,y1,y2
      character*4 cyear1,cyear2

      real dwwtp(nloadmax),dindus(nloadmax),dcso(nloadmax)
      real dsep(nloadmax),drib(nloadmax),drpa(nloadmax),datdep(nloadmax)
      real acres,atdepacres

      integer nl   ! index

      character*(*) DFmethod(nloadmax)
      real resLoad(nloadmax),segLoad(nloadmax),basLoad(nloadmax)

      character*(*) LongScen
      integer lenLongScen

************** END DECLARATION **************************************

      call lencl(LongScen,lenLongScen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************* eof = eos for these constituents
      call lencl(wwtp,lenwwtp)
      call lencl(indus,lenindus)
      call lencl(cso,lencso)
      call lencl(sep,lensep)
      call lencl(rib,lenrib)
      call lencl(rpa,lenrpa)
      call lencl(atdep,lenatdep)

************ pointsource -wwtp
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//wwtp(:lenwwtp)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
   
      read(dfile,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dwwtp(nl) = 0.0
        end do

      else
         
        if (type.eq.'del') then
          read(dfile,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              dwwtp(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              dwwtp(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              dwwtp(nl) = resLoad(nl)
            else
              go to 993
           end if
         end do

        else
          read(dfile,1234,err=992) y1,y2,(dwwtp(nl),nl=1,nloads),acres
        end if

      end if

      close (dfile)

************ pointsource -indusrtial
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//indus(:lenindus)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dindus(nl) = 0.0
        end do

      else

        if (type.eq.'del') then
          read(dfile,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                             (basLoad(nl),nl=1,nloads),
     .                             (resLoad(nl),nl=1,nloads),
     .                              acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              dindus(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              dindus(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              dindus(nl) = resLoad(nl)
            else
              go to 993
           end if
         end do

        else
          read(dfile,1234,err=992) y1,y2,(dindus(nl),nl=1,nloads),acres
        end if

      end if

      close (dfile)

************ pointsource -cso
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//cso(:lencso)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dcso(nl) = 0.0
        end do

      else

        if (type.eq.'del') then
          read(dfile,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                             (basLoad(nl),nl=1,nloads),
     .                             (resLoad(nl),nl=1,nloads),
     .                              acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              dcso(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              dcso(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              dcso(nl) = resLoad(nl)
            else
              go to 993
           end if
         end do

        else
          read(dfile,1234,err=992) y1,y2,(dcso(nl),nl=1,nloads),acres
        end if

      end if

      close (dfile)

**************** SEPTIC ********************************
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//sep(:lensep)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          dsep(nl) = 0.0
        end do

      else 

        if (type.eq.'del') then
          read(11,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              dsep(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              dsep(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              dsep(nl) = resLoad(nl)
            else
              go to 993
            end if
          end do

        else
          read(11,1234,err=992) y1,y2,(dsep(nl),nl=1,nloads),acres
        end if

      end if

      close (11)

**************** RIB ********************************
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//rib(:lenrib)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//rib(:lenrib)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          drib(nl) = 0.0
        end do

      else

        if (type.eq.'del') then
          read(11,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              drib(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              drib(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              drib(nl) = resLoad(nl)
            else
              go to 993
            end if
          end do

        else
          read(11,1234,err=992) y1,y2,(drib(nl),nl=1,nloads),acres
        end if

      end if

      close (11)


**************** RPA ********************************
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//rpa(:lenrpa)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//rpa(:lenrpa)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          drpa(nl) = 0.0
        end do
      
      else
        
        if (type.eq.'del') then 
          read(11,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                acres
          do nl = 1,nloads
            if (DFmethod(nl) .eq. 'seg') then
              drpa(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              drpa(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              drpa(nl) = resLoad(nl)
            else
              go to 993
            end if
          end do

        else
          read(11,1234,err=992) y1,y2,(drpa(nl),nl=1,nloads),acres
        end if

      end if

      close (11)

************ atdep
      fnam = outdir//'eos/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//LongScen(:lenLongScen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_'//atdep(:lenatdep)//'_'//cyear1//'-'//cyear2//'.ave'
      end if 
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a8)') line
      if (line(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          datdep(nl) = 0.0
        end do
        acres = 0.0
      else 

        if (type.eq.'del') then
          read(11,1234,err=992) y1,y2,(segLoad(nl),nl=1,nloads),
     .                                (basLoad(nl),nl=1,nloads),
     .                                (resLoad(nl),nl=1,nloads),
     .                                atdepacres
  
          do nl = 1,nloads
            if (DFmethod (nl) .eq. 'seg') then
              datdep(nl) = segLoad(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              datdep(nl) = basLoad(nl)
            else if (DFmethod(nl) .eq. 'res') then
              datdep(nl) = resLoad(nl)
            else
              go to 993
            end if
          end do

        else
         read(11,1234,err=992) y1,y2,(datdep(nl),nl=1,nloads),atdepacres
        end if

      end if

      close (11)

      return

1001  format(a15,',',60(5x,a4,','))
1234  format(i5,1x,i4,60(1x,e14.7))

**************ERROR SPACE ******************************************
991   report(1) = 'readDATaveann Problem opening following file'
      write(report(2),*) fnam
      report(3) = ' '
      print*,fnam
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      go to 999

993   report(1) = 'Delivery Factor method must be'
      report(2) = 'res - reservoir, seg - segment, bas - basin'
      report(3) = 'method read was '//DFmethod(nl)
      go to 999

999   call stopreport(report)

      end
