************************************************************************
**  writes out the edge-of-stream loads by year                       **
************************************************************************
      subroutine  readDATaveann
     I                         (rscen,onelseg,rseg,
     I                          type,year1,year2,
     I                          nloads,loadname,
     O                          lps,lsep,latdep)
     
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'
      include '../../../lib/inc/land_use.inc'

      integer nloads
      character*(*) onelseg    ! land use segment
      character*(*) type  ! eos,eof,del
c      character*(*) lu   ! land use name
      character*3 lu(nlu)  ! land use name
      character*4 loadname(nloads),Tload

      integer year1,year2,y1,y2
      character*4 cyear1,cyear2

      real lps(nloads),lsep(nloads),latdep(nloads)

      real lwwtp(nloads),lindus(nloads),lcso(nloads)

      character*300 header,dataline

      integer nl   ! index

      logical found(nloads),allfound

************** END DECLARATION **************************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lencl(onelseg,lenlseg)
      write(cyear1,'(i4)') year1
      write(cyear2,'(i4)') year2

************* eof = eos for these constituents
************ pointsource - add the three types together
        do nl = 1,nloads
          lps(nl) = 0.0
        end do

*************** wwtp
      call lencl(wwtp,lenwwtp)
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           wwtp(:lenwwtp)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           wwtp(:lenwwtp)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a300)',err=992,end=994) header
      if (header(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          lwwtp(nl) = 0.0
        end do

      else

        read(11,'(a300)',err=993,end=994) dataline

        allfound = .false.
        do nl = 1,nloads
          found(nl) = .false.
        end do

        do while (.not.allfound)
          call shift(header)
          call shift(dataline)
          read(header,*,err=995,end=995) Tload
          do nl = 1,nloads
            if (Tload.eq.loadname(nl)) then
              read(dataline,*,err=996,end=996) lwwtp(nl)
              found(nl) = .true.
            end if
          end do
          allfound = .true.
          do nl = 1,nloads
            if (.not.found(nl)) allfound = .false.
          end do
        end do
      end if

      close (11)

      do nl = 1,nloads      ! add to point source variable
        lps(nl) = lps(nl) + lwwtp(nl)
      end do

*************** cso
      call lencl(cso,lencso)
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           cso(:lencso)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           cso(:lencso)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a300)',err=992,end=994) header
      if (header(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          lcso(nl) = 0.0
        end do

      else

        read(11,'(a300)',err=993,end=994) dataline

        allfound = .false.
        do nl = 1,nloads
          found(nl) = .false.
        end do

        do while (.not.allfound)
          call shift(header)
          call shift(dataline)
          read(header,*,err=995,end=995) Tload
          do nl = 1,nloads
            if (Tload.eq.loadname(nl)) then
              read(dataline,*,err=996,end=996) lcso(nl)
              found(nl) = .true.
            end if
          end do
          allfound = .true.
          do nl = 1,nloads
            if (.not.found(nl)) allfound = .false.
          end do
        end do
      end if

      close (11)

      do nl = 1,nloads      ! add to point source variable
        lps(nl) = lps(nl) + lcso(nl)
      end do

*************** indus
      call lencl(indus,lenindus)
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           indus(:lenindus)//'_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           indus(:lenindus)//'_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a300)',err=992,end=994) header
      if (header(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          lindus(nl) = 0.0
        end do

      else

        read(11,'(a300)',err=993,end=994) dataline

        allfound = .false.
        do nl = 1,nloads
          found(nl) = .false.
        end do

        do while (.not.allfound)
          call shift(header)
          call shift(dataline)
          read(header,*,err=995,end=995) Tload
          do nl = 1,nloads
            if (Tload.eq.loadname(nl)) then
              read(dataline,*,err=996,end=996) lindus(nl)
              found(nl) = .true.
            end if
          end do
          allfound = .true.
          do nl = 1,nloads
            if (.not.found(nl)) allfound = .false.
          end do
        end do
      end if

      close (11)

      do nl = 1,nloads      ! add to point source variable
        lps(nl) = lps(nl) + lindus(nl)
      end do

************ septic
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_septic_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_septic_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a300)',err=992,end=994) header
      if (header(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          lsep(nl) = 0.0
        end do

      else

        read(11,'(a300)',err=993,end=994) dataline

        allfound = .false.
        do nl = 1,nloads
          found(nl) = .false.
        end do

        do while (.not.allfound)
          call shift(header)
          call shift(dataline)
          read(header,*,err=995,end=995) Tload
          do nl = 1,nloads
            if (Tload.eq.loadname(nl)) then
              read(dataline,*,err=996,end=996) lsep(nl)
              found(nl) = .true.
            end if
          end do
          allfound = .true.
          do nl = 1,nloads
            if (.not.found(nl)) allfound = .false.
          end do
        end do

      end if
      close (11)

************ atdep
      fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_atdep_'//cyear1//'-'//cyear2//'.ave'
      if (type.eq.'del') then
        fnam = outdir//'del/lseg/aveann/'//rscen(:lenrscen)//
     .           '/'//onelseg(:lenlseg)//'_to_'//rseg(:lenrseg)//
     .           '_atdep_'//cyear1//'-'//cyear2//'.ave'
      end if
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(11,'(a300)',err=992,end=994) header
      if (header(:8).eq.'NO LOADS') then
        do nl = 1,nloads
          latdep(nl) = 0.0
        end do

      else

        read(11,'(a300)',err=993,end=994) dataline

        allfound = .false.
        do nl = 1,nloads
          found(nl) = .false.
        end do
  
        do while (.not.allfound)
          call shift(header)
          call shift(dataline)
          read(header,*,err=995,end=995) Tload
          do nl = 1,nloads
            if (Tload.eq.loadname(nl)) then
              read(dataline,*,err=996,end=996) latdep(nl)
              found(nl) = .true.
            end if
          end do
          allfound = .true.
          do nl = 1,nloads
            if (.not.found(nl)) allfound = .false.
          end do
        end do

      end if
      close(11)

      return

1001  format(a15,',',25(5x,a4,','))
1234  format(i5,1x,i4,76x,e14.7,31x,e14.7)

**************ERROR SPACE ******************************************
991   print*,fnam
      report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading header line' 
      go to 999

993   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with reading load line' 
      go to 999

994   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' end of file reached unexpectedly'
      go to 999

995   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with parsing header line' 
      go to 999

996   report(1) = 'Problem reading following file'
      report(2) = fnam
      report(3) = ' problem with parsing load line' 
      go to 999

999   call stopreport(report)

      end
