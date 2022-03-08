************************************************************************
**  subroutine to get the load for 1 land segment in terms of the     **
**     delivery variables                                             **
************************************************************************
      subroutine datave(
     I                   rseg,lenrseg,lseg,rscen,lenrscen,
     I                   ndel,delname,year1,year2,nloads,
     I                   dops,doatdep,dosep,
     M                   inave)
      implicit none
      include 'tfs.inc'

      integer nl,ndl,lu

      integer columndel(ndelmax)      ! column of each delivered load

      character*4 Tloadname(nloadmax),Tdum1,Tdum2,Tdum3,cy1,cy2

      logical founddel(ndelmax)

      real Tload(nloadmax)

      character*11 datname(5)
      integer ndats,ndat,lendat

      logical dops,doatdep,dosep

******************* END DECLARATION ********************************
      call lencl(lseg,lenlseg)

      write(cy1,'(i4)')year1
      write(cy2,'(i4)')year2

************ fild data files
      ndats = 0
      if (dops) then             ! three files for pointsource
        ndats = ndats + 1
        datname(ndats) = wwtp
      end if
      if (dops) then
        ndats = ndats + 1
        datname(ndats) = indus
      end if
      if (dops) then
        ndats = ndats + 1
        datname(ndats) = cso
      end if
      if (doatdep) then
        ndats = ndats + 1
        datname(ndats) = atdep
      end if
      if (dosep) then
        ndats = ndats + 1
        datname(ndats) = sep
      end if

************ read and add loads together from each data file
      do ndat = 1,ndats

        do ndl = 1,ndel
          founddel(ndl) = .false.
        end do

        call lencl(datname(ndat),lendat)
        
        fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .           '/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           datname(ndat)(:lendat)//'_'//cy1//'-'//cy2//'.ave'
        open(11,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
         
        read (11,'(a100)') line
        if (line(:8).eq.'NO LOADS') cycle   !   go to next landuse
        backspace 11
  
        read (11,*,err=992) Tdum1,(Tloadname(nl),nl=1,nloads)
        do ndl = 1,ndel
          do nl = 1,nloads
            if (Tloadname(nl).eq.delname(ndl)) then
              founddel(ndl) = .true.
              columndel(ndl) = nl
            end if
          end do
        end do

        do ndl = 1,ndel
          if (.not.founddel(ndl)) goto 994
        end do

        read(11,*,err=993)Tdum1,(Tload(nl),nl=1,nloads)
        do ndl = 1,ndel
          inave(ndl) = inave(ndl)+Tload(columndel(ndl))
        end do

        close(11)

      end do     ! end loop over ndats

      return

************************* ERROR SPACE
991   report(1) = '  $$postproc/del/tfs/datave Problem opening '
      report(2) = fnam
CBHATT      report(3) = 'error code = '
      report(3) = rseg(:lenrseg)//'_'//
     .           datname(ndat)(:lendat)//'_'//cy1//'-'//cy2//'.ave'
      write(report(3)(33:35),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' in first line' 
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = ' Did not find load in file'
      report(2) = fnam
      report(3) = delname(ndl)
      go to 999

995   report(1) = ' Dates in file:'
      report(2) = fnam
      report(3) = ' did not match river scenario control file'
      go to 999

999   call stopreport(report)

      end

