************************************************************************
**  subroutine to get the load for 1 land segment in terms of the     **
**     delivery variables                                             **
************************************************************************
      subroutine lsegann(
     I                   rseg,lenrseg,lseg,rscen,lenrscen,
     I                   ndel,delname,sdate,edate,nloads,
     M                   inann)
      implicit none
      include 'tfs.inc'

      integer nl,ndl,lu,ny,Tny

      integer columndel(ndelmax)      ! column of each delivered load

      character*4 Tloadname(nloadmax),Tdum1,Tdum2,Tdum3

      logical founddel(ndelmax)

      real Tload(nloadmax)

      call lencl(lseg,lenlseg)

      do lu = 1,nlu
        if (luname(lu).eq.'wat') cycle

        do ndl = 1,ndel
          founddel(ndl) = .false.
        end do

        fnam = outdir//'eor/annual/'//rscen(:lenrscen)//
     .           '/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           luname(lu)//'.ann'
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

        do ny = sdate(1),edate(1)
          read(11,*,err=993)Tny,(Tload(nl),nl=1,nloads)
          if (ny.ne.Tny) go to 995
          do ndl = 1,ndel
            inann(ndl,ny) = inann(ndl,ny)+Tload(columndel(ndl))
          end do

        end do

        close(11)

      end do     ! end loop over land uses

      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' in first line' 
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      write(report(3),*) ' for year: ',ny
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
