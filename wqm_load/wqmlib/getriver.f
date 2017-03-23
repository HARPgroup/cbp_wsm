************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getriver(
     I                    geoscen,lengeoscen,
     O                    rsegs,uniqindex,uniqid,dsid,nrsegs,fnam,err)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'
      logical comment
      external comment
      integer ic

      nrsegs = 0
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          nrsegs = nrsegs + 1
          read(Tseg(5:8),'(i4)')uniqid(nrsegs)
          read(Tseg(10:13),'(i4)')dsid(nrsegs)
          rsegs(nrsegs) = Tseg
          uniqindex(uniqid(nrsegs))=nrsegs
        end if
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
      end do
      close (dfile)
      return

******************* error space ****************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'literal '//char(39)//'end'//char(39)//' not found'
      go to 999

999   call stopreport(report)
      end

