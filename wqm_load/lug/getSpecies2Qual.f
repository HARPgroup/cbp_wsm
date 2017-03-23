************************************************************************** gets the special action species and the PQUAL and IQUAL variables  **
**   that correspond to them for parameter modification               **
************************************************************************
      subroutine getSpecies2Qual(
     I                           lscen,lenlscen,nspecies,species,
     O                           nquals,nspecies2qual,species2qual)
      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include 'acts.inc'
      logical comment,found
      external comment
      character*4 tempspec(maxSpecies2Qual) ! reading variables
      integer ns,ns2  ! indices

**************** END DECLARATIONS **************************************
      call readcontrol_Lgeoscen(
     I                          lscen,lenlscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'iovars/'//geoscen(:lengeoscen)//
     .       '/quals_to_special_action_species'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nquals = 0
      do
        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
        if (comment(line)) cycle
        if (line(:20).eq.'                    ') cycle

        nquals = nquals + 1
        if (nquals.gt.maxQuals) go to 996
        read(line,*,end=993,err=993) nq,nspecies2qual(nq),
     .                            (tempspec(ns),ns=1,nspecies2qual(nq))
        if (nspecies2qual(nq).gt.maxSpecies2Qual) go to 995
        do ns = 1,nspecies2qual(nq)
          found = .false.
          do ns2 = 1,nspecies
            if (species(ns2).eq.tempspec(ns)) then
              found = .true.
              species2qual(nq,ns) = ns2
              exit
            end if
          end do
          if (.not.found) go to 994
        end do
      end do
111   close(dfile)
      return
************* error space
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'species '//tempspec(ns)//' found in file'
      report(2) = fnam
      report(3) = ' does not match list of allowable species'
      go to 999

995   report(1) = ' problem with number of species matched to qual in'
      report(2) = fnam
      report(3) = ' modify file or increase maxSpecies2Qual in code'
      go to 999

996   report(1) = 'more quality constituents than expected in file'
      report(2) = fnam
      report(3) = 'be extremely careful with modifications to this file'
      go to 999

999   call stopreport(report)
      end


