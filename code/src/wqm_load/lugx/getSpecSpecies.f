************************************************************************** gets the special action species and the PQUAL and IQUAL variables  **
**   that correspond to them for parameter modification               **
************************************************************************
      subroutine getSpecSpecies(
     I                          lscen,lenlscen,
     O                          species,nspecies)
      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include 'acts.inc'
      logical comment
      external comment

      call readcontrol_Lioscen(
     I                          lscen,lenlscen,
     O                          ioscen)
      call lencl(ioscen,lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/special_action_species'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nspecies = 0
      do
        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
        if (comment(line)) cycle
        if (line(:20).eq.'                    ') cycle

        nspecies = nspecies + 1
        read(line,*,end=993,err=993) species(nspecies)
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

999   call stopreport(report)
      end


