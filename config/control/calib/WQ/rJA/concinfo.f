************************************************************************
*** Routine to get names of possible WQ constituents (concentrations) **
**** using the file  ./config/catalog/iovars/$scen/rchres_out_to_conc **
************************************************************************
      subroutine concinfo(
     I                    ioscen,lenioscen,
     O                    nconcs,concname)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      character*200 pline    ! line long enough to read concfile line

      integer Tnccons         ! temporary variable for reading a line

*********** END DECLARATIONS
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_to_conc'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 

      nconcs = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(16x,i4)',iostat=err) Tnccons
        if (err.eq.0.and.Tnccons.ne.0) then
          nconcs = nconcs + 1
          concname(nconcs) = pline(:4)
        end if
        read(11,'(a200)') pline
      end do

      close (11)

      return

*************** ERROR SPACE ********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end 
