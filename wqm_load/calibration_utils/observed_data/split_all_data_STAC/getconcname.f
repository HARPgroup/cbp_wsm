************************************************************************
******* Routine to populate the variables that describe the           **
**        constituents of each water quality variable                 **
********** using the file  ./pp/lib/catalogs/rchres_out_to_conc       **
************************************************************************
      subroutine getconcname(nconcs,concname)

      implicit none
      include 'WQdata.inc'
      character*200 pline    ! line long enough to read concfile line

      integer Tnccons         ! temporary variable for reading a line
********************* END DECLARATIONS ********************************** 

      fnam = catdir//'iovars/rchres_out_to_conc'  
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 

      nconcs = 0
      read(dfile,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(16x,i4)',iostat=err) Tnccons
        if (err.eq.0.and.Tnccons.ne.0) then
          nconcs = nconcs + 1
          concname(nconcs) = pline(:4)
        end if
        read(dfile,'(a200)') pline
      end do

      close (dfile)

      return

*************** ERROR SPACE ********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end 
