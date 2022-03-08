************************************************************************
******* Routine to populate variables containing information on the   **
*********** variables to report                                       **
********** using the files in the pp/lib/catalogs/ directory          **
****** this program gets the unique load names in the file            **
******** scenario_parameter_modifications.  They must be the same     **
******* as in the rchres_out_to_load file                             **
************************************************************************
      subroutine getparloads(
     I                       ioscen,lenioscen,
     O                       nparloads,parloadname)

      implicit none
      include 'scencompare.inc'

      character*200 pline   ! line long enough to read loadfile line

      character*4 Tload

      integer np
      logical found

      logical comment
      external comment

********** END DECLARATIONS 

********** open data file
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/scenario_parameter_modifications'  
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

******** loop over lines, looking for unique loadnames
      nparloads = 0
      do 
        read(11,'(a200)') pline
        if (comment(pline)) cycle
        if (pline(:3).eq.'end') exit
        if (pline(:3).eq.'END') exit

        read(pline(39:42),'(a4)',iostat=err) Tload 
        if (err.ne.0) cycle
        if (Tload.eq.'    ') cycle

        found = .false.
        do np = 1,nparloads
          if (Tload.eq.parloadname(np)) found = .true.
        end do
        if (found) cycle

********** passed all tests, new load
************** process the line
        nparloads = nparloads + 1
        parloadname(nparloads) = Tload

      end do

      close (11)

      return

***************** ERROR SPACE ******************************************
991   report(1) = 'Problem opening '
      report(2) = fnam
      write(report(3),*)' error code = ',err
      go to 999

992   report(1) = 'Problem in file:'
      report(2) = fnam
      report(3) = 'on line for '//loadname(nloads)
      go to 999

999   call stopreport(report)

      end 
