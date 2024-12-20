***************************************************************************
** Subroutine to get the unique ID of each rive segs                     **
**                                                                       **
***************************************************************************
      subroutine getvsegs(
     O                     nr,rsegs)

      implicit none
      include 'WQdata.inc'

      character*110 cline
      character*13 catnam          ! reiver segment
     
      integer nr,ic 

      logical comment
      external comment
************************* END DECLARATIONS ***********************************

      fnam = tree//'pp/observed/validation_segs.csv'
      open (unit=dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nr = 0
      do 
        read(dfile,'(a110)',err=992,end=992) cline                ! read line
        call d2x(cline,last)
        if (comment(cline)) cycle  ! ditch header

        if (cline(:3).eq.'end') exit

***********loop over all river segments
        nr = nr + 1
        call findcomma(cline,ic)              
        catnam = cline(:ic-1)                 ! get the name of river segment
        call trims(catnam,ic)
        rsegs(nr)=catnam(:ic)
      end do                                 !loop over segments
 
993   close(dfile)

     
      return 

*********** ERROR SPACE *******************************************************

991   report(1) = 'Problem with opening gage file'//fnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999


999   call stopreport(report)

      end 
