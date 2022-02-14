************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine gettarscen(calscen,
     O                      tarscen)

      implicit none
      include 'imp_sed.inc'

      logical found,comment
      character*11 tabname
      character*3 templu
      character*6 tempar
      character*(*)tarscen

      integer i           ! index
      integer nc

************** END DECLARATION ****************************************

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/SEDMNT/'//calscen(:lencalscen)
     .          //'/sed_parms.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      
      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)

      do while (line(:3).ne.'end')
       if (.not.comment(line).and.line(:6).eq.'TARGET') then
         read(dfile,'(a100)',err=992,end=993) line
         do while (line(:10).ne.'END TARGET')
           if (.not.comment(line)) then
             tarscen = line(:6)
           end if
           read(dfile,'(a100)',err=992,end=993) line
         end do
       end if 
          
       read(dfile,'(a100)') line
       call d2x(line,last)
    
      end do               ! finish reading the whole file

      close (dfile)

      return
            
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'file ended without finding literal =>end'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
     
      end


