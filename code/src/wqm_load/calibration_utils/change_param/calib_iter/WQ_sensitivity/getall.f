************************************************************************
** subroutine get lake flags for all rivers                           **
************************************************************************
      subroutine getlakeflags(
     I                        paramscen,uniqindex,
     O                        lakeflags)
      implicit none
      include 'sens.inc'

      logical comment
      external comment
      integer ic,uniqueID,i,lwc,Nexits

      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/gen_info_rseg.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a1)') c  ! get rid of header
      read(dfile,*)Tseg
      do while (Tseg.ne.'end')
        read(Tseg(5:8),'(i4)') uniqueID
        backspace(dfile) 
        read(dfile,*,end=992,err=993) 
     .       Tseg,Nexits,lakeflags(uniqindex(uniqueID))
        read(dfile,*,end=992,err=993) Tseg
      end do

      close (dfile)

      return

********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

992   report(1) = 'end of file found unexpectedly:'
      report(2) = fnam
      write(report(3),*) 'near line ',Tseg
      go to 999

993   report(1) = 'Problem reading file:'
      report(2) = fnam
      write(report(3),*) 'near line ',Tseg
      go to 999

999   call stopreport(report)
      end


