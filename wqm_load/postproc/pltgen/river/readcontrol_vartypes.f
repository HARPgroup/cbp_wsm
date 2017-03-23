************************************************************************
** given the land scenario, finds the variable types in pltgens       **
************************************************************************
      subroutine readcontrol_vartypes(rscen,maxplts,
     O                                pltname,numplts)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer maxplts, numplts
      character*8 pltname(maxplts)

      logical comment,pltgen
      external comment

      integer i                     ! index
**************** END DECLARATIONS *************************
      
      call lencl(rscen,lenrscen)
      
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      numplts = 0
      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        read(dfile,'(a100)')line
        call d2x(line,last)
        if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
          read(dfile,'(a100)')line
          call d2x(line,last)
          do while (line(:10).ne.'END PLTGEN')
                numplts = numplts + 1
                pltgen = .true.
             if (pltgen) then
              call d2x(line,last)
              read(line,*) pltname(numplts)
            end if
            read(dfile,'(a100)')line
            call d2x(line,last)
          end do
        end if
      end do
   
      close(dfile)

      return
************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)
      end

