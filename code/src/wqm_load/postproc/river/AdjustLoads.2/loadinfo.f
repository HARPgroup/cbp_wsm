************************************************************************
******* Routine to populate variables containing information on the   **
*********** variables to report                                       **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine loadinfo(rscen,lenrscen,nRvar,Rname,
     .                    nloads,loadname,unit,ncons,con,confactor)

      implicit none
      include 'reservoir_type_II.inc'

      integer i,j,nc,nl,Rvar     ! indices

      character*200 pline                    ! line long enough to read loadfile line

      integer Tncons                  ! temporary variable for reading a line

      logical found,allfound

************ END DECLARATIONS
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      fnam=catdir//'iovars/'//ioscen(:lenioscen)//
     .      '/rchres_reservoir_type_II'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nloads = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(10x,i4)',iostat=err) Tncons
        if (err.eq.0.and.Tncons.ne.0) then
          nloads = nloads + 1
          loadname(nloads) = pline(:4)
          do i = 1,4
            if (loadname(nloads)(i:i).eq.' ') loadname(nloads)(i:i) ='_'
          end do
          unit(nloads) = pline(6:9)
          ncons(nloads) = Tncons
          do i = 1,Tncons
            con(nloads,i) = pline(2+14*i:5+14*i)
            if (con(nloads,i).eq.'    ') go to 992
            if (pline(7+14*i:15+14*i).eq.'         ') then
              confactor(nloads,i) = 1.0
            else
              read(pline(7+14*i:15+14*i),'(f9.0)') confactor(nloads,i)
            end if
          end do

          allfound = .true.
          do nc = 1,ncons(nloads)    ! test for existance of all loads
            found = .false.
            do Rvar = 1,nRvar
              if (con(nloads,nc).eq.Rname(Rvar)) found = .true.
            end do
            if (.not.found) allfound = .false.
          end do
          if (.not.allfound) nloads = nloads - 1 ! forget the load

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

992   write(report(1),*) Tncons,' factors were specified in file'
      report(2) = fnam
      write(report(3),*)'only ',(con(nloads,j),j=1,Tncons),' were named'
      go to 999

999   call stopreport(report)

      end 
