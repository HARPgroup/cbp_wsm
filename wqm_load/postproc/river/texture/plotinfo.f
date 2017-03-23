************************************************************************
******* Routine to populate the variables that describe the           **
**        constituents of each water quality variable                 **
********** using the file  ./pp/lib/catalogs/rchres_out_to_conc       **
************************************************************************
      subroutine plotinfo(rscen,lenrscen,nRvar,Rname,
     O                    nplots,plotname,unit,norm,ncons,con,confactor)

      implicit none
      include 'Rdaily.inc'

      integer i,np,Rvar     ! indices

      character*200 pline    ! line long enough to read plotfile line

      integer Tncons         ! temporary variable for reading a line

      logical found,allfound

*********** END DECLARATIONS
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_to_conc'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 

      nplots = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(16x,i4)',iostat=err) Tncons
        if (err.eq.0.and.Tncons.ne.0) then
          nplots = nplots + 1
          plotname(nplots) = pline(:4)
          unit(nplots) = pline(6:9)
          norm(nplots) = pline(11:14)
          ncons(nplots) = Tncons
          do i = 1,Tncons
            con(nplots,i) = pline(8+14*i:11+14*i)
            read(pline(13+14*i:21+14*i),'(f9.0)') confactor(nplots,i)
            if (con(nplots,i).eq.'    ') go to 992
          end do
          allfound = .true.
          do np = 1,ncons(nplots)    ! test for existance of all plots
            found = .false.
            do Rvar = 1,nRvar
              if (con(nplots,np).eq.Rname(Rvar)) found = .true.
            end do
            if (.not.found) allfound = .false.
          end do
          if (.not.allfound) nplots = nplots - 1 ! forget the plot
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

992   report(1) = 'file '//fnam
      report(2) = 'specifies '//pline(17:20)//' factors for plot = '//
     .            pline(:4)
      report(3) = '  but only supplies '
      write(report(3)(21:22),'(i2)') i-1
      go to 999

999   call stopreport(report)

      end 
