************************************************************************
******* Routine to populate the variables that describe the           **
**        constituents of each water quality variable                 **
********** using the file  ./pp/lib/catalogs/rchres_out_to_conc       **
************************************************************************
      subroutine concinfo(rscen,lenrscen,nRvar,Rname,
     O                    nconcs,concname,cunit,norm,nccons,ccon,
     O                    cconfactor)

      implicit none
      include 'Rstats.inc'

      integer i,np,Rvar     ! indices

      character*200 pline    ! line long enough to read concfile line

      integer Tnccons         ! temporary variable for reading a line
      logical found,allfound

*********** END DECLARATIONS
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

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
          cunit(nconcs) = pline(6:9)
          norm(nconcs) = pline(11:14)
          nccons(nconcs) = Tnccons
          do i = 1,Tnccons
            ccon(nconcs,i) = pline(8+14*i:11+14*i)
            read(pline(13+14*i:21+14*i),'(f9.0)') cconfactor(nconcs,i)
            if (ccon(nconcs,i).eq.'    ') go to 992
          end do
          allfound = .true.
          do np = 1,nccons(nconcs)    ! test for existance of all concs
            found = .false.
            do Rvar = 1,nRvar
              if (ccon(nconcs,np).eq.Rname(Rvar)) found = .true.
            end do
            if (.not.found) allfound = .false.
          end do
          if (.not.allfound) nconcs = nconcs - 1 ! forget the conc
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
      report(2) = 'specifies '//pline(17:20)//' factors for conc = '//
     .            pline(:4)
      report(3) = '  but only supplies '
      write(report(3)(21:22),'(i2)') i-1
      go to 999

999   call stopreport(report)

      end 
