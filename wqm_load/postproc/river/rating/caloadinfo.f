************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine caloadinfo(rscen,lenrscen,nRvar,Rname,
     O                    nloads,loadname,unit,ncons,con,confactor)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/masslinks.inc'
 
      integer maxcons
      parameter (maxcons = 10)            ! maximum number of constituents in a load
      integer nloadmax                    !
      parameter (nloadmax = 20)           ! maximum number of loads
      integer nloads                      ! number of loads
      character*4 loadname(nloadmax)      ! name of each load (e.g. 'totn', 'flow')
      character*4 unit(nloadmax)          ! units for load (e.g. mg/l cfs )
      character*4 norm(nloadmax)          ! normalization variable (usually volume of water)
      integer ncons(nloadmax)             ! number of constituents to each load
      character*4 con(nloadmax,maxcons)   ! concentration variable to load
      real confactor(nloadmax,maxcons)    ! factor for the variable

      integer i,np,Rvar     ! indices

      character*200 pline                    ! line long enough to read plotfile line

      integer Tncons                  ! temporary variable for reading a line

      logical found,allfound

*********** END DECLARATIONS
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_to_conc'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 

      nloads = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(16x,i4)',iostat=err) Tncons
        if (err.eq.0.and.Tncons.ne.0) then
          nloads = nloads + 1
          loadname(nloads) = pline(:4)
          unit(nloads) = pline(6:9)
          ncons(nloads) = Tncons
          do i = 1,Tncons
            con(nloads,i) = pline(8+14*i:11+14*i)
            read(pline(13+14*i:21+14*i),'(f9.0)') confactor(nloads,i)
            if (con(nloads,i).eq.'    ') go to 992
          end do
          allfound = .true.
          do np = 1,ncons(nloads)    ! test for existance of all loads
            found = .false.
            do Rvar = 1,nRvar
              if (con(nloads,np).eq.Rname(Rvar)) found = .true.
            end do
            if (.not.found) allfound = .false.
          end do
          if (.not.allfound) nloads = nloads - 1         ! forget the plot
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
      report(2) = 'specifies '//pline(17:20)//' factors for '//
     .            pline(:4)
      report(3) = '  but only supplies '
      write(report(3)(21:22),'(i2)') i-1
      go to 999

999   call stopreport(report)

      end 
