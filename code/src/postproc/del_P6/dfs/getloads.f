************************************************************************
******* Routine to populate variables containing information on the   **
*********** variables to report                                       **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine getloads(ioscen,lenioscen,nRvar,Rname,
     O                    nloads,loadname)

      implicit none
      include 'dfs.inc'

      character*200 pline     ! line long enough to read loadfile line

      integer Tncons
      integer i,nc,Rvar                  ! indice
      logical found,allfound

      integer maxcons
      parameter (maxcons=10)
      integer ncons(nloadmax)     ! number of constituents to each load
      character*4 con(nloadmax,maxcons) ! concentration variable to load

********** END DECLARATIONS 
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_to_load'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nloads = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(10x,i4)',iostat=err) Tncons
        if (err.eq.0.and.Tncons.ne.0) then
          nloads = nloads + 1
          loadname(nloads) = pline(:4)
          ncons(nloads) = Tncons
          do i = 1,Tncons
            con(nloads,i) = pline(2+14*i:5+14*i)
            if (con(nloads,i).eq.'    ') go to 992
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
