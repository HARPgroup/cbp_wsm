************************************************************************
******* Routine to populate variables containing information on the   **
*********** variables to report                                       **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine loadinfo(
     I                    ioscen,lenioscen,
     I                    nRvar,Rname,
     I                    nparloads,parloadname,
     O                    nloads,loadname,unit,ncons,con,confactor)

      implicit none
      include 'scencompare.inc'

      character*200 pline                    ! line long enough to read loadfile line

      integer Tncons
      integer i,nc,Rvar,np                  ! indices
      logical found,foundallcon,foundparload

********** END DECLARATIONS 
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_to_load'
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

******** loop over lines, looking for valid loadnames
      nloads = 0
      read(11,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(10x,i4)',iostat=err) Tncons
        if (err.eq.0.and.Tncons.ne.0) then

************** process the line
          nloads = nloads + 1
          loadname(nloads) = pline(:4)
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

***************** test for existance of all cons
          foundallcon = .true.
          do nc = 1,ncons(nloads)   
            found = .false.
            do Rvar = 1,nRvar
              if (con(nloads,nc).eq.Rname(Rvar)) found = .true.
            end do
            if (.not.found) foundallcon = .false.
          end do

***************** test if parameter modifcation load
          foundparload = .false.
          do np = 1,nparloads
            if (parloadname(np).eq.loadname(nloads)) foundparload=.true.
          end do

**************** forget the load if not relevant and simulated
          if (.not.foundallcon.or..not.foundparload) nloads = nloads - 1

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
