************************************************************************
** Makes the External Sources block                                   **
************************************************************************
      subroutine rextsources(rscen,lenrscen,ioscen,lenioscen,
     .                       rseg,Nexits,timestep)
      implicit none
      include 'rug.inc'

      integer dsn          ! index
      integer numsegs
      real factor

      logical comment,scompare
      character*100 templine

      integer nnodiv,nd
      parameter (nnodiv=3)       ! input variables that are 
      character*6 nodiv(nnodiv)  ! not timestep dependent
      data nodiv/'DEWTMP','GATMP ','CLOUD '/
      logical found

*************** END DECLARATION ****************************************

      call getl2r(rseg,rscen,lenrscen,
     O            numsegs,l2r)       ! get land segs for each river

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_extsources'
      open(dfile,file=fnam,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      line = ' '
      call ryt(line,uci)

***************get ps, septic, and diversion 
      read(dfile,'(a100)') line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (line(:8).eq.'Nexits=4') then
          if (Nexits.eq.4) then
            if (timestep.lt.60) then
              found = .false.
              do nd = 1,nnodiv
                if (line(74:79).eq.nodiv(nd)) found = .true.
              end do
              if (.not.found.and.line(47:50).eq.'SAME') 
     .          line(47:50) = 'DIV '
            end if
            call ryt(line(9:),uci)
          end if
        else
          if (timestep.lt.60) then
            found = .false.
            do nd = 1,nnodiv
              if (line(66:71).eq.nodiv(nd)) found = .true.
            end do
            if (.not.found.and.line(39:42).eq.'SAME')
     .        line(39:42) = 'DIV '
          end if
          call ryt(line,uci)
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      close (dfile)

*************** get upstream loads
      line = '*** UPSTREAM and EOS INPUT ***'
      call ryt(line,uci)
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_in'
      open (dfile,file=fnam,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      line = 'WDM4   dsn name     ENGLZERO          SAME RCHRES   1'
     .       //'     group_ member____'
      if (timestep.lt.60) line(39:42) = 'DIV '

      read(dfile,'(a100)') templine
      call d2x(templine,last)
      do while (templine(:3).ne.'end')
        if (.not.comment(templine).and.templine(:3).ne.'   ') then
          line(59:64) = templine(14:19)          ! group
          line(66:75) = templine(21:30)         ! member
          read(templine(8:10),'(i3)')dsn         ! dsn
          write(line(8:10),'(i3)') dsn
          line(12:15) = templine(35:38)         ! name
          call ryt(line,uci)
        end if
        read(dfile,'(a100)')templine
        call d2x(templine,last)
      end do
      close (dfile)

      line = 'END EXT SOURCES'
      call ryt(line,uci)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end
