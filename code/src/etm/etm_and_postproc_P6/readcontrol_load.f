************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times                                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_load(
     I                            rscen,lenrscen,
     I                            C_STABLE, I_STABLE,
     I                            C_ETABLE, I_ETABLE,
     O                            nLB,LByear,LBfile)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
 
      integer maxTimeBreaks
      parameter (maxTimeBreaks = 35)
      integer LByear(maxTimeBreaks)             ! year of break
      integer LBmonth(maxTimeBreaks)            ! month
      integer LBday(maxTimeBreaks)              ! day
      character*40 LBfile(maxTimeBreaks)

      character*100 templine(maxTimeBreaks)

      logical comment

      integer nLB,l,n,i

      logical findload

      character*40 C_STABLE, C_ETABLE
      integer      I_STABLE, I_ETABLE 

*************** END OF DECLARATION *************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      findload = .false.
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
        if (.not.comment(line)) then

C          print*,'->',line(:I_STABLE),':',C_STABLE(:I_STABLE),'<-'
          if (line(:I_STABLE).eq.C_STABLE(:I_STABLE)) then
            findload = .true.
            nLB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:I_ETABLE).ne.C_ETABLE(:I_ETABLE))
C              print*,'->',line(:I_ETABLE),':',C_ETABLE(:I_ETABLE),'<-'
              if (.not.comment(line)) then
                nLB = nLB + 1
                templine(nLB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nLB
              read(templine(n),1234)
     .              LByear(n),LBmonth(n),LBday(n),LBfile(n)
            end do
          end if

        end if
      end do

      close (dfile)

      if (.not.findload) go to 992

1234  format(i4,i3,i3,1x,a40)

      return
*********** ERROR SPACE ************************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'did not find '//C_STABLE(:I_STABLE)//
     .             ' line in control file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end


