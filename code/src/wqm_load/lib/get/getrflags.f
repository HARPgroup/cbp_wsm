************************************************************************
** sub to get flags for a river segment                               **
************************************************************************
      subroutine getrflags(
     I                     paramscen,lenparamscen,rseg,
     O                     Nexits,lakeflag,resflag,timestep)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'

      integer i,lwc,Nexits,lakeflag,timestep

      character*1 resflag

      Nexits = -1
      lakeflag = -1
      resflag = 'X'

      call findopen(lwc)
      fnam = pardir//'river/'//paramscen(:lenparamscen)
     .       //'/gen_info_rseg.csv'
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(lwc,'(a1)') c    ! ditch header
      do 
        read(lwc,'(a100)',err=1001,end=992) line
        call d2x(line,last)
        read(line,*,err=995,end=995) Tseg
        if (Tseg.eq.rseg) exit
        call lowercase(Tseg)
        if (Tseg(:3).eq.'end') go to 992
      end do
      read(line,*,err=995,end=996) Tseg,Nexits,lakeflag,resflag,timestep
      if (timestep.gt.60) go to 993
      if (mod(60,timestep).ne.0) go to 994
      close (lwc)

      if (Nexits.lt.1.or.lakeflag.lt.0) go to 1003

      return

*********** ERROR SPACE
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'Could not find segment '//rseg//' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'segment '//rseg//' in file:'
      report(2) = fnam
      report(3) = ' had a time step greater than 1 hour: not allowed'
      go to 999

994   report(1) = 'segment '//rseg//' in file:'
      report(2) = fnam
      report(3) = ' had a time step of which 60 is not a multiple'
      go to 999

995   report(1) = 'error parsing file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

996   report(1) = 'not enough data in line of file:'
      report(2) = fnam
      report(3) = line
      go to 999

1001  report(1) = 'Could not read file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1002  report(1) = 'Could not read line for segment '//rseg
      report(2) = ' in file: '
      report(3) = fnam
      go to 999

1003  report(1) = 'invalid numbers for Nexits and lake flag, segment'
     .            //rseg
      report(2) = 'Nexits = XXX, lakeflag = YYY'
      write(report(2)(10:12),'(i3)') Nexits
      write(report(2)(26:28),'(i3)') lakeflag
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

************************************************************************
** sub to get flags for a set of river segments                       **
************************************************************************
      subroutine getallrflags(
     I                     paramscen,lenparamscen,rsegs,nrsegs,
     O                     Nexitss,lakeflags,resflags,timesteps)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      integer i,lwc,nr
      integer Nexitss(maxrsegs),lakeflags(maxrsegs),timesteps(maxrsegs)
      logical found(maxrsegs),rivercheck
      external rivercheck

      character*1 resflags(maxrsegs)

      do nr = 1,nrsegs
        found(nr) = .false.
      end do

      do nr = 1,nrsegs  ! set non-rivers to default parameters
        if (.not.rivercheck(rsegs(nr))) then
          found(nr) = .true.
          Nexitss(nr) = 0
          lakeflags(nr) = -9
          resflags(nr) = 'x'
          timesteps(nr) = 60
        end if
      end do

      call findopen(lwc)
      fnam = pardir//'river/'//paramscen(:lenparamscen)
     .       //'/gen_info_rseg.csv'
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(lwc,'(a1)') c    ! ditch header
      do 
        read(lwc,'(a100)',err=1001,end=992) line
        call d2x(line,last)
        read(line,*,err=995,end=995) Tseg
        do nr = 1,nrsegs
          if (Tseg.eq.rsegs(nr)) then
            found(nr) = .true.
            read(line,*,err=995,end=995) Tseg,Nexitss(nr),lakeflags(nr),
     .                                        resflags(nr),timesteps(nr)
            if (timesteps(nr).gt.60) go to 993
            if (mod(60,timesteps(nr)).ne.0) go to 994
            if (Nexitss(nr).lt.1.or.lakeflags(nr).lt.0) go to 1003
            exit
          end if
        end do
        call lowercase(Tseg)
        if (Tseg(:3).eq.'end') exit
      end do
      close (lwc)

      do nr = 1,nrsegs
        if (.not.found(nr)) go to 997
      end do

      return

*********** ERROR SPACE
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'file ended before literal '//char(39)//'end'//
     .            char(39)//' found'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'segment '//rsegs(nr)//' in file:'
      report(2) = fnam
      report(3) = ' had a time step greater than 1 hour: not allowed'
      go to 999

994   report(1) = 'segment '//rsegs(nr)//' in file:'
      report(2) = fnam
      report(3) = ' had a time step of which 60 is not a multiple'
      go to 999

995   report(1) = 'error parsing file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

996   report(1) = 'not enough data in line of file:'
      report(2) = fnam
      report(3) = line
      go to 999

997   report(1) = 'did not find segement '//rsegs(nr)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

1001  report(1) = 'Could not read file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1002  report(1) = 'Could not read line for segment '//rseg
      report(2) = ' in file: '
      report(3) = fnam
      go to 999

1003  report(1) = 'invalid numbers for Nexits and lake flag, segment'
     .            //rseg
      report(2) = 'Nexits = XXX, lakeflag = YYY'
      write(report(2)(10:12),'(i3)') Nexitss(nr)
      write(report(2)(26:28),'(i3)') lakeflags(nr)
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end


