************************************************************************
** sub to get special action flags for a landuse                      **
************************************************************************
      subroutine getspecflags(lseg,lscen,clu,specflags)

      implicit none
      include 'lug.inc'
      include 'acts.inc'
      include '../../lib/inc/land_use.inc'

      character*300 dline
      character*6 ctemp
      character*6 Tlseg

      character*4 land
      integer order          ! indexed to the column, not land use
      integer i,l,nm

      logical scompcase                ! string compare subroutineompare
      logical found

      character*25 specscen
      integer lenspecscen

************* END DECLARATIONHS ****************************************
      call lencl(lscen,lenlscen)
      call lencl(lseg,lenlseg)

      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)
      
      call lencl(specscen,lenspecscen)

      fnam = ScenDatDir//'land/spec_flags/'//specscen(:lenspecscen)//
     .       '/spec_flags.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 
      
      land = 'land'

      read(dfile,'(a300)',err=1000)dline         ! read header line
      call findcomma(dline,i)                    ! check that first column is land
      ctemp = dline(:i-1)
      call trims(ctemp,i)
      if (.not.scompcase(ctemp(:i),land)) go to 992
      call shift(dline)

************* FIND THE ORDER OF THE LAND USE ***************

      order = 0 
      found = .false.

      do l = 1,nlu                               
        if (.not. found) then
          call findcomma(dline,i)
          ctemp = dline(:i-1)
          call trims(ctemp,i)
          if (ctemp(:i).ne.' ') then
            if (ctemp(:i) .eq. clu) then
              order = l
              found = .true.
            end if
            call shift(dline)
          end if
        end if
      end do

      if (.not.found) go to 993

*********** READ DOWN TO FIND LAND SEG **********************
      found = .false.
      do while ( .not. found)                                
        read(dfile,'(a300)',err=1001) dline

        if (dline(:3).eq.'end') go to 994

        call findcomma(dline,last)
        Tlseg = dline(:last-1)
        if (Tlseg .eq. lseg) then
          found = .true.
          if (.not.found) go to 995
          do l = 1,order                    ! we have land seg, read across
            call shift(dline)                ! according to order found above
            call findcomma(dline,last)
            if (last.gt.flaglength+1) go to 996
            specflags = dline(:last-1)
          end do
        end if
      end do

      close (dfile)

      return

*********** ERROR SPACE ************************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'first column must be the Land segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

993   report(1) = 'Could not find landuse '//clu//' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = 'reach the end of file '
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'did not find land seg '//lseg//' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

996   report(1) = 'flag for seg, land use:'//lseg//', '//clu//'in file'
      report(2) = fnam
      report(3) = ' is too long: modify pp/src/lug/lug.inc '
      go to 999

1000  report(1) = 'Could not read file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read line for landseg '//lseg
      report(2) = ' in file: '
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

