***********************************************************************
** subroutine get percent of export of lst 3 years Refractory ORGN  ***
***********************************************************************
      subroutine getvarfac(
     I                     ioscen,lenioscen,landvar, rivervar,thislseg,
     O                     varfac)
 
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      character*4 land
      character*6 Tlseg,thislseg

      character*4 rivervar,landvar
      integer lenLvar,lenRvar

      character*4 ltemp
      integer ic,i
      real varfac

      logical found
      logical scompcase   ! string compare subroutine
************* END DECLARATION *****************************************

      land = 'land'
      call lencl(landvar,lenLvar)
      call lencl(rivervar,lenRvar)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/variable_l2r_factors/'//
     .       landvar(:lenLvar)//'_to_'//rivervar(:lenRvar)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
   
      read(dfile,'(a100)',err=992,end=993) line
      call d2x(line,last)
      call findcomma(line,ic)    ! check that first column is land
      ltemp = line(:ic-1)
      call trims(ltemp,last)
      call lowercase(ltemp)
      if (ltemp.ne.'land'.and.ltemp.ne.'lseg') go to 995
      
      found = .false.
      do 
        read(dfile,'(a100)',err=992,end=111)line
        call findcomma(line,ic)
        Tlseg = line(:ic-1)
        call trims(Tlseg,last)
        if (Tlseg(:6) .eq. thislseg) then
          found = .true.
          call shift(line)
          read(line,*,err=992,end=993) varfac 
          exit
        end if
      end do

111   close(dfile)

      if (.not.found) go to 994
       
      return

************ ERROR SPACE **************************************
991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'unexpected end of line in file'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'segment '//thislseg// 'not found in '
      report(2) = fnam
      report(3) = fnam(60:)
      go to 999

995   report(1) = 'problem with file:'
      report(2) = fnam
      report(3) = 'first column of first line should be land or lseg'
      go to 999


999   call stopreport(report)
      end



      subroutine getvarfac2(
     I                     ioscen,lenioscen,c3varid,
     I                     landvar, rivervar,thislseg,
     O                     varfac)

      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      character*4 land
      character*6 Tlseg,thislseg

      character*4 rivervar,landvar
      integer lenLvar,lenRvar

      character*4 ltemp
      integer ic,i

      character*3 c3varid
      real varfac

      logical found
      logical scompcase   ! string compare subroutine
************* END DECLARATION *****************************************

      land = 'land'
      call lencl(landvar,lenLvar)
      call lencl(rivervar,lenRvar)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/variable_'//c3varid//'_factors/'//
     .       landvar(:lenLvar)//'_to_'//rivervar(:lenRvar)//'.csv'
c      print*,fnam
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)',err=992,end=993) line
      call d2x(line,last)
      call findcomma(line,ic)    ! check that first column is land
      ltemp = line(:ic-1)
      call trims(ltemp,last)
      call lowercase(ltemp)
      if (ltemp.ne.'land'.and.ltemp.ne.'lseg') go to 995

      found = .false.
      do
        read(dfile,'(a100)',err=992,end=111)line
        call findcomma(line,ic)
        Tlseg = line(:ic-1)
        call trims(Tlseg,last)
        if (Tlseg(:6) .eq. thislseg) then
          found = .true.
          call shift(line)
          read(line,*,err=992,end=993) varfac
          exit
        end if
      end do

111   close(dfile)

      if (.not.found) go to 994

      return

************ ERROR SPACE **************************************
991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'unexpected end of line in file'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'segment '//thislseg// 'not found in '
      report(2) = fnam
      report(3) = fnam(60:)
      go to 999

995   report(1) = 'problem with file:'
      report(2) = fnam
      report(3) = 'first column of first line should be land or lseg'
      go to 999


999   call stopreport(report)
      end


