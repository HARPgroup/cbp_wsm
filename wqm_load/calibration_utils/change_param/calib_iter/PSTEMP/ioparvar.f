************************************************************************
** takes a row of comma delimited variables and reads or writes       **
************************************************************************
      subroutine getvar(parline,varcolumn,
     O                  value)
      implicit none
      include '../../../../lib/inc/standard.inc'
      character*(*) parline
      real value   ! output from that column
      integer varcolumn  ! column that the variable of interest occupies
 
      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) go to 991
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      read(parline(firstcomma+1:lastcomma-1),*,err = 991) value

      return
991   report(1) = 'problem reading line in parameter file'
      report(2) = parline(:64)
      report(3) = ' '
      go to 999

999   call stopreport(report)
      return
      end
     
      subroutine putvar(
     M                  parline,
     I                  varcolumn,value)
      implicit none
      include '../../../../lib/inc/standard.inc'
      character*(*) parline
      real value   ! output from that column
      integer varcolumn  ! column that the variable of interest occupies

      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) go to 991
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      parline(firstcomma+11:) = parline(lastcomma:)
      call ritef10(parline(firstcomma+1:firstcomma+10),value)
      call noblanks(parline,i)

      return
991   report(1) = 'problem writing line in parameter file'
      report(2) = parline(:64)
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end
 
