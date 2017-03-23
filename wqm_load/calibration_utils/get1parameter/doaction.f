************************************************************************
** takes a row of comma delimited variables and returns the           **
**   specified column value                                           **
************************************************************************
      subroutine doaction(parline,varcolumn,typeflag,
     .                    oldval,oldival,err)
      implicit none
      character*(*) parline

      character*1 action  ! [m,a,e] = [multiply,add,equals]
      real value, oldval, newval   ! value for action and old value
      integer ivalue,oldival,newival   ! value for action and old value


      character*1 typeflag  ! [i,r] = [integer,real]

      integer varcolumn  ! column that the variable of interest occupies
 
      integer err

      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      err = 0
      oldival = -999
      oldval = -999.0

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) then
          err = 1
          return
        end if
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        read(parline(firstcomma+1:lastcomma-1),*,err = 992) oldival  
      else
        read(parline(firstcomma+1:lastcomma-1),*,err = 993) oldval
      end if

      return

992   err = 2
      return

993   err = 3
      return

      end
      
