************************************************************************
** takes a row of comma delimited variables and performs the          **
**   specified action on the specified column                         **
************************************************************************
      subroutine doaction(parline,action,value,ivalue,variable,
     .                    varcolumn,typeflag,Tseg,paramscen,err)
      implicit none
      character*(*) parline
      character*(*) Tseg
      character*(*) variable
      character*(*) paramscen

      character*1 action  ! [m,a,e,p] = [multiply,add,equals,percentile]
      real value, oldval, newval   ! value for action and old value
      integer ivalue,oldival,newival      ! value for action and old value


      character*1 typeflag  ! [i,r] = [integer,real]

      integer varcolumn  ! column that the variable of interest occupies
 
      integer err

      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      real findtaupercent
      external findtaupercent

      logical scompcase
      external scompcase

      character*2 na
      data na /'na'/

      err = 0

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
      if (scompcase(parline(firstcomma+1:lastcomma-1),na)) return
      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        read(parline(firstcomma+1:lastcomma-1),*,err = 992) oldival  
        if (action.eq.'m'.or.action.eq.'M') then
          newival = oldival * ivalue
        else if (action.eq.'a'.or.action.eq.'A') then
          newival = oldival + ivalue
        else if (action.eq.'e'.or.action.eq.'E') then
          newival = ivalue
        end if
        parline(firstcomma+11:) = parline(lastcomma:)
        write(parline(firstcomma+1:firstcomma+10),'(i10)') newival
        call noblanks(parline,i)
        print*,Tseg,' ',oldival,' ',newival
      else
        read(parline(firstcomma+1:lastcomma-1),*,err = 993) oldval
        if (action.eq.'m'.or.action.eq.'M') then
          newval = oldval * value
          if (variable(:4).eq.'AGWR') then
            newval = 1.0 - (1.0 - oldval)/value
          end if
        else if (action.eq.'a'.or.action.eq.'A') then
          newval = oldval + value
        else if (action.eq.'e'.or.action.eq.'E') then
          newval = value
        else if (action.eq.'p'.or.action.eq.'P') then
          newval = findtaupercent(Tseg,paramscen,value)
        end if
        parline(firstcomma+11:) = parline(lastcomma:)
        call ritef10(parline(firstcomma+1:firstcomma+10),newval)
        call noblanks(parline,i)
        print*,Tseg,' ',oldval,' ',newval
      end if

      return

992   err = 2
      return

993   err = 3
      return

      end
      
