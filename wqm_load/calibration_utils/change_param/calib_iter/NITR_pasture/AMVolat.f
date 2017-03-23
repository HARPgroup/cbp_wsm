************************************************************************
*** program to update Param for table NIT-AMVOLAT 
*** The table is not turned on for all landuses
************************************************************************
      subroutine AMVolat(parline,vcolumn,clu,TsimNH4,TNH4tar)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/land_use.inc'

      character*(*) parline
      real value   ! output from that column
      real TsimNH4,TNH4tar
   
      integer vcolumn  ! column that the variable of interest occupies
      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index
      logical scompare
   
      character*3 na(6),clu
      data na /'na','NA','Na','n/a','N/a','N/A'/

**************** END DECLARATION ****************************************
      ncomma = 0
      i = 0

      do while (ncomma.lt.vcolumn-1)
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

      if (scompare(parline(firstcomma+1:lastcomma-1),na(1)).or.
     .    scompare(parline(firstcomma+1:lastcomma-1),na(2)).or.
     .    scompare(parline(firstcomma+1:lastcomma-1),na(3)).or.
     .    scompare(parline(firstcomma+1:lastcomma-1),na(4)).or.
     .    scompare(parline(firstcomma+1:lastcomma-1),na(5)).or.
     .    scompare(parline(firstcomma+1:lastcomma-1),na(6))) then

         print*,'NH4 VOLAT is not applicable to landuse  ', clu
      else
         read(parline(firstcomma+1:lastcomma-1),*,err = 991) value
         value = value*TsimNH4/TNH4tar
         if (value .lt. 0.000001) value = 0.000001         !set max and min value
         if (value .gt. 50.) value = 50.

         parline(firstcomma+11:) = parline(lastcomma:)
         call ritef10(parline(firstcomma+1:firstcomma+10),value)
         call noblanks(parline,i)   
      end if

      return

****************** ERROR SPACE *******************************************
991   report(1) = 'problem reading line in parameter file'
      report(2) = parline(:2000)
      report(3) = ' '
      go to 999

999   call stopreport(report)
      return
      end

 
