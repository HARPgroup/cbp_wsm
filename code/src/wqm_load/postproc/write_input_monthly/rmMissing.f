************************************************************************
** checks the variable 'monthly' for missing values, and takes them   **
**  out.  Adjusts the date variables as well                          **
************************************************************************
      subroutine rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                            nB,Jday,Byear,Bmonth,Bday,monthly)
      implicit none
      include '../../lib/inc/standard.inc'

      integer maxTimeBreaks, nB  ! number of breaks

      integer Jday(maxTimeBreaks)  ! dates of breaks
      integer Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks)
      integer Bday(maxTimeBreaks)

      integer year1,year2 ! start and stop of the run

      character*3 clu  ! land use

      logical somemissing(maxTimeBreaks)  ! helpful variables
      integer missing,n,n2,month
      integer ndaysinyear
      external ndaysinyear

      real monthly(maxTimeBreaks,12)  ! variable to check for missing

*************** check for missing values
      missing = 0
      do n = 1,nB
        somemissing(n) = .false.
        if (monthly(n,1).lt.-8) somemissing(n) = .true.
        do month = 2,12
          if (monthly(n,month).lt.-8.and..not.somemissing(n)) go to 991
          if (monthly(n,month).gt.-8.and.somemissing(n)) go to 991
        end do
        if (somemissing(n)) missing = missing + 1
      end do
      do n = 1,nB - missing
        do while (somemissing(n))
          do n2 = n,nB-1
            do month = 1,12
              monthly(n2,month) = monthly(n2+1,month)
            end do
            somemissing(n2) = somemissing(n2+1)
            Jday(n2) = Jday(n2+1)
            Byear(n2) = Byear(n2+1)
            Bmonth(n2) = Bmonth(n2+1)
            Bday(n2) = Bday(n2+1)
          end do
        end do
      end do
      nB = nB - missing

********** with missing values removed, it may be necessary to make another point
      if (nB.eq.1) then
        nB = 2
        do month = 1,12
          monthly(2,month) = monthly(1,month)
        end do
        Byear(2) = Byear(1) + 1
        Bmonth(2) = Bmonth(1)
        Bday(2) = Bday(1)
        if (Bmonth(1).lt.3) then
          Jday(2) = Jday(1) + ndaysinyear(Byear(1)) - 1
        else
          Jday(2) = Jday(1) + ndaysinyear(Byear(2)) - 1
        end if
      end if

C      if (nB.eq.0) then  ! this is fine.  means no data, do not write

      return

991   report(1) = 'problem in lug: If some monthly data are missing for'
      report(2) = ' the nutrient input files, than all months must be'
      write(report(3),*) ' missing ',lseg,' ',clu
      do n = 1,nB
        print*,n,(',',monthly(n,month),month = 1,12)
      end do
      go to 999

999   call stopreport(report)
      end


************************************************************************
** checks the variable 'monspecies' for missing values, and removes   **
**  dates with all missing values.  When some species exist for a     **
**  sets all missing values for other species to zero.                **
**        Adjusts the date variables as well                          **
************************************************************************
      subroutine rmMissingMonthlySpecies(
     I                            maxTimeBreaks,lseg,clu,year1,year2,
     I                            nspecies,
     M                            nB,Jday,Byear,Bmonth,Bday,monspecies)
      implicit none
      include '../../lib/inc/standard.inc'

      integer maxTimeBreaks, nB  ! number of breaks
      integer nspecies  ! number of species

      integer Jday(maxTimeBreaks)  ! dates of breaks
      integer Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks)
      integer Bday(maxTimeBreaks)

      integer year1,year2 ! start and stop of the run

      character*3 clu  ! land use

      logical missingTime(maxTimeBreaks)  ! helpful variables
      logical missingTimeSpec(maxTimeBreaks,nspecies)
      integer missing,n,n2,month,ns
      integer ndaysinyear
      external ndaysinyear

      real monspecies(maxTimeBreaks,nspecies,12)  ! data variable

*************** check for missing values in some months while
******* values exist for other months in that time/species combination
******* determine which time/species combinations are missing
      do ns = 1,nspecies
        do n = 1,nB
          missingTimeSpec(n,ns) = .false.
          if (monspecies(n,ns,1).lt.-8) missingTimeSpec(n,ns) = .true.
          do month = 2,12
            if (monspecies(n,ns,month).lt.-8.0 .and.
     .          .not.missingTimeSpec(n,ns)) go to 991
            if (monspecies(n,ns,month).gt.-8.0 .and.
     .          missingTimeSpec(n,ns)) go to 991
          end do
        end do
      end do

********* loop over time breaks.  If all species missing
******** then mark the time break as missing.  If not, change any
******** missing values to zero
      missing = 0
      do n = 1,nB

        missingTime(n) = .true.  ! get missingTime
        do ns = 1,nspecies
          if (.not.missingTimeSpec(n,ns)) missingTime(n) = .false.
        end do

        if (.not.missingTime(n)) then
          do ns = 1,nspecies
            do month = 1,12
              if (monspecies(n,ns,month).lt.-8.0) 
     .            monspecies(n,ns,month) = 0.0
            end do
          end do
        else
          missing = missing + 1
        end if

      end do

******** adjust variable for missing time periods
      do n = 1,nB - missing
        do while (missingTime(n))
          do n2 = n,nB-1
            do ns = 1,nspecies
              do month = 1,12
                monspecies(n2,ns,month) = monspecies(n2+1,ns,month)
              end do
            end do
            missingTime(n2) = missingTime(n2+1)
            Jday(n2) = Jday(n2+1)
            Byear(n2) = Byear(n2+1)
            Bmonth(n2) = Bmonth(n2+1)
            Bday(n2) = Bday(n2+1)
          end do
        end do
      end do
      nB = nB - missing

********** with missing values removed, it may be necessary to make another point
      if (nB.eq.1) then
        nB = 2
        do month = 1,12
          do ns = 1,nspecies
            monspecies(2,ns,month) = monspecies(1,ns,month)
          end do
        end do
        Byear(2) = Byear(1) + 1
        Bmonth(2) = Bmonth(1)
        Bday(2) = Bday(1)
        if (Bmonth(1).lt.3) then
          Jday(2) = Jday(1) + ndaysinyear(Byear(1)) - 1
        else
          Jday(2) = Jday(1) + ndaysinyear(Byear(2)) - 1
        end if
      end if

C      if (nB.eq.0) then  ! this is fine.  means no data, do not write

      return

991   report(1) = 'problem in lug: If some monthly data are missing for'
      report(2) = ' the nutrient input files, than all months must be'
      write(report(3),*) ' missing ',lseg,' ',clu
      do n = 1,nB
        print*,n,(',',monspecies(n,ns,month),month = 1,12)
      end do
      go to 999

999   call stopreport(report)
      end

************************************************************************
** checks the variable 'NandP' for missing values, and takes them   **
**  out.  Adjusts the date variables as well                          **
************************************************************************
      subroutine rmMissing2(maxTimeBreaks,lseg,clu,year1,year2,
     M                            nB,Jday,Byear,Bmonth,Bday,NandP)
      implicit none
      include '../../lib/inc/standard.inc'

      integer maxTimeBreaks, nB  ! number of breaks

      integer Jday(maxTimeBreaks)  ! dates of breaks
      integer Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks)
      integer Bday(maxTimeBreaks)

      integer year1,year2 ! start and stop of the run

      character*3 clu  ! land use

      logical somemissing(maxTimeBreaks)  ! helpful variables
      integer missing,n,n2,month
      integer ndaysinyear
      external ndaysinyear

      real NandP(maxTimeBreaks,2)  ! variable to check for missing

*************** check for missing values
      missing = 0
      do n = 1,nB
        somemissing(n) = .false.
        if (NandP(n,1).lt.-8) somemissing(n) = .true.
        if (NandP(n,2).lt.-8.and..not.somemissing(n)) go to 991
        if (NandP(n,2).gt.-8.and.somemissing(n)) go to 991
        if (somemissing(n)) missing = missing + 1
      end do
      do n = 1,nB - missing
        do while (somemissing(n))
          do n2 = n,nB-1
            NandP(n2,1) = NandP(n2+1,1)
            NandP(n2,2) = NandP(n2+1,2)
            somemissing(n2) = somemissing(n2+1)
            Jday(n2) = Jday(n2+1)
            Byear(n2) = Byear(n2+1)
            Bmonth(n2) = Bmonth(n2+1)
            Bday(n2) = Bday(n2+1)
          end do
        end do
      end do
      nB = nB - missing

********** with missing values removed, it may be necessary to make another point
      if (nB.eq.1) then
        nB = 2
        NandP(2,1) = NandP(1,1)
        NandP(2,2) = NandP(1,2)
        Byear(2) = Byear(1) + 1
        Bmonth(2) = Bmonth(1)
        Bday(2) = Bday(1)
        if (Bmonth(1).lt.3) then
          Jday(2) = Jday(1) + ndaysinyear(Byear(1)) - 1
        else
          Jday(2) = Jday(1) + ndaysinyear(Byear(2)) - 1
        end if
      end if

C      if (nB.eq.0) then  ! this is fine.  means no data, do not write

      return

991   report(1) = 'problem in lug: If some N and P data are missing for'
      report(2) = ' the nutrient uptake files, than both must be'
      write(report(3),*) ' missing ',lseg,' ',clu
      go to 999

999   call stopreport(report)
      end


