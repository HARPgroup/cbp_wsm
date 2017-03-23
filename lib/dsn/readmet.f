************************************************************************
**  program to read from annie export (sequential) files              **
************************************************************************
      implicit none
      integer year1,year2
      parameter (year1=1983,year2=1998)

      real hourly(year1:year2,12,31,24)
      real daily(year1:year2,12,31)

      character*4 dsn

      character*100 fnam,line
      character*1,dummy

      integer maxvalues,nv  ! number of consecutive different values 
      parameter (maxvalues=1000)
      real value(maxvalues)

      integer repeatfg    ! flag for repeating values

      integer nd,nm,ny,nh,nt  ! indicies
      integer err       ! error value for file open

      integer i,j,k,l,m,n  ! dummies

      integer ndaysinmonth  ! function
      external ndaysinmonth

****************** END SPECIFICATIONS ----------------------------------

      read*,fnam                             ! open data file
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991


      read(11,'(a100)') line              ! ditch headers
      do  while (line(:3).ne.'DSN')
        read(11,'(a100)') line
      end do
      backspace(11)

      do

        read(11,'(a100)',end=111) line
        dsn = line(13:16)

        do while (line(:9).ne.'    TCODE')
          read(11,'(a100)') line
        end do

        if (line(20:20).eq.'4') then  ! daily

          do while (line(:6).ne.'  DATA')
            read(11,'(a100)') line
          end do
          read(11,'(a100)') line

          call dailyinit(daily,year1,year2)

          do while (line(:10).ne.'  END DATA')
  
            read(line,*,err=992)ny,nm,nd,i,j,k,l,m,n,
     .                          nv,repeatfg,value(1)

            if (nv.gt.maxvalues) go to 993

            if (repeatfg.eq.1) then   ! repeats
              do i = 1,nv
                value(i) = value(1)
              end do
            else         ! need to read them all
              backspace 11
              read(11,*,err=992)ny,nm,nd,i,j,k,l,m,n,nv,repeatfg,
     .                          (value(i),i=1,nv)
            end if

            do i = 1,nv
              call tomorrow(ny,nm,nd)
              daily(ny,nm,nd) = value(i)
            end do

            read(11,'(a100)',err=992,end=994) line
          end do

          call dailywrite(daily,fnam,dsn,year1,year2)

        else if (line(20:20).eq.'3') then  ! hourly

          do while (line(:6).ne.'  DATA')
            read(11,'(a100)') line
          end do
          read(11,'(a100)') line

          call hourlyinit(hourly,year1,year2)

          do while (line(:10).ne.'  END DATA')
  
            read(line,*,err=992)ny,nm,nd,nh,j,k,l,m,n,
     .                          nv,repeatfg,value(1)

            if (nv.gt.maxvalues) go to 993

            if (repeatfg.eq.1) then   ! repeats
              do i = 1,nv
                value(i) = value(1)
              end do
            else         ! need to read them all
              backspace 11
              read(11,*,err=992)ny,nm,nd,nh,j,k,l,m,n,nv,repeatfg,
     .                          (value(i),i=1,nv)
            end if

            do i = 1,nv
              call increment(ny,nm,nd,nh)
              hourly(ny,nm,nd,nh) = value(i)
            end do

            read(11,'(a100)',err=992,end=994) line
          end do

          call hourlywrite(hourly,fnam,dsn,year1,year2)

        else

          go to 996

        end if

        read(11,'(a100)') line  ! this line should say 'END DSN'

      end do

111   close(11)

      stop

************* ERROR SPACE **********************************************
991   print*,'could not open file'
      print*,fnam
      print*,'error = ',err
      stop

992   print*,'error reading file near line'
      print*,fnam
      print*,line
      stop

993   print*,'maxvalues parameter must be increased'
      stop

994   print*,'file'
      print*,fnam
      print*,'file ended before END DATA found'
      stop

996   print*,'only allowable time codes are 3 and 4'
      print*,'recode the program'
      stop
      end


************************************************************************
***** increments one hour                                             **
************************************************************************
      subroutine increment(ny,nm,nd,nh)
      nh = nh + 1
      if (nh.eq.25) then
        nh = 1
        call tomorrow(ny,nm,nd)
      end if
      end

************************************************************************
***** increments one day                                              **
************************************************************************

      subroutine tomorrow(iyear,imonth,iday)

      iday = iday + 1

      if (iday.gt.ndaysinmonth(iyear,imonth)) then
        iday = 1
        imonth = imonth + 1
        if (imonth.gt.12) then
          imonth = 1
          iyear = iyear + 1
        end if
      end if

      end
************************************************************************
***** finds days in month for a given year                            **
************************************************************************

      function ndaysinmonth(iyear,imonth)

      integer nominal(12)

      data nominal /31,28,31,30,31,30,31,31,30,31,30,31/

      ndaysinmonth = nominal(imonth)

      if (imonth.eq.2) then
        if (mod(iyear,4).eq.0)   ndaysinmonth = 29
        if (mod(iyear,100).eq.0) ndaysinmonth = 28
        if (mod(iyear,400).eq.0) ndaysinmonth = 29
      end if

      end

************************************************************************
** subroutines to initialize daily and hourly variables               **
************************************************************************
      subroutine dailyinit(daily,year1,year2)
      implicit none
      integer year1,year2
      real daily(year1:year2,12,31)
      integer ny,nm,nd
      do nd = 1,31
        do nm = 1,12
          do ny = year1,year2
            daily(ny,nm,nd) = -9.0
          end do
        end do
      end do
      end

      subroutine hourlyinit(hourly,year1,year2)
      implicit none
      integer year1,year2
      real hourly(year1:year2,12,31,24)
      integer ny,nm,nd,nh
      do nh = 1,24
        do nd = 1,31
          do nm = 1,12
            do ny = year1,year2
              hourly(ny,nm,nd,nh) = -9.0
            end do
          end do
        end do
      end do
      end

************************************************************************
** subroutines to print daily and hourly variables                    **
************************************************************************
      subroutine dailywrite(daily,fnam,dsn,year1,year2)
      implicit none
      integer year1,year2
      character*(*) dsn
      character*(*) fnam
      character*100 fnam2
      real daily(year1:year2,12,31)
      integer ny,nm,nd,i
      integer err
      integer ndaysinmonth
      external ndaysinmonth
      do i = 1,len(dsn)
        if (dsn(i:i).eq.' ') dsn(i:i) = '0'
      end do
      i = 1
      do while (fnam(i:i).ne.'.'.and.fnam(i:i).ne.' ')
        fnam2(i:i)=fnam(i:i)
        i = i + 1
      end do
      fnam2(i:i+len(dsn)-1) = dsn
      fnam2(i+len(dsn):) = fnam(i:)
      open(12,file=fnam2,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      do ny = year1,year2
        do nm = 1,12
          do nd = 1,ndaysinmonth(ny,nm)
            write(12,*)ny,',',nm,',',nd,',',daily(ny,nm,nd)
          end do
        end do
      end do
      close(12)
      return
************* ERROR SPACE **********************************************
991   print*,'could not open file'
      print*,fnam
      print*,'error = ',err
      stop
      end

      subroutine hourlywrite(hourly,fnam,dsn,year1,year2)
      implicit none
      integer year1,year2
      character*(*) dsn
      character*(*) fnam
      character*100 fnam2
      real hourly(year1:year2,12,31,24)
      integer ny,nm,nd,nh,i
      integer err
      integer ndaysinmonth
      external ndaysinmonth
      do i = 1,len(dsn)
        if (dsn(i:i).eq.' ') dsn(i:i) = '0'
      end do
      i = 1
      do while (fnam(i:i).ne.'.'.and.fnam(i:i).ne.' ')
        fnam2(i:i)=fnam(i:i)
        i = i + 1
      end do
      fnam2(i:i+len(dsn)-1) = dsn
      fnam2(i+len(dsn):) = fnam(i:)
      open(12,file=fnam2,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      do ny = year1,year2
        do nm = 1,12
          do nd = 1,ndaysinmonth(ny,nm)
            do nh = 1,24
              write(12,*)ny,',',nm,',',nd,',',nh,',',hourly(ny,nm,nd,nh)
            end do
          end do
        end do
      end do
      close(12)
      return
************* ERROR SPACE **********************************************
991   print*,'could not open file'
      print*,fnam
      print*,'error = ',err
      stop
      end

