************************************************************************
**  program to read from annie export (sequential) files              **
************************************************************************
      implicit none
      integer year1,year2
      parameter (year1=1980,year2=2003)

      real flow(year1:year2,12,31)

      character*100 fnam,line
      character*1,dummy

      integer maxvalues,nv  ! number of consecutive different values 
      parameter (maxvalues=400)
      real value(400)

      integer repeatfg    ! flag for repeating values

      integer nd,nm,ny  ! indicies
      integer err       ! error value for file open

      integer i,j,k,l,m,n  ! dummies

      integer ndaysinmonth  ! function
      external ndaysinmonth

****************** END SPECIFICATIONS ----------------------------------

      do nd = 1,31                    ! Initialize
        do nm = 1,12
          do ny = year1,year2
            flow(ny,nm,nd) = -9.0
          end do
        end do
      end do

      read*,fnam                             ! open data file
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991


      read(11,'(a100)') line              ! ditch headers
      do  while (line(:6).ne.'  DATA')
        read(11,'(a100)') line
      end do

      read(11,'(a100)') line
      do while (line(:10).ne.'  END DATA')

	read(line,*,err=992)ny,nm,nd,i,j,k,l,m,n,nv,repeatfg,value(1)

        if (nv.gt.maxvalues) go to 993

        if (repeatfg.eq.1) then   ! repeats
          do i = 1,nv
            value(i) = value(1)
          end do
        else         ! need to read them all
          read(line,*,err=992)ny,nm,nd,i,j,k,l,m,n,nv,repeatfg,
     .                        (value(i),i=1,nv)
        end if

        do i = 1,nv
          flow(ny,nm,nd) = value(i)
          call tomorrow(ny,nm,nd)
        end do

        read(11,'(a100)',err=992,end=994) line
      end do

      close(11)

      do i = 1,90
        if (fnam(i:i+2).eq.'txt') fnam(i:i+2) = 'csv'
      end do
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do ny = year1,year2
        do nm = 1,12
          do nd = 1,ndaysinmonth(ny,nm)
            write(11,*)ny,',',nm,',',nd,',',flow(ny,nm,nd)
          end do
        end do
      end do

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

