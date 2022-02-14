      subroutine getscenmod(
     I              rscen,lenrscen,rseg,lenrseg,C4_scenmod,
     O              R_scenmod)

      include 'scrorgx.inc'

      integer ifl
      parameter ( ifl = 21 )

      logical foundld

      character*4 Tload
      character*50 Tscen
      real st,sstr,sp,ss,sa
      real ct,cstr,cp,cs,ca

      print*,' getscenmod'
      fnam = outdir//'river/scenario_compare/'//
     .       rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundld = .false.
            
      read(ifl,*,err=992,end=992) line    ! get rid of header line
      do  
        read(ifl,*,err=993,end=111) Tload,Tscen,st,sstr,sp,ss,sa,
     .                                    Tscen,ct,cstr,cp,cs,ca

        if (ct.lt.0.0001) go to 994  ! check for div0 error

        print*,'<',Tload,'>','<',C4_scenmod,'>'
        if (Tload .eq. C4_scenmod) then
            foundld = .true.
            R_scenmod = st/ct
            print*,C4_scenmod, ' R_scenmod = ',R_scenmod
            close(ifl)
            return
        end if

      end do

      if (.not.foundld) go to 995
          
111   close(ifl)

      stop


991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = fnam//' error reading file'
      report(2) = line
      write(report(3),*) 'error = ',err
      go to 999

993   report(1) = 'error parsing line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'check for div0 error, value too low'
      report(2) = fnam
      write(report(3),*) ct,cstr,cp,cs,ca
      go to 999

995   report(1) = 'entry for load not found in the file'
      report(2) = 'Load parameter '//C4_scenmod
      report(3) = 'File '//fnam
      go to 999

999   call stopreport(report)
      end
