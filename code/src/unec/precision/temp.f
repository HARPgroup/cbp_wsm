      implicit none

      integer dfile
      parameter (dfile=10)
      character*100 line
      character*10  str

      double precision i,k,l
      integer j
      integer err

      open (dfile,file='temp.txt',status='old',iostat=err)
      if (err.ne.0) go to 991

      read (dfile,'(a100)',err=992,end=115) line
      print*,line

      read (line,*,end=995,err=995) str,i,l
      i = 10.21D0
      do j=1,100
        k = j*i
        print*,'\n\nHello World',k
      end do

115   close (dfile)

      return

991   print*,'Error Opening File'
      goto 999

992   print*,'Error Reading File'
      goto 999

995   print*,'Error Reading Line'
      goto 999

999   end
