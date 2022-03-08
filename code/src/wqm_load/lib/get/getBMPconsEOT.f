************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nLvar, Ldsn, Lname    **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
**  if recoding for more land uses is necessary, you should only have **
**     to change the length of the variable 'lin3', the format of any **
**     statement that reads 'lin3', and the test line going to 992    **
************************************************************************
      subroutine getBMPconsEOT(ioscen,lenioscen,
     I                      BMPconname,nbcon,
     O                      C_BMPconnameEOT,I_BMPconnameEOT,nbconEOT,
     O                      C_EOTrvars,I_EOTrvars)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/masslinks.inc'

      logical comment
      external comment

      integer nbconEOT,ibcon
      integer irvar
      logical found
******** populate nRvar, Rdsn, Rname, RvarBMP from 'rchres_in'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .        '/bmp_constituents_EOT'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nbconEOT = 0
      do 
        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
        if (line(:3).eq.'   ') cycle
        if (comment(line)) cycle
        if (line(:3).eq.'end') exit
        nbconEOT = nbconEOT + 1
        if (nbconEOT.gt.maxBMPcon) go to 993
        read(line(:4),'(a4)',iostat=err) C_BMPconnameEOT(nbconEOT)
        if (err.ne.0) go to 994
        found = .false.
        print*,'>',line(6:8),'<'
        do ibcon=1,nbcon
           if(line(6:8).eq.BMPconname(ibcon))  then
              I_BMPconnameEOT(nbconEOT) = ibcon
              found = .true.
           end if
        end do
        if (.not. found) goto 800

        read(line(10:13),'(I4)',iostat=err) I_EOTrvars(nbconEOT)
        print*,'fn getBMPconsEOT ',I_EOTrvars(nbconEOT)
        do irvar = 1,I_EOTrvars(nbconEOT)
           read(line(10+irvar*5:10+irvar*5+3),'(A4)',iostat=err) 
     .              C_EOTrvars(nbconEOT,irvar)
        end do
        
      end do

111   close (dfile)

      return

***************** ERROR SPACE ******************************************
800   report(1) = 'BMP constituent not found'
      report(2) = fnam
      report(3) = line
      go to 999
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' problem reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) ='problem with file, more BMP constituents than allowed'
      report(2) = fnam
      report(3) = 'modify src/lib/inc/masslinks.inc'
      go to 999

994   report(1) = ' problem reading file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

999   call stopreport(report)

      end 
