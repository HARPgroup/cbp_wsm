************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nLvar, Ldsn, Lname    **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
**  if recoding for more land uses is necessary, you should only have **
**     to change the length of the variable 'lin3', the format of any **
**     statement that reads 'lin3', and the test line going to 992    **
************************************************************************
      subroutine getBMPcons(ioscen,lenioscen,
     O                      BMPconname,nbcon)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/masslinks.inc'

      logical comment
      external comment

******** populate nRvar, Rdsn, Rname, RvarBMP from 'rchres_in'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/bmp_constituents'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nbcon = 0
      do 
        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
        if (line(:3).eq.'   ') cycle
        if (comment(line)) cycle
        if (line(:3).eq.'end') exit
        nbcon = nbcon + 1
        if (nbcon.gt.maxBMPcon) go to 993
        read(line,'(a3)',iostat=err) BMPconname(nbcon)
        if (err.ne.0) go to 994
      end do

111   close (dfile)

      return

***************** ERROR SPACE ******************************************
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
