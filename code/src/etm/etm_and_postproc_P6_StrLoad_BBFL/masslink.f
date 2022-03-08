************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nLvar, Ldsn, Lname    **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
**  if recoding for more land uses is necessary, you should only have **
**     to change the length of the variable 'lin3', the format of any **
**     statement that reads 'lin3', and the test line going to 992    **
************************************************************************
      subroutine masslink(ioscen,lenioscen,modules,nmod,
     O                    nRvar,Rdsn,Rname,RvarBMP,
     O                    nLvar,Ldsn,Lname,Lfactor)
      implicit none

      include 'land.inc'

      integer i,l,n,nR,nm     ! indices

      integer dsnR,dsnL                   ! temp variables for reading
      character*1 cl                      ! temp character variable
      character*4 name,nameR,nameL            !     catalog files
      character*3 bmp

      character*6 Tmod                  ! temp variable for modules
      character*300 lin3                  ! reading variable
      real factor

      logical found,scompare,scompcase,comment
      external scompare,scompcase,comment

      integer lenf                               ! length of land use field
      parameter (lenf=nlu*4)                     !   in file land_to_water
      character lus(lenf)                        ! change to lenf
                                                 ! also change in format statement
******** populate nRvar, Rdsn, Rname, RvarBMP from 'rchres_in'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_in'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvar = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4,3x,a3)',iostat=err)Tmod,dsn,name,bmp
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod
            if (scompcase(Tmod,modules(nm))) found=.true.
          end do
          if (found) then
            nRvar = nRvar + 1
            Rdsn(nRvar) = dsn
            Rname(nRvar) = name
            RvarBMP(nRvar) = bmp
          end if
        end if
        read(dfile,'(a100)') line
      end do

      close (dfile)

******* populate nLvar, Ldsn, Lname, Lfactor from 'land_to_river'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/land_to_river'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,maxRvar     ! initialize number of land variables
        do l = 1,nlu
          nLvar(i,l) = 0
        end do
      end do

      read(dfile,'(a300)') lin3
      if (lin3(296:300).ne.'      ') go to 992

      do while (lin3(:3).ne.'end')
        read(lin3,1234,iostat=err)Tmod,dsnR,nameR,dsnL,nameL,factor
        if (lin3(35:44).eq.'          ') factor = 1.0
        if (err.eq.0.and.dsnR.ne.0.and..not.comment(lin3)) then
          found = .false.
          do nm = 1,nmod       ! test if module in use
            if (scompare(Tmod,modules(nm))) found = .true.
          end do
          if (found) then
            do nR = 1,nRvar
              if (Rdsn(nR).eq.dsnR) then
                do l = 1,nlu
                  i = 47         ! first column with land use
                  do while(i.lt.298)
                    if (luname(l).eq.lin3(i:i+2)) then
                      nLvar(nR,l) = nLvar(nR,l) + 1
                      Ldsn(nR,l,nLvar(nR,l)) = dsnL
                      Lname(nR,l,nLvar(nR,l)) = nameL
                      Lfactor(nR,l,nLvar(nR,l)) = factor
                    end if
                    i = i+4
                  end do
                end do
              end if
            end do
          end if
        end if
        read(dfile,'(a300)') lin3
      end do
              
      close (dfile)

      return

1234  format(a6,3x,i4,2x,a4,3x,i4,2x,a4,2x,f10.0)

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1)='Characters near the far right of file: land_to_river'
      report(2)=' indicate that recoding may be necessary to allow for'
      report(3)=' more land uses ./code/src/etm/transfer_wdm/masslink.f'
      go to 999

999   call stopreport(report)

      end 
