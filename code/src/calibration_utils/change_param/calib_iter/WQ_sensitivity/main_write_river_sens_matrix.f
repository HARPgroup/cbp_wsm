************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program reads the sensitivity matrix and writes the results   **
**   of the latest run back to the matrix                             **
** Values in the matrix are for the cfd bins                          **
************************************************************************
      implicit none
      include 'sens.inc'
      include 'sensmatrix.inc'

      integer Tnb  ! temp reading variable
      character*1 dummy  ! temp reading variable

      integer nr
 
      integer testnrsegs,testnconcs

************* END DECLARATIONS *****************************************
      read*,rscen,year1,year2
      call lencl(rscen,lenrscen)
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      print*,'reading and writing sensitivity matrix'

*********** get rsegs
      call getrsegs(rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs) 

********** get possible concs
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call concinfo(               ! POPULATE concentration variables
     I              ioscen,
     O              nconcs,concname)

********** get paramscen
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

********** get np and nv indices (parameter and value)
      call getnpnv(
     I             paramscen,
     O             np,nv)

********** read current sensmatrix if it exists
      if (np.gt.1.or.nv.gt.1) then
        fnam = outdir//'river/cfd/'//rscen(:lenrscen)//
     .         '/sensmatrix.bin'
        open(dfile,file=fnam,form='unformatted',status='unknown',
     .       iostat=err)
        if (err.ne.0) go to 996
        read(dfile,err=997) sensmatrix,sensksKstat,
     .                      testnrsegs,testnconcs
        if (nrsegs.ne.testnrsegs.or.nconcs.ne.testnconcs) go to 998
        close(dfile)
      end if

********** initialize sensmatrix for this (np,nv) pair
      do nr = 1,maxrsegs
        do nb = 1,nbinmax
          do nc = 1,nconcmax
            sensmatrix(np,nv,nc,nb,nr) = -999.0
          end do
        end do
      end do

********** loop over all rsegs and populate sens matrix if file exists
      do nr = 1,nrsegs
        call lencl(rsegs(nr),lenrseg)
        do nc = 1,nconcs
          fnam = outdir//'river/cfd/'//rscen(:lenrscen)//'/'//
     .           rsegs(nr)(:lenrseg)//'_'//cy1//'_'//cy2//'.'//
     .           concname(nc)
          open(dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) cycle  ! if can't open, move on

          read(dfile,*,err=991,end=991) sensksKstat(np,nv,nc,nr)
          read(dfile,*,err=992,end=992) nbins
          read(dfile,*,err=993,end=993) dummy
          do nb = 1,nbins
            read(dfile,*,err=994,end=994) Tnb,sensmatrix(np,nv,nc,nb,nr)
            if (Tnb.ne.nb) go to 995
          end do 
        end do
      end do 

      close(dfile)

      fnam = outdir//'river/cfd/'//rscen(:lenrscen)//
     .       '/sensmatrix.bin'
      open(dfile,file=fnam,form='unformatted',status='unknown',
     .     iostat=err)
      if (err.ne.0) go to 996
      write(dfile,err=951) sensmatrix,sensksKstat,nrsegs,nconcs
      close(dfile)

      return

********************** ERROR SPACE *************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = ' can not read K-S Kstat'
      go to 999

992   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = ' can not read number of bins'
      go to 999

993   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = ' can not read past bin header line'
      go to 999

994   report(1) = 'Problem reading file'
      report(2) = fnam
      write(report(3),*) ' can not read bin number ',nb
      go to 999

995   report(1) = 'Problem reading file'
      report(2) = fnam
      write(report(3),*) ' bins out of order ',nb,' ',Tnb
      go to 999

996   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

997   report(1) = 'Problem reading file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

998   report(1) = 'Problem reading file, nrsegs and/or nconcs were not'
     .            //' read correctly'
      report(2) = fnam
      write(report(3),*) nrsegs,' ',testnrsegs,' ',nconcs,' ',testnconcs
      go to 999

999   call stopreport(report)

      end




