************************************************************************
**  This program multiplies all the edge-of-stream loads for a land   **
***    segment by the delivery factor for the river segment with      **
***    which it is associated                                         **
**                                                                    **
**  The strategy for this program is to loop over all lsegs in a seg  **
**    opening all the eos and data files and rewriting them after     **
**    multiplying by the delivery factors                             **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/masslinks.inc'
      include '../../../lib/inc/modules.inc'
      include '../../../lib/inc/rsegs.inc'

      integer nloadmax,nloads      ! number of loads
      parameter (nloadmax = 20)    ! maximum number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')

      character*100 basin
      integer lenbasin

      character*400 longline
      character*13 riverseg(maxrsegs)

************ specific dfs
      real segDF(nloadmax)
      real basDF(nloadmax)
      real resDF(nloadmax)
      real df(maxrsegs,nloadmax)

************ delivery variables
      integer ndelmax      ! maximum number of delivery variables
      parameter (ndelmax=10)
      integer ndel,ndl
      character*4 delname(ndelmax)

      integer load2del(nloadmax)    ! delivery surrogate for each load

********** indices and accounting variables
      integer nl,nrseg,nsegs,ns   ! indices

      logical foundfile

      integer year1,year2
      character*4 cy1,cy2
      character*10 averyear

      character*3 DFmethod(nloadmax)  ! poss values 'seg','bas','res'

********************* END DECLARATIONS *********************************

      read*, rscen,basin,year1,year2

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      call lencl(basin,lenbasin)
      call lencl(rscen,lenrscen)

      print*,'summarizing delivery factors for scenario ',
     .       rscen(:lenrscen),' river basin ',basin(:lenbasin)

************* read in river segments
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)

      call getRvars(              ! get active Rvars
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rname)

      call getloads(            ! get loads for active Rvars
     I              ioscen,lenioscen,nRvar,Rname,
     O              nloads,loadname)

      call delinfo(
     I             ioscen,lenioscen,nloads,loadname,
     O             ndel,delname,load2del)  ! number of delivery factors

*********** read in DF method for each load type
      call getDFmethod(rscen,lenrscen,ndel,delname,
     O                 DFmethod)

      do nl = 1, ndel
        call lowercase(DFmethod(nl)) ! check for correct DFmethod
        if (DFmethod(nl).ne.'seg'.and.DFmethod(nl).ne.'bas'
     .                 .and.DFmethod(nl).ne.'res') go to 993

      end do

************** read from each output file 
      nsegs = 0
      do nrseg = 1,nrsegs
        foundfile = .false.
        fnam = outdir//'del/dfs/aveann/'//rscen(:lenrscen)//'/'//
     .            rsegs(nrseg)//'_'//rscen(:lenrscen)//'_'//
     .            cy1//'_'//cy2//'.txt'

        inquire (file=fnam,exist=foundfile)       ! check if delivery file already exist for the segment

        if (foundfile) then
          open(dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) go to 991
          
          read(dfile,'(a400)',end=111,err=992) longline

          nsegs = nsegs + 1
          riverseg(nsegs) = rsegs(nrseg)
          
          read(dfile,123,err=992) averyear,(segDF(ndl),ndl=1,ndel),
     .                                     (basDF(ndl),ndl=1,ndel),
     .                                     (resDF(ndl),ndl=1,ndel)
          do nl = 1,ndel
            if (DFmethod(nl) .eq. 'seg') then
              df(nsegs,nl) = segDF(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              df(nsegs,nl) = basDF(nl)
            else if (DFmethod(nl) .eq. 'res') then
              df(nsegs,nl) = resDF(nl)
            else
              go to 993
            end if
          end do

111       close(dfile)

        end if

      end do

*********** write delivery factors into one file
      fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//'/'//
     .     basin(:lenbasin)//'_delivery_factors_'//cy1//'_'//cy2//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'rseg,','averyear',
     .                          (delname(nl),nl=1,ndel)

      do ns = 1,nsegs
      write(dfile,1234,err=951) riverseg(ns),',',averyear,
     .                         (df(ns,nl),nl=1,ndel)
      end do

      close(dfile)

123   format(a9,30(1x,f14.7))
1233  format(a5,a9,7(',',a10))
1234  format(a13,a1,a10,7(',',e14.7))

      stop

************************ ERROR SPACE *************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'problem in transport factor program'
      report(2) = ' model run is earlier than allowed, change '//
     .         char(39)//'y1'//char(39)//' in file'
      report(3) = ' pp/src/postproc/del/tfs/tfs.inc'
      go to 999

992   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = longline
      go to 999

993   report(1) = 'Delivery Factor method must be'
      report(2) = 'res - reservoir, seg - segment, bas - basin'
      report(3) = 'method read was '//DFmethod(nl)
      go to 999

999   call stopreport(report)

      end
