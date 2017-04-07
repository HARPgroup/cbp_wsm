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

************ specific dfs
      real segDF(nloadmax)
      real basDF(nloadmax)
      real resDF(nloadmax)
      real df(nloadmax)

************ delivery variables
      integer ndelmax      ! maximum number of delivery variables
      parameter (ndelmax=10)
      integer ndel,ndl
      character*4 delname(ndelmax)

      integer load2del(nloadmax)    ! delivery surrogate for each load

********** indices and accounting variables
      integer nl,nrseg,numsegs,ns   ! indices

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

*********** open delivery factor summary file
      fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//'/'//
     .     basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .     rscen(:lenrscen)//'_dfs.csv'
      open(dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile+1,1233,err=951) 'lseg,rseg,','averyear',
     .                          (delname(nl),nl=1,ndel)
************** read from each output file 
      do nrseg = 1,nrsegs
        rseg = rsegs(nrseg)
        call getl2rlist(
     I                  rseg,rscen,lenrscen,
     O                  numsegs,l2r)
        do ns = 1,numsegs
          fnam = outdir//'del/dfs/aveann/'//rscen(:lenrscen)//'/'//
     .           l2r(ns)//'_'//rseg//'_'//rscen(:lenrscen)//'_'//
     .           cy1//'_'//cy2//'.txt'
          open(dfile,file=fnam,status='old',iostat=err)
          if (err.ne.0) go to 991
          
          read(dfile,'(a100)',end=992,err=992) line

          read(dfile,123,end=992,err=992) averyear,
     .                                    (segDF(nl),nl=1,ndel),
     .                                    (basDF(nl),nl=1,ndel),
     .                                    (resDF(nl),nl=1,ndel)
          close(dfile)

          do nl = 1,ndel
            if (DFmethod(nl) .eq. 'seg') then
              df(nl) = segDF(nl)
            else if (DFmethod(nl) .eq. 'bas') then
              df(nl) = basDF(nl)
            else if (DFmethod(nl) .eq. 'res') then
              df(nl) = resDF(nl)
            else
              go to 993
            end if
          end do

          write(dfile+1,1234,err=951) l2r(ns),',',rseg,',',averyear,
     .                                (df(nl),nl=1,ndel)

        end do

      end do

      close(dfile+1)

      stop

123   format(a9,30(1x,f14.7))
1233  format(a10,a9,7(',',a10))
1234  format(a6,a1,a13,a1,a10,7(',',e14.7))

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
      report(3) = line
      go to 999

993   report(1) = 'Delivery Factor method must be'
      report(2) = 'res - reservoir, seg - segment, bas - basin'
      report(3) = 'method read was '//DFmethod(nl)
      go to 999

999   call stopreport(report)

      end
