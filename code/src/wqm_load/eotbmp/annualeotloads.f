      subroutine AnnualEOTLoads(
     I       rscen,lenrscen,C_allup,nallup,syear,eyear,nBmpConEOT,
     I       I_DBG,
     O       AnnEOTLoads)

      implicit none
      include 'eotbmp.inc'


      real tAnnEOTLoads(EarliestYear:LatestYear,maxBMPcon)


      character*50 tstr
      character*4  tBMPconnameEOT(maxBMPcon)

      print*,syear,eyear

      do iyr = syear,eyear
        do nb = 1,nBmpConEOT
          AnnEOTLoads(iyr,nb) = 0.0
        end do
      end do


      do irseg=1,nallup
         call lencl(C_allup,lenrseg)
         if(I_DBG.ge.1)
     .    print*,'... ... aggregating ',C_allup(irseg)(:lenrseg)
         fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .            C_allup(irseg)(:lenrseg)//'.csv'

         open (dfile,file=fnam,status='old',iostat=err)
         if (err.ne.0) go to 991

         read(dfile,*,end=992,err=800) tstr,
     .      (tBMPconnameEOT(icon),icon=1,nBmpConEOT)

         do
            read(dfile,*,end=992,err=800)iyr,
     .         (tAnnEOTLoads(iyr,icon),icon=1,nBmpConEOT)
            do icon=1,nBmpConEOT
               AnnEOTLoads(iyr,icon) = AnnEOTLoads(iyr,icon) + 
     .                                  tAnnEOTLoads(iyr,icon)
            end do
            if ( iyr.eq.eyear ) exit
         end do
         close(dfile)
      end do



      return

800   report(1) = 'Error reading file '
      report(2) = fnam
      report(3) = ' '
      go to 999

991   report(1) = 'Error: problem opening file= '
      report(2) = fnam
      report(3) = 'error ='
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Error reading file '
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
