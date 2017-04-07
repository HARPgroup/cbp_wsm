************************************************************************
** program to test for convergence.  All passed parameters are input  **
**  This subroutine stops the program and writes a file if convergence**
**  is reached.                                                       **
** criteria will be that:                                             **
**   sim does not change more than 3% of obs for any quintile         **
**   sim is within 5% of obs for every quintile                       **
**     or kolmogorov-smirnov stat is very low                         **
**   sim is oscillating                                               **
************************************************************************
      subroutine convergetest(
     I                        calscen,rscen,csegs,ncsegs,cy1,cy2,
     I                        itnum,concname,nconcs,sim,obs,ksKstat)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      integer it

      call lencl(calscen,lencalscen) 
      call lencl(rscen,lenrscen) 

      if (itnum.lt.3) return ! first or 2nd run, not calibrated
      do ns = 1,ncsegs
        do nc = 1,nconcs
          if (.not.missing(ksKstat(ns,nc,itnum),misval)) then ! if conc exists
            if (ksKstat(ns,nc,itnum).gt.0.05) then ! if not near perfect
              do nb = 1,nbinmax
                if (.not.missing(obs(ns,nc,nb,itnum),misval)) then ! bin exists
                  if (abs(sim(ns,nc,nb,itnum)-obs(ns,nc,nb,itnum))
     .                      .gt.0.02) then
                    if (abs(sim(ns,nc,nb,itnum)-sim(ns,nc,nb,itnum-1))
     .                        .gt.0.01) then
                      if ((sim(ns,nc,nb,itnum)-obs(ns,nc,nb,itnum)) * 
     .                   (sim(ns,nc,nb,itnum-1)-obs(ns,nc,nb,itnum-1))
     .                       .gt. 0.0) then  ! not oscillating
                        return
                      end if
                    end if
                  end if
                end if
              end do
            end if
          end if
        end do
      end do

      fnam = tree//'run/calibration/WQ/'//calscen(:lencalscen)//'_'//
     .       rscen(:lenrscen)//'.converge'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile,*)'done'
      close(dfile)
      stop

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)

      end
          
        
