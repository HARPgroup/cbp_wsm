************************************************************************
** program to summarize output in the ./output/ diretory              **
**  output is initially by lrseg and land use or data type            **
**  this program takes the input of a river basin and summarizes by   **
**  land use type, by data type, by lseg, by river seg, and by basin  **
************************************************************************
      subroutine getEOSload(
     I                      rscen,rsegs,nrsegs,year1,year2,
     I                      nloads,loadname,
     O                      EOSland,EOSsep,EOSatdep,EOSps)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/rsegs.inc'
      include '../../../lib/inc/lsegs.inc'
      include '../../../lib/inc/land_use.inc'

      integer nloads               ! number of loads
      character*4 loadname(nloads)  ! name of each load (e.g. 'totn')

********* reading variables for loads
      real SEGps(nloads) 
      real SEGatdep(nloads) 
      real SEGsep(nloads) 
      real SEGland(nloads) 

********** output load
      real EOSps(maxrsegs,nloads) ! Point sources
      real EOSatdep(maxrsegs,nloads) ! Direct atdep to water
      real EOSsep(maxrsegs,nloads) ! Septic
      real EOSland(maxrsegs,nloads) ! edge-of-stream from land

********** indices and accounting variables
      integer nr,ns,nl,nload   ! indices

      integer numsegs ! number of lsegs associated with an rseg

      integer npssegs   ! ps-only segments
      character*6 psl2r(maxL2R)

      character*3 EOF,EOS,DEL
      data EOF,EOS,DEL /'eof','eos','del'/

      integer year1,year2

************ END DECLARATIONS ******************************************

      call lencl(rscen,lenrscen) 

************ initialize variables

*********** populate variables by reading ASCII output
************ loop over river segs
      do nr = 1,nrsegs
        call ttyput(rsegs(nr)(5:8))
        call ttyput(' ')
        call getl2r(
     I              rsegs(nr),rscen,lenrscen,
     O              numsegs,l2r)
        call getpsonlyl2r(
     I                    rsegs(nr),rscen,lenrscen,numsegs,l2r,
     O                    npssegs,psl2r)


        do nload = 1,nloads
          EOSps(nr,nload) = 0.0
          EOSatdep(nr,nload) = 0.0
          EOSsep(nr,nload) = 0.0
          EOSland(nr,nload) = 0.0
        end do

************** loop over land segs in river seg
        do ns = 1,numsegs
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            call readEOaveann(
     I                        rscen,l2r(ns),rsegs(nr),
     I                        luname(nl),EOS,year1,year2,
     I                        nloads,loadname,
     O                        SEGland)
            do nload = 1,nloads  ! add loads
              EOSland(nr,nload) = EOSland(nr,nload) + SEGland(nload)
            end do
          end do

          call readDATaveann(
     I                       rscen,l2r(ns),rsegs(nr),
     I                       EOS,year1,year2,
     I                       nloads,loadname,
     O                       SEGps,SEGsep,SEGatdep)
          do nload = 1,nloads
            EOSps(nr,nload) = EOSps(nr,nload) + SEGps(nload)
            EOSsep(nr,nload) = EOSsep(nr,nload) + SEGsep(nload)
            EOSatdep(nr,nload) = EOSatdep(nr,nload) + SEGatdep(nload)
          end do

        end do   ! loop over lsegs in rseg

*************** loop over ps-only segs in river seg
        do ns = 1,npssegs

          call readPSaveann(
     I                      rscen,psl2r(ns),rsegs(nr),
     I                      EOS,year1,year2,
     I                      nloads,loadname,
     O                      SEGps)
          do nload = 1,nloads
            EOSps(nr,nload) = EOSps(nr,nload) + SEGps(nload)
          end do

        end do   ! loop over ps-only segs in rseg

      end do   ! loop over rsegs in basin
      print*,' '
           
      return
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

997   report(1) = 'Error initializing sumout program'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and calling script'
      go to 999

998   report(1) = 'Error initializing land postprocessor'
      report(2) = 'not enough input data in calling script'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


