*********************************************************************
**  reads calibration and scenario data and creates regressions    **
**  between them on a rseg and month basis                         **
**  CBPO, March 2010                                               **
*********************************************************************
      
      implicit none
      include 'Rdaily.inc'

********* commonly used variables
      character*25 calib !name of calibration scenario
      integer lencalib   !length of character variable      
      integer Nexits,idummy,np
      character*1 resflag,timestep

********** date variables
      integer ndaysinmonth
      external ndaysinmonth
      integer ny,nm,nd,nh,nmin

*********** WQ variables 
      real cNO3temp !calibration NO3 data
      real sNO3temp !scenario NO3 data
      
************ file variables
      integer cfile ! scenario data file
      parameter(cfile=12)
      character*100 cfnam

      integer sfile ! calibration data file
      parameter(sfile=13)
      character*100 sfnam

      integer rfile ! regression file
      parameter (rfile=14)
      character*100 rfnam

*********** variables for holding WQ values
      real cNO3ave(EarliestYear:LatestYear,12,31),
     .                 sNO3ave(EarliestYear:LatestYear,12,31)
      real Cal(31), Scen(31)

*********** variables to call the regression subroutine
      integer nv,maxvals
      parameter (maxvals=31)
      real m,b,r2  ! regression output
      real stats(4)  ! regression stats
      real limit

************* END DECLARATIONS *******************************
      read*,calib,rscen,rseg
      call lencl(calib, lencalib)
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      
*************WQ file extension
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)

      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname) 

      call plotinfo(rscen,lenrscen,nRvar,Rname,
     O              nplots,plotname,unit,norm,ncons,con,confactor)

*****************open calibration and scenario files
      do np = 1,nplots

         cfnam = outdir//'river/daily/'//calib(:lencalib)
     .           //'/'//rseg(:lenrseg)//'.'//plotname(np)
         open (cfile,file = cfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

         sfnam = outdir//'river/daily/'//rscen(:lenrscen)
     .           //'/'//rseg(:lenrseg)//'.'//plotname(np)
         open (sfile,file = sfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

*************open regression output file
         rfnam = outdir//'river/regressions/'//rscen(:lenrscen)
     .          //'/'//rseg(:lenrseg)//'_regression'//'.'//
     .          plotname(np)
         open(rfile,file=rfnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991
         print*,'creating',rfnam
 
*****************read values
        do
           read(cfile,*,end=111)ny,nm,nd,nh,nmin,cNO3temp 
           if(ny.lt.Earliestyear.or.ny.gt.LatestYear) cycle
           cNO3ave(ny,nm,nd)= cNO3temp
         end do
111      close(cfile)

        do 
           read(sfile,*,end=112)ny,nm,nd,nh,nmin,sNO3temp
           if(ny.lt.EarliestYear.or.ny.gt.LatestYear) cycle
           sNO3ave(ny,nm,nd)= sNO3temp
        end do
112     close(sfile)

********* regress and store outputs

        print*,'regressing ',calib(:lencalib),
     .       ' against ',rscen(:lenrscen)

        do ny = EarliestYear,Latestyear
           do nm = 1,12

              nv = 0
                do nd = 1,ndaysinmonth(ny,nm)

                   if (cNO3ave(ny,nm,nd).lt.-8 .and.
     .                 sNO3ave(ny,nm,nd).lt.-8) cycle
                   nv = nv + 1

                   Cal(nv) = cNO3ave(ny,nm,nd)
                   Scen(nv) = sNO3ave(ny,nm,nd)

                end do

                if (nv.gt.5 ) then
                   call regress(
     I                          Cal,Scen,nv,maxvals,
     O                          m,b,r2,limit,err)
                   write(rfile,101)ny,nm,m,b,r2,err
                end if

           end do

        end do
        close(rfile)

      end do

      stop

**********************************************************************
101   format(i6,',',i3,3(',',e14.7),',',i2)

************************ error reporting

991   print*,'problem opening file for river segment '//rseg
      print*,' ' 
      go to 999

999   stop
      end





