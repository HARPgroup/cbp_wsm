*********************************************************************
**  Reads daily simulated data and hourly wdm                      ** 
**  Calculates daily texture                                       **
**  CBPO, Sep 2012                                                 **
*********************************************************************
      
      implicit none
      include 'Rdaily.inc'

********* commonly used variables
c      character*25 calib !name of calibration scenario
c      integer lencalib   !length of character variable      
c      integer Nexits,idummy,np
c      character*1 resflag,timestep

********* date variables
      integer ndaysinmonth
      external ndaysinmonth
      integer ny,nm,nd,nh,nmin

********* variables 
      real FLO, SED ! Observed data
      real WATR,SAND,SILT,CLAY,PHYT  ! WDM data

********* file variables
      integer ffile ! Flow data file
      parameter(ffile=13)
      character*100 ffnam

      integer sfile ! Sediment data file
      parameter (sfile=14)
      character*100 sfnam 

      integer wfile ! WDM file
      parameter (wfile=15)
      character*100 wfnam

      integer tfile ! texture file
      parameter (tfile=16)
      character*100 tfnam

********* variables for holding values
      real fdat(EarliestYear:LatestYear,12,31),
     .     sdat(EarliestYear:LatestYear,12,31)
      real wdat(EarliestYear:LatestYear,12,31,24),
     .     sadat(EarliestYear:LatestYear,12,31,24), 
     .     sidat(EarliestYear:LatestYear,12,31,24),
     .     cdat(EarliestYear:LatestYear,12,31,24),
     .     pdat(EarliestYear:LatestYear,12,31,24)

      real fd(nyearmax*12*31),sd(nyearmax*12*31)
      real wd(nyearmax*12*31),snd(nyearmax*12*31),
     .     sid(nyearmax*12*31),cd(nyearmax*12*31),
     .     pd(nyearmax*12*31)

      real frac1(nyearmax*12*31),frac2(nyearmax*12*31),
     .     frac3(nyearmax*12*31),frac4(nyearmax*12*31),
     .     fracT(nyearmax*12*31)

********* variables to call subroutine
      integer nv,mv,maxvals,maxvalsm,dd,mm
      parameter (maxvals=31*12*nyearmax)
c      parameter (maxvalsm=12*nyearmax)

********* utility variables
      logical FirstInstance

********* END DECLARATIONS *******************************
c      read*,calib,rseg !,rscen
c      call lencl(calib, lencalib)
c      call lencl(rseg,lenrseg)

********* open data files

         ffnam = outdir//'river/daily/p532cal_062211/SL9_2700_2720.FLOW' !//calib(:lencalib)//'/'
c     .           //rseg(:lenrseg)//'.FLOW'
         open (ffile,file = ffnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

         sfnam = outdir//'river/daily/p532cal_062211/SL9_2700_2720.TSSX'!//calib(:lencalib)//'/'
c     .           //rseg(:lenrseg)//'.TSSX'
         open (sfile,file = sfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

         wfnam = outdir//'river/texture/SL9_2700_2720.csv'
c     .           //rseg(:lenrseg)//'.csv'
         open (wfile,file = wfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

********* create texture file
        
         tfnam = outdir//'river/texture'//
     .           '/SL9_2700_2720out.csv'
c    .           //calib(:lencalib)//'/'//rseg(:lenrseg)//
c    .           '_OUT.csv'
         open(tfile,file=tfnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991

********* read values
        do
           read(ffile,*,end=111)ny,nm,nd,nh,nmin,FLO 
           if(ny.lt.Earliestyear.or.ny.gt.LatestYear) cycle
           fdat(ny,nm,nd)= FLO
        end do
111     close(ffile)

        do 
           read(sfile,*,end=112)ny,nm,nd,nh,nmin,SED
           if(ny.lt.EarliestYear.or.ny.gt.LatestYear) cycle
           sdat(ny,nm,nd)= SED
        end do
112     close(sfile)

        do
           read(wfile,*,end=113)ny,nm,nd,nh,WATR,SAND,SILT,CLAY,PHYT
           if(ny.lt.EarliestYear.or.ny.gt.LatestYear) cycle
           wdat(ny,nm,nd,nh)= WATR
           sadat(ny,nm,nd,nh)= SAND
           sidat(ny,nm,nd,nh)= SILT
           cdat(ny,nm,nd,nh)= CLAY
           pdat(ny,nm,nd,nh)= PHYT
        end do
113     close(wfile)

********* calculates texture and store outputs

        print*,'calculating daily texture'

        write(tfile,*)'FLOW,TSSX,SAND,SILT,CLAY,PHYT'
        nv = 0
        do ny = EarliestYear,Latestyear
          do nm = 1,12
            do nd = 1,ndaysinmonth(ny,nm)
                  nv = nv + 1
                  fd(nv) = fdat(ny,nm,nd)
                  sd(nv) = sdat(ny,nm,nd)
                  wd(nv) = 0
               do nh = 1,24
                  wd(nv) = wd(nv)+wdat(ny,nm,nd,nh)
                  snd(nv) = snd(nv)+sadat(ny,nm,nd,nh)*735.4
                  sid(nv) = sid(nv)+sidat(ny,nm,nd,nh)*735.4
                  cd(nv) = cd(nv)+cdat(ny,nm,nd,nh)*735.4
                  pd(nv) = pd(nv)+pdat(ny,nm,nd,nh)*0.3677
                 
                  fracT(nv) = (snd(nv)+sid(nv)+cd(nv)+pd(nv))/wd(nv)
                  frac1(nv) = (snd(nv)/wd(nv))/fracT(nv)
                  frac2(nv) = (sid(nv)/wd(nv))/fracT(nv)
                  frac3(nv) = (cd(nv)/wd(nv))/fracT(nv)
                  frac4(nv) = (pd(nv)/wd(nv))/fracT(nv)

               end do
            end do
          end do
        end do 
*************************************************
        do mm = 1,nv
          write(tfile,101)fd(mm),sd(mm),frac1(mm),frac2(mm),
     .          frac3(mm),frac4(mm)
        end do
      close(tfile)

      stop

**********************************************************************


102   format('Daily',',',A13,',',i4,8(',',e14.7),',',i1)
103   format('Monthly',',',A13,',',i4,8(',',e14.7),',',i1)
104   format(i4,',',i2,',',i2,2(',',e14.7))
105   format(i4,',',i2,2(',',e14.7))
101   format( 5(e14.5,','),e14.5)

************************ error reporting

991   print*,'problem opening file for river segment '//rseg
      print*,' ' 
      go to 999

999   stop
      end






