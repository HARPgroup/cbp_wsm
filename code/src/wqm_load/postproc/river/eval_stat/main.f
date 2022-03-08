*********************************************************************
**  Reads observed and simulated data, calculates daily and        **
**  monthly FLOW evaluation statistics                             **
**  CBPO, Jan 2011                                                 **
*********************************************************************
      
      implicit none
      include 'Rdaily.inc'

********* commonly used variables
      character*25 calib,odata !name of calibration scenario
      integer lencalib,lenodata   !length of character variable      
      integer year1,year2
      character*4 cy1,cy2

********* date variables
      integer ndaysinmonth
      external ndaysinmonth
      integer ny,nm,nd,nh,nmin

********* variables 
      real Odat ! Observed data
      real Sdat ! Simulated data
      
********* file variables
      integer xfile ! observed data file
      parameter(xfile=13)
      character*200 xfnam

      integer yfile !simulated data file
      parameter (yfile=14)
      character*200 yfnam 

      integer rfile ! evaluation statistic file
      parameter (rfile=15)
      character*200 rfnam

      integer afile ! OBS vs SIM daily data file
      parameter (afile=16)
      character*200 afnam

      integer mfile ! OBS vs SIM monthly data file
      parameter (mfile=17)
      character*200 mfnam

********* variables for holding values
      real Obsdat(EarliestYear:LatestYear,12,31),
     .     Simdat(EarliestYear:LatestYear,12,31)
      real year(nyearmax*12*31),month(nyearmax*12*31),
     .     day(nyearmax*12*31)
      real Obsd(nyearmax*12*31),Simd(nyearmax*12*31)
      real Obsm(nyearmax*12),Simm(nyearmax*12)

********* variables to call subroutine
      integer nv,mv,maxvals,maxvalsm,dd,mm
      parameter (maxvals=31*12*nyearmax)
      parameter (maxvalsm=12*nyearmax)

********* evaluation statistics variables 
      real NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD

********* utility variables
      logical FirstInstance

********* END DECLARATIONS *******************************
      read*,calib,rseg,year1,year2,odata
      call lencl(calib, lencalib)
      call lencl(rseg,lenrseg)
      call lencl(odata,lenodata)
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

********* open observed and simulated data files

         xfnam = calibdir//'observed/'//odata(:lenodata)//'/FLOW/'
     .           //rseg(:lenrseg)//'.OFLOW'
         open (xfile,file = xfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991

         yfnam = outdir//'river/daily/'//calib(:lencalib)//'/'
     .           //rseg(:lenrseg)//'.FLOW'
         open (yfile,file = yfnam, status = 'old',iostat = err)
         if (err.ne.0) go to 991
         
********* evaluation statistic file
        
         rfnam = outdir//'river/evaluation_statistics/'
     .           //calib(:lencalib)//'/'//rseg(:lenrseg)//
     .           '_'//cy1//'_'//cy2//'_stat_FLOW.csv'
         open(rfile,file=rfnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991

         print*,'creating',rfnam

********* observed vs simulated data file

         afnam = outdir//'river/evaluation_statistics/'
     .           //calib(:lencalib)//'/'//rseg(:lenrseg)//
     .           '_'//cy1//'_'//cy2//'_vsd_FLOW.csv'
         open(afile,file=afnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991

         mfnam = outdir//'river/evaluation_statistics/'
     .           //calib(:lencalib)//'/'//rseg(:lenrseg)//
     .           '_'//cy1//'_'//cy2//'_vsm_FLOW.csv'
         open(mfile,file=mfnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991

********* read values
        do
           read(xfile,*,end=111)ny,nm,nd,nh,nmin,Odat 
           if(ny.lt.Earliestyear.or.ny.gt.LatestYear) cycle
           Obsdat(ny,nm,nd)= Odat
        end do
111     close(xfile)

        do 
           read(yfile,*,end=112)ny,nm,nd,nh,nmin,Sdat
           if(ny.lt.EarliestYear.or.ny.gt.LatestYear) cycle
           Simdat(ny,nm,nd)= Sdat
        end do
112     close(yfile)

*******************

        print*,"Calculating statistics from ",year1,
     .          " to ",year2,' using ',odata

********* calculates daily statistics

        write(rfile,101)'Time step','Rseg','n','NSE','BIAS','RSR',
     .           'NBIAS','NSTDEV','r','SIGN','NRMSD','err'
        nv = 0
        do ny = year1,year2
          do nm = 1,12
            do nd = 1,ndaysinmonth(ny,nm)
              if (Obsdat(ny,nm,nd).le.-0)cycle
                  nv = nv + 1
                  year(nv)=ny
                  month(nv)=nm
                  day(nv)=nd
                  Obsd(nv) = Obsdat(ny,nm,nd)
                  Simd(nv) = Simdat(ny,nm,nd)
            end do
          end do
        end do 

        write(afile,*)'year,','month,','day,','Obs,','Sim'
        do dd = 1,nv
          write(afile,*)year(dd),',',month(dd),',',day(dd),',',
     .                   Obsd(dd),',',Simd(dd)
        end do
      
        call eval(
     I          Obsd,Simd,nv,maxvals,
     O          NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD,err)

        write(rfile,102)rseg,nv,NSE,Bias,RSR,
     .            Nbias,Nsd,r,sdd,NRMSD,err

********* calculates monthly statistics

      mv=0
      do ny = year1,year2
        do nm = 1,12

          FirstInstance = .true.
          do nd = 1,ndaysinmonth(ny,nm)
            if (Obsdat(ny,nm,nd).le.-0)cycle

            if (FirstInstance) then
              mv = mv + 1
              Obsm(mv) = Obsdat(ny,nm,nd)
              Simm(mv) = Simdat(ny,nm,nd)
              FirstInstance = .false.
            else
              Obsm(mv) = Obsm(mv) + Obsdat(ny,nm,nd)
              Simm(mv) = Simm(mv) + Simdat(ny,nm,nd)
            end if
          end do
        end do
      end do

      write(mfile,*)'Obs ',',','Sim'
      do mm = 1,mv
         write(mfile,*)Obsm(mm),',',Simm(mm)
      end do

      call eval(
     I          Obsm,Simm,mv,maxvalsm,
     O          NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD,err)

      write(rfile,103)rseg,mv,NSE,Bias,RSR,
     .           Nbias,Nsd,r,sdd,NRMSD,err

      close(rfile)

      stop

**********************************************************************
101   format(A9,',',A4,',',A1,',',A3,',',A4,',',A3,',',A5,',',A6,','
     .       ,A1,',',A4,',',A5,',',A3)
102   format('Daily',',',A13,',',i4,8(',',e14.7),',',i1)
103   format('Monthly',',',A13,',',i4,8(',',e14.7),',',i1)
104   format(i4,',',i2,',',i2,2(',',e14.7))
105   format(i4,',',i2,2(',',e14.7))

************************ error reporting

991   print*,'problem opening file for river segment '//rseg
      print*,' ' 
      go to 999

999   stop
      end






