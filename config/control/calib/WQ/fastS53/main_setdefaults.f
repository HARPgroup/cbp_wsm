************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      include 'params.inc'
      include 'outfile.inc'

      integer nr  ! index
      real findtaupercent
      external findtaupercent

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen,basin

      call lencl(rscen,lenrscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

************* HOUSEKEEPING
      call lencl(calscen,lencalscen)
      call lencl(basin,lenbasin)
      fnam = tree//'config/control/calib/WQ/'
     .       //calscen(:lencalscen)//'/weights/'//basin(:lenbasin)//
     .       '_weights.bin'
      open(dfile,file=fnam,form='unformatted',
     .           status='unknown',iostat=err)
      if (err.ne.0) go to 991
      read(dfile) rsegs,csegs,nrsegs,ncsegs,goodobs,weight,testend
      close(dfile)
      if (checkend.ne.testend) go to 991

      print*,' setting defaults for segments '
      do nr = 1,nrsegs
        write(*,'(a13," ",$)')rsegs(nr)
      end do
      print*,' '

************ get lake flags
      call getlakeflags(
     I                  rsegs,nrsegs,paramscen,lenparamscen,
     O                  lakeflags)

*********** get list of all possible parameters
      call readparams(
     I                calscen,
     O                parLKflag,parModule,parTable,parName,
     O                parAorM,parstart,parmin,parmax,npar)

*********** get indices for all parameters being used
      call GetParIndices(
     I                   parModule,parTable,parName,npar,calscen,
     O                   parUsed,
     O                   kREAK,kCFOREA,kSUPSAT,kBENOD,
     O                   kCTAUCS,kSTAUCS,kCTAUCD,kSTAUCD,
     O                   kSM,kCM,kCW,kSW,kKSAND,kEXPSND,
     O                   kPHYSET,kREFSET,kMALGR,kALDL,kTALGRM,
     O                   kSEED,kMXSTAY,kKTAM20,kKNO320,kDENOXT,
     O                   kBRPO41,kBRPO42,kBRTAM1,kBRTAM2,kANAER,
     O                   kBEDNH4SILT,kBEDNH4CLAY,kBEDNH4SAND,
     O                   kADSNH4SILT,kADSNH4CLAY,kADSNH4SAND,
     O                   kBEDPO4SILT,kBEDPO4CLAY,kBEDPO4SAND,
     O                   kADSPO4SILT,kADSPO4CLAY,kADSPO4SAND)

*********** modify them
      do nr = 1,nrsegs
        if (lakeflags(nr).eq.0) then
          if (parused(kREAK)) parval(kREAK,nr) = parstart(kREAK)
        else
          if (parused(kCFOREA)) parval(kCFOREA,nr) = parstart(kCFOREA)
        end if
        if (parused(kSUPSAT)) parval(kSUPSAT,nr) = parstart(kSUPSAT)
        if (parused(kBENOD)) parval(kBENOD,nr) = parstart(kBENOD)
        if (parused(kCTAUCS)) parval(kCTAUCS,nr) = 
     .             findtaupercent(rsegs(nr),paramscen,parstart(kCTAUCS))
        if (parused(kSTAUCS)) parval(kSTAUCS,nr) = 
     .             findtaupercent(rsegs(nr),paramscen,parstart(kSTAUCS))
        if (parused(kCTAUCD)) parval(kCTAUCD,nr) = 
     .             findtaupercent(rsegs(nr),paramscen,parstart(kCTAUCD))
        if (parused(kSTAUCD)) parval(kSTAUCD,nr) = 
     .             findtaupercent(rsegs(nr),paramscen,parstart(kSTAUCD))
        if (parused(kSM)) parval(kSM,nr) = parstart(kSM)
        if (parused(kCM)) parval(kCM,nr) = parstart(kCM)
        if (parused(kCW)) parval(kCW,nr) = parstart(kCW)
        if (parused(kSW)) parval(kSW,nr) = parstart(kSW)
        if (parused(kKSAND)) parval(kKSAND,nr) = parstart(kKSAND)
        if (parused(kEXPSND)) parval(kEXPSND,nr) = parstart(kEXPSND)
        if (parused(kPHYSET)) parval(kPHYSET,nr) = parstart(kPHYSET)
        if (parused(kREFSET)) parval(kREFSET,nr) = parstart(kREFSET)
        if (parused(kMALGR)) parval(kMALGR,nr) = parstart(kMALGR)
        if (parused(kALDL)) parval(kALDL,nr) = parstart(kALDL)
        if (parused(kTALGRM)) parval(kTALGRM,nr) = parstart(kTALGRM)
        if (parused(kSEED)) parval(kSEED,nr) = parstart(kSEED)
        if (parused(kMXSTAY)) parval(kMXSTAY,nr) = parstart(kMXSTAY)
        if (parused(kKTAM20)) parval(kKTAM20,nr) = parstart(kKTAM20)
        if (parused(kKNO320)) parval(kKNO320,nr) = parstart(kKNO320)
        if (parused(kDENOXT)) parval(kDENOXT,nr) = parstart(kDENOXT)
        if (parused(kBRPO41)) parval(kBRPO41,nr) = parstart(kBRPO41)
        if (parused(kBRPO42)) parval(kBRPO42,nr) = parstart(kBRPO42)
        if (parused(kBRTAM1)) parval(kBRTAM1,nr) = parstart(kBRTAM1)
        if (parused(kBRTAM2)) parval(kBRTAM2,nr) = parstart(kBRTAM2)
        if (parused(kANAER)) parval(kANAER,nr) = parstart(kANAER)
        if (parused(kBEDNH4SAND)) 
     .       parval(kBEDNH4SAND,nr) = parstart(kBEDNH4SAND)
        if (parused(kBEDNH4SILT)) 
     .       parval(kBEDNH4SILT,nr) = parstart(kBEDNH4SILT)
        if (parused(kBEDNH4CLAY)) 
     .       parval(kBEDNH4CLAY,nr) = parstart(kBEDNH4CLAY)
        if (parused(kBEDPO4SAND)) 
     .       parval(kBEDPO4SAND,nr) = parstart(kBEDPO4SAND)
        if (parused(kBEDPO4SILT)) 
     .       parval(kBEDPO4SILT,nr) = parstart(kBEDPO4SILT)
        if (parused(kBEDPO4CLAY)) 
     .       parval(kBEDPO4CLAY,nr) = parstart(kBEDPO4CLAY)
        if (parused(kADSNH4SAND)) 
     .       parval(kADSNH4SAND,nr) = parstart(kADSNH4SAND)
        if (parused(kADSNH4SILT)) 
     .       parval(kADSNH4SILT,nr) = parstart(kADSNH4SILT)
        if (parused(kADSNH4CLAY)) 
     .       parval(kADSNH4CLAY,nr) = parstart(kADSNH4CLAY)
        if (parused(kADSPO4SAND)) 
     .       parval(kADSPO4SAND,nr) = parstart(kADSPO4SAND)
        if (parused(kADSPO4SILT)) 
     .       parval(kADSPO4SILT,nr) = parstart(kADSPO4SILT)
        if (parused(kADSPO4CLAY)) 
     .       parval(kADSPO4CLAY,nr) = parstart(kADSPO4CLAY)
      end do

      print*,' '
      print*,'overwriting parameter files.  Do not stop program'
      print*,'  during this step or you may lose data'
*********** put them back
      call putParValues(
     I                  parModule,parTable,parName,parUsed,npar,
     I                  parLKflag,lakeflags,
     I                  paramscen,rsegs,nrsegs,
     I                  parval)
      print*,'done writing files'

      stop

********************* ERROR SPACE **************************************
991   report(1) = 'problem with weight file:'
      report(2) = fnam
      report(3) = 'file or variable size do not match'
      go to 999

998   report(1) = 'maximum iteration surpassed'
      report(2) = ' increase maxitnum in WQcal include file'
      report(3) = ' ./code/src/calibration_utils/change_param/'//
     .            'calib_iter/PSTEMP_river/WQcal.inc'
      go to 999

999   call stopreport(report)

      end
