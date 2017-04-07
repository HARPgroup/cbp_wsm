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

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen,basin,itnum,cy1,cy2
      read(cy1,'(i4)') year1
      read(cy2,'(i4)') year2
      if (itnum.gt.maxitnum) go to 998

      call lencl(rscen,lenrscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      print*,' run number ',itnum
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

      print*,'optimizing segments'
      do nr = 1,nrsegs
        write(*,'(a13," ",$)')rsegs(nr)
      end do
      print*,' '
      print*,'iteration ',itnum,' for ',calscen(:lencalscen),' ',
     .       basin(:lenbasin)

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
   
******** get concentration info
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)
      call concinfo(
     I              ioscen,lenioscen,
     O              nconcs,concname)

**********    read old stats, if any
      call getoldstats(
     I                 rscen,basin,lenbasin,cy1,cy2,itnum,
     I                 concname,nconcs,
     O                 sim,obs,ksKstat,oldparval)
      
**********    read old simulation quartiles, if any
      call getoldsim(
     I               rscen,basin,lenbasin,cy1,cy2,itnum,
     I               concname,nconcs,
     O               noObsSim)

*********** get current parameters from files
      call getParValues(
     I                  parModule,parTable,parName,parUsed,npar,
     I                  parLKflag,lakeflags,
     I                  paramscen,rsegs,nrsegs,
     O                  parval)

************ read in current river statistics
      call readnewstats(
     I                  rscen,basin,lenbasin,csegs,ncsegs,
     I                  cy1,cy2,itnum,
     I                  concname,nconcs,parval,
     M                  sim,obs,ksKstat)
       
************ read in current simulation quartiles
      call readnewsim(
     I                rscen,basin,lenbasin,rsegs,nrsegs,
     I                cy1,cy2,itnum,
     I                concname,nconcs,
     M                noObsSim)

********** test for convergence and write file if done
      call convergetest(
     I                  calscen,rscen,csegs,ncsegs,cy1,cy2,
     I                  itnum,concname,nconcs,sim,obs,ksKstat)

********** get mass balance information from out file
      call getbalance(
     I                rscen,csegs,ncsegs,rsegs,nrsegs,
     I                year1,year2,
     O                sandin,sandout,sandtf,
     O                siltin,siltout,silttf,
     O                clayin,clayout,claytf,
     O                sanddiv,siltdiv,claydiv,
     O                tnin,tnout,tntf,tnbrbod,tnbods,tnbenal,
     O                tntamscr,tnbrtam,tntamvol,tnnitden,
     O                tnrefset,tnphyset,tndiv,
     O                tpin,tpout,tptf,tpbrbod,tpbods,tpbenal,
     O                tppo4scr,tpbrpo4,tprefset,tpphyset,tpdiv,
     O                Csandin,Csandout,Csandtf,
     O                Csiltin,Csiltout,Csilttf,
     O                Cclayin,Cclayout,Cclaytf,
     O                Csanddiv,Csiltdiv,Cclaydiv,
     O                Ctnin,Ctnout,Ctntf,Ctnbrbod,Ctnbods,
     O                Ctnbenal,Ctntamscr,Ctnbrtam,Ctntamvol,
     O                Ctnnitden,Ctnrefset,Ctnphyset,Ctndiv,
     O                Ctpin,Ctpout,Ctptf,Ctpbrbod,Ctpbods,
     O                Ctpbenal,Ctppo4scr,Ctpbrpo4,Ctprefset,
     O                Ctpphyset,Ctpdiv,
     O                overscour)

*********** modify them
      call newPar(
     I            parUsed,parmin,parmax,sim,obs,ksKstat,
     I            rsegs,csegs,nrsegs,ncsegs,goodobs,weight,
     I            concname,nconcs,itnum,lakeflags,rscen,
     I            paramscen,parAorM,noObsSim,oldparval,
     I                sandin,sandout,sandtf,
     I                siltin,siltout,silttf,
     I                clayin,clayout,claytf,
     I                sanddiv,siltdiv,claydiv,
     I                tnin,tnout,tntf,tnbrbod,tnbods,tnbenal,
     I                tntamscr,tnbrtam,tntamvol,tnnitden,
     I                tnrefset,tnphyset,tndiv,
     I                tpin,tpout,tptf,tpbrbod,tpbods,tpbenal,
     I                tppo4scr,tpbrpo4,tprefset,tpphyset,tpdiv,
     I                Csandin,Csandout,Csandtf,
     I                Csiltin,Csiltout,Csilttf,
     I                Cclayin,Cclayout,Cclaytf,
     I                Csanddiv,Csiltdiv,Cclaydiv,
     I                Ctnin,Ctnout,Ctntf,Ctnbrbod,Ctnbods,
     I                Ctnbenal,Ctntamscr,Ctnbrtam,Ctntamvol,
     I                Ctnnitden,Ctnrefset,Ctnphyset,Ctndiv,
     I                Ctpin,Ctpout,Ctptf,Ctpbrbod,Ctpbods,
     I                Ctpbenal,Ctppo4scr,Ctpbrpo4,Ctprefset,
     I                Ctpphyset,Ctpdiv,
     I                overscour,
     I            kREAK,kCFOREA,kSUPSAT,kBENOD,
     I            kCTAUCS,kSTAUCS,kCTAUCD,kSTAUCD,
     I            kSM,kCM,kCW,kSW,kKSAND,kEXPSND,
     I            kPHYSET,kREFSET,kMALGR,kALDL,kTALGRM,
     I            kSEED,kMXSTAY,kKTAM20,kKNO320,kDENOXT,
     I            kBRPO41,kBRPO42,kBRTAM1,kBRTAM2,kANAER,
     I            kBEDNH4SILT,kBEDNH4CLAY,kBEDNH4SAND,
     I            kADSNH4SILT,kADSNH4CLAY,kADSNH4SAND,
     I            kBEDPO4SILT,kBEDPO4CLAY,kBEDPO4SAND,
     I            kADSPO4SILT,kADSPO4CLAY,kADSPO4SAND,
     M            parval)
      
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
