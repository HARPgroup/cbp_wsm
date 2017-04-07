************************************************************************
** find the indices one at a time                                     **
************************************************************************
      subroutine GetParIndices(
     I                        parModule,parTable,parName,npar,calscen,
     O                         parUsed,
     O                         kREAK,kCFOREA,kSUPSAT,kBENOD,
     O                         kCTAUCS,kSTAUCS,kCTAUCD,kSTAUCD,
     O                         kSM,kCM,kCW,kSW,kKSAND,kEXPSND,
     O                         kPHYSET,kREFSET,kMALGR,kALDL,kTALGRM,
     O                         kSEED,kMXSTAY,kKTAM20,kKNO320,kDENOXT,
     O                         kBRPO41,kBRPO42,kBRTAM1,kBRTAM2,kANAER,
     O                         kBEDNH4SILT,kBEDNH4CLAY,kBEDNH4SAND,
     O                         kADSNH4SILT,kADSNH4CLAY,kADSNH4SAND,
     O                         kBEDPO4SILT,kBEDPO4CLAY,kBEDPO4SAND,
     O                         kADSPO4SILT,kADSPO4CLAY,kADSPO4SAND)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      include 'params.inc'

      character*6 Tmodule
      character*14 Ttable
      character*10 Tname

      integer GetParIndex
      external GetParIndex

      do np = 1,npar
        parUsed(np) = .false.
      end do

      Tmodule = 'OXRX'
      Ttable = 'OX-REAPARM'
      Tname = 'REAK'
      kREAK = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kREAK) = .true.

      Tmodule = 'OXRX'
      Ttable = 'OX-CFOREA'
      Tname = 'CFOREA'
      kCFOREA = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kCFOREA) = .true.

      Tmodule = 'OXRX'
      Ttable = 'OX-GENPARM'
      Tname = 'SUPSAT'
      kSUPSAT = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSUPSAT) = .true.

      Tmodule = 'OXRX'
      Ttable = 'OX-BENPARM'
      Tname = 'BENOD'
      kBENOD = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBENOD) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#2'
      Tname = 'CTAUCS'
      kCTAUCS = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kCTAUCS) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#1'
      Tname = 'STAUCS'
      kSTAUCS = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSTAUCS) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#2'
      Tname = 'CTAUCD'
      kCTAUCD = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kCTAUCD) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#1'
      Tname = 'STAUCD'
      kSTAUCD = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSTAUCD) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#1'
      Tname = 'SM'
      kSM = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSM) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#2'
      Tname = 'CM'
      kCM = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kCM) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#2'
      Tname = 'CW'
      kCW = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kCW) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SILT-CLAY-PM#1'
      Tname = 'SW'
      kSW = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSW) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SAND-PM'
      Tname = 'KSAND'
      kKSAND = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kKSAND) = .true.


      Tmodule = 'SEDTRN'
      Ttable = 'SAND-PM'
      Tname = 'EXPSND'
      kEXPSND = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kEXPSND) = .true.



      Tmodule = 'PLANK'
      Ttable = 'PHYTO-PARM'
      Tname = 'PHYSET'
      kPHYSET = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kPHYSET) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PHYTO-PARM'
      Tname = 'REFSET'
      kREFSET = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kREFSET) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PLNK-PARM1'
      Tname = 'MALGR'
      kMALGR = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kMALGR) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PLNK-PARM3'
      Tname = 'ALDL'
      kALDL = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kALDL) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PLNK-PARM2'
      Tname = 'TALGRM'
      kTALGRM = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kTALGRM) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PHYTO-PARM'
      Tname = 'SEED'
      kSEED = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kSEED) = .true.


      Tmodule = 'PLANK'
      Ttable = 'PHYTO-PARM'
      Tname = 'MXSTAY'
      kMXSTAY = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kMXSTAY) = .true.



      Tmodule = 'NUTRX'
      Ttable = 'NUT-NITDENIT'
      Tname = 'KTAM20'
      kKTAM20 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kKTAM20) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-NITDENIT'
      Tname = 'KNO320'
      kKNO320 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kKNO320) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-NITDENIT'
      Tname = 'DENOXT'
      kDENOXT = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kDENOXT) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BENPARM'
      Tname = 'BRPO41'
      kBRPO41 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBRPO41) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BENPARM'
      Tname = 'BRPO42'
      kBRPO42 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBRPO42) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BENPARM'
      Tname = 'BRTAM1'
      kBRTAM1 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBRTAM1) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BENPARM'
      Tname = 'BRTAM2'
      kBRTAM2 = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBRTAM2) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BENPARM'
      Tname = 'ANAER'
      kANAER = GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kANAER) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'NH4-silt'
      kBEDNH4SILT=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDNH4SILT) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'NH4-clay'
      kBEDNH4CLAY=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDNH4CLAY) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'NH4-sand'
      kBEDNH4SAND=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDNH4SAND) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'NH4-silt'
      kADSNH4SILT=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSNH4SILT) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'NH4-clay'
      kADSNH4CLAY=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSNH4CLAY) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'NH4-sand'
      kADSNH4SAND=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSNH4SAND) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'PO4-silt'
      kBEDPO4SILT=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDPO4SILT) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'PO4-clay'
      kBEDPO4CLAY=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDPO4CLAY) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-BEDCONC'
      Tname = 'PO4-sand'
      kBEDPO4SAND=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kBEDPO4SAND) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'PO4-silt'
      kADSPO4SILT=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSPO4SILT) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'PO4-clay'
      kADSPO4CLAY=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSPO4CLAY) = .true.


      Tmodule = 'NUTRX'
      Ttable = 'NUT-ADSPARM'
      Tname = 'PO4-sand'
      kADSPO4SAND=GetParIndex(parModule,parTable,parName,npar,calscen,
     I                    Tmodule,Ttable,Tname)
      parUsed(kADSPO4SAND) = .true.


      end

      function GetParIndex(parModule,parTable,parName,npar,calscen,
     I                     Tmodule,Ttable,Tname)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      character*(*) Tmodule
      character*(*) Ttable
      character*(*) Tname
      integer GetParIndex

      do np = 1,npar
        if (Tname.eq.parName(np)) then
          if (Ttable.eq.parTable(np)) then
            if (Tmodule.eq.parModule(np)) then
              GetParIndex = np
              return
            end if
          end if
        end if
      end do

      call lencl(calscen,lencalscen)   ! should not get here
      report(1) = 'Problem in GetParIndex'
      report(2) = ' could not find '//Tmodule//' '//Ttable//' '//Tname
      report(3) = ' in ./config/control/calib/WQ/'//calscen(:lencalscen)
     .            //'/param_list.csv'
      call stopreport(report)
      end

