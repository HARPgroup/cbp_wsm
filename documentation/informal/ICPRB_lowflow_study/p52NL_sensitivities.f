      function calfacLandEvap(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacLandEvap
      calfacLandEvap = 2. / (2. - Tbias)
C      calfacLandEvap = 1.0
      end

      function calfacLZSN(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacLZSN
C      calfacLZSN = ((3.0 - Sstat/Wstat) / 2.0)
      calfacLZSN = ((2.5 - Sstat/Wstat) / 1.5)
C      calfacLZSN = 1.0
      end

      function calfacINFILT(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacINFILT
      calfacINFILT = 1.0/Bstat
C      calfacINFILT = (1.0 + 0.1*Tbias)/((1.0 + Tbias)*Bstat)   !ICPRB try - to reduce the influence of Tbias
C      calfacINFILT = 1.0
      end

      function calfacIRC(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacIRC
      calfacIRC = 2.0 / (1.0 + QaveRI)
C      calfacIRC = 1.0
      end

      function calfacAGWR(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacAGWR
      calfacAGWR = 2.0 / (1.0 + BaveRI) 
C      calfacAGWR = (2.0 / (1.0 + BaveRI))*(1.0 / (1.0 - lo05)) ! ICPRB try - to bring low flows for positive lo05 bias
C      calfacAGWR = 1.0
      end

      function calfacINTFW(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacINTFW
      if (abs(VPbias+9).lt.0.01 .or. abs(Pbias+9.0).lt.0.01) then
        calfacINTFW = 1.0
      else
        if (VPbias*Pbias.gt.0) then
          if (abs(Pbias).gt.abs(VPbias)) then
            calfacINTFW = 1.0 + Pbias / 2.0
C            calfacINTFW = 1.0
          else
            calfacINTFW = 1.0 + VPbias / 2.0
C            calfacINTFW = 1.0
          end if
        else
          calfacINTFW = 1.0
        end if
      end if
C      calfacINTFW = 1.0
      end

      function calfacAGWETP(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacAGWETP
       calfacAGWETP = 1.0 + (Sstat-1.0)*5.0   !used in ICPRB's series 8 runs
C        calfacAGWETP = 1.0 + (Sstat-1.0)*8.0
      CalfacAGWETP = 1.0
      end

      function calfacKVARY(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacKVARY
C      calfacKVARY = (1.0 + Tbias)/(1.0 + lo10)  ! p52 original?
      calfacKVARY = 1.0/(1.0 + lo05)   ! ICPRB 1st try - used with 4a and 4b
C      calfacKVARY = (1.0/(1.0 + lo05))*(1.0/(1.0 - lo10))  ! ICPRB 2nd try, with lo10 redefined as lo50
C      calfacKVARY = 1.0/( 1.0 + 0.5*(SBaveRI - 1.0) )   !ICPRB 3rd try; SBaveRI = betapos_s/betapos_o
C      calfacKVARY = 1.0
      end
      
