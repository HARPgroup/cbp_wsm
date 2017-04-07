      function calfacLandEvap(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacLandEvap
      calfacLandEvap = 2. / (2. - Tbias)
      end

      function calfacLZSN(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacLZSN
      calfacLZSN = ((2.5 - Sstat/Wstat) / 1.5)
      end

      function calfacINFILT(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacINFILT
      calfacINFILT = 1.0/Bstat
      end

      function calfacIRC(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,
     .                        lo10,lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacIRC
      calfacIRC = 2.0 / (1.0 + QaveRI)
      end

      function calfacAGWR(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacAGWR
      calfacAGWR = 2.0 / (1.0 + BaveRI) 
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
          else
            calfacINTFW = 1.0 + VPbias / 2.0
          end if
        else
          calfacINTFW = 1.0
        end if
      end if
      end

      function calfacAGWETP(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacAGWETP
        calfacAGWETP = 1.0 + (Sstat-1.0)*8.0
      end

      function calfacKVARY(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10,
     .                        lo05)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,lo05,calfacKVARY
C      calfacKVARY = 1.0/(1.0 + lo05)   ! ICPRB method to improve low flow simulation
      calfacKVARY = 1.0
      end
      
