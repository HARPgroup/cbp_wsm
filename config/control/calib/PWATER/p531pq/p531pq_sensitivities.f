      function calfacLandEvap(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacLandEvap
      calfacLandEvap = 2. / (2. - Tbias)
      end

      function calfacLZSN(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacLZSN
C      calfacLZSN = ((3.0 - Sstat/Wstat) / 2.0)
      calfacLZSN = ((2.5 - Sstat/Wstat) / 1.5)
      end

      function calfacINFILT(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacINFILT
      calfacINFILT = 1.0/Bstat
      end

      function calfacIRC(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacIRC
      calfacIRC = 2.0 / (1.0 + QaveRI)
      end

      function calfacAGWR(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacAGWR
      calfacAGWR = 2.0 / (1.0 + BaveRI)
      end

      function calfacINTFW(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacINTFW
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
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacAGWETP
C       calfacAGWETP = 1.0 + (Sstat-1.0)*5.0
        calfacAGWETP = 1.0 + (Sstat-1.0)*8.0
      end

      function calfacKVARY(Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,
     .                        BaveRI,Pbias,VPbias,WBaveRI,SBaveRI,lo10)
      implicit none
      real Tbias,Wstat,Sstat,Qstat,Bstat,QaveRI,BaveRI,
     .     Pbias,VPbias,WBaveRI,SBaveRI,lo10,calfacKVARY
      calfacKVARY = 1.0
      end
      
