**************************************************************************
** main calling program for developing rating curve for simulated data  **
** Strategy - 1. seperate simulated daily flow into ascending,          **
**               descending, and base flow                              **
**            2. get simulated hourly flow and concentration            **
**            3. put hourly data into different part of hydrograph      **
**            4. develop rating curve for each set                      **
**************************************************************************
     
      subroutine rating_SMcensor(rscen,rseg,year1,year2,pltfil)
    
      implicit none
      include 'Rating_censor.inc'

      integer:: ifl,pltfil

      character(100):: pfname 

      integer:: nd,np,n,nm,m,ndays2                   
      integer:: year1,year2  

      integer yearT,monthT,dayT,hourT,minuteT
      integer yearI,monthI,dayI,hourI,minuteI

      real tssT, tssTmax,tssTmin
      real flowT, flowTmax,flowTmin

******************* END DECLARATIONS ***********************************
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

***********get simulated daily flow
      fnam = outdir//'river/daily/'//
     .        rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.FLOW'
      open(21,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      ndays = 0
      do
        read(21,*,end=111)yearT,monthT,dayT,hourT,minuteT,
     .                    flowTmax,flowT,flowTmin
        ndays = ndays + 1
        simflow(ndays) = flowT
      end do
 
111   close(21)

      if (ndays.gt.2) then
        nm = 1
        nd = 1
        call readcontrol_Rgeoscen(rscen,lenrscen,
     O                            geoscen)
        call lencl(geoscen,lengeoscen)

        fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .            '/watershed_area.csv'
                      
        call PART(simflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,    ! for simulated flow
     O              bsim,qsim,err)
        if (err.ne.0) go to 993

        call seperation(qsim,bsim,ndays,                   ! seperate ascending/descending rib for simulated flow
     O                  nasdays,ndsdays,asmatch,dsmatch)

       end if        

*********** get censored daily TSS concentration
       write(*,*) 'Making simulated sediment rating curve for ', rseg
       
       fnam = outdir//'river/daily/'//
     .        rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.TSSX'
       open(22,file=fnam,status='old',iostat=err)
       if (err.ne.0) go to 991

       do nd = 1, ndays
       read(22,*,end=112) yearT,monthT,dayT,hourT,minuteT,
     .                    tssTmax,tssT,tssTmin
       simcon(nd) = tssT
       end do
 
112   close(22)

************** developing rating curve ************************

************ for ascending rib 
      do n =1, ndays              
        do m = 1, nasdays
          if (n .eq. asmatch(m)) then    ! found the matching day
           simasfl(m)= simflow(n)  
           simascon(m)=simcon(n)
          end if
        end do
      end do 
      
      call transform(nasdays,simasfl,simascon)                     ! get ride of non-positive values
      call regress(simasfl,simascon,nasdays,ndaymax,
     O                simaslp,simasint,simasr2,aslimit,err)
       if (err.ne.0) go to 994
       
**************for dscending rib
      do n =1, ndays      
        do m = 1, ndsdays
         if (n .eq. dsmatch(m)) then    ! found the matching day
           simdsfl(m)=simflow(n)
           simdscon(m)=simcon(n)
         end if
        end do
      end do
      
      call transform(ndsdays,simdsfl,simdscon)                       ! get ride of non-positive values
      call regress(simdsfl,simdscon,ndsdays,ndaymax,
     O                simdslp,simdsint,simdsr2,dslimit,err)
       if (err.ne.0) go to 994
     
*********** for base flow
      nbsdays = 0
      do n =1, ndays               
       if (bsim(n) .ne. -9.0) then  ! found base flow
         nbsdays = nbsdays + 1  
         simbsfl(nbsdays)=simflow(n)
         simbscon(nbsdays)=simcon(n)
       end if
      end do
      
      call transform(nbsdays,simbsfl,simbscon)      ! get ride of non-positive values
      call regress(simbsfl,simbscon,nbsdays,ndaymax,
     O                simbslp,simbsint,simbsr2,bslimit,err)
       if (err.ne.0) go to 994

************ for total flow hydrograph
      ndays2 = ndays
      call transform(ndays2,simflow,simcon)                     ! get ride of non-positive values
      call regress(simflow,simcon,ndays2,ndaymax,
     O             simslp,simint,simr2,limit,err)
      if (err.ne.0) go to 994

************** write outputs into a file

       write(pltfil,100,err=951) 'TSSX'
       write(pltfil,*,err=951)'========================================'
       write(pltfil,200) 'total','ascend','descend','base'

       write(pltfil,300,err=951) 'data points',ndays2,nasdays,
     .                                             ndsdays,nbsdays
       write(pltfil,400,err=951) 'slope',simslp,simaslp,
     .                                       simdslp,simbslp
       write(pltfil,400,err=951) 'intercept',simint,simasint,
     .                                           simdsint,simbsint
       write(pltfil,400,err=951) 'correlation',simr2,simasr2,
     .                                             simdsr2,simbsr2
       write(pltfil,400,err=951) 'interval S1',simslp-limit,
     .           simaslp-aslimit,simdslp-dslimit,simbslp-bslimit
       write(pltfil,400,err=951) 'interval S2',simslp+limit,
     .           simaslp+aslimit,simdslp+dslimit,simbslp+bslimit
       write(pltfil,*,err=951) ' '
       write(pltfil,*,err=951) ' '

100   format(3x,'regression curve for simulated',1x,a4)
200   format(11x,a10,4(1x,a8))
300   format(1x,a11,4(1x,i8))
400   format(1x,a11,4(1x,f8.3))

      return
  
************************ ERROR REPORTING *******************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening file for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

993   report(1) = 'Error in PART program'
      report(2) = 'unspecified error, check file'
      report(3) = './pp/src/postproc/river/part/part_sub.f'
      go to 999

994   report(1) ='Problem making simulated regression for segment'//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

