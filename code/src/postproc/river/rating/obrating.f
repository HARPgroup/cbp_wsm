*************************************************************************
** main calling program for developing rating curve for observed data  **
** Strategy - 1. seperate observed daily flow into ascending,          **
**               descending, and base flow                             **
**            2. get observed instantous flow and concentration        **
**            3. put instantous data into different part of hydrograph **
**            4. develop rating curve for each set                     **
*************************************************************************
      
      subroutine obrating(rscen,rseg,year1,year2,Nexits,pltfil)
    
      implicit none
      include 'Rating.inc'

      integer:: ifl,pltfil
    
      character(100):: pfname, obfnam, obldfnam, tfnam
  
      integer:: i,np,n,m                    ! indices 
      integer:: ny,nm,nd,nh,nmin,oh,no                   
      integer:: ny2,nm2,nd2,nh2,nmin2

      integer nobshour(EarliestYear:LatestYear,12,31,24)

      integer:: year1,year2                 ! first and last year to average
      integer:: obhour(EarliestYear:LatestYear,12,31,24)
     .                                      ! the hour of instantous measurement 

      real:: tflo, hflow, hconc             ! instantous flow and concentration
      real:: tflow, tconc

      integer:: Nexits, day

******************* END DECLARATIONS ***********************************

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname) 
                                           ! POPULATE nRvar, Rdsn, Rname
*********** get observed daily flow  

      obfnam=calibdir//'observed/FLOW/'//rseg(:lenrseg)//
     .                     '.OFLOW'

      call findopen(ifl)
      open(ifl,file=obfnam,status='old',iostat=err)

      if (err .eq. 0) then             ! found obserbed file for this segment
      
        ndays=0
        do
          read(ifl,*, end=111) ny,nm,nd,nh,nmin,tflo

          if (ny.ge.year1 .and. ny.le.year2) then
            ndays = ndays+1
            if (tflo.lt.0.1) tflo = 0.1
            obflow(ndays) = tflo            ! unit: cfs
            obyear(ndays) = ny
            obmonth(ndays) = nm
            obday(ndays) = nd
            obfl(ny,nm,nd) = tflo           ! observed daily flow
          end if

         end do

111     close(ifl)                                                    ! close observed flow file
        
************** partition quick flow and base flow

         if (ndays.gt.2) then
           nm = 1
           nd = 1
           call readcontrol_Rgeoscen(rscen,lenrscen,
     O                               geoscen)
           call lencl(geoscen,lengeoscen)

           fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .              '/watershed_area.csv'
           
           call PART(obflow,ndaymax,ndays,fnam,year1,nm,nd,rseg,     ! for observed flow 
     O              bobs,qobs,err)
           if (err.ne.0) go to 993

           call seperation(qobs,bobs,ndays,                  ! seperate ascending/descending rib for observed flow
     O                   nasdays,ndsdays,asmatch,dsmatch)
  
        end if                                       

      end if                                       ! end if observed file exists
       
    
************** get concentration infomation

      call caloadinfo(rscen,lenrscen,nRvar,Rname,
     O              nloads,loadname,unit,ncons,con,confactor)  ! POPULATE loading variables
                                          
      do np = 1,nloads

       if (loadname(np) .ne. 'FLOW') then

******************** check if observed file exist
       obldfnam=calibdir//'observed/'//loadname(np)//
     .           '/'//rseg(:lenrseg)//'.O'//loadname(np)

       call findopen(ifl)
            
       open(ifl,file=obldfnam,status='old',iostat=err)

       if (err .eq. 0) then                       ! find observed file

       write(*,*)'Making observed sediment rating curve for ', rseg
    
************* read observed concentration and instantous flow
        do ny = year1,year2
          do nm =1,12
            do nd = 1,31
              do nh = 1,24
               nobshour(ny,nm,nd,nh) = 0
              end do
            end do
          end do
        end do

        do
          read(ifl,*,end=222) ny2,nm2,nd2,nh2,nmin2,hconc
           if (ny2.ge.year1 .and. ny2.le.year2) then
              nobshour(ny2,nm2,nd2,nh2) = nobshour(ny2,nm2,nd2,nh2) + 1
              obcon(ny2,nm2,nd2,nh2,nobshour(ny2,nm2,nd2,nh2)) = hconc
           end if              
        end do

222     close(ifl)           ! close observed load file

************ DEVELOPING RATING CURVE ************************

************ for ascending rib 
      naspts = 0
      do n =1, ndays              
        do m = 1, nasdays
         if (n .eq. asmatch(m)) then    ! found the matching day
           do nh = 1, 24
             if (nobshour(obyear(n),obmonth(n),obday(n),nh).gt.0) then
              do no = 1,nobshour(obyear(n),obmonth(n),obday(n),nh)
               tflow = obfl(obyear(n),obmonth(n),obday(n))
               tconc = obcon(obyear(n),obmonth(n),obday(n),nh,no)
              
               if (tflow.gt.0.0001 .and. tconc .gt. 0.0001) then  ! both daily flow and inst.concentration exist at the time  
                naspts = naspts + 1
                obasfl(naspts) = tflow
                obascon(naspts)= tconc
               end if
              end do
             end if
           end do
         end if
        end do
      end do

      call transform (naspts,obasfl,obascon)           ! get ride of non-positive values
      
       if (naspts .gt. 2) then
         call regress(obasfl,obascon,naspts,nptsmax,
     O                obaslp,obasint,obasr2,aslimit,err)
         if (err.ne.0) go to 994
       else
         obaslp = 0.0            ! when less than 2 points, no regression
         obasint= 0.0
         obasr2 = 0.0
         aslimit= 0.0
       end if

************ for dscending rib 
      ndspts = 0
      do n =1, ndays               ! for dscending rib
        do m = 1, ndsdays
         if (n .eq. dsmatch(m)) then    ! found the matching day
           do nh = 1, 24
            if (nobshour(obyear(n),obmonth(n),obday(n),nh).gt.0) then   ! found observed instantous point
              do no = 1,nobshour(obyear(n),obmonth(n),obday(n),nh)
                tflow = obfl(obyear(n),obmonth(n),obday(n))
                tconc = obcon(obyear(n),obmonth(n),obday(n),nh,no)
    
                if (tflow.gt.0.0001 .and. tconc .gt. 0.0001) then    ! both daily flow and inst. conc. exist at the time
                 ndspts = ndspts + 1
                 obdsfl(ndspts)= tflow
                 obdscon(ndspts)= tconc
                end if
              end do  
            end if
           end do
         end if
        end do
      end do
      
      call transform(ndspts,obdsfl,obdscon)     ! get ride of non-positive values
   
       if (ndspts .gt. 2) then
         call regress(obdsfl,obdscon,ndspts,nptsmax,
     O                obdslp,obdsint,obdsr2,dslimit,err)
         if (err.ne.0) go to 994
       else
         obdslp = 0.0            ! when less than 2 points, no regression
         obdsint= 0.0
         obdsr2 = 0.0
         dslimit= 0.0
       end if

************ for base flow
      nbspts = 0
      do n =1, ndays                 
        if (bobs(n) .ne. -9.0) then     ! found base flow
          do nh = 1, 24
            if (nobshour(obyear(n),obmonth(n),obday(n),nh).gt.0) then
              do no = 1,nobshour(obyear(n),obmonth(n),obday(n),nh)     ! found observed instantous point
               tflow = obfl(obyear(n),obmonth(n),obday(n))
               tconc = obcon(obyear(n),obmonth(n),obday(n),nh,no)
              
               if (tflow.gt.0.0001 .and. tconc .gt. 0.0001) then      ! both daily flow and inst. concentration exist at the time
                nbspts = nbspts + 1
                obbsfl(nbspts) = tflow
                obbscon(nbspts)= tconc
               end if
             end do 
           end if
          end do
        end if
      end do

      call transform(nbspts,obbsfl,obbscon)    ! get ride of non-positive values

       if (nbspts .gt. 2) then
         call regress(obbsfl,obbscon,nbspts,nptsmax,
     O                obbslp,obbsint,obbsr2,bslimit,err)
         if (err.ne.0) go to 994
 
       else
         obbslp = 0.0            ! when less than 2 points, no regression
         obbsint= 0.0
         obbsr2 = 0.0
         bslimit= 0.0
       end if

************ for total flow hydrograph
      npts = 0
      do n =1, ndays
        do nh = 1, 24
          if (nobshour(obyear(n),obmonth(n),obday(n),nh).gt.0) then
           do no = 1,nobshour(obyear(n),obmonth(n),obday(n),nh)
             tflow = obfl(obyear(n),obmonth(n),obday(n))
             tconc = obcon(obyear(n),obmonth(n),obday(n),nh,no)
            if (tflow.gt.0.0001 .and. tconc .gt. 0.0001) then  ! both daily flow and inst. concentration exist at the time
             npts = npts + 1
             oballfl(npts) = tflow
             oballcon(npts)= tconc
            end if
           end do
          end if
        end do
      end do

      call transform (npts,oballfl,oballcon)           ! get ride of non-positive values
      if (npts .gt. 2) then
        call regress(oballfl,oballcon,npts,nptsmax,
     O                obslp,obint,obr2,limit,err)
        if (err.ne.0) go to 994
      else
        obslp= 0.0            ! when less than 2 points, no regression
        obint= 0.0
        obr2 = 0.0
        limit= 0.0
      end if

************** write outputs into a file
         
        write(pltfil,100,err=951) loadname(np)
        write(pltfil,*,err=951)'======================================='
        write(pltfil,200,err=951) 'total','ascend','descend','base'     
        write(pltfil,300,err=951) 'data points',npts,naspts,
     .                                              ndspts,nbspts
        write(pltfil,400,err=951) 'slope      ',obslp,obaslp,
     .                                              obdslp,obbslp
        write(pltfil,400,err=951) 'intercept  ',obint,obasint,
     .                                              obdsint,obbsint
        write(pltfil,400,err=951) 'correlation',obr2,obasr2,
     .                                              obdsr2,obbsr2
        write(pltfil,400,err=951) 'interval S1',obslp-limit,
     .                obaslp-aslimit,obdslp-dslimit,obbslp-bslimit
        write(pltfil,400,err=951) 'interval S2',obslp+limit,
     .                obaslp+aslimit,obdslp+dslimit,obbslp+bslimit  

        write(pltfil,*,err=951) ' '
        write(pltfil,*,err=951) ' '
        end if           ! end if observed file exist                          
       end if            ! end if for non-flow constituents  
      end do             ! end loop over all loads in 'rchres_out_to_cali'

100   format(3x,'regression curve for observed ',1x,a4)
200   format(11x,a10,4(1x,a8))
300   format(1x,a11,4(1x,i8))
400   format(1x,a11,4(1x,f8.3))
   
      return
  
************************ ERROR REPORTING *******************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

990   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

991   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

992   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

993   report(1) = 'Error in PART program'
      report(2) = 'unspecified error, check file'
      report(3) = './pp/src/postproc/river/part/part_sub.f'
      go to 999

994   report(1) = 'Problem making observed regression for segment'//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

