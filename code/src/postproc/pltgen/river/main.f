************************************************************************
** subroutine to get pltgen file for each output variable of interest **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/pltgen.inc'

      character*100 dfnam

      integer wdmfil
      parameter (wdmfil=12)           ! file number for wdm

      integer nt   ! number of variable types
      integer year,month,day,hour,zero
      integer year1,oldyear             ! the first and last simulation year
      integer annevent,annnotzero,allevent,allnotzero
      integer annnot1,annnot2,annnot3
      integer allnot1,allnot2,allnot3
      integer allnot4,allnot5,allnot6
      
      real value,annvalue,allvalues
      real aveann, aveload

      logical founddouble
      character*13 conseg 
      character*1 resflag

      integer Nexits,idummy,timestep

************** END OF DECLARATIONS  ************************************

      read*,rseg,rscen

      call readcontrol_vartypes(rscen,maxplts,pltname,numplts)

      call lencl(rseg,lenrseg)
      call lencl(rscen,lenrscen)
      
      fnam = outdir//'pltgen/river/'//rscen(:lenrscen)//'/'//
     .              rseg(:lenrseg)//'.sums'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,100,err=951)'scen',',','segment',',','var',',',
     .                'years',',',
     .                'feet/ton',',', 'perc',',', 'frac>0.',',', 
     .                'frac>3.',',', 'frac>5.',',', 'frac>6.',',', 
     .                'frac>6.5',',', 'frac>7.',',', 'frac>10'
      
      do nt = 1,numplts
                
        dfnam = outdir//'pltgen/river/'//rscen(:lenrscen)//'/'//
     .              rseg(:lenrseg)//'.'//pltname(nt)
        open(dfile+1,file=dfnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile+1,'(A26)') line                         
        if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! origan pltgen from HSPF simulation
           call plt2cal(dfile+1,dfnam)                      ! modify original pltgen file  
           read(dfile+1,'(A26)') line                      ! read the first line of modified file 
        end if

        read(line,*) oldyear
        year1 = oldyear
        backspace (dfile+1)

        annvalue = 0.0
        allvalues = 0.0
        annevent = 0
        allevent = 0
        annnotzero = 0
        annnot1 = 0
        annnot2 = 0
        annnot3 = 0
        allnotzero = 0
        allnot1 = 0
        allnot2 = 0
        allnot3 = 0
        allnot4 = 0
        allnot5 = 0
        allnot6 = 0

        do
          read(dfile+1,*,err=994,end=111) year,month,day,hour,zero,value
          if (year.ne.oldyear) then
            annvalue = 0.0
            annevent = 0
            annnotzero = 0
            oldyear = year
          end if

          annvalue = annvalue + value
          allvalues = allvalues + value
          
          if (abs(value).gt.1.0) annnotzero = annnotzero + 1
          if (abs(value).gt.0.1) annnot1 = annnot1 + 1
          if (abs(value).gt.0.01) annnot2 = annnot2 + 1
          if (abs(value).gt.0.001) annnot3 = annnot3 + 1
          if (abs(value).gt.0.0) allnotzero = allnotzero + 1
          if (abs(value).gt.3.0) allnot1 = allnot1 + 1
          if (abs(value).gt.5.0) allnot2 = allnot2 + 1
          if (abs(value).gt.6.0) allnot3 = allnot3 + 1
          if (abs(value).gt.6.5) allnot4 = allnot4 + 1
          if (abs(value).gt.7.0) allnot5 = allnot5 + 1
          if (abs(value).gt.10.0) allnot6 = allnot6 + 1
          
          annevent = annevent + 1
          allevent = allevent + 1
        
        end do            ! loop over the opened pltgen file

111     close(dfile+1)

*********** Calculate the percent of DEPSCR/average annual sediment load
        
      if ( pltname(nt) .eq. 'depscr') then    ! do the percent calculation for DEPSCR only   
                
*********** Calculate average anuual sediment loads 

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)
                  !look for rseg in doubles.csv file

      if (founddouble) then
        Nexits = 3
        call loadavann(rscen,conseg,year1,oldyear,wdmfil+1,Nexits,
     O              aveload)
        call wdflc1(wdmfil+1,err)    ! close wdm file
      end if

      call riverstop(rseg)     ! stop if not a simulated river

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)

      call loadavann(rscen,rseg,year1,oldyear,wdmfil,Nexits,
     O            aveload)

      if (founddouble) then
        call wdflcl(wdmfil,err)    ! close wdm file
      else
        call wdflc1(wdmfil,err)    ! close wdm file
      end if
       
        aveann = allvalues/real(oldyear-year1+1)  ! get average annual depscr
      end if                    
      
************* write the rsult file
        write(dfile,200,err=951)rscen(:lenrscen),',',rseg(:lenrseg),',',
     .         pltname(nt),',', year1,'-',oldyear,',',
     .         allvalues/real(oldyear-year1+1),',',
     .         aveann/aveload,',',                      ! the percent of depscr/annual load
     .         real(allnotzero)/real(allevent),',',
     .         real(allnot1)/real(allevent),',',
     .         real(allnot2)/real(allevent),',',
     .         real(allnot3)/real(allevent),',',
     .         real(allnot4)/real(allevent),',',
     .         real(allnot5)/real(allevent),',',
     .         real(allnot6)/real(allevent)

      end do             ! loop over all variables     

      close(dfile)

100   format(a8,a1,1x,a13,a1,1x,a6,a1,1x,a9,a1,a10,
     .       a1,a7,4(a1,a8), a1,a9, 2(a1,a8))

200   format(a8,a1,1x,a13,a1,1x,a6,a1,1x,i4,a1,i4,a1,f10.3,
     .       a1,f7.4,4(a1,f8.4),a1,f9.4, 2(a1,f8.4))

      stop

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,zero,value
      go to 999

999   call stopreport(report)
      end

