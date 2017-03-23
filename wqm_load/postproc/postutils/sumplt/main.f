************************************************************************
** subroutine to modify HSPF pltgen files for readiblilty             **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer file1,file2          ! file number

      character*3 clu  ! character land use

      integer maxtypes,ntypes,nt   ! number of variable types
      parameter (maxtypes=10)

      character*4 vartype(maxtypes)    ! variable type matches obserations

      integer year,month,day,hour,zero,oldyear,year1
      integer annevent,annnotzero,allevent,allnotzero
      integer annnot1,annnot2,annnot3,allnot1,allnot2,allnot3
      real value,annvalue,allvalues

********** END DECLARATIONS  *******************************************
      read*,lseg,clu,lscen

      call readcontrol_vartypes(clu,lscen,maxtypes,vartype,ntypes)

      call findopen(file2)           ! open pltgen

      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)
      
      fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//'/'//clu//
     .         '_'//lseg(:lenlseg)//'.RO_inches'
      open(file2,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(file2,'(a,a,a)',err=951)
     .                    '      scen, lu,   seg, var,  years  ,  ',
     .                    'inches  ,  frac>1  ,   frac>.1,  frac>.01',
     .                    ', frac>.001 '
      write(*,'(a,a,a)',err=951)
     .                    '      scen, lu,   seg, var,  years  ,  ',
     .                    'inches  ,  frac>1  ,   frac>.1,  frac>.01',
     .                    ', frac>.001 '
      do nt = 1,ntypes

        call findopen(file1)           ! open pltgen
        fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//'/'//clu//
     .         '_'//lseg(:lenlseg)//'.'//vartype(nt)

        open(file1,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(file1,'(A26)') line
        if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! not modified
          close(file1)
          print*,'you must run plt2cal.com first'
          return
        end if

        read(line,*) oldyear
        year1 = oldyear
        backspace file1

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
        do
          read(file1,*,err=994,end=111) year,month,day,hour,zero,value
          if (year.ne.oldyear) then
C            print*,lscen(:lenlscen),',',clu,',',lseg(:lenlseg),',',
C     .             vartype(nt),',',oldyear,',',annvalue,',',
C     .             real(annnotzero)/real(annevent),',',
C     .             real(annnot1)/real(annevent),',',
C     .             real(annnot2)/real(annevent),',',
C     .             real(annnot3)/real(annevent)
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
          if (abs(value).gt.1.0) allnotzero = allnotzero + 1
          if (abs(value).gt.0.1) allnot1 = allnot1 + 1
          if (abs(value).gt.0.01) allnot2 = allnot2 + 1
          if (abs(value).gt.0.001) allnot3 = allnot3 + 1
          annevent = annevent + 1
          allevent = allevent + 1
        end do

111     close(file1)

C        print*,lscen(:lenlscen),',',clu,',',lseg(:lenlseg),',',
C     .             vartype(nt),',',oldyear,',',annvalue,',',
C     .             real(annnotzero)/real(annevent),',',
C     .             real(annnot1)/real(annevent),',',
C     .             real(annnot2)/real(annevent),',',
C     .             real(annnot3)/real(annevent)

        print 222,lscen(:lenlscen),',',clu,',',lseg(:lenlseg),',',
     .         vartype(nt),',',year1,'-',oldyear,',',
     .         allvalues/real(oldyear-year1+1),',',
     .         real(allnotzero)/real(allevent),',',
     .         real(allnot1)/real(allevent),',',
     .         real(allnot2)/real(allevent),',',
     .         real(allnot3)/real(allevent)

        write(file2,222,err=951)
     .         lscen(:lenlscen),',',clu,',',lseg(:lenlseg),',',
     .         vartype(nt),',',year1,'-',oldyear,',',
     .         allvalues/real(oldyear-year1+1),',',
     .         real(allnotzero)/real(allevent),',',
     .         real(allnot1)/real(allevent),',',
     .         real(allnot2)/real(allevent),',',
     .         real(allnot3)/real(allevent)

      end do

      close(file2)

      return

222   format(A10,a1,a3,a1,a6,a1,a4,a1,i4,a1,i4,a1,f10.6,4(a1,f10.6))

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

