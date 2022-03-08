************************************************************************
** subroutine get number of constituent with targets                  **
************************************************************************
      subroutine getname(paramscen,
     O                   tarscen,ntargets,tarnam,nplts,pltnam,
     O                   limitKIMNI,limitKAM,limitKDNI,limitKNI,
     O                   limitKIMAM,limitKLON,limitKRON)

      implicit none
      include 'calib_nitr.inc'

      character*6 tempar
      integer i
      logical found,comment
************** END DECLARATION ****************************************

       call lencl(paramscen,lenparamscen)
       fnam = controldir//'calib/NITR/'//paramscen(:lenparamscen)
     .          //'/nitr_names.dat'
       open(dfile,file=fnam,status='old',iostat=err)
       if (err.ne.0) go to 991

       line = 'GO HOOS'
       do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
       if (.not.comment(line).and.line(:6).eq.'TARGET') then
         read(dfile,'(a100)',err=1001)line
         do while (line(:10).ne.'END TARGET')
           if (.not.comment(line)) then
             tarscen = line(:10)
           end if
           read(dfile,'(a100)',err=1001)line
         end do

       else if (.not.comment(line).and.line(:12).eq.'CONSTITUENTS') then
            ntargets = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:16).ne.'END CONSTITUENTS')
              if (.not.comment(line)) then
                ntargets = ntargets + 1
                tarnam(ntargets) = line(:4)
              end if
              read(dfile,'(a100)',err=1001)line
            end do

        else if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
            nplts = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:10).ne.'END PLTGEN')
              if (.not.comment(line)) then
                nplts = nplts + 1
                pltnam(nplts) = line(:4)
              end if
              read(dfile,'(a100)',err=1001)line
            end do

        else if (.not.comment(line).and.line(:6).eq.'LIMITS') then
          read(dfile,'(a100)',err=992,end=1001)line
          do while (comment(line).or.line(:10).ne.'END LIMITS')
            read(line,*,err=992,end=1001) tempar
            call shift(line)
            if (.not.comment(line)) then
              if (tempar.eq.'KIMNI ') then
                read(line,*,err=992,end=1001) (limitKIMNI(i),i=1,4)
              else if (tempar.eq.'KAM  ') then
                read(line,*,err=992,end=1001) (limitKAM(i),i=1,4)
              else if (tempar.eq.'KDNI  ') then
                read(line,*,err=992,end=1001) (limitKDNI(i),i=1,4)
              else if (tempar.eq.'KNI  ') then
                read(line,*,err=992,end=1001) (limitKNI(i),i=1,4)
              else if (tempar.eq.'KIMAM  ') then
                read(line,*,err=992,end=1001) (limitKIMAM(i),i=1,4)
              else if (tempar.eq.'KLON  ') then
                read(line,*,err=992,end=1001) (limitKRON(i),i=1,4)
              else if (tempar.eq.'KRON  ') then
                read(line,*,err=992,end=1001) (limitKLON(i),i=1,4)
              else
                go to 993
              end if
            end if
            read(dfile,'(a100)',err=992,end=1001)line
          end do
        end if

      end do               ! finish reading the whole file

      close (dfile)

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'problem reading: variable name not understood'
      report(2) = fnam
      report(3) = line
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end
                                                                                      1,1           Top

************************************************************************
** subroutine get EOF targets provided by NRI                         **
************************************************************************
      subroutine gettargets(
     I                      tarscen,clu,lsegs,nlsegs,ntargets,tarnam,
     O                      targets)

      implicit none
      include 'calib_nitr.inc'

      character*400 dline
      character*13 ltemp

      integer order                ! indexed to the column, not land use
      logical found
      integer nc,ns,i,nl,nt

      character*13 Tlseg

      logical foundseg(maxlsegs)
**************END DECLARATION ******************************************

      do nt = 1, maxtargets
       do nl = 1, maxlsegs             ! initialize Target data
         targets(nt,nl) = 1.0
       end do
      end do
       
************ open EOF targets file
      call lencl(tarscen,lentarscen)
      do nt = 1, ntargets

        call lencl(tarnam(nt),last)
        fnam = calibdir//'target/'//tarscen(:lentarscen)//'/'
     .             //tarnam(nt)(:last)//'_target.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        
        read(dfile,'(a400)',err=992)dline            ! read header line
        call d2x(dline,i)
        call shift(dline)

        order = 1        ! find order
        do while (dline(:3).ne.clu)
          order = order + 1
          call shift(dline)
          if (dline(:3).eq.'   ') go to 993
        end do

        do ns = 1, nlsegs
          foundseg(ns) = .false.
        end do

*************** loop over all segments in file, look for active segs
        read(dfile,'(a400)',err=992,end=996)dline            ! read first line
        call d2x(dline,i)

        do while (dline(:3).ne.'end') 
          read(dline,*) Tseg
          do ns = 1,nlsegs
            if (lsegs(ns).eq.Tseg) then
              foundseg(ns) = .true.
              if (dline(len(dline)-3:len(dline)).ne.'    ') go to 994
              do nc = 1,order
                call shift(dline)
              end do
              read(dline,*,err=995) targets(nt,ns)
              exit
            end if
          end do
          read(dfile,'(a400)',err=992,end=996)dline    ! read next line
          call d2x(dline,i)
        end do

996     close (dfile)

        do ns = 1, nlsegs
          if (.not.foundseg(ns)) go to 997
        end do

      end do          ! end all target files

      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'could not find land use '//clu//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./gettargets.f'
      go to 999

995   report(1) = 'error reading target value in file'
      report(2) = fnam
      report(3) = dline
      go to 999

997   report(1) = 'did not find segment '//lsegs(ns)//' in file'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      end 

***********************************************************************
** subroutine get land EOF results                                   **
***********************************************************************
      subroutine getsimEOF(
     I                     lscen,clu,version,lsegs,nlsegs,nplts,pltnam,
     O                     simEOF)
      
      implicit none
      include 'calib_nitr.inc'

      character*13 Tlseg,Tlu
      character*300 dline
      logical found

      integer ns,l,ic,nl,np
      integer i,lpltnam
      character*(*) version
************* END DECLARATION *****************************************

      call lencl(lscen,lenlscen)
      do np = 1, nplts
        call lencl(pltnam(np),lpltnam)

        fnam = outdir//'pltgen/summary/'//lscen(:lenlscen)//'/'//
     .         clu//'_'//pltnam(np)(:lpltnam)//'_sum_'//version//'.csv'
        open(dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) then
          err = 991
          return
        end if
      
        fnam = outdir//'pltgen/summary/'//lscen(:lenlscen)//
     .            '/'//clu//'_'//pltnam(np)(:lpltnam)//'_sum.csv'
        open(dfile,file=fnam,status='old',iostat=err)

        if (err .eq. 0) then    ! find the output file for this landuse
         
************ Save the results for each run      
          read(dfile,'(a300)',err=992,end=993) dline
          call d2x(dline,last)
          call ryt(dline,dfile+1)
           
          do
            call shift(dline)
            call findcomma(dline,ic)
            Tlu = dline(:ic-1)
            call trims(Tlu,last)
            found = .false.
            if (Tlu(:3).eq.clu) then
             found = .true.
            end if
            if (.not.found) go to 994
         
            call shift(dline)
            call findcomma(dline,ic)
            Tlseg = dline(:ic-1)
            call trims(Tlseg,last)
            found = .false.
            do i = 1,nlsegs
             if (Tlseg(:6).eq.lsegs(i)) then
               found = .true.
               ns = i
               exit
             end if
            end do
            if (.not.found) go to 995
                 
            call shift(dline)
            call shift(dline)
            call shift(dline)
            read(dline,*,err=992,end=993) simEOF(np,ns)
            read(dfile,'(a300)',err=992,end=993) dline
            call d2x(dline,last)
            call ryt(dline,dfile+1)
          end do

993       close(dfile)
          close(dfile+1)

        else
          err = 996
          return
        end if

      end do            ! end loop for all constituents 
 
      return

************ ERROR SPACE **************************************
991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'land use '//Tlu(:3)// 'is not valid'
      report(2) = fnam
      report(3) = dline
      go to 999

995   report(1) = 'segment '//Tseg(:6)// 'is not valid'
      report(2) = fnam
      report(3) = dline
      go to 999

996   report(1) = 'Could not find output file for landuse'//clu
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end



