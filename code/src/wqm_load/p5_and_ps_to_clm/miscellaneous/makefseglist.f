       program makefseglist
       character*100 newfnam
       integer newfnum
       parameter (newfnum=10)
       character*100 fnam
       integer fnum
       parameter (fnum=11)
       character*4 cbseg
       character*6 lseg
       character*13 rseg
       integer weight
       integer err
       integer i,j
       character*100 line
       integer filelength
       parameter (filelength=535)
       character*100 fsegfnam
       integer fsegfnum
       parameter (fsegfnum=12)
       integer numfsegs
       parameter (numfsegs=132)
       character*6 fseg(numfsegs)
C      END DECLARATIONS
C      OPEN FILES
       fnam = 'p5_to_basins_clm_lrsegs.csv'
       open(UNIT=fnum,FILE=fnam,STATUS='old', IOSTAT=err, 
     .      FORM='FORMATTED')
       if(err.ne.0) stop 'error opening infile'
       newfnam = 'p5_to_basins_clm_lrsegs_new.csv'
       open(UNIT=newfnum,FILE=newfnam, STATUS='new', IOSTAT=err, 
     .      FORM='FORMATTED')
       if(err.ne.0) stop 'error opening outfile'
       fsegfnam = 'fsegs.csv'
       open(UNIT=fsegfnum,FILE=fsegfnam, STATUS='old', IOSTAT=err, 
     .      FORM='FORMATTED')
       if(err.ne.0) stop 'error opening fseg file'
C      READ IN FSEGS
       do j = 1, numfsegs
            read(fsegfnum,222) fseg(j)
       end do
C      READ IN FILE
       do i = 1, filelength
           read(fnum,999) cbseg,lseg,rseg,weight
           do j = 1, numfsegs
              if(lseg(2:6).eq.(fseg(j)(2:6))) then
                   write(newfnum,999) cbseg,fseg(j),rseg,weight 
              end if
           end do
           write(newfnum,999) cbseg,lseg,rseg,weight 
       end do 
 222   FORMAT (A6)
 999   FORMAT (A4,1X,A6,1X,A13,1X,I1)
 
       end program        
