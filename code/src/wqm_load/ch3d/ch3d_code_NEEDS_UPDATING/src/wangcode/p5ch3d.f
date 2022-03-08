
          character*5  ccnty(10) 
          character*4  cseg(10) 
          character*80 cdum         
          real         flw(10), r(10) 
          integer      iseg(10), icnty(10) 
          character*30 infil(10) 


           open(11, file='linp5w2_no10river.prn', status='old') 
           open(31, file='p5ch3dflw_no10river.txt',
     .                     status='unknown') 

           read(11,100) cdum
           do n = 1, 2376    ! 2376 edgecells 
            read(11, *) icell, isrc,             ! up to 10 sources for a cell
     .     (r(i), icnty(i), iseg(i), i = 1, isrc)  ! percent, county, segment  
             write(cseg(i), 940) iseg(i)     ! converting integer to char 
             write(ccnty(i),950) icnty(i) 
             do i = 1, isrc
              ii = i + 10 
              close(ii) 
              if(icnty(i) .eq. 0) then     ! fallline river 
               infil(i) = '0001'//'_'//cseg(i)//'.out'   ! depends ....
              else                         ! county-seg 0000
               infil(i) = ccnty(i)//'_'//cseg(i)//'.out'  ! depends ....
              endif 
              open (ii,file=infil(i),status='old') 
             enddo 
             do j = 1, 3652     ! total days  !!! needs to change 
              do i = 1, isrc
               ii = i + 10 
               read(ii, *)iyr,imn,idy, flw (i) 
              enddo 
              flow = 0 
              do i = 1, isrc
               flow = flow + r(i)*flw(i) 
               write(31, 1234) icell, iyr,imn,idy,flow   
              enddo 
             enddo 
           enddo 
             
100         format(a80) 
940         format(a4) 
950         format(a5) 
1234        format(2(i4,1x),2(i2,1x), e12.7)  

            end 
