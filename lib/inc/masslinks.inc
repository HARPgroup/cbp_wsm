************************************************************************
**  variables to read file involved in mass links, necessary for      **
**    rchres_in, and rchres_out*                                      **
************************************************************************
      integer maxRvar                           ! maximum number of river vars
      parameter (maxRvar = 40)                  ! per river segment
      integer nRvar              ! number of river variables
      integer Rdsn(maxRvar)      ! dsn for each river variable
      character*4 Rname(maxRvar)  ! variable name for Rdsn

******** maximum number of BMP constituents (aggregations of River vars
      integer maxBMPcon,nbcon
      parameter (maxBMPcon = 5)  ! set low for speed

      character*3 BMPconname(maxBMPcon)
      
