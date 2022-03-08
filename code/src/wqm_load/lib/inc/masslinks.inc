************************************************************************
**  variables to read file involved in mass links, necessary for      **
**    rchres_in, and rchres_out*                                      **
************************************************************************
      integer maxRvar                           ! maximum number of river vars
      parameter (maxRvar = 46)                  ! per river segment
      integer nRvar              ! number of river variables
      integer Rdsn(maxRvar)      ! dsn for each river variable
      character*4 Rname(maxRvar)  ! variable name for Rdsn

******** maximum number of BMP constituents (aggregations of River vars
      integer maxBMPcon,nbcon
      integer nBmpCon
      parameter (maxBMPcon = 5)  ! set low for speed

      character*3 BMPconname(maxBMPcon)

      integer nBmpConEOT
      character*4 C_BMPconnameEOT(maxBMPcon) ! EOT BMP con name
      integer     I_BMPconnameEOT(maxBMPcon) ! EOT BMP con linkage to BMPcon (tnx, tpx, etc)
      character*4 C_EOTrvars(maxBMPcon,10)
      integer     I_EOTrvars(maxBMPcon)
