************************************************************************
***** land constituents with names that match wdm variables and       **
*****    surrogates that match BMP variables                          **
************************************************************************

      integer nlcon

********* parameters to size the problem
      parameter (nlcon=31)           ! number of constituents
 
      character*4 lconname(nlcon),lconsurrogate(nlcon)
       !           dsn name,       bmp to apply

      data lconname /'SURO','IFWO','AGWO','SEDM','SOHT','IOHT','AOHT',
     .               'SDOX','IDOX','ADOX','DLON','DNH3','DRON','SNH3',
     .               'INH3','ANH3','SNO3','INO3','ANO3','SLON','ILON',
     .               'ALON','SRON','IRON','ARON','DORP','DPO4','SPO4',
     .               'IPO4','APO4','SBOD'/

      data lconsurrogate /'flo','flo','flo','sed','flo','flo','flo',
     .                    'flo','flo','flo','tn ','tn ','tn ','tn ',
     .                    'tn ','tn ','tn ','tn ','tn ','tn ','tn ',
     .                    'tn ','tn ','tn ','tn ','tp ','tp ','tp ',
     .                    'tp ','tp ','bod'/
      
