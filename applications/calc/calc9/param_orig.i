C! Param.i
C
C   Parameters:
C
       Integer*4 IRECL
C   JPL_eph 1s the path name for the JPL DE/LE403 ephemeris file on your system.
C     (GSFC: length=34, JPL_eph = '/data18/mk3/src/calc9.0/JPL.DE403 ')
C   
       Character*34 JPL_eph
       Parameter (JPL_eph = '/data18/mk3/src/calc9.0/JPL.DE403 ')
       Parameter (IRECL = 7184)
C
C     The default name of the leapsecond file.
C       (USNO: '/data/erp/ut1ls.dat';
C        GSFC: '/data1/apriori_files/ut1ls.dat')
C       (DFLEAP length - USNO: 19; GSFC: 30)
C
C     Character  DFLEAP*30
C     Parameter (DFLEAP = '/data1/apriori_files/ut1ls.dat')
      Character  DFLEAP*33
      Parameter (DFLEAP = '/data18/mk3/src/calc9.0/ut1ls.dat')
C
