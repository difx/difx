C! Param.i
C
C   Parameters:
C
       Integer*4 IRECL
C   JPL_eph 1s the path name for the JPL DE/LE403 ephemeris file on your system.
C     (GSFC: length=34, JPL_eph = '/data18/mk3/src/calc9.0/JPL.DE403 ')
C   
C       Character*34 JPL_eph
C       Parameter (JPL_eph = '/data18/mk3/src/calc9.0/JPL.DE403 ')
       Character*256 JPL_eph
       Common /JPLfile/ JPL_eph
C      
C      Jpl_eph path and file name are entered through a CALL GETENV in
C      function calcinit.f
C       Parameter (JPL_eph = '/home/kepler/jbenson/vlba/JPL/JPLEPH ')
C       Parameter (JPL_eph = '/home/pecos2/jbenson/pgm/JPL/JPLEPH ')
C       Parameter (IRECL = 7184)
       Parameter (IRECL = 8144)
C
C     The default name of the leapsecond file.
C       (USNO: '/data/erp/ut1ls.dat';
C        GSFC: '/data1/apriori_files/ut1ls.dat')
C       (DFLEAP length - USNO: 19; GSFC: 30)
C
C     Character  DFLEAP*30
C     Parameter (DFLEAP = '/data1/apriori_files/ut1ls.dat')
      Character  DFLEAP*42
C
C     NRAO implementation of Calc doe not use this file.
C      Parameter (DFLEAP = '/home/pecos2/jbenson/pgm/calc9.0/ut1ls.dat')
C

