C     Include file for PLOTAPD.
C
      INTEGER    MBAS, MTIME, INUNIT, MTBAS, MCHAN
      REAL       PI, RADDEG
      PARAMETER  (MBAS=10)           ! Max baselines in a pass.
      PARAMETER  (MTIME=10000)
      PARAMETER  (INUNIT=8)
      PARAMETER  (MTBAS=190)         ! Maximum total baselines.
      PARAMETER  (MCHAN=32)
      PARAMETER  (PI=3.141592653)
      PARAMETER  (RADDEG=PI/180.0)
C
      INTEGER    NTIME(MBAS)
      REAL       TIME(MTIME,MBAS)
      REAL       AMP(MCHAN,MTIME,MBAS), PHASE(MCHAN,MTIME,MBAS)
      REAL       DELAY(MCHAN,MTIME,MBAS), RATE(MCHAN,MTIME,MBAS)
      REAL       TMIN, TMAX, PMIN(MBAS), PMAX(MBAS), TMINA, TMAXA
      REAL       AMIN(MBAS), AMAX(MBAS), DMIN(MBAS), DMAX(MBAS)
      REAL       RMIN(MBAS), RMAX(MBAS)
      INTEGER    NBAS, NCHAN, JDAY1, DOCH(MCHAN), NDOCH, NANT
      INTEGER    IP1, IP2
      LOGICAL    XWIN
      CHARACTER  NAMEA(MTBAS)*8, NAMEB(MTBAS)*8, DOANT(10)*8
      CHARACTER  DATFILE*80, PLTFILE*80, EXPNAM*8
      CHARACTER  SOURCE(MTIME,MBAS)*12
C
      COMMON  / PTWD / NTIME, TIME, AMP, PHASE, DELAY, RATE, TMIN, TMAX,
     1                 PMIN, PMAX, TMINA, TMAXA, AMIN, AMAX, DMIN, DMAX,
     2                 RMIN, RMAX, NBAS, NCHAN, JDAY1, DOCH, NDOCH, 
     3                 NANT, IP1, IP2, XWIN
      COMMON  / PTWC / NAMEA, NAMEB, DOANT, DATFILE, PLTFILE, EXPNAM, 
     1                 SOURCE
C
C     Variables for reading lines of data.  Put in common to
C     avoid compiler complaints about unused variables.
C
      INTEGER    MWORDS
      PARAMETER  (MWORDS=MCHAN*4+50)
      INTEGER    WLEN(MWORDS), NWORDS
      CHARACTER  WORD(MWORDS)*20
      COMMON / RLNDAT / WLEN, NWORDS
      COMMON / RLNCHR / WORD
C --------------------------------------------------------------




