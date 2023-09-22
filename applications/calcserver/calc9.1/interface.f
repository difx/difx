       SUBROUTINE GET4 (VARNAM,RVARB,NDIM1,NDIM2,NDIM3,NDO,IERR) 
C--------------------------------------------------------------------------
C 
C      Replaces the GET4 in CALC. Provides the interface between 
C      CALC and the Correlator job script tables.
C
C--------------------------------------------------------------------------
       INTEGER*2  NDIM1,NDIM2,NDIM3
       INTEGER*2  NDO,IERR
       INTEGER*4  FSTRCMP, FSTRCPY, N_ROWS, I_FTOC, IROW, I, NROWS
       INTEGER*4  MJDROW, IMJDATE, ITYPE, OCE_FETCH
C
       REAL*8   D_FTOC, F_FTOC, MJD2JD, D_MATHCNST, MJD_FTOC
       REAL*8   RVARB(NDIM1,NDIM2,NDIM3)
       REAL*8   X, Y, Z, ROWTIME, MJDTIME, PRESSURE(2,2), TEMP(2,2)
       REAL*8   VX, VY, VZ, AX, AY, AZ, DEWP(2,2)
       REAL*8   TAIUTC, UT1UTC, UTCTIME, POLYTIME
C       REAL*4   F_FTOC
C
       CHARACTER*14 VARNAM
       CHARACTER*16 SITNAM, NAME, C_FTOC
C
       include 'CALCIO.INC'
C
       IERR = 0
       IROW = 0
       RVARB(1,1,1) = -99999.0

C       write (6,*) "VARNAM = ", VARNAM, ".", JOBNUM
C
C  Catch illegal job numbers
       IF (JOBNUM.LE.0.OR.JOBNUM.GE.9999) THEN 
          CALL I1WRITE ('ERROR GET4, JOBNUM = ',JOBNUM)
          IERR = 1
          GO TO 999
       END IF

C  Get physical constants from MATHCNST.H
       IF (NDIM1.EQ.1) THEN
          IF (fstrcmp(VARNAM,'VLIGHT        ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('VLIGHT')
          IF (fstrcmp(VARNAM,'GAUSS         ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('GAUSS')
          IF (fstrcmp(VARNAM,'ACCELGRV      ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('ACCELGRV')
          IF (fstrcmp(VARNAM,'GMSUN         ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('GMSUN')
          IF (fstrcmp(VARNAM,'GMEARTH       ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('GMEARTH')
          IF (fstrcmp(VARNAM,'GMMOON        ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('GMMOON')
          IF (fstrcmp(VARNAM,'TSEC/AU       ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('TSECAU')
          IF (fstrcmp(VARNAM,'EARTHRAD      ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('EARTHRAD')
          IF (fstrcmp(VARNAM,'EMS/MMS       ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('EMSMMS')
          IF (fstrcmp(VARNAM,'U-GRV-CN      ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('UGRVCN')
          IF (fstrcmp(VARNAM,'E-FLAT        ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('EFLAT')
          IF (fstrcmp(VARNAM,'PRE DATA      ').eq.0) 
     +       RVARB(1,1,1)=D_MATHCNST('PRECONST')
          IF (fstrcmp(VARNAM,'REL DATA      ').eq.0) 
     +       RVARB(1,1,1) = 1.0
C
C  Send a reference frequency to CALC for feed rotation 
C   correction. The phase correction is not actually used by CALC.
C  Called for from cpanm.f, the actual call is disabled because
C   cpanm.f is turned off.
          IF (fstrcmp(VARNAM,'REF FREQ      ').eq.0) 
     +       RVARB(1,1,1)=5.0D3
C
C  Get UTC seconds of requested Calc model time
          IF (fstrcmp(VARNAM,'SEC TAG       ').eq.0) 
     +        RVARB(1,1,1) = GETSEC
C
          GO TO 900
       END IF
C
C  Get TAI-UTC 
C
       IF (NDIM1.GT.1) THEN
          IF (fstrcmp(VARNAM,'TAI- UTC      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'UTC')
             IF (NROWS.LE.0) GO TO 900
             DO I = 1, NROWS
                ROWTIME = MJD_FTOC(JOBNUM,'UTC',I-1,'TIME')
                MJDROW = ROWTIME
                IF (MJDROW.EQ.MJDATE) THEN
                   RVARB(1,1,1) = 0.0
                   RVARB(2,1,1) = D_FTOC(JOBNUM,'UTC',I-1,'TAIUTC')
                   RVARB(3,1,1) = 0.0
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the atmospheric surface pressure (millibars)
C
          IF (fstrcmp(VARNAM,'ATM PRES      ').eq.0) THEN
C
C  Calc defaults for no values found for atmospheric module
C
             RVARB(1,1,1)=-99900.0D0
             RVARB(2,1,1)=-99900.0D0 
             RVARB(1,2,1)=-99900.0D0
             RVARB(2,2,1)=-99900.0D0
             IERR = 1
C
             NROWS = N_ROWS(JOBNUM, 'METRO')
             IF (NROWS.LE.0) GO TO 900
C
C  For NROWS = 1, this is a CALC RPC Server request
C
             IF (NROWS.EQ.1) THEN
                RVARB(2,1,1) = F_FTOC (JOBNUM, 'METRO', 1, 
     +                                   'PRESSURE')
                RVARB(2,2,1) = 0.0
                IERR = 0
                GO TO 900
             END IF
C
C  The current station name is :
             IROW = GETSIT(2) - 2
             IERR = FSTRCPY(SITNAM,
     +              C_FTOC(JOBNUM,'STATION', IROW, 'NAME'))
C
C  Calculate the time of day in fraction of day
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0D0
     +               +  DFLOAT(GETTAG(5)) / 1440.0D0
     +               +  GETSEC / 86400.0D0)
             MJDTIME = DFLOAT(MJDATE) + UTCTIME
C
C  Loop through METRO table, find rows for current station
             PRESSURE(1,1) = 0.0D0
             PRESSURE(1,2) = 0.0D0
             PRESSURE(2,1) = 0.0D0
             PRESSURE(2,2) = 0.0D0
             DO I = 1, NROWS
                IERR = FSTRCPY(NAME,
     +                 C_FTOC(JOBNUM,'METRO', I-1, 'NAME'))
                IF (fstrcmp (NAME, SITNAM).eq.0) THEN
                   PRESSURE(1,1) = PRESSURE(2,1)
                   PRESSURE(1,2) = PRESSURE(2,2)
                   PRESSURE(2,1) = MJD_FTOC(JOBNUM,'METRO',I-1,'TIME')
                   PRESSURE(2,2) = F_FTOC (JOBNUM, 'METRO', I-1, 
     +                                   'PRESSURE')
                   IF (MJDTIME .GE. PRESSURE(1,1) .AND.
     +                 MJDTIME .LE. PRESSURE(2,1)) THEN
                      RVARB(2,1,1) = PRESSURE(1,2)
     +                          + (PRESSURE(2,2) - PRESSURE(1,2))
     +                          * (MJDTIME - PRESSURE(1,1))
     +                          / (PRESSURE(2,1) - PRESSURE(1,1))
                      RVARB(2,2,1) = 0.0D0
                      IERR = 0
                      IF (RVARB(2,1,1) .LT. 450.0D0 .OR.
     +                    RVARB(2,1,1) .GT. 1300.0D0)
     +                   RVARB(2,1,1) = -99900.0D0
                      GO TO 900
                   END IF
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the surface temoerature (degrees C)
C
          IF (fstrcmp(VARNAM,'TEMP C        ').eq.0) THEN
C
C  Calc defaults for no values found for atmospheric module
C
             RVARB(1,1,1)=-999.0D0
             RVARB(2,1,1)=-999.0D0 
             RVARB(1,2,1)=-999.0D0
             RVARB(2,2,1)=-999.0D0
             IERR = 1
C  Currently surface temps are not present in the job scripts or
C  in the CALC RPC Server request records
             GO TO 900
C
             NROWS = N_ROWS(JOBNUM, 'METRO')
             IF (NROWS.LE.0) GO TO 900
C
C  The current station name is :
             IROW = GETSIT(2) - 2
             IERR = FSTRCPY(SITNAM,
     +              C_FTOC(JOBNUM,'STATION', IROW, 'NAME'))
C
C  Calculate the time of day in fraction of day
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0D0
     +               +  DFLOAT(GETTAG(5)) / 1440.0D0
     +               +  GETSEC / 86400.0D0)
             MJDTIME = DFLOAT(MJDATE) + UTCTIME
C
C  Loop through METRO table, find rows for current station
             TEMP(2,2) = 0.0D0
             DO I = 1, NROWS
                IERR = FSTRCPY(NAME,
     +                 C_FTOC(JOBNUM,'METRO', I-1, 'NAME'))
                IF (fstrcmp (NAME, SITNAM).eq.0) THEN
                   TEMP(1,1) = TEMP(2,1)
                   TEMP(1,2) = TEMP(2,2)
                   TEMP(2,1) = MJD_FTOC(JOBNUM,'METRO',I-1,'TIME')
                   TEMP(2,2) = F_FTOC (JOBNUM, 'METRO', I-1, 
     +                                   'TEMP')
                   IF (MJDTIME .GE. TEMP(1,1) .AND.
     +                 MJDTIME .LE. TEMP(2,1)) THEN
                      RVARB(2,1,1) = TEMP(1,2)
     +                          + (TEMP(2,2) - TEMP(1,2))
     +                          * (MJDTIME - TEMP(1,1))
     +                          / (TEMP(2,1) - TEMP(1,1))
                      RVARB(2,2,1) = 0.0D0
                      IERR = 0
                      GO TO 900
                   END IF
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the surface relative humidity
C
          IF (fstrcmp(VARNAM,'REL.HUM.      ').eq.0) THEN
C
C  Calc defaults for no values found for atmospheric module
C
             RVARB(1,1,1)=-999.0D0
             RVARB(2,1,1)=-999.0D0 
             RVARB(1,2,1)=-999.0D0
             RVARB(2,2,1)=-999.0D0
             IERR = 1
C  Currently dew points are not present in the job scripts or
C  in the CALC RPC Server request records
             GO TO 900
C
             NROWS = N_ROWS(JOBNUM, 'METRO')
             IF (NROWS.LE.0) GO TO 900
C
C  The current station name is :
             IROW = GETSIT(2) - 2
             IERR = FSTRCPY(SITNAM,
     +              C_FTOC(JOBNUM,'STATION', IROW, 'NAME'))
C
C  Calculate the time of day in fraction of day
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0D0
     +               +  DFLOAT(GETTAG(5)) / 1440.0D0
     +               +  GETSEC / 86400.0D0)
             MJDTIME = DFLOAT(MJDATE) + UTCTIME
C
C  Loop through METRO table, find rows for current station
             DEWP(2,2) = 0.0D0
             DO I = 1, NROWS
                IERR = FSTRCPY(NAME,
     +                 C_FTOC(JOBNUM,'METRO', I-1, 'NAME'))
                IF (fstrcmp (NAME, SITNAM).eq.0) THEN
                   DEWP(1,1) = DEWP(2,1)
                   DEWP(1,2) = DEWP(2,2)
                   DEWP(2,1) = MJD_FTOC(JOBNUM,'METRO',I-1,'TIME')
                   DEWP(2,2) = F_FTOC (JOBNUM, 'METRO', I-1, 
     +                                   'DEWPOINT')
                   IF (MJDTIME .GE. DEWP(1,1) .AND.
     +                 MJDTIME .LE. DEWP(2,1)) THEN
                      RVARB(2,1,1) = DEWP(1,2)
     +                          + (DEWP(2,2) - DEWP(1,2))
     +                          * (MJDTIME - DEWP(1,1))
     +                          / (DEWP(2,1) - DEWP(1,1))
                      IERR = 0
                      RVARB(2,2,1) = 0.0D0
                      GO TO 900
                   END IF
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the earth tides coefficients
C
          IF (fstrcmp(VARNAM,'ETD DATA      ').eq.0) THEN
             RVARB(1,1,1)=D_MATHCNST('ETIDE_LAG')
             RVARB(2,1,1)=D_MATHCNST('LOVE_H')
             RVARB(3,1,1)=D_MATHCNST('LOVE_L')
             GO TO 900
          END IF
C
C  Not used because we provide EOPs externally
C
          IF (fstrcmp(VARNAM,'A1 - TAI      ').eq.0) THEN
             RVARB(1,1,1) = 0.0
             RVARB(2,1,1) = 0.0
             RVARB(3,1,1) = 0.0
             GO TO 900
          END IF
C
C  Called for from the Planetary Ephm. routine, cpepu.f. But
C   apparently not actually used.
C
          IF (fstrcmp(VARNAM,'DPSI          ').eq.0) THEN
             RVARB(1,1,1) = 0.00d0
             RVARB(2,1,1) = 0.00d0
             GO TO 900
          END IF
C
C  Called for from the Planetary Ephm. routine, cpepu.f. But
C   apparently not actually used.
C
          IF (fstrcmp(VARNAM,'DEPS          ').eq.0) THEN
             RVARB(1,1,1) = 0.00d0
             RVARB(2,1,1) = 0.00d0
             GO TO 900
          END IF
C
C  Not called for because cpanm.f is utrned off.
C
          IF (fstrcmp(VARNAM,'ROTEPOCH      ').eq.0) THEN
             IMJDATE = MJD_FTOC(JOBNUM,'MASTER',IROW,'START')
             RVARB(1,1,1) =  MJD2JD(IMJDATE, 0.0D0)
             RVARB(2,1,1) = 0.0D0
             GO TO 900
          END IF
C
C  Get the UT1-UTC reference date and the time interval between 
C   rows.
C
          IF (fstrcmp(VARNAM,'FUT1 INF      ').eq.0) THEN
             RVARB(1,1,1) = MJD_FTOC(JOBNUM,'UTC',IROW,'TIME')
     +                    + 2400000.5D0
             RVARB(2,1,1) = 1.0D0
             RVARB(3,1,1) = DFLOAT(N_ROWS(JOBNUM,'UTC'))
             RVARB(4,1,1) = 1.0D0
             GO TO 900
          END IF
C
C  Get the TAI - UT1 rows.
C
          IF (fstrcmp(VARNAM,'FUT1 PTS      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'UTC')
             IF (NROWS.LE.0) GO TO 900
C
C  Make TAI-UT1 from TAI-UTC and UT1-UTC in job script
             DO I = 1, NROWS
                TAIUTC = D_FTOC(JOBNUM,'UTC',I-1,'TAIUTC')
                UT1UTC = D_FTOC(JOBNUM,'UTC',I-1,'UT1UTC')
                RVARB(I,1,1) = TAIUTC - UT1UTC
             END DO
             GO TO 900
          END IF
C
C  Get the xpole, ypole reference date and the time interval
C   between rows.
C
           IF (fstrcmp(VARNAM,'FWOB INF      ').eq.0) THEN
             RVARB(1,1,1) = MJD_FTOC(JOBNUM,'UTC',IROW,'TIME')
     +                    + 2400000.5D0
             RVARB(2,1,1) = 1.0D0
             RVARB(3,1,1) = DFLOAT(N_ROWS(JOBNUM,'UTC'))
             GO TO 900
          END IF
C
C  Get the polar offset rows.
C
          IF (fstrcmp(VARNAM,'FWOBX&YT      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'UTC')
             IF (NROWS.LE.0) GO TO 900
C
C  CALC expects polar offsets in milliarcsecs
             DO I = 1, NROWS
                RVARB(1,I,1) = 1.0D3*D_FTOC(JOBNUM,'UTC',I-1,'XPOLE')
                RVARB(2,I,1) = 1.0D3*D_FTOC(JOBNUM,'UTC',I-1,'YPOLE')
             END DO
             GO TO 900
          END IF
C
C  Get the barycentric earth position and velocity vectors
C
          IF (fstrcmp(VARNAM,'EARTH CE      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'solar') 
             IF (NROWS.LE.0)
     +          CALL C2WRITE ('ERROR: CALC GET4, NROWS = 0 ',VARNAM)
             IF (NROWS.LE.0) GO TO 900
C
C  Calculate the CALC model time in fractional MJD
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0
     +               +  DFLOAT(GETTAG(5)) / 1440.0) * 86400.0
             DO I = 1, NROWS
                IROW = I - 1
C
C  Calculate the solar table row time in fract. MJD
                MJDROW = I_FTOC(JOBNUM,'solar',IROW,'DATE')
C
C  Advance the earth polynomial to the CALC model time
                IF (MJDROW.EQ.MJDATE) THEN
                  X = D_FTOC(JOBNUM,'solar',IROW,'EARTH_X')
                  Y = D_FTOC(JOBNUM,'solar',IROW,'EARTH_Y')
                  Z = D_FTOC(JOBNUM,'solar',IROW,'EARTH_Z')
                  VX = D_FTOC(JOBNUM,'solar',IROW,'EARTH_VX')
                  VY = D_FTOC(JOBNUM,'solar',IROW,'EARTH_VY')
                  VZ = D_FTOC(JOBNUM,'solar',IROW,'EARTH_VZ')
                  AX = D_FTOC(JOBNUM,'solar',IROW,'EARTH_AX')
                  AY = D_FTOC(JOBNUM,'solar',IROW,'EARTH_AY')
                  AZ = D_FTOC(JOBNUM,'solar',IROW,'EARTH_AZ')
                  RVARB(1,1,1) = X + VX*UTCTIME + AX*UTCTIME*UTCTIME
                  RVARB(2,1,1) = Y + VY*UTCTIME + AY*UTCTIME*UTCTIME
                  RVARB(3,1,1) = Z + VZ*UTCTIME + AZ*UTCTIME*UTCTIME
                  RVARB(1,2,1) = VX + 2.0*AX*UTCTIME
                  RVARB(2,2,1) = VY + 2.0*AY*UTCTIME
                  RVARB(3,2,1) = VZ + 2.0*AZ*UTCTIME
                  RVARB(1,3,1) = AX
                  RVARB(2,3,1) = AY
                  RVARB(3,3,1) = AZ
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the geocentric solar position and velocity vectors
C
          IF (fstrcmp(VARNAM,'SUN DATA      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'solar') 
             IF (NROWS.LE.0)
     +          CALL C2WRITE ('ERROR: CALC GET4, NROWS = 0 ',VARNAM)
             IF (NROWS.LE.0) GO TO 900
C
C  Calculate the time of day in solar seconds
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0
     +               +  DFLOAT(GETTAG(5)) / 1440.0) * 86400.0
             DO I = 1, NROWS
                IROW = I - 1
                MJDROW = I_FTOC(JOBNUM,'solar',IROW,'DATE')
                IF (MJDROW.EQ.MJDATE) THEN
                  X = D_FTOC(JOBNUM,'solar',IROW,'SUN_X')
                  Y = D_FTOC(JOBNUM,'solar',IROW,'SUN_Y')
                  Z = D_FTOC(JOBNUM,'solar',IROW,'SUN_Z')
                  VX = D_FTOC(JOBNUM,'solar',IROW,'SUN_VX')
                  VY = D_FTOC(JOBNUM,'solar',IROW,'SUN_VY')
                  VZ = D_FTOC(JOBNUM,'solar',IROW,'SUN_VZ')
                  RVARB(1,1,1) = X + VX*UTCTIME
                  RVARB(2,1,1) = Y + VY*UTCTIME
                  RVARB(3,1,1) = Z + VZ*UTCTIME
                  RVARB(1,2,1) = VX
                  RVARB(2,2,1) = VY
                  RVARB(3,2,1) = VZ
                END IF
             END DO
             GO TO 900
          END IF
C
C  Get the geocentric moon position and velocity vectors
C
          IF (fstrcmp(VARNAM,'MOONDATA      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'solar') 
             IF (NROWS.LE.0)
     +          CALL C2WRITE ('ERROR: CALC GET4, NROWS = 0 ',VARNAM)
             IF (NROWS.LE.0) GO TO 900
C
C  Calculate the time of day in solar seconds
             UTCTIME = (DFLOAT(GETTAG(4)) / 24.0
     +               +  DFLOAT(GETTAG(5)) / 1440.0) * 86400.0
             DO I = 1, NROWS
                IROW = I - 1
                MJDROW = I_FTOC(JOBNUM,'solar',IROW,'DATE')
                IF (MJDROW.EQ.MJDATE) THEN
                  X = D_FTOC(JOBNUM,'solar',IROW,'MOON_X')
                  Y = D_FTOC(JOBNUM,'solar',IROW,'MOON_Y')
                  Z = D_FTOC(JOBNUM,'solar',IROW,'MOON_Z')
                  VX = D_FTOC(JOBNUM,'solar',IROW,'MOON_VX')
                  VY = D_FTOC(JOBNUM,'solar',IROW,'MOON_VY')
                  VZ = D_FTOC(JOBNUM,'solar',IROW,'MOON_VZ')
                  RVARB(1,1,1) = X + VX*UTCTIME
                  RVARB(2,1,1) = Y + VY*UTCTIME
                  RVARB(3,1,1) = Z + VZ*UTCTIME
                  RVARB(1,2,1) = VX
                  RVARB(2,2,1) = VY
                  RVARB(3,2,1) = VZ
                END IF
             END DO
             GO TO 900
          END IF
C
C  THE STATION LIST IN CALC IS ONE ENTRY LONGER THAN THE STATION 
C  LIST IN THE SCRIPT FILE AND 'STATIONS' DATA TABLE. THE FIRST
C  ENTRY IN THE CALC STATIONS LIST IS STATION A, THE EARTH CENTER.
C  THE STATION INDICES BETWEEN CALC AND THE DATA TABLE ARE DIFFERENT
C  BY +2, ONE FOR THE DIFFERENCE BETWEEN C AND FORTRAN, ONE FOR 
C  THE CENTER EARTH STATION.
C
          IF (fstrcmp(VARNAM,'SITERECS      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station')
             IF (NROWS.LE.0)
     +          CALL C2WRITE ('ERROR: CALC GET4, NROWS = 0 ',VARNAM)
             IF (NROWS.LT.(NDIM2-1).OR.NROWS.LE.0) GO TO 900
             IROW = 0
             RVARB(1,1,1) = 0.0000D0
             RVARB(2,1,1) = 0.0000D0
             RVARB(3,1,1) = 0.0000D0
             DO I = 1, NDIM2
                RVARB(1,I,1) = D_FTOC(JOBNUM,'station',I-1,'X')
                RVARB(2,I,1) = D_FTOC(JOBNUM,'station',I-1,'Y')
                RVARB(3,I,1) = D_FTOC(JOBNUM,'station',I-1,'Z')
             END DO
             GO TO 900
          END IF
C
C  CALC 9.0 receives station x, y, z and station velocity vector
C  and epoch in SITERECV. We send zeros for velocity because the 
C  site positions are already shifted in CJOBGEN.
C
          IF (fstrcmp(VARNAM,'SITERECV      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station')
             IF (NROWS.LE.0)
     +          CALL C2WRITE ('ERROR: CALC GET4, NROWS = 0 ',VARNAM)
             IF (NROWS.LT.(NDIM2-1).OR.NROWS.LE.0) GO TO 900
             IROW = 0
             RVARB(1,1,1) = 0.0000D0
             RVARB(2,1,1) = 0.0000D0
             RVARB(3,1,1) = 0.0000D0
             RVARB(4,1,1) = 0.0000D0
             RVARB(5,1,1) = 0.0000D0
             RVARB(6,1,1) = 0.0000D0
             RVARB(7,1,1) = 0.0000D0
             DO I = 1, NDIM2
                RVARB(1,I,1) = D_FTOC(JOBNUM,'station',I-1,'X')
                RVARB(2,I,1) = D_FTOC(JOBNUM,'station',I-1,'Y')
                RVARB(3,I,1) = D_FTOC(JOBNUM,'station',I-1,'Z')
                RVARB(4,I,1) = 0.0000D0
                RVARB(5,I,1) = 0.0000D0
                RVARB(6,I,1) = 0.0000D0
                RVARB(7,I,1) = 0.0000D0
             END DO
             GO TO 900
          END IF

C
C  Get the antenna axis offset
C
          IF (fstrcmp(VARNAM,'AXISOFFS      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station')
             IF (NROWS.LT.(NDIM1-1).OR.NROWS.LE.0) GO TO 900
             RVARB(1,1,1) = 0.0D0
             DO I = 1, NDIM1
                RVARB(I,1,1) = F_FTOC(JOBNUM,'station',I-1,'AXISOFF_X')
             END DO
             GO TO 900
          END IF
C
C  Return 0.0 for the zenith atmospheric delay at the current site
C
          IF (fstrcmp(VARNAM,'SITEZENS      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station')
             IF (NROWS.LT.(NDIM1-1).OR.NROWS.LE.0) GO TO 900
             RVARB(1,1,1) = 0.0D0
             GO TO 900
          END IF
C
C  Load the Calc sources table with all sources in the correlator
C   job script table
C
          IF (fstrcmp(VARNAM,'STAR2000      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'source')
             IF (NROWS.LT.NDIM2.OR.NROWS.LE.0) GO TO 900
             DO I = 1, NDIM2
                RVARB(1,I,1) = D_FTOC(JOBNUM,'source',I-1,'RA')
                RVARB(2,I,1) = D_FTOC(JOBNUM,'source',I-1,'DEC')
             END DO
             GO TO 900
          END IF
C
C  Ocean loading stuff...
C
C  Vertical amplitude
          IF (fstrcmp(VARNAM,'SITOCAMP      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station') 
             ITYPE = 1
             DO I = 1, NROWS
                IERR = OCE_FETCH (I, ITYPE, RVARB(1,I,1))
             END DO
             GO TO 900
          END IF
C
C  Vertical phase
          IF (fstrcmp(VARNAM,'SITOCPHS      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station') 
             ITYPE = 2
             DO I = 1, NROWS
                IERR = OCE_FETCH (I, ITYPE, RVARB(1,I,1))
             END DO
             GO TO 900
          END IF
C
C  Horizontal amplitude
          IF (fstrcmp(VARNAM,'SITHOCAM      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station') 
             ITYPE = 3
             DO I = 1, NROWS
                IERR = OCE_FETCH (I, ITYPE, RVARB(1,1,I))
             END DO
             GO TO 900
          END IF
C
C  Horizontal phase
          IF (fstrcmp(VARNAM,'SITHOCPH      ').eq.0) THEN
             NROWS = N_ROWS(JOBNUM,'station') 
             ITYPE = 4
             DO I = 1, NROWS
                IERR = OCE_FETCH (I, ITYPE, RVARB(1,1,I))
             END DO
             GO TO 900
          END IF



        END IF
C   
C
 900      IF (RVARB(1,1,1).NE.-99999.0) GO TO 999
          IERR = 1
C          CALL C2WRITE 
C     +    ('ERROR: CALC GET4, PARM = ',VARNAM)
C
 999      CONTINUE
C
C       write (6,*) "GET4",VARNAM,NDIM1,NDIM2,NDIM3,"------------------"
C       do i = 1, NDIM1
C          do j = 1, NDIM2
C             do k = 1, NDIM3  
C                write (6,*) RVARB(i,j,k)
C             enddo
C          enddo
C       enddo
C
          RETURN
          END
C
       SUBROUTINE GETI (VARNAM,IVARB,NDIM1,NDIM2,NDIM3,NDO,IERR)
C--------------------------------------------------------------------------
C 
C      Replaces the GET4 in CALC. Provides the interface between 
C      CALC and the Correlator job script tables.
C
C--------------------------------------------------------------------------
       INTEGER*2  NDIM1,NDIM2,NDIM3,NDO,IERR
C
       INTEGER*2   IVARB(NDIM1,NDIM2,NDIM3)
       INTEGER*4 FSTRCMP,FSTRCPY,N_ROWS,IROW,I,NROWS
C
       CHARACTER*16 C_FTOC
       CHARACTER*14 VARNAM
       CHARACTER*4  KEYNAME
C
       INCLUDE 'CALCIO.INC'
C
C
C  Catch illegal job numbers
       IF (JOBNUM.LE.0.OR.JOBNUM.GE.9999) THEN 
          CALL I1WRITE ('ERROR GETI, JOBNUM = ',JOBNUM)
          IERR = 1
          GO TO 999
       END IF

          IERR = 0
          IROW = 0
          IVARB(1,1,1) = -999
C
          IF (NDIM1.EQ.1) THEN
             IF (fstrcmp(VARNAM,'NDELAY        ').eq.0) 
     +           IVARB(1,1,1) = 1
             IF (fstrcmp(VARNAM,'NRATE         ').eq.0) 
     +           IVARB(1,1,1) = 1
             IF (fstrcmp(VARNAM,'NEPOCH        ').eq.0) 
     +           IVARB(1,1,1) = 1
             IF (fstrcmp(VARNAM,'DELTFLAG      ').eq.0) 
     +           IVARB(1,1,1) = 1
             IF (fstrcmp(VARNAM,'TIDALUT1      ').eq.0) 
     +           IVARB(1,1,1) = 1
             IF (fstrcmp(VARNAM,'EPHEPOCH      ').eq.0) 
     +           IVARB(1,1,1) = 2000
             IF (fstrcmp(VARNAM,'# SITES       ').eq.0) 
     +           IVARB(1,1,1) = N_ROWS(JOBNUM,'station')
             IF (fstrcmp(VARNAM,'# STARS       ').eq.0) 
     +           IVARB(1,1,1) = N_ROWS(JOBNUM,'source')
             IF (fstrcmp(VARNAM,'N SITES       ').eq.0) 
     +           IVARB(1,1,1) = N_ROWS(JOBNUM,'station')
             IF (fstrcmp(VARNAM,'N STARS       ').eq.0) 
     +           IVARB(1,1,1) = N_ROWS(JOBNUM,'source')
             IF (fstrcmp(VARNAM,'STAR ID       ').eq.0) 
     +           IVARB(1,1,1) = GETSRC
             GO TO 900
          END IF
C
C         Changed by Adam Deller to not skip first station on 
C         account of it being geocenter (replace I-2 with I-1)
          IF (NDIM1.GT.1) THEN
             IF (fstrcmp(VARNAM,'AXISTYPS      ').eq.0) THEN
                NROWS = N_ROWS(JOBNUM,'station')
                IF (NROWS.LT.(NDIM1-1).OR.NROWS.LE.0) GO TO 999
                IVARB(1,1,1) = 3
                DO I = 1, NDIM1
                  IERR=fstrcpy(KEYNAME,C_FTOC(JOBNUM,'station',
     +                                      I-1,'AXISTYPE'))
C               Compare with axistypes and pass integer code
                  IVARB(I,1,1) = 3
                  IF (fstrcmp(KEYNAME,'EQUA').eq.0)
     +               IVARB(I,1,1) = 1
                  IF (fstrcmp(KEYNAME,'XYNS').eq.0)
     +               IVARB(I,1,1) = 2
                  IF (fstrcmp(KEYNAME,'XYEW').eq.0)
     +               IVARB(I,1,1) = 4
                END DO
                GO TO 900
             END IF
C
             IF (fstrcmp(VARNAM,'BASELINE      ').EQ.0) THEN
                IVARB(1,1,1) = GETSIT(1)
                IVARB(2,1,1) = GETSIT(2)
                GO TO 900
             END IF
C
             IF (fstrcmp(VARNAM,'UTC TAG       ').eq.0 .or.
     +           fstrcmp(VARNAM,'UTC TAG4      ').eq.0) THEN
                IVARB(1,1,1) = GETTAG(1)
                IVARB(2,1,1) = GETTAG(2)
                IVARB(3,1,1) = GETTAG(3)
                IVARB(4,1,1) = GETTAG(4)
                IVARB(5,1,1) = GETTAG(5)
                GO TO 900
             END IF
          END IF
C
 900     CONTINUE
         IF (IVARB(1,1,1).NE.-999) GO TO 999
         IERR = 1
         CALL C2WRITE ('ERROR: CALC GETI, PARM = ',VARNAM)
C
 999      CONTINUE
C
C
C       write (6,*) "GETI",VARNAM,NDIM1,NDIM2,NDIM3,"------------------"
C       do i = 1, NDIM1
C          do j = 1, NDIM2
C             do k = 1, NDIM3  
C                write (6,*) IVARB(i,j,k)
C             enddo
C          enddo
C       enddo
C
         RETURN
         END
C
       SUBROUTINE GETA (VARNAM,LVARB,NDIM1,NDIM2,NDIM3,NDO,IERR)
C--------------------------------------------------------------------------
C 
C      Replaces the GET4 in CALC. Provides the interface between 
C      CALC and the Correlator job script tables.
C
C--------------------------------------------------------------------------
       INTEGER*2  NDIM1,NDIM2,NDIM3,NDO,IERR,LVARB(4,20)
       INTEGER*2  IARRAY(8)
C
       INTEGER*4 N_ROWS,FSTRCMP,FSTRCPY,I,J,NROWS
C
       CHARACTER*2  KEY2(8)
       CHARACTER*14 VARNAM
       CHARACTER*16 C_FTOC, KEY16
C
       EQUIVALENCE (IARRAY(1),KEY2(1),KEY16)
C
       include 'CALCIO.INC'
C
       IERR = 0
       DO J = 1, 8
           IARRAY(J) = 2048
       END DO
       DO I = 1, 4
          DO J = 1, 10
             LVARB(I,J) = 0
          END DO
       END DO
C
       LVARB(1,1) = -999
C
C  Catch illegal job numbers
       IF (JOBNUM.LE.0.OR.JOBNUM.GE.9999) THEN 
          CALL I1WRITE ('ERROR GETA, JOBNUM = ',JOBNUM)
          IERR = 1
          GO TO 999
       END IF
C
        IF (fstrcmp(VARNAM,'SITNAMES      ').eq.0) THEN
C Add one row because the Calc internal stations table has
C  an extra entry for the geocentric reference station
          NROWS = N_ROWS(JOBNUM,'station') + 1
          IF (NROWS.LT.NDIM2.OR.NROWS.LE.0) GO TO 900
          IERR =  FSTRCPY (KEY16, 'GEOCENTER')
          DO I = 2, NROWS
             IERR = FSTRCPY(KEY16,C_FTOC(JOBNUM,'station',I-2,'NAME'))
             LVARB(1,I) = IARRAY(1)
             LVARB(2,I) = IARRAY(2)
             LVARB(3,I) = IARRAY(3)
             LVARB(4,I) = IARRAY(4)
C             CALL C2WRITE('KEY16 = ',KEY16)
          END DO
          GO TO 900
        END IF
C
C
        IF (fstrcmp(VARNAM,'STRNAMES      ').eq.0) THEN
          NROWS = N_ROWS(JOBNUM,'source')
          IF (NROWS.LT.NDIM2.OR.NROWS.LE.0) GO TO 900
          DO I = 1, NROWS
             IERR = FSTRCPY(KEY16,C_FTOC(JOBNUM,'source',I-1,'NAME'))
             LVARB(1,I) = IARRAY(1)
             LVARB(2,I) = IARRAY(2)
             LVARB(3,I) = IARRAY(3)
             LVARB(4,I) = IARRAY(4)
C             CALL C2WRITE('KEY16 = ',KEY16)
          END DO
          GO TO 900
        END IF

 900      IF (LVARB(1,1).NE.-999) GO TO 999
          IERR = 1
          CALL C2WRITE (' GETA ERROR, PARM = ',VARNAM)
          CALL I1WRITE (' GETA NROWS       = ', NROWS)
          CALL I1WRITE (' GETA NDIM2       = ', NDIM2)
          RETURN
C
 999      CONTINUE
C
C
C       write (6,*) "GETA",VARNAM,NDIM1,NDIM2,NDIM3,"------------------"
C       do i = 1, NDIM1
C          write (6,*) LVARB(1,i),LVARB(2,i),LVARB(3,i),LVARB(4,i)
C       enddo
C

          RETURN
          END
          SUBROUTINE PUT4 (STR1,STR2,NCHR2,N1,N2)
C--------------------------------------------------------------------------
C
C  Replace PUT4 in CALC. Place input values into COMMON variables
C   in CALCIO.INC
C
C--------------------------------------------------------------------------
          INTEGER*2 NCHR2,N1,N2
          INTEGER*4 FSTRCMP, I, J
          REAL*8  STR2(NCHR2,N1)
          CHARACTER*14 STR1
C
          include 'CALCIO.INC'
C
C          write (6,*) str1, "--------------------------------------"
C          do i = 1, nchr2
C             do j = 1, n1
c                write (6,*) str2(i,j)
C             enddo
c          enddo 
C
C  Eubanks Consensus Total Delay
C
          IF (fstrcmp(STR1,'CONSNDEL      ').eq.0) THEN
             PUTDLY(1) = STR2(1,1)
             PUTDLY(2) = STR2(2,1)
          END IF
C
C  Eubanks Consensus Total Delay Rate
C
          IF (fstrcmp(STR1,'CONSNRAT      ').eq.0) 
     +       PUTRAT    = STR2(1,1)
C
C  Save the total solar system graviational bending
C
          IF (fstrcmp(STR1,'CON CONT      ').eq.0) THEN
             GRAVDLY = STR2(1,1)
             GRAVRAT = STR2(2,1)
C             write (6,*) "gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the total solar system graviational bending - 2nd order solar
C
          IF (fstrcmp(STR1,'DELTATGRAV    ').eq.0) THEN
             DELTGRAV = STR2(1,1)
          END IF
C
C  Save the earth graviational bending 
C
          IF (fstrcmp(STR1,'EARTHGRAV     ').eq.0) THEN
             EARTHGRAV = STR2(1,1)
          END IF
C
C  Save the total solar  graviational bending
C
          IF (fstrcmp(STR1,'SUN CONT      ').eq.0) THEN
             SUN_CNTRB(1) = STR2(1,1)
             SUN_CNTRB(2) = STR2(2,1)
C             write (6,*) "solar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the solar higher order graviational bending
C
          IF (fstrcmp(STR1,'SUN2CONT      ').eq.0) THEN
             SUN2CONT(1) = STR2(1,1)
             SUN2CONT(2) = STR2(2,1)
C             write (6,*) "gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the SSBC vector Sun - station #1
C
          IF (fstrcmp(STR1,'R1SUNT1       ').eq.0) THEN
             R1SUNT1(1) = STR2(1,1)
             R1SUNT1(2) = STR2(2,1)
             R1SUNT1(3) = STR2(3,1)
C             write (6,*) "solar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the SSBC vector Sun - station #2
C
          IF (fstrcmp(STR1,'R2SUNT1       ').eq.0) THEN
             R2SUNT1(1) = STR2(1,1)
             R2SUNT1(2) = STR2(2,1)
             R2SUNT1(3) = STR2(3,1)
C             write (6,*) "solar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save solar grav bending coefficient
C
          IF (fstrcmp(STR1,'C_SUN         ').eq.0) THEN
             C_SUN = STR2(1,1)
C             write (6,*) "solar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the SSBC vector Earth- station #1
C
          IF (fstrcmp(STR1,'R1EARTHT1     ').eq.0) THEN
             R1EARTHT1(1) = STR2(1,1)
             R1EARTHT1(2) = STR2(2,1)
             R1EARTHT1(3) = STR2(3,1)
C             write (6,*) "lunar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the SSBC vector Earth - station #2
C
          IF (fstrcmp(STR1,'R2EARTHT1     ').eq.0) THEN
             R2EARTHT1(1) = STR2(1,1)
             R2EARTHT1(2) = STR2(2,1)
             R2EARTHT1(3) = STR2(3,1)
C             write (6,*) "lunar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save Earth grav bending coefficient
C
          IF (fstrcmp(STR1,'C_EARTH       ').eq.0) THEN
             C_EARTH = STR2(1,1)
          END IF
C
C  Save the SSBC vector Moon- station #1
C
          IF (fstrcmp(STR1,'R1MOONT1      ').eq.0) THEN
             R1MOONT1(1) = STR2(1,1)
             R1MOONT1(2) = STR2(2,1)
             R1MOONT1(3) = STR2(3,1)
C             write (6,*) "lunar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save the SSBC vector Moon - station #2
C
          IF (fstrcmp(STR1,'R2MOONT1      ').eq.0) THEN
             R2MOONT1(1) = STR2(1,1)
             R2MOONT1(2) = STR2(2,1)
             R2MOONT1(3) = STR2(3,1)
C             write (6,*) "lunar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save lunar grav bending coefficient
C
          IF (fstrcmp(STR1,'C_MOON        ').eq.0) THEN
             C_MOON = STR2(1,1)
          END IF
C
C  Save the SSBC vector Planet - station #1
C
          IF (fstrcmp(STR1,'R1PLANT1      ').eq.0) THEN
             DO I = 1, 3
                DO J = 1, 7
                   R1PLANT1(I,J) = STR2(I,J)
                   R1PLANT1(I,J) = STR2(I,J)
                   R1PLANT1(I,J) = STR2(I,J)
               END DO
             END DO
          END IF
C
C  Save the SSBC vector Planet - station #2
C
          IF (fstrcmp(STR1,'R2PLANT1      ').eq.0) THEN
             DO I = 1, 3
                DO J = 1, 7
                   R2PLANT1(I,J) = STR2(I,J)
                   R2PLANT1(I,J) = STR2(I,J)
                   R2PLANT1(I,J) = STR2(I,J)
               END DO
             END DO

C             write (6,*) "lunar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save planet grav bending coefficients
C
          IF (fstrcmp(STR1,'C_PLAN        ').eq.0) THEN
             DO J = 1, 7
                C_PLAN(J) = STR2(J,1)
             END DO
          END IF
C
C  Save normalization term
C
          IF (fstrcmp(STR1,'TERM4         ').eq.0) THEN
             TERM4 = STR2(1,1)
C             write (6,*) "solar gravdly, rat =", STR2(1,1), STR2(2,1)
          END IF
C
C  Save antenna elevation angles
C
          IF (fstrcmp(STR1,'EL-THEO       ').eq.0) THEN
             PUTEL(1,1) = STR2(1,1)
             PUTEL(2,1) = STR2(2,1)
             PUTEL(1,2) = STR2(1,2)
             PUTEL(2,2) = STR2(2,2)
          END IF
C
C  Save antenna azimuth angles
C
          IF (fstrcmp(STR1,'AZ-THEO       ').eq.0) THEN
             PUTAZ(1,1) = STR2(1,1)
             PUTAZ(2,1) = STR2(2,1)
             PUTAZ(1,2) = STR2(1,2)
             PUTAZ(2,2) = STR2(2,2)
          END IF
C
C  Save UT1 - TAI.
C
          IF (fstrcmp(STR1,'UT1 -TAI      ').eq.0) THEN
             OUTUT1 = STR2(1,1)
          END IF
C
C  Save xpole, ypole.
C
          IF (fstrcmp(STR1,'POLAR XY      ').eq.0) THEN
             OUTWBX = STR2(1,1)
             OUTWBY = STR2(2,1)
          END IF
C
C  Save dry atmospheric delay component
C
          IF (fstrcmp(STR1,'NDRYCONT      ').eq.0) THEN
             ATMDLY(1)  = STR2(2,1)
             ATMRATE(1) = STR2(2,2)
          END IF
C
C  Save wet atmospheric component
C
          IF (fstrcmp(STR1,'NWETCONT      ').eq.0) THEN
             ATMDLY(2)  = STR2(2,1)
             ATMRATE(2) = STR2(2,2)
          END IF
C
C  U and V from Calc, units of fringes / arcsec.
C
          IF (fstrcmp(STR1,'UVF/ASEC      ').eq.0) THEN
C             write (6,*) "u,v =", STR2(1,1), STR2(2,1)
          END IF
          RETURN
          END
C
C--------------------------------------------------------------------------
C
C  Dummy functions follow...
C
C--------------------------------------------------------------------------
C
          SUBROUTINE DELR (N1, STR1)
C
C
          INTEGER*2 NCHR2,N1,N2
          CHARACTER*14 STR1
C
C          CALL CNWRITE (STR1, STR2(1), NCHR2)
C
          RETURN
          END
          SUBROUTINE DELA (N1, STR1)
C
C
          INTEGER*2 NCHR2,N1,N2
          CHARACTER*14 STR1
C
C          CALL CNWRITE (STR1, STR2(1), NCHR2)
C
          RETURN
          END
          SUBROUTINE PUTR (STR1, STR2, NCHR2, N1, N2)
C
C         Replace PUTR in CALC. JMB.
C
          INTEGER*2 NCHR2,N1,N2
          REAL*8 STR2(NCHR2,N1)
          CHARACTER*14 STR1
C
C          CALL CNWRITE (STR1, STR2(1), NCHR2)
C
C          write (6,*) "PUTR", str1, "---------------------------------"
C          do i = 1, nchr2
C             do j = 1, n1
C                write (6,*) str2(i,j)
C             enddo
c          enddo
C
          RETURN
          END
          SUBROUTINE PUTA (STR1,STR2,NCHR2,N1,N2)
C
C         Replace PUTA in CALC. JMB.
C
          INTEGER*2 NCHR2,N1,N2
          CHARACTER*1 STR2(2*NCHR2)

          CHARACTER*14 STR1
C
          INCLUDE 'CALCIO.INC'
C
          IF (MSGCTRL.EQ.1) CALL CFWRITE (STR1, STR2(1), 2*NCHR2)
C
          RETURN
          END
          SUBROUTINE PUTI (STR1,STR2,NCHR2,N1,N2)
C
C         Replace PUTI in CALC. JMB.
C
          INTEGER*2 NCHR2,N1,N2
          INTEGER*2 STR2(NCHR2)
          CHARACTER*14 STR1
C
          INCLUDE 'CALCIO.INC'
C
C          CALL I1WRITE (STR1,STR2(1))
C
          RETURN
          END
          SUBROUTINE ADDA (N1,STR1,STR2,NSTR2,N2,N3)
C
          INTEGER*2    N1,N2,N3,NSTR2
          CHARACTER*8  STR1
          CHARACTER*32 STR2
C
          RETURN
          END
          SUBROUTINE ADDR (N1,STR1,STR2,NSTR2,N2,N3)
C
          INTEGER    N1,N2,N3,NSTR2
          CHARACTER*8  STR1
          CHARACTER*32 STR2
C
          IUNIT = 10
C
C          WRITE (IUNIT,1000) STR1,STR2
C 1000     FORMAT (1X,'ADDR: ',A,A)
C
          RETURN
          END
          SUBROUTINE ADDI (N1,STR1,STR2,NSTR2,N2,N3)
C
          INTEGER    N1,N2,N3,NSTR2
          CHARACTER*8  STR1
          CHARACTER*32 STR2
C
          IUNIT = 10
C
C          WRITE (IUNIT,1000) STR1,STR2
C 1000     FORMAT (1X,'ADDI: ',A,A)
C
          RETURN
          END
