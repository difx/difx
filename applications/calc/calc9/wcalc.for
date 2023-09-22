

      SUBROUTINE TBCALC (CHRSIT,CHRSRC,UTCTAG,UTCSEC,DELAY,DRATE)
C-------------------------------------------------------------------
C
C     CALC in a subroutine. 
C
C      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT NONE
                                    
      REAL*8   UTCSEC,DELAY(3),DRATE
      INTEGER*2  UTCTAG(5)
      INTEGER*4  KOUNT, KEND, I, J
      INTEGER*2  SITES(4,2),SOURCE(4) 
      CHARACTER*8 CHRSIT(2),CHRSRC,DUMSIT(2),DUMSRC
      CHARACTER*2  C2SIT(4,2),C2SRC(4)
C
C
      include 'CALCDB.i'
C
      EQUIVALENCE (DUMSIT(1),C2SIT(1,1)),(DUMSRC,C2SRC(1)),
     *           (C2SIT(1,1),SITES(1,1)),(C2SRC(1),SOURCE(1))
     C
      DUMSIT(1) = CHRSIT(1) 
      DUMSIT(2) = CHRSIT(2)
      DUMSRC    = CHRSRC
C
      DO 101 I = 1, 2
      DO 101 J = 1, 4
       GETSIT(J,I) = SITES(J,I)
 101  CONTINUE

      DO 102 I = 1, 4
       GETSRC(I) = SOURCE(I)
 102  CONTINUE

      DO 103 I = 1, 5
       GETTAG(I) = UTCTAG(I)
 103  CONTINUE

      GETSEC = UTCSEC
C                                       Call start-up and initialization
C                                       subs.
      
c      CALL INITL (KOUNT) 
C                                       Calculate delay/rates.
      CALL OBSNT ( KOUNT, KEND ) 
      CALL DRIVR
C
c WEW convert to usecs
c NOTE !!!!!
c  This should not be necessary - output of calc, i.e. SHAPDE, should
c  already be in usec - BUT - because of a bug in subroutine THERY - in
c  converting to usec ( integer part and fractional part ) the result ends
c  up in seconds.
c THIS WILL NEED TO BE MODIFIED WHEN WE GO TO CALC8.1 or GREATER
c
C - For CALC8.0
c      DELAY(1) = SHAPDE(1,1) * 1.0D06
c      DELAY(2) = SHAPDE(2,1) * 1.0D06
c      DELAY(3) = HELLEM(1) * 1.0D06
C - For CALC8.1
c      DELAY(1) = SHAPDE(1,1)
c      DELAY(2) = SHAPDE(2,1)
c      DELAY(3) = HELLEM(1)
c Following still required - SHAPRA in seconds/sec.
c      DRATE    = SHAPRA(1) * 1.0D06

C - For CALC9
      DELAY(1) = CONSNDEL(1)
      DELAY(2) = CONSNDEL(2)
c Following still required - CONSNRAT in seconds/sec.
      DRATE = CONSNRAT(1) * 1.0D06

C
 900  CONTINUE
      RETURN
      END                                                                     

      SUBROUTINE DBCOM (LUN, IERR)
C-----------------------------------------------------------------------
C   Reads DBINT/DBREAL COMMON values for CALC from INFILE.
C
C   Inputs:
C      LUN      I           Logical unit number
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C                              1 => error
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*4   IERR, LUN
      INTEGER*4   NVALS, I, J, K

      INTEGER*2   IVALS(200)

      REAL*8    RVALS(200)
      CHARACTER*8    AVALS(100) 
      CHARACTER*14   KEYWD,OKEYWD
      
      INCLUDE 'CALCDB.i'


*----------------------------------------------------------------------

*  load fairly well defined quantities (hard-wired)

	VLIGHT       = 299792.458d+3    ! v light m/s
	GAUSS        = 0.01720209       ! grav constant kg-m**3/sec**2
	ACCELG       = 9.78031846       ! accel grav at equator m/sec**2
	GMSUN        = 1.32712499d+20   ! sol mass * newt grav m**3/sec**2
C WEW Add GMEARTH for CALC9
	GMEARTH      = 3.986004418d+14  ! ear mass * newt grav m**3/sec**2
	GMMOON       = 4.90279750d+12   ! lun mass * newt grav m**3/sec**2
	TSECAU       = 499.00478        ! au in light secs sec/au
	EARTHR       = 6378145.0        ! eq radius m
	EMSMMS       = 81.3006592       ! earth mass/moon mass
	UGRVCN       = 6.668d-11        ! newt grav cnst m**3/kg-sec**2
	EFLAT        = 0.00335289       ! sqr eccentricity of earth shape
        RELDAT       = 1.0		! Post newtonian expansion parm
	PREDAT       = 5029.0966	! precession constant (asec/century)
	DEPS(1)      = 0.	! nutation in obliquity ead  (pep)
	DEPS(2)      = 0.	! ct time derivative (rad/sec)
	DPSI(1)      = 0.	! nutation in longitude rad (pep)
	DPSI(2)      = 0.	! ct time derivative (rad/sec)

*-------------------------------------------------------------

      IERR = 0 
C                                        Main loop                              
   10 CONTINUE

C                                        Check error status 
      IF (IERR.NE.0) THEN
	STOP 'Error in dbcom 1' 
      ENDIF 
C                                        Save last successful read 
      OKEYWD = KEYWD 

      READ (LUN,1000,ERR=900,END=900) KEYWD
 1000 FORMAT (A)
      IF (KEYWD.EQ.'              ') GO TO 10
C
      BACKSPACE LUN
      NVALS = 1
C                                        parse the various options...
      IF (KEYWD.EQ.'NDELAY') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         NDELAY = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'NRATE') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         NRATE = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'NEPOCH') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         NEPOCH = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'DELTFLAG') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         DELTFL = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'N SITES'.OR.
     *    KEYWD.EQ.'# SITES') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         NSITES = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'AXISTYPS') THEN
         NVALS = NSITES
         CALL READIN (LUN,IVALS,NVALS,IERR)
         DO 100 I = 1, NSITES
 100        AXISTY(I) = IVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'N STARS'.OR.
     *    KEYWD.EQ.'# STARS') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         NSTARS = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'EPHEPOCH') THEN
         CALL READIN (LUN,IVALS,NVALS,IERR)
         EPHEPO = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'TIDALUT1') THEN
	 NVALS = 1 
         CALL READIN (LUN,IVALS,NVALS,IERR)
         TIDALU = IVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'VLIGHT') THEN
         READ (LUN,1010) KEYWD, VLIGHT
         READ (LUN,1010) KEYWD, GAUSS
         READ (LUN,1010) KEYWD, ACCELG
         READ (LUN,1010) KEYWD, GMSUN
         READ (LUN,1010) KEYWD, GMMOON
         READ (LUN,1010) KEYWD, TSECAU
         READ (LUN,1010) KEYWD, EARTHR
         READ (LUN,1010) KEYWD, EMSMMS
         READ (LUN,1010) KEYWD, UGRVCN
         READ (LUN,1010) KEYWD, EFLAT
 1010    FORMAT (A,D16.8)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'ROTEPOCH') THEN
         NVALS = 2 * NEPOCH
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 90 I = 1, NVALS
            J = 2 * (I - 1) 
            ROTEPO(1,I) = RVALS(J+1)
 90         ROTEPO(2,I) = RVALS(J+2)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'TAI- UTC') THEN
         NVALS = 3
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         TAIUTC(1) = RVALS(1)
         TAIUTC(2) = RVALS(2)
         TAIUTC(3) = RVALS(3)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'COR DATA') THEN
         NVALS = 2
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         CORDAT(1) = RVALS(1)
         CORDAT(2) = RVALS(2)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'REF FREQ') THEN
         NVALS = NDELAY
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 110 I = 1, NVALS
 110        REFFRE(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'A1 - TAI') THEN
         NVALS = 3
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 130 I = 1, NVALS
 130        A1TAI(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'DNP DATA') THEN
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DNPDAT = RVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'ETD DATA') THEN
         NVALS = 3
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 140 I = 1, NVALS
 140        ETDDAT(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'EARTH CE') THEN
         NVALS = 9
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 150 I = 1, NVALS
 150        EARTHC(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SUN DATA') THEN
         NVALS = 6
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 160 I = 1, NVALS
 160        SUNDAT(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'MOONDATA') THEN
         NVALS = 6
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 170 I = 1, NVALS
 170        MOONDA(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'DPSI') THEN
         NVALS = 2
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 180 I = 1, NVALS
 180        DPSI(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'DEPS') THEN
         NVALS = 2
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 190 I = 1, NVALS
 190        DEPS(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'PRE DATA') THEN
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         PREDAT = RVALS(1)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'REL DATA') THEN
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         RELDAT = RVALS(1) 
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITNAMES') THEN
         NVALS = NSITES
         CALL READA8 (LUN,AVALS,NVALS,IERR)
         DO 200 I = 1, NVALS
 200        SITNAM(I) = AVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'AXISOFFS') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 210 I = 1, NVALS 
 210        AXISOF(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITERECS') THEN
         NVALS = NSITES * 3
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 220 I = 1, NSITES
            DO 220 J = 1, 3
               K = 3 * (I - 1) + J
 220           SITERE(J,I) = RVALS(K)
         GO TO 10
      END IF
C Ocean loading not really implemented yet or ever?
      IF (KEYWD.EQ.'SITHOCAM') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 230 I = 1, NVALS
C230        SITHOA(I) = RVALS(I)
 230        CONTINUE
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITHOCPH') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 232 I = 1, NVALS
C232        SITHOP(I) = RVALS(I)
 232        CONTINUE
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITOCAMP') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 234 I = 1, NVALS
C234        SITOCA(I) = RVALS(I)
 234        CONTINUE
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITOCPHS') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 236 I = 1, NVALS
C236        SITOCP(I) = RVALS(I)
 236        CONTINUE
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'SITEZENS') THEN
         NVALS = NSITES
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 238 I = 1, NVALS
 238        SITEZE(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'STRNAMES') THEN
         NVALS = NSTARS
         CALL READA8 (LUN,AVALS,NVALS,IERR)
         DO 240 I = 1, NVALS
 240        STRNAM(I) = AVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'STAR2000') THEN
         NVALS = 2 * NSTARS
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 250 I = 1, NSTARS
            J = 2 * (I - 1) 
            STAR20(1,I) = RVALS(J+1)
 250        STAR20(2,I) = RVALS(J+2)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'FUT1 INF') THEN
         NVALS = 4
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 260 I = 1, NVALS
 260        FUT1IN(I) = RVALS(I)
         NUT1 = RVALS(3)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'FUT1 PTS') THEN
         NVALS = NUT1
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 270 I = 1, NVALS
 270        FUT1PT(I) = RVALS(I)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'FWOB INF') THEN
         NVALS = 3
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 280 I = 1, NVALS
 280        FWOBIN(I) = RVALS(I)
         NWOB = RVALS(3)
         GO TO 10
      END IF
C
      IF (KEYWD.EQ.'FWOBX&YT') THEN
         NVALS = 2 * NWOB
         CALL READR8 (LUN,RVALS,NVALS,IERR)
         DO 290 I = 1, NVALS
            J = 2 * (I - 1) 
            FWOBXY(1,I) = RVALS(J+1)
 290        FWOBXY(2,I) = RVALS(J+2)
         GO TO 10
      END IF
C                                           Unknown/unimplemented data  
      IF (KEYWD.NE.'              ') THEN
C        IERR = 1 
         WRITE (6, '( a ,a )') ' CALC.DB: Undefined keyword = ',KEYWD
         READ (LUN,1000) KEYWD
         GO TO 10
      END IF
C
     
 900  CONTINUE
 999  CONTINUE
      
      RETURN
      END
      SUBROUTINE READIN (LUN,IVALS,NVALS,IERR)
C-----------------------------------------------------------------------
C   Reads list of integer values from CALC input file record.
C
C   Inputs:
C      LUN      I*4           Logical unit number
C      NVALS    I*4           Number of reals to be read
C   Outputs:
C      IVALS    I*2(100)      Array of integers read from input record
C      IERR     I*4           Error return code:
C                              0 => no error
C                              1 => error
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*4    LUN,IERR
      INTEGER*4    NVALS, I

      INTEGER*2    IVALS(100)

      CHARACTER*14 KEYNAM
C
      IERR = 0
      IF (NVALS.LE.0) THEN
         IERR = 0
         RETURN
      END IF
C
      DO 100 I = 1, NVALS
         READ (LUN,1000,END=900,ERR=900) KEYNAM,IVALS(I)    
 1000    FORMAT (A,I6)
 100  CONTINUE
      GO TO 999
C
 900  IERR = 1
 999  CONTINUE
      RETURN
      END      
      SUBROUTINE READR8 (LUN, RVALS, NVALS, IERR)
C-----------------------------------------------------------------------
C   Reads list of real values from CALC input file record.
C
C   Inputs:
C      LUN      I*4           Logical unit number
C      NVALS    I*4           Number of reals to be read
C   Outputs:
C      RVALS    R*8(200)    Array of reals read from input record
C      IERR     I*4           Error return code:
C                              0 => no error
C                              1 => error
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*4  LUN,IERR
      INTEGER*4  NVALS,I
      REAL*8     RVALS(200)
      CHARACTER*14 KEYNAM
C
      IERR = 0
      IF (NVALS.LE.0) THEN
         IERR = 0
         RETURN
      END IF
C
      DO 100 I = 1, NVALS
         READ (LUN,1000,END=900,ERR=900) KEYNAM,RVALS(I)    
 1000    FORMAT (A,D16.8)
 100  CONTINUE
      GO TO 999
C
 900  IERR = 1
 999  CONTINUE
      RETURN
      END      
      SUBROUTINE READA8 (LUN,AVALS,NVALS,IERR)
C-----------------------------------------------------------------------
C   Reads list of characters strings from CALC input file record.
C
C   Inputs:
C      LUN      I*4           Logical unit number
C      NVALS    I*4           Number of strings to be read
C   Outputs:
C      AVALS    C*8(100)    Array of strings read from input record
C      IERR     I*4           Error return code:
C                              0 => no error
C                              1 => error
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*4     LUN,IERR
      INTEGER*4     NVALS,I
C
      CHARACTER*8   AVALS(100)
      CHARACTER*14  KEYNAM      
C
      IERR = 0
      IF (NVALS.LE.0) THEN
         IERR = 1
         RETURN
      END IF
C
      DO 100 I = 1, NVALS
         READ (LUN,1000,END=900,ERR=900) KEYNAM,AVALS(I)
 1000    FORMAT (A,A)
 100  CONTINUE
      GO TO 999
C
 900  IERR = 1
 999  CONTINUE
      RETURN
      END

      SUBROUTINE DBFLAG (LUN, IFLAG, IERR)
C-----------------------------------------------------------------------
C   Reads control flags for CALC from INFILE.
C
C   Inputs:
C      LUN      I*4           Logical unit number
C      IFLAG   I*4(62)        Flag integers
C   Outputs:
C      IERR    I*4            Error return code:
C                              0 => no error
C                              1 => error
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER*4   IERR, LUN
      INTEGER*4   IFLAG(62)
      integer*4   I, I2, K 
      CHARACTER*80   LINE 
     

      IERR = 0 
C                                       Initialize flags..
C                                       62 flags in current CALC version 
      DO 400  I = 1, 62                                                       
           IFLAG(I) = 0                                                       
  400 CONTINUE                                                                
C
      I2 = -7
      DO 10 I = 1, 7
	    I2 = I2 + 8
         READ (LUN,1000,ERR=900,END=900) (IFLAG(K),K=I2,I2+7)
 10   CONTINUE
C
      READ (LUN,1001,ERR=900,END=900) (IFLAG(K),K=57,62)
      GO TO 999
C
 900  IERR = 1

C
 999  CONTINUE

C                                       Read past comment lines
      DO 20 I=1,7
      READ (LUN,1002) LINE
  20  CONTINUE

1000  FORMAT(10I1)
1001  FORMAT(6I1)
1002  FORMAT(A80)
C
      RETURN
      END

C---------------------------------------------------------------------------
C---------------------------------------------------------------------------
C----------------DATABASE FAKING BEGINS HERE--------------------------------
C---------------------------------------------------------------------------
 
C---------------------------------------------------------------------------
          SUBROUTINE PUTr (STR1,ARRAYR,NDIM1,NDIM2,NDIM3)
C
C                                   Replace PUT4 in CALC. Do not compile
C                                   these routines with bound-checking on.
          IMPLICIT NONE

          INCLUDE 'CALCDB.i'
C
          INTEGER*2 NDIM1,NDIM2,NDIM3,I,J,K
          REAL*8  ARRAYR(NDIM1,NDIM2,NDIM3)
          CHARACTER*(*) STR1

	  entry put4 (str1, arrayr, ndim1, ndim2, ndim3)

C                                   Flesh out STR1 for testing, some of the
C                                   CALC calling routines are sloppy. 
C
          IF (index (STR1, 'SHAP DEL') .gt. 0) THEN
             SHAPDE(1,1) = ARRAYR(1,1,1)
             SHAPDE(2,1) = ARRAYR(2,1,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'SHAP RAT') .gt. 0) THEN
             SHAPRA(1) = ARRAYR(1,1,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'HELL DEL') .gt. 0) THEN
             HELLIN(1,1) = ARRAYR(1,1,1)
             HELLIN(2,1) = ARRAYR(2,1,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'HELL RAT') .gt. 0) THEN
             HELLRA(1) = ARRAYR(1,1,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'HELL EMS') .gt. 0) THEN
             HELLEM(1) = ARRAYR(1,1,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'EL-THEO') .gt. 0) THEN
             ELTHEO(1,1) = ARRAYR(1,1,1)
             ELTHEO(1,2) = ARRAYR(1,2,1)
             ELTHEO(2,1) = ARRAYR(2,1,1)
             ELTHEO(2,2) = ARRAYR(2,2,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'AZ-THEO') .gt. 0) THEN
             AZTHEO(1,1) = ARRAYR(1,1,1)
             AZTHEO(1,2) = ARRAYR(1,2,1)
             AZTHEO(2,1) = ARRAYR(2,1,1)
             AZTHEO(2,2) = ARRAYR(2,2,1)
             GO TO 999
          END IF
C
          IF (index (STR1,'ETD PART') .gt. 0) THEN
             ETDPAR(1,1,1) = ARRAYR(1,1,1)
             ETDPAR(1,1,2) = ARRAYR(1,1,2)
             ETDPAR(1,2,1) = ARRAYR(1,2,1)
             ETDPAR(1,2,2) = ARRAYR(1,2,2)
             ETDPAR(2,1,1) = ARRAYR(2,1,1)
             ETDPAR(2,1,2) = ARRAYR(2,1,2)
             ETDPAR(2,2,1) = ARRAYR(2,2,1)
             ETDPAR(2,2,2) = ARRAYR(2,2,2)
             ETDPAR(3,1,1) = ARRAYR(3,1,1)
             ETDPAR(3,1,2) = ARRAYR(3,1,2)
             ETDPAR(3,2,1) = ARRAYR(3,2,1)
             ETDPAR(3,2,2) = ARRAYR(3,2,2)
             GO TO 999
	  ENDIF 
C
          IF (index (STR1,'OCE DELD') .gt. 0) THEN
             OCEDEL(1,1,1) = ARRAYR(1,1,1)
             OCEDEL(1,1,2) = ARRAYR(1,1,2)
             OCEDEL(1,2,1) = ARRAYR(1,2,1)
             OCEDEL(1,2,2) = ARRAYR(1,2,2)
             OCEDEL(2,1,1) = ARRAYR(2,1,1)
             OCEDEL(2,1,2) = ARRAYR(2,1,2)
             OCEDEL(2,2,1) = ARRAYR(2,2,1)
             OCEDEL(2,2,2) = ARRAYR(2,2,2)
             OCEDEL(3,1,1) = ARRAYR(3,1,1)
             OCEDEL(3,1,2) = ARRAYR(3,1,2)
             OCEDEL(3,2,1) = ARRAYR(3,2,1)
             OCEDEL(3,2,2) = ARRAYR(3,2,2)
             GO TO 999
	     ENDIF 
C
          IF (index (STR1,'NT1 PART') .gt. 0) THEN
             NT1PAR(1,1,1) = ARRAYR(1,1,1)
             NT1PAR(1,1,2) = ARRAYR(1,1,2)
             NT1PAR(1,2,1) = ARRAYR(1,2,1)
             NT1PAR(1,2,2) = ARRAYR(1,2,2)
             NT1PAR(2,1,1) = ARRAYR(2,1,1)
             NT1PAR(2,1,2) = ARRAYR(2,1,2)
             NT1PAR(2,2,1) = ARRAYR(2,2,1)
             NT1PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'NT2 PART') .gt. 0) THEN
             NT2PAR(1,1,1) = ARRAYR(1,1,1)
             NT2PAR(1,1,2) = ARRAYR(1,1,2)
             NT2PAR(1,2,1) = ARRAYR(1,2,1)
             NT2PAR(1,2,2) = ARRAYR(1,2,2)
             NT2PAR(2,1,1) = ARRAYR(2,1,1)
             NT2PAR(2,1,2) = ARRAYR(2,1,2)
             NT2PAR(2,2,1) = ARRAYR(2,2,1)
             NT2PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'NT3 PART') .gt. 0) THEN
             NT3PAR(1,1,1) = ARRAYR(1,1,1)
             NT3PAR(1,1,2) = ARRAYR(1,1,2)
             NT3PAR(1,2,1) = ARRAYR(1,2,1)
             NT3PAR(1,2,2) = ARRAYR(1,2,2)
             NT3PAR(2,1,1) = ARRAYR(2,1,1)
             NT3PAR(2,1,2) = ARRAYR(2,1,2)
             NT3PAR(2,2,1) = ARRAYR(2,2,1)
             NT3PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'NT4 PART') .gt. 0) THEN
             NT4PAR(1,1,1) = ARRAYR(1,1,1)
             NT4PAR(1,1,2) = ARRAYR(1,1,2)
             NT4PAR(1,2,1) = ARRAYR(1,2,1)
             NT4PAR(1,2,2) = ARRAYR(1,2,2)
             NT4PAR(2,1,1) = ARRAYR(2,1,1)
             NT4PAR(2,1,2) = ARRAYR(2,1,2)
             NT4PAR(2,2,1) = ARRAYR(2,2,1)
             NT4PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'NT5 PART') .gt. 0) THEN
             NT5PAR(1,1,1) = ARRAYR(1,1,1)
             NT5PAR(1,1,2) = ARRAYR(1,1,2)
             NT5PAR(1,2,1) = ARRAYR(1,2,1)
             NT5PAR(1,2,2) = ARRAYR(1,2,2)
             NT5PAR(2,1,1) = ARRAYR(2,1,1)
             NT5PAR(2,1,2) = ARRAYR(2,1,2)
             NT5PAR(2,2,1) = ARRAYR(2,2,1)
             NT5PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'NT6 PART') .gt. 0) THEN
             NT6PAR(1,1,1) = ARRAYR(1,1,1)
             NT6PAR(1,1,2) = ARRAYR(1,1,2)
             NT6PAR(1,2,1) = ARRAYR(1,2,1)
             NT6PAR(1,2,2) = ARRAYR(1,2,2)
             NT6PAR(2,1,1) = ARRAYR(2,1,1)
             NT6PAR(2,1,2) = ARRAYR(2,1,2)
             NT6PAR(2,2,1) = ARRAYR(2,2,1)
             NT6PAR(2,2,2) = ARRAYR(2,2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'SIT PART') .gt. 0) THEN
             SITPAR(1,1,1) = ARRAYR(1,1,1)
             SITPAR(1,1,2) = ARRAYR(1,1,2)
             SITPAR(1,2,1) = ARRAYR(1,2,1)
             SITPAR(1,2,2) = ARRAYR(1,2,2)
             SITPAR(2,1,1) = ARRAYR(2,1,1)
             SITPAR(2,1,2) = ARRAYR(2,1,2)
             SITPAR(2,2,1) = ARRAYR(2,2,1)
             SITPAR(2,2,2) = ARRAYR(2,2,2)
             SITPAR(3,1,1) = ARRAYR(3,1,1)
             SITPAR(3,1,2) = ARRAYR(3,1,2)
             SITPAR(3,2,1) = ARRAYR(3,2,1)
             SITPAR(3,2,2) = ARRAYR(3,2,2)
             GO TO 999
          END IF
C        
	  IF (index (STR1,'UT1EPOCH') .gt. 0) THEN
	     IF (NEPOCH.LT.1.OR.NEPOCH.GT.20) THEN
 	WRITE (6,*) 'PUT4: Invalid NEPOCH in UT1EPOCH = ', NEPOCH
                 GOTO 999
             ENDIF
	     DO 120 I=1,2
		 DO 125 J=1,NEPOCH
		     UT1EPO(I,J) = ARRAYR(I,J,1)
  125            CONTINUE
  120        CONTINUE 
	     GOTO 999
	  ENDIF 
C        
	  IF (index (STR1,'WOBEPOCH') .gt. 0) THEN
	     IF (NEPOCH.LT.1.OR.NEPOCH.GT.20) THEN
 	WRITE (6,*) 'PUT4: Invalid NEPOCH in WOBEPOCH = ', NEPOCH
                 GOTO 999
             ENDIF
	     DO 130 I=1,2
		 DO 135 J=1,NEPOCH
		     WOBEPO(I,J) = ARRAYR(I,J,1)
  135            CONTINUE
  130        CONTINUE 
	     GOTO 999
	  ENDIF 
C  
          IF (index (STR1,'CALC VER') .gt. 0) THEN
	     CALCVE = ARRAYR(1,1,1)
	     GOTO 999 
	  ENDIF
C
	  IF (index (STR1,'CT SITE1') .gt. 0) THEN
	     CTSITE = ARRAYR(1,1,1) 
	     GOTO 999 
	  ENDIF
C
	  IF (index (STR1,'UT1 -TAI') .gt. 0) THEN
	     UT1TAI = ARRAYR(1,1,1) 
	     GOTO 999 
	  ENDIF
C
	  IF (index (STR1,'POLAR XY') .gt. 0) THEN
	     POLARX(1) = ARRAYR(1,1,1)  
	     POLARX(2) = ARRAYR(2,1,1) 
	     GOTO 999 
	  ENDIF
C
	  IF (index (STR1,'PARANGLE') .gt. 0) THEN 
	     PARANG(1) = ARRAYR(1,1,1)
	     PARANG(2) = ARRAYR(2,1,1) 
	     GOTO 999 
	  ENDIF 
C         
	  IF (index (STR1,'FEED.COR') .gt. 0) THEN 
	     FEEDCO(1,1) = ARRAYR(1,1,1)
	     FEEDCO(1,2) = ARRAYR(1,2,1)
	     FEEDCO(2,1) = ARRAYR(2,1,1)
	     FEEDCO(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C         
	  IF (index (STR1,'ATM PART') .gt. 0) THEN
	     ATMPAR(1,1) = ARRAYR(1,1,1)
	     ATMPAR(1,2) = ARRAYR(1,2,1)
	     ATMPAR(2,1) = ARRAYR(2,1,1)
	     ATMPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'WET PART') .gt. 0) THEN
	     WETPAR(1,1) = ARRAYR(1,1,1)
	     WETPAR(1,2) = ARRAYR(1,2,1)
	     WETPAR(2,1) = ARRAYR(2,1,1)
	     WETPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'AXO PART') .gt. 0) THEN
	     AXOPAR(1,1) = ARRAYR(1,1,1)
	     AXOPAR(1,2) = ARRAYR(1,2,1)
	     AXOPAR(2,1) = ARRAYR(2,1,1)
	     AXOPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NUT PART') .gt. 0) THEN
	     NUTPAR(1,1) = ARRAYR(1,1,1)
	     NUTPAR(1,2) = ARRAYR(1,2,1)
	     NUTPAR(2,1) = ARRAYR(2,1,1)
	     NUTPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT1 AMPS') .gt. 0) THEN
	     NT1AMP(1,1) = ARRAYR(1,1,1)
	     NT1AMP(1,2) = ARRAYR(1,2,1)
	     NT1AMP(2,1) = ARRAYR(2,1,1)
	     NT1AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT2 AMPS') .gt. 0) THEN
	     NT2AMP(1,1) = ARRAYR(1,1,1)
	     NT2AMP(1,2) = ARRAYR(1,2,1)
	     NT2AMP(2,1) = ARRAYR(2,1,1)
	     NT2AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT3 AMPS') .gt. 0) THEN
	     NT3AMP(1,1) = ARRAYR(1,1,1)
	     NT3AMP(1,2) = ARRAYR(1,2,1)
	     NT3AMP(2,1) = ARRAYR(2,1,1)
	     NT3AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT4 AMPS') .gt. 0) THEN
	     NT4AMP(1,1) = ARRAYR(1,1,1)
	     NT4AMP(1,2) = ARRAYR(1,2,1)
	     NT4AMP(2,1) = ARRAYR(2,1,1)
	     NT4AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT5 AMPS') .gt. 0) THEN
	     NT5AMP(1,1) = ARRAYR(1,1,1)
	     NT5AMP(1,2) = ARRAYR(1,2,1)
	     NT5AMP(2,1) = ARRAYR(2,1,1)
	     NT5AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'NT6 AMPS') .gt. 0) THEN
	     NT6AMP(1,1) = ARRAYR(1,1,1)
	     NT6AMP(1,2) = ARRAYR(1,2,1)
	     NT6AMP(2,1) = ARRAYR(2,1,1)
	     NT6AMP(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'PRE PART') .gt. 0) THEN 
	     PREPAR(1) = ARRAYR(1,1,1)
	     PREPAR(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'STR PART') .gt. 0) THEN
	     STRPAR(1,1) = ARRAYR(1,1,1)
	     STRPAR(1,2) = ARRAYR(1,2,1)
	     STRPAR(2,1) = ARRAYR(2,1,1)
	     STRPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'UT1 PART') .gt. 0) THEN
	     UT1PAR(1,1) = ARRAYR(1,1,1)
	     UT1PAR(1,2) = ARRAYR(1,2,1)
	     UT1PAR(2,1) = ARRAYR(2,1,1)
	     UT1PAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
	  IF (index (STR1,'WOB PART') .gt. 0) THEN
	     WOBPAR(1,1) = ARRAYR(1,1,1)
	     WOBPAR(1,2) = ARRAYR(1,2,1)
	     WOBPAR(2,1) = ARRAYR(2,1,1)
	     WOBPAR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'PLX PART') .gt. 0) THEN 
	     PLXPAR(1) = ARRAYR(1,1,1)
	     PLXPAR(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'ATM CONT') .gt. 0) THEN
	     ATMCON(1,1) = ARRAYR(1,1,1)
	     ATMCON(1,2) = ARRAYR(1,2,1)
	     ATMCON(2,1) = ARRAYR(2,1,1)
	     ATMCON(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'AXO CONT') .gt. 0) THEN
	     AXOCON(1,1) = ARRAYR(1,1,1)
	     AXOCON(1,2) = ARRAYR(1,2,1)
	     AXOCON(2,1) = ARRAYR(2,1,1)
	     AXOCON(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'ETD CONT') .gt. 0) THEN
	     ETDCON(1) = ARRAYR(1,1,1)
	     ETDCON(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'PTD CONT') .gt. 0) THEN
	     PTDCON(1) = ARRAYR(1,1,1)
	     PTDCON(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'OCE CONT') .gt. 0) THEN
	     OCECON(1) = ARRAYR(1,1,1)
	     OCECON(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'OCE HORZ') .gt. 0) THEN
	     OCEHOR(1,1) = ARRAYR(1,1,1)
	     OCEHOR(1,2) = ARRAYR(1,2,1)
	     OCEHOR(2,1) = ARRAYR(2,1,1)
	     OCEHOR(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'OCE VERT') .gt. 0) THEN
	     OCEVER(1,1) = ARRAYR(1,1,1)
	     OCEVER(1,2) = ARRAYR(1,2,1)
	     OCEVER(2,1) = ARRAYR(2,1,1)
	     OCEVER(2,2) = ARRAYR(2,2,1) 
	     GOTO 999
	  ENDIF
C

c Modify for CLAC8.1 - WEW - 27/2/96
c          IF (index (STR1,'WOB CONT') .gt. 0) THEN 
c	     WOBCON(1,1) = ARRAYR(1,1,1)
c	     WOBCON(1,2) = ARRAYR(1,2,1)
c	     WOBCON(2,1) = ARRAYR(2,1,1)
c	     WOBCON(2,2) = ARRAYR(2,2,1) 
c	     GOTO 999
c	  ENDIF
c
          IF (index (STR1,'WOBXCONT') .gt. 0) THEN 
	     WOBXCON(1) = ARRAYR(1,1,1)
	     WOBXCON(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'WOBYCONT') .gt. 0) THEN 
	     WOBYCON(1) = ARRAYR(1,1,1)
	     WOBYCON(2) = ARRAYR(2,1,1)
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'REL CONT') .gt. 0) THEN
	     RELCON(1) = ARRAYR(1,1,1)
	     RELCON(2) = ARRAYR(2,1,1)
             GOTO 999
	  ENDIF
C 
          IF (index (STR1,'REL PART') .gt. 0) THEN
	     RELPAR(1) = ARRAYR(1,1,1)
	     RELPAR(2) = ARRAYR(2,1,1)
             GOTO 999
	  ENDIF
C 
          IF (index (STR1,'NUT WAHR') .gt. 0) THEN
	     NUTWAH(1,1) = ARRAYR(1,1,1)
	     NUTWAH(2,1) = ARRAYR(2,1,1)
	     NUTWAH(1,2) = ARRAYR(1,2,1)
	     NUTWAH(2,2) = ARRAYR(2,2,1)
             GOTO 999
	  ENDIF
C 
          IF (index (STR1,'SHAP T62') .gt. 0) THEN
	     SHAPT6 = ARRAYR(1,1,1) 
	     GOTO 999
	  ENDIF

          IF (index (STR1,'EQE DIFF') .gt. 0) THEN
	     GASTD = arrayr(1,1,1)
	     goto 999
	  end if

          IF (index (STR1,'EQE CONT') .gt. 0) THEN
	     deqec(1) = arrayr(1,1,1)
	     deqec(2) = arrayr(2,1,1)
	     goto 999
	  end if

          IF (index (STR1,'NDRYPART') .gt. 0) THEN
	    do j = 1, 2
	     do i = 1, 2
	      NDRYPART(i,j) = arrayr(i,j,1)
	     end do
	    end do
 	     goto 999
	  end if
          IF (index (STR1,'NWETPART') .gt. 0) THEN
	    do j = 1, 2
	     do i = 1, 2
	      NWETPART(i,j) = arrayr(i,j,1)
	     end do
	    end do
 	     goto 999
	  end if
          IF (index (STR1,'NDRYCONT') .gt. 0) THEN
	    do j = 1, 2
	     do i = 1, 2
	      NDRYCONT(i,j) = arrayr(i,j,1)
	     end do
	    end do
 	     goto 999
	  end if
          IF (index (STR1,'NWETCONT') .gt. 0) THEN
	    do j = 1, 2
	     do i = 1, 2
	      NWETCONT(i,j) = arrayr(i,j,1)
	     end do
	    end do
 	     goto 999
	  end if


          IF (index (STR1,'AXO2CONT') .gt. 0) THEN
	     AXO2CONT(1) = arrayr(1,1,1)
	     AXO2CONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'AXIS OLD') .gt. 0) THEN
	     AXIS_OLD(1) = arrayr(1,1,1)
	     AXIS_OLD(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'ETD IERS') .gt. 0) THEN
	     ETD_IERS(1) = arrayr(1,1,1)
	     ETD_IERS(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'ETD3CONT') .gt. 0) THEN
	     ETD3CONT(1) = arrayr(1,1,1)
	     ETD3CONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'ETDKCONT') .gt. 0) THEN
	     ETDKCONT(1) = arrayr(1,1,1)
	     ETDKCONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'ETD2CONT') .gt. 0) THEN
	     ETHLCONT(1) = arrayr(1,1,1)
	     ETHLCONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'CONSNDEL') .gt. 0) THEN
	     CONSNDEL(1) = arrayr(1,1,1)
	     CONSNDEL(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'CONSNRAT') .gt. 0) THEN
	     CONSNRAT(1) = arrayr(1,1,1)
cccc	     CONSNRAT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'CON CONT') .gt. 0) THEN
	     CON_CONT(1) = arrayr(1,1,1)
	     CON_CONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'CON PART') .gt. 0) THEN
	     CON_PART(1) = arrayr(1,1,1)
	     CON_PART(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'CONSCONT') .gt. 0) THEN
	     CONSCONT(1) = arrayr(1,1,1)
	     CONSCONT(2) = arrayr(2,1,1)
	     goto 999
	  end if
          IF (index (STR1,'SHAPCONT') .gt. 0) THEN
	     SHAPCONT(1) = arrayr(1,1,1)
	     SHAPCONT(2) = arrayr(2,1,1)
	     goto 999
	  end if

C WEW - CALC9.12 additions
          IF (index (STR1,'EARTH CE') .gt. 0) THEN
             do j = 1, 3
                do i = 1, 3
                   C9_EARTH_CE(i,j) = arrayr(i,j,1)
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'SUN DATA') .gt. 0) THEN
             do j = 1, 2
                do i = 1, 3
                   C9_SUNDATA(i,j) = arrayr(i,j,1)
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'MOONDATA') .gt. 0) THEN
             do j = 1, 2
                do i = 1, 3
                   C9_MOONDATA(i,j) = arrayr(i,j,1)
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'FUNDARGS') .gt. 0) THEN
             do j = 1, 2
                do i = 1, 5
                   C9_FUNDARGS(i,j) = arrayr(i,j,1)
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'NUT 1996') .gt. 0) THEN
             do j = 1, 2
                do i = 1, 2
                   C9_NUT1996(i,j) = arrayr(i,j,1)
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'CF2J2000') .gt. 0) THEN
             do k = 1, 3
                do j = 1, 3
                   do i = 1, 3
                      C9_CF2J2000(i,j,k) = arrayr(i,j,k)
                   end do
                end do
             end do
             goto 999
	  end if
          IF (index (STR1,'UVF/') .gt. 0) THEN
             do i = 1, 2
                C9_UVF(i) = arrayr(i,1,1)
             end do
             goto 999
	  end if
          IF (index (STR1,'NGRADPAR') .gt. 0) THEN
             do k = 1, 2
                do j = 1, 2
                   do i = 1, 2
                      C9_NGRADPAR(i,j,k) = arrayr(i,j,k)
                   end do
                end do
             end do
             goto 999
	  end if
c Temporary DUMMY entries follow - may need fixing
          IF (index (STR1,'PTDXYPAR') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'PLX1PSEC') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'PTOLDCON') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'WAHRCONT') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'GDNUTCON') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'OLDEQCON') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'SUN CONT') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'CONSPART') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'BENDPART') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'SUN2CONT') .gt. 0) THEN
             goto 999
	  end if
          IF (index (STR1,'SITEXYZS') .gt. 0) THEN
             goto 999
	  end if


C                                       Unknown parameter 
 	  WRITE (6,*) 'PUT4: Error in request to store: ',STR1 

 999      CONTINUE
          RETURN
          END

C-----------------------------------------------------------------------
       SUBROUTINE GET4 (STR1,ARRAYR,NDIM1,NDIM2,NDIM3,NDO,IERR)
C
C                                       Replaces GET4 in CALC. Do not
C                                       compile with bound-checking. 
C
       IMPLICIT NONE

       INTEGER*2  NDIM1, NDIM2, NDIM3, NDO, IERR
       INTEGER*2  I,J,K
       REAL*8   ARRAYR(NDIM1,NDIM2,NDIM3)
       CHARACTER*(*) STR1
C
       INCLUDE 'CALCDB.i'
C

*--------------------------------


          IERR = 0

          IF (index (STR1,'SEC TAG') .gt. 0) THEN
             ARRAYR(1,1,1) = GETSEC
             GO TO 999
          END IF
C
          IF (index (STR1,'VLIGHT') .gt. 0) THEN
	     ARRAYR(1,1,1)=VLIGHT
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'GAUSS') .gt. 0) THEN
	     ARRAYR(1,1,1)=GAUSS
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'ACCELGRV') .gt. 0) THEN
	     ARRAYR(1,1,1)=ACCELG
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'GMSUN') .gt. 0) THEN
	     ARRAYR(1,1,1)=GMSUN
	     GOTO 999
	  ENDIF
C
C WEW - added for CALC9
          IF (index (STR1,'GMEARTH') .gt. 0) THEN
	     ARRAYR(1,1,1)=GMEARTH
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'GMMOON') .gt. 0) THEN
	     ARRAYR(1,1,1)=GMMOON
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'TSEC/AU') .gt. 0) THEN
	     ARRAYR(1,1,1)=TSECAU
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'EARTHRAD') .gt. 0) THEN
	     ARRAYR(1,1,1)=EARTHR
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'EMS/MMS') .gt. 0) THEN
	     ARRAYR(1,1,1)=EMSMMS
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'U-GRV-CN') .gt. 0) THEN
	     ARRAYR(1,1,1)=UGRVCN
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'E-FLAT') .gt. 0) THEN
	     ARRAYR(1,1,1)=EFLAT
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'DNP DATA') .gt. 0) THEN
	     ARRAYR(1,1,1)=DNPDAT
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'PRE DATA') .gt. 0) THEN
	     ARRAYR(1,1,1)=PREDAT
	     GOTO 999
	  ENDIF
C
          IF (index (STR1,'REL DATA') .gt. 0) THEN
	     ARRAYR(1,1,1)=RELDAT
	     GOTO 999
	  ENDIF
C
C
          IF (index (STR1,'TAI- UTC') .gt. 0) THEN
             DO 100 I = 1, 3
 100            ARRAYR(I,1,1) = TAIUTC(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'COR DATA') .gt. 0) THEN
             ARRAYR(1,1,1) = CORDAT(1)
             ARRAYR(2,1,1) = CORDAT(2)
             GO TO 999
          END IF
C
          IF (index (STR1,'ETD DATA') .gt. 0) THEN
             ARRAYR(1,1,1) = ETDDAT(1)
             ARRAYR(2,1,1) = ETDDAT(2)
             ARRAYR(3,1,1) = ETDDAT(3)
             GO TO 999
          END IF
C
          IF (index (STR1,'A1 - TAI') .gt. 0) THEN
             ARRAYR(1,1,1) = A1TAI(1)
             ARRAYR(2,1,1) = A1TAI(2)
             ARRAYR(3,1,1) = A1TAI(3)
             GO TO 999
          END IF
C
          IF (index (STR1,'DPSI') .gt. 0) THEN
             ARRAYR(1,1,1) = DPSI(1)
             ARRAYR(2,1,1) = DPSI(2)
             GO TO 999
          END IF
C
          IF (index (STR1,'DEPS') .gt. 0) THEN
             ARRAYR(1,1,1) = DEPS(1)
             ARRAYR(2,1,1) = DEPS(2)
             GO TO 999
          END IF
C
          IF (index (STR1,'FUT1 INF') .gt. 0) THEN
             DO 110 I = 1, 4
 110            ARRAYR(I,1,1) = FUT1IN(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'FUT1 PTS') .gt. 0) THEN
             DO 120 I = 1, NUT1
 120            ARRAYR(I,1,1) = FUT1PT(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'ROTEPOCH') .gt. 0) THEN
             DO 130 I = 1, NEPOCH
                ARRAYR(1,I,1) = ROTEPO(1,I)
 130            ARRAYR(2,I,1) = ROTEPO(2,I)
             GO TO 999
          END IF
C
          IF (index (STR1,'FWOB INF') .gt. 0) THEN
             DO 140 I = 1, 3
 140            ARRAYR(I,1,1) = FWOBIN(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'FWOBX&YT') .gt. 0) THEN
             DO 150 I = 1, NWOB
                ARRAYR(1,I,1) = FWOBXY(1,I)
 150            ARRAYR(2,I,1) = FWOBXY(2,I)
             GO TO 999
          END IF
C
          IF (index (STR1,'EARTH CE') .gt. 0) THEN
             K = 0
             DO 160 I = 1, 3
                DO 160 J = 1, 3
                   K = K + 1
 160               ARRAYR(J,I,1) = EARTHC(K)
             GO TO 999
          END IF
C
          IF (index (STR1,'SUN DATA') .gt. 0) THEN
             K = 0
             DO 170 I = 1, 2
                DO 170 J = 1, 3
                   K = K + 1
 170               ARRAYR(J,I,1) = SUNDAT(K)
             GO TO 999
          END IF
C
          IF (index (STR1,'MOONDATA') .gt. 0) THEN
             K = 0
             DO 180 I = 1, 2
                DO 180 J = 1, 3
                   K = K + 1
 180               ARRAYR(J,I,1) = MOONDA(K)
             GO TO 999
          END IF
C
          IF (index (STR1,'SITERECS') .gt. 0) THEN
             DO 190 I = 1, NSITES
                DO 190 J = 1, 3
 190               ARRAYR(J,I,1) = SITERE(J,I)
             GO TO 999
          END IF
C
          IF (index (STR1,'AXISOFFS') .gt. 0) THEN
             DO 200 I = 1, NSITES
 200            ARRAYR(I,1,1) = AXISOF(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'SITEZENS') .gt. 0) THEN
             DO 210 I = 1, NSITES
 210            ARRAYR(I,1,1) = SITEZE(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'SITHOCAM') .gt. 0) THEN
             DO 212 I = 1, NSITES
             DO 213 J = 1, 11
                ARRAYR(J,1,I) = SITHOA(J,1,I)
 213            ARRAYR(J,2,I) = SITHOA(J,2,I)
 212            CONTINUE
             GO TO 999
          END IF
C
          IF (index (STR1,'SITHOCPH') .gt. 0) THEN
             DO 214 I = 1, NSITES
             DO 215 J = 1, 11
 215            ARRAYR(J,1,I) = SITHOP(J,1,I)
ccccccccccc 215            ARRAYR(J,I,1) = SITHOP(J,1,I)
 214            CONTINUE
             GO TO 999
          END IF
C
          IF (index (STR1,'SITOCAMP') .gt. 0) THEN
             DO 216 I = 1, NSITES
             DO 217 J = 1, 11
 217            ARRAYR(J,I,1) = SITOCA(J,I)
 216            CONTINUE
             GO TO 999
          END IF
C
          IF (index (STR1,'SITOCPHS') .gt. 0) THEN
             DO 218 I = 1, NSITES
             DO 219 J = 1, 11
 219            ARRAYR(J,I,1) = SITOCP(J,I)
 218            CONTINUE
             GO TO 999
          END IF
C
          IF (index (STR1,'STAR2000') .gt. 0) THEN
             DO 220 I = 1, NSTARS
             ARRAYR(1,I,1) = STAR20(1,I)
 220         ARRAYR(2,I,1) = STAR20(2,I)
             GO TO 999
          END IF
C
          IF (index (STR1,'REF FREQ') .gt. 0) THEN
             DO 230 I = 1, NDELAY
 230            ARRAYR(I,1,1) = REFFRE(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'STR PART......') .gt. 0) THEN
             ARRAYR(1,1,1) = STRPAR(1,1)
             ARRAYR(2,1,1) = STRPAR(1,2)
             ARRAYR(1,2,1) = STRPAR(2,1)
             ARRAYR(2,2,1) = STRPAR(2,2)
             GO TO 999
          END IF
C
          IF (index (STR1,'SUN DATA......') .gt. 0) THEN
             ARRAYR(1,1,1) = -1.0
             ARRAYR(2,1,1) = -1.0
             ARRAYR(3,1,1) = -1.0
             ARRAYR(1,2,1) = -1.0
             ARRAYR(2,2,1) = -1.0
             ARRAYR(3,2,1) = -1.0
             GO TO 999
          END IF
C
C
C WEW  - Additions for CALC9

          IF (index (STR1,'ATM PRES') .gt. 0) THEN
C Return error to get default values
             IERR = 1
             GO TO 999
          END IF
          IF (index (STR1,'TEMP C') .gt. 0) THEN
C Return error to get default values
             IERR = 1
             GO TO 999
          END IF
          IF (index (STR1,'REL.HUM.') .gt. 0) THEN
C Return error to get default values
             IERR = 1
             GO TO 999
          END IF
          IF (index (STR1,'SITERECV') .gt. 0) THEN
C Return error to use old method with keyword SITERECS - no velocities
             IERR = 1
             GO TO 999
          END IF



 990      IERR = 1
          WRITE (6,*) ' GET4:  ERROR SEARCHING FOR PARM = ', STR1
C
 999      CONTINUE
          RETURN
          END

C-----------------------------------------------------------------------------
       SUBROUTINE GETI (STR1,ARRAYI,NDIM1,NDIM2,NDIM3,NDO,IERR)
C
C                                            Replaces the GETI in CALC. Do
C                                            not compile with bound-checking.
C
       IMPLICIT NONE

       INTEGER*2  NDIM1,NDIM2,NDIM3,NDO,IERR
       INTEGER*2  ARRAYI(NDIM1,NDIM2,NDIM3)
	integer*2   I
 
       CHARACTER*(*) STR1
C
       INCLUDE 'CALCDB.i'
C
C
       IERR = 0

          IF (index (STR1,'UTC TAG') .gt. 0) THEN
             DO 5 I = 1, 5
 5              ARRAYI(I,1,1) = GETTAG(I)
             GO TO 999
          END IF
C
          IF (index (STR1,'NDELAY') .gt. 0) THEN
	     ARRAYI(1,1,1) = NDELAY
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'NRATE') .gt. 0) THEN
	     ARRAYI(1,1,1) = NRATE
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'NEPOCH') .gt. 0) THEN
	     ARRAYI(1,1,1) = NEPOCH
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'DELTFLAG') .gt. 0) THEN
	     ARRAYI(1,1,1) = DELTFL
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'EPHEPOCH') .gt. 0) THEN
	     ARRAYI(1,1,1) = EPHEPO
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'# SITES') .gt. 0) THEN
	     ARRAYI(1,1,1) = NSITES
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'# STARS') .gt. 0) THEN
	     ARRAYI(1,1,1) = NSTARS
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'N SITES') .gt. 0) THEN
	     ARRAYI(1,1,1) = NSITES
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'N STARS') .gt. 0) THEN
	     ARRAYI(1,1,1) = NSTARS
	     GOTO 999
	  ENDIF 
C
          IF (index (STR1,'AXISTYPS') .gt. 0) THEN
                DO 100 I = 1, NSITES
 100               ARRAYI(I,1,1) = AXISTY(I)
                GO TO 999
          END IF
C
          IF (index (STR1,'TIDALUT1') .gt. 0) THEN
		ARRAYI(1,1,1) = TIDALU 
                GO TO 999
          END IF
C
 990      IERR = 1
          WRITE (6,*) ' GETI:  ERROR SEARCHING FOR PARM = ',STR1
C
 999      CONTINUE
C
          RETURN
          END
C-------------------------------------------------------------------------

       SUBROUTINE GETA (VARNAM,LVARB,NDIM1,NDIM2,NDIM3,NDO,IERR)
C
C      Replaces the GETA in CALC. 
C
       IMPLICIT NONE

       INTEGER*2   NDIM1,NDIM2,NDIM3,NDO,IERR
	integer*2    I, j
       INTEGER*2    KEYVAL(4), LVARB(4,10)
       CHARACTER*8       TEMP
       CHARACTER*(*) VARNAM
C
       include 'CALCDB.i'
C
       EQUIVALENCE (KEYVAL(1),TEMP)
C
       IERR = 0
       LVARB(1,1) = -999
       IF (index (VARNAM,'BASELINE') .gt. 0) THEN
          DO 5 I = 1, 2
             DO 5 J = 1, 4
 5              LVARB(J,I) = GETSIT(J,I)
          GO TO 999
       END IF
C      
       IF (index (VARNAM,'STAR ID') .gt. 0) THEN
          DO 7 I = 1, 4
 7           LVARB(I,1) = GETSRC(I)
          GO TO 999
       END IF
C
       IF (index (VARNAM,'SITNAMES') .gt. 0) THEN
          DO 100 I = 1, NDIM2
             TEMP = SITNAM(I)
             LVARB(1,I) = KEYVAL(1)
             LVARB(2,I) = KEYVAL(2)
             LVARB(3,I) = KEYVAL(3)
 100         LVARB(4,I) = KEYVAL(4)
          GO TO 999
        END IF
C
        IF (index (VARNAM,'STRNAMES') .gt. 0) THEN
           DO 110 I = 1, NDIM2
              TEMP = STRNAM(I)
              LVARB(1,I) = KEYVAL(1)
              LVARB(2,I) = KEYVAL(2)
              LVARB(3,I) = KEYVAL(3)
 110          LVARB(4,I) = KEYVAL(4)
          GO TO 999
        END IF

 900      IF (LVARB(1,1).NE.-999) GO TO 999
          IERR = 1
          WRITE (6,*) ' GETA ERROR: PARM = ',VARNAM 
 999      CONTINUE
C
          RETURN
          END


      SUBROUTINE PUTA 
      RETURN
      END

      SUBROUTINE PUTI 
      RETURN
      END

      SUBROUTINE ADDA
      RETURN
      END

      SUBROUTINE ADDR 
      RETURN
      END

      SUBROUTINE ADDI
      RETURN
      END

      SUBROUTINE DELA
      RETURN
      END

      SUBROUTINE DELR
      RETURN
      END

      SUBROUTINE CLNDR
      RETURN
      END

      SUBROUTINE FINIS
      RETURN
      END

      SUBROUTINE TIMEGET
      RETURN
      END

      SUBROUTINE MVREC( i, j, k, m )
      IMPLICIT NONE
      integer*2 i, j, k, m
      m = 0
      RETURN
      END

      SUBROUTINE ADD4
      RETURN
      END

      SUBROUTINE WRIDR
      RETURN
      END

      SUBROUTINE ASK (str, i1, i2, iv, i3)

*  dummied to satisfy ut1m initialisation

	implicit none

	include 'CALCDB.i'

	character*(*) str
	integer*2 i1, i2, iv, i3

*----------------------------------------

	if (index (str, 'ROT') .gt. 0) iv = NEPOCH

      RETURN
      END






