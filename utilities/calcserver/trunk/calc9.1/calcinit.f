      SUBROUTINE CALCINIT(JOBN,MJD,KFLAGS,IRETURN)
C-------------------------------------------------------------------
C
C     Calls the initialization portions of CALC
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                                    
      INTEGER*4 JOBN,IRETURN,I4FLGS(60),IWORD,IERR,I_FTOC
      INTEGER*4 MJD, EXTFLAGS(64), LASTFLAGS(64), extmsg1
      INTEGER*2 IFLAG(63), KFLAGS(64), NEWFLAGS
C
      include 'CALCIO.INC'
      include 'ccon.i'
      include 'cobsn.i'
      include 'param.i'
      include 'cuser.i'
      include 'cmxst.i'
C
      Real*8    XCALC
      Integer*2 NFLAG,NFLAGC,loadm(7),LFILE(3),ISECU,IDISC,IOPEN
      Character*2  cval(0:9)
      Character*40 CalcVrsn(2)
      Character*64 Computing_center
C
      EQUIVALENCE (EXTFLAGS(1), KATMC)
      COMMON /STACM/ Computing_center,XCALC,NFLAG,NFLAGC,loadm,LFILE
C
      SAVE LASTFLAGS
C
      DATA  cval /' 0',' 1',' 2',' 3',' 4',' 5',' 6',' 7',' 8',' 9'/
      DATA  CalcVrsn /
     .'CALC Version 9.1 - /home/pecos2/jbenson/',
     .'pgm/calc9.1                             '/
      DATA  KRUC / 2 /    
C                                                    
C--------------------------------------------------------------------
C
C                                       Call start-up and initialization
C                                       subs.
      IRET = 0
      ILUOUT = -1
      Zero_site = 0
C
C
C     Load the pathname for the JPL DE403 Planetary Ephemeris file into
C     Jpl_eph (declared in param.i).
      CALL GETENV ('JPLEPH', JPL_eph)
C
C     Load the control variables Calc_user and Wet_atm from env variables
      CALL GETENV ('CALC_USER', Calc_user)
      IF (Calc_user.ne.'A'.and.Calc_user.ne.'C') Then
         write (6,*) "ERROR : CALC_USER ENV VARIABLE NOT EQ TO A OR C"
         STOP
      ENDIF
      CALL GETENV ('WET_ATM', Wet_atm)
      IF (Wet_atm.ne.'Y'.and.Wet_atm.ne.'N') Then
         write (6,*) "ERROR : WET_ATM ENV Variable NOT EQ TO Y OR N"
         STOP
      ENDIF
C
C     Put the CALC 'PUTA' message control flag into the CALCIO.INC
C     global common. MSGCTRL = 0, do not write the standard CALC startup
C     messages, MSGCTRL = 1, write them into external logfile whose
C     (FILE *) is defined by the C program that is calling 'calcinit'.
      MSGCTRL = IRETURN
      IF (MSGCTRL.EQ.1) THEN
         CALL PUTA ('CALC MESS     ', CalcVrsn, 40, 1, 1) 
         CALL PUTA ('JPL_eph     : ', JPL_eph,  36, 1, 1)
         CALL PUTA ('Calc_user   : ', Calc_user, 1, 1, 1)
         CALL PUTA ('Wet_atm     : ', Wet_atm, 1, 1, 1)
      ENDIF
C   
C     Put job number into the CALCIO.INC global common
      JOBNUM = JOBN
C
C     Put the MJD into the CALCIO.INC global common
      MJDATE = MJD
C
C     Set all of the 'K' control flags to zero
      DO 5 I = 1, 63
         EXTFLAGS(I) = 0
 5    CONTINUE
C
C     Check for externally set flags
C
      IF (KFLAGS(1).NE.-1) THEN
         DO I = 1, 64
            EXTFLAGS(I) = KFLAGS(I)
         END DO
      END IF
C
      IF (KFLAGS(1).EQ.-1) THEN
C     Manually set the non-zero cotrol flags here for the VLBA 
C     Correlator. All switches present in the ccon.i COMMON. Calc9.0
C
C     KATMC = 0 turn off application of atmospheric delay contribution
C     but still calculate the atmopheric delays.
C     KATMC = 1 apply atmospheric delays to CALC total delay
      KATMC = 1
      KAXOC = 0
      KPTDC = 0
      KDNPC = 0
C     Switch off earth tides for speed test, KETDC = 1
      KETDC = 0
      KIONC = 0
C     Nutation model: = 0, new 1996 IERS model,
C                     = 2, old WAHR model
      KNUTC = 0
      KPREC = 0
      KRELC = 0
      KSITC = 0
C     Do not use proper motions, KSTRC = 0
      KSTRC = 0
C      KSTRC = 2
C      KSTRC = 3
      KUT1C = 0
      KWOBC = 0
      KUTCC = 0
      KATIC = 0
      KCTIC = 0
C     Get solar system values from calls to JPL DE403, = 0
C     Get solar system values from calls to GET4, = 1
C     Remember, for KPEPC = 1 to work, cpepumod.f must be substituted
C     for cpepu.f...
      KPEPC = 0
      KDIUC = 0
      KM20C = 0
      KROSC = 0
      KSTEC = 0
      KSUNC = 0
      KSARC = 0
      KTHEC = 0
      KMATC = 0
      KVECC = 0
C     Ocean loading: = 0, calculate but don't apply, = 1, turned off
C                    = 2, calculate and apply to delays
C     IF Calc_user.eq.'C' .and. Apply_ocean.eq.'Y' applies the ocean 
C     loading regardless of the KOCEC swx, except for KOCEC = 1 (off).
      KOCEC = 2
C     Turn on the calculation of U and V. KASTC = 1, on.
C     Calc 9.1 U and V do not use aberrated start position.
      KASTC = 0
      KSTAC = 0
C     Turn parallax module off
      KPLXC = 0
C     Feed horn rotation turned off
      KPANC = 1
C
      KPEPD = 0
C
C     A few of the debugging print out swx's often used
C
      KUT1D = 1
      KUTCD = 0
      KAXOD = 0
      KSITD = 0
      KETDD = 0
      KDIUD = 0
      KTHED = 0
      KOCED = 0
C
      END IF
C
C     Check to see if a flag has changed since last calcinit call.
C
      NEWFLAGS = 0
      DO I = 1, 62
         IF (EXTFLAGS(I).NE.LASTFLAGS(I)) NEWFLAGS = 1
         LASTFLAGS(I) = EXTFLAGS(I)
      END DO
C     
C     Awkard for now..
C
      IF (NEWFLAGS.EQ.1.AND.MSGCTRL.NE.-1) MSGCTRL = 1
C
      IF (MSGCTRL.EQ.1) THEN
         CALL PUTA ('KATMC      :  ', cval(KATMC), 1, 1, 1) 
         CALL PUTA ('KAXOC      :  ', cval(KAXOC), 1, 1, 1) 
         CALL PUTA ('KPTDC      :  ', cval(KPTDC), 1, 1, 1) 
         CALL PUTA ('KDNPC      :  ', cval(KDNPC), 1, 1, 1) 
         CALL PUTA ('KETDC      :  ', cval(KETDC), 1, 1, 1) 
         CALL PUTA ('KIONC      :  ', cval(KIONC), 1, 1, 1) 
         CALL PUTA ('KNUTC      :  ', cval(KNUTC), 1, 1, 1) 
         CALL PUTA ('KPREC      :  ', cval(KPREC), 1, 1, 1) 
         CALL PUTA ('KRELC      :  ', cval(KRELC), 1, 1, 1) 
         CALL PUTA ('KSITC      :  ', cval(KSITC), 1, 1, 1) 
         CALL PUTA ('KSTRC      :  ', cval(KSTRC), 1, 1, 1) 
         CALL PUTA ('KUT1C      :  ', cval(KUT1C), 1, 1, 1) 
         CALL PUTA ('KWOBC      :  ', cval(KWOBC), 1, 1, 1) 
         CALL PUTA ('KUTCC      :  ', cval(KUTCC), 1, 1, 1) 
         CALL PUTA ('KATIC      :  ', cval(KATIC), 1, 1, 1) 
         CALL PUTA ('KCTIC      :  ', cval(KCTIC), 1, 1, 1) 
         CALL PUTA ('KPEPC      :  ', cval(KPEPC), 1, 1, 1) 
         CALL PUTA ('KDIUC      :  ', cval(KDIUC), 1, 1, 1) 
         CALL PUTA ('KM20C      :  ', cval(KM20C), 1, 1, 1) 
         CALL PUTA ('KROSC      :  ', cval(KROSC), 1, 1, 1) 
         CALL PUTA ('KSTEC      :  ', cval(KSTEC), 1, 1, 1) 
         CALL PUTA ('KSUNC      :  ', cval(KSUNC), 1, 1, 1) 
         CALL PUTA ('KSARC      :  ', cval(KSARC), 1, 1, 1) 
         CALL PUTA ('KTHEC      :  ', cval(KTHEC), 1, 1, 1) 
         CALL PUTA ('KMATC      :  ', cval(KMATC), 1, 1, 1) 
         CALL PUTA ('KVECC      :  ', cval(KVECC), 1, 1, 1) 
         CALL PUTA ('KOCEC      :  ', cval(KOCEC), 1, 1, 1) 
         CALL PUTA ('KASTC      :  ', cval(KASTC), 1, 1, 1) 
         CALL PUTA ('KSTAC      :  ', cval(KSTAC), 1, 1, 1) 
         CALL PUTA ('KPLXC      :  ', cval(KPLXC), 1, 1, 1) 
         CALL PUTA ('KPANC      :  ', cval(KPANC), 1, 1, 1) 
      ENDIF

C     The CALC subroutine TOCUP used to call the "A" modules. Do that
C     here only for those that are necessary.
C
      CALL UT1A
      CALL WOBA
      CALL PEPA
C
C     INITL "gets" mathematical, physical, and observing parameters
C     from the job script tables.
C
      CALL INITL (KOUNT)
      IRETURN = IRET
 
      RETURN
      END                                                                     









