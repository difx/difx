      SUBROUTINE THERA
      IMPLICIT None 
C
C     THERA ADDs entries to the table of contents for the Theory routine text
C     message and the theoretical delay and delay rate arrays. Also deletes 
C     obsolete entries of the old (Calc 8.x and earlier) relativity values.
C
C     THERA Program Interface:
C
C     Common blocks used -
       INCLUDE 'ccon.i'
C           Variables 'from':
C             1.  KTHEC  -  The Theory module flow control flag.
C                           (No longer has any function.)
C             2.  KTHED  -  The Theory routine debug output flag.
C             3.  KRELC  -  The relativity module flow control flag.
C                           0     --> Gravitational bending used in Shapiro,
C                                     Hellings, and Consensus models.
C                           NOT 0 --> Gravitational bending not used.
C
C     Database access -
C           Access codes:
C             1. 'THE MESS' - The database access code for the Theory routine
C                             text message.
C             2. 'CONSNDEL' - The database access code for the VLBI delay
C                             using the Eubanks' Consensus model.
C             3. 'CONSNRAT' - The database access code for the VLBI delay 
C                             rate using the Eubanks' Consensus model.
C             4. 'CONSPART' - Database access code for partial derivatives 
C                             of the Consensus model delays and rates with 
C                             respect to Gamma.
C             5. 'CON CONT' - Database access code for the TOTAL relativistic
C                             bending contributions based on the Consensus
C                             model.
C             6. 'SUN CONT' - Database access code for the Sun's relativistic
C                             bending contributions based on the Consensus
C                             model.
C             7. 'BENDPART' - Database access code for partial derivatives 
C                             of the Consensus gravitational bending portion
C                             of the delays and rates with respect to Gamma.
C
C     Subroutine interface -
C             Caller subroutines: TOCUP
C             Called subroutines: ADDA, ADDR, DELR
C
C     Programmer - DALE MARKHAM  01/17/77
C                  PETER DENATALE 07/20/77
C                  CHOPO MA / DAVID GORDON 04/12/84
C                  DAVID GORDON  07/31/84  ('RGD CONT' ADDED)
C                  SAVITA GOEL   06/03/87  (CDS FOR A900)
C                  GREC COOKE AND JIM RYAN 06/06/89 (Mods for Shipiro)
C                  Jim Ryan 89/09/13 Robertson algorithm removed.
C                  Jim Ryan 89/09/25 Hellings delay algorithm added.
C                  Jim Ryan 89/10/08 Relativity module code moved here.
C                  Jim Ryan 89.12.12 UNIX-like database interface
C                           implimented.
C                  Jim Ryan 89:12:14 Additional Shapiro and Helings
C                           information stored in database.
C                  Jim Ryan 90:11:20 Debug statements and some
C                           comments cleaned up.
C                  David Gordon 07/06/93 Access codes CONSNDEL and CONSNRAT
C                               added for Eubank's Consensus relativity model.
C                  David Gordon 12/27/93 Added access code CONSCONT, correction
C                               to convert Hellings to Consensus theoreticals.
C                  David Gordon 01/21/94 Added access code SHAPCONT, correction
C                               to convert Hellings to Shapiro theoreticals.
C                  David Gordon 94/04/18 Convert to Implicit None.
C                  David Gordon 94.10.05 Changed 'REL CFLG' from 31 to 30 words.
C                  David Gordon 96.01.30 Removed 'CON PART' L-code. Added 
C                               'CONSPART' and 'SUN CONT'.
C                  David Gordon 98.08.18 DELR's to delete Shapiro and Hellings
C                               model Lcodes.
C                  David Gordon 98.11.16 ADDR for 'SUN2CONT', higher order
C                               solar bending contributions.
C                  David Gordon 98.12.16 ADDR for 'BENDPART', partials of the
C                               gravitional bending contributions with respect
C                               to Gamma.
C
C   THERA program structure
C
C   Do ADD for THEORY routine text message.
      CALL ADDA (1,'THE MESS','Theory module identification    ',
     .    40, 1, 1 )
C
C   Remove old Shapiro model Lcodes.
      CALL DELR (2,'SHAP DEL')
      CALL DELR (2,'SHAP RAT')
      CALL DELR (2,'SHAP T62')
      CALL DELR (2,'SHAPCONT')
      CALL DELR (2,'REL PART')
      CALL DELR (2,'REL CONT')
C
C   Remove old Hellings model Lcodes.
      CALL DELR (2,'HELL DEL')
      CALL DELR (2,'HELL RAT')
      CALL DELR (2,'HELL EMS')
C
C   Do ADD's for the Eubanks' Consensus model information. 07/06/93
      CALL ADDR (2,'CONSNDEL','Consensus theo. delay (microsec)  ',
     .     2, 1, 1 )
      CALL ADDR (2,'CONSNRAT','Consensus theo. rate (sec/sec)    ',
     .     1, 1, 1 )
C   Remove old Consensus correction to Hellings
      CALL DELR (2,'CONSCONT')
C
C   Do DEL's to delete any vestige of the old (incorrect) Shapiro relativistic
C   delay correction. Also delete the old Robertson delay and rate theoretical
C   lcodes. Also delete 'CON PART' from Calc 8.1 databases.
      CALL DELA(1,'REL MESS')
      CALL DELA(1,'RGD MESS')
      CALL DELR(2,'RGD CONT')
      CALL DELR(2,'THODELAY')
      CALL DELR(2,'THODRATE')
      CALL DELR(2,'CON PART')
C
C   Do the Add's for the relativity partials, contributions, and         
C   relativity application status.
      CALL ADDA (1,'REL CFLG','Relativisitc bending use status ',
     .    30, 1, 1 )
      CALL ADDR (2,'CONSPART','Consensus partial w.r.t. Gamma  ',
     .     2, 1, 1 )
      CALL ADDR (2,'CON CONT','Consensus bending contrib. (sec)',
     .     2, 1, 1 )
      CALL ADDR (2,'SUN CONT','Consensus bending contrib. (sec)',
     .     2, 1, 1 )
      CALL ADDR (2,'SUN2CONT','High order bending contrib.(sec)',
     .     2, 1, 1 )
      CALL ADDR (2,'BENDPART','Grav. bend. partial w.r.t. Gamma',
     .     2, 1, 1 )
C
  500 RETURN
      END
C--------------------------------------------------------------------------
      SUBROUTINE THERI
      IMPLICIT None
C
C     THERI is the Theory input and initialization section.
C
C     THERI program interface:
C
C     Common blocks used -
       INCLUDE 'ccon.i'
C          Variables 'from':
C            1.  KTHEC  -  The Theory routine flow control flag.
C                          (No longer has any function.)
C            2.  KTHED  -  The Theory routine debug control flag.
C            3.  KRELC  -  Relativity module flow control flag.
C
C     Program specifications -
C
      INTEGER*2      LTHEU(40)
      CHARACTER*40 C_LTHEU(2)
      INTEGER*2      L_on(30),  L_off(30)
      CHARACTER*30 C_L_on(2), C_L_off(2)
      EQUIVALENCE (LTHEU, C_LTHEU), (L_on, C_L_on), (L_off, C_L_off)
C
      DATA C_LTHEU /
     . 'THEORY ROUTINE: Eubanks Consensus Relati',
     . 'vity Model, Last mod 98DEC16, D. Gordon '/
C
      DATA C_L_on  /
     . 'Relativistic bending turned ON',
     . ' in theoreticals computation. '/
C
      DATA C_L_off /
     . 'Relativistic bending turned OF',
     . 'F in theoreticals computation.'/
C 
C    Database access -
C          'PUT' variables:
C            1. LTHEU(40)  - The theory routine text message.
C          Access codes:
C            1. 'THE MESS' - The database access code for the Theory routine 
C                            text message.
C
C    Subroutine interface -
C          Caller subroutines: INITL
C          Called subroutines: PUTA
C
C    Programmer - DALE MARKHAM  01/17/77
C                 PETER DENATALE 07/20/77
C                 CHOPO MA /DAVID GORDON 04/12/84
C                 JIM RYAN 89/06/06 Some character string used.
C                 Jim Ryan 89/10/08 Relativity module code moved here.
C                 Jim Ryan 89.12.12 UNIX-like database interface
C                          implimented.
C                 Jim Ryan 90:11:20 Comments cleaned up and debug fixed.
C                 David Gordon 93AUG02 Data base message revised for Consensus 
C                              model.
C                 David Gordon 93DEC23 Fixed up debug output.
C                 David Gordon 94.04.18 Converted to Implicit None.
C                 David Gordon 94.10.05 Updated database text message. Added 
C                              character variables for 'REL CFLG' message.
C                 David Gordon 94.12.15 Corrected dimensioning of C_L_on and
C                              C_L_off.
C                 David Gordon 96.01.30 Changed text message date.
C                 David Gordon 98.08.18 Changed text message.
C
C     THERI program structure
C
C     PUT the Theory Module text message into the database.
      CALL PUTA ('THE MESS      ', LTHEU, 40, 1, 1 )
C
C     PUT the relativistic bending status into the database.
      IF(KRELC .eq. 0)  then
        CALL PUTA ('REL CFLG      ', L_on, 30, 1, 1)
      else
        CALL PUTA ('REL CFLG      ', L_off, 30, 1, 1)
      Endif
C
      RETURN
      END
C--------------------------------------------------------------------------
      SUBROUTINE THERY ( DATMC, DAXOC, DIONC, DLPGR, EARTH, EPBASE,
     1           SITEP, SITEV, SITEA, SUN, STAR, XMOON, AT )
      Implicit none
C
C     Routine THERY computes the theoretical values of delay and delay rate
C     using the information generated by the modules. Subroutine CONSEN is
C     called to perform the computations using the Eubanks "Consensus" 
C     relativity model. The Shapiro and Hellings relativity model computations
C     have been removed with Calc 9.0 and are no longer used here. 
C   
C     References: None
C
C     Program Interface
C       Input variables:
C         1. DATMC(2,2)  - The contributions to the delay and delay rate due to
C                          tropospheric refraction at each site. (sec, sec/sec)
C         2. DAXOC(2,2)  - The contributions to the delay and rate due to the 
C                          antenna axis offsets. First index runs over sites,
C                          the second runs over delay and rate (sec,sec/sec).
C         3. DIONC(2)    - The contributions to the delay and rate due to 
C                          ionospheric refraction at each site. (sec, sec/sec)
C         4. DLPGR       - The CT time derivative of the long period terms in 
C                          'AT MINUS CT' offset. (sec/sec) (Not used)
C         5. EARTH(3,3)  - The solar system barycentric Earth position
C                          velocity, and acceleration vectors. The first index
C                          runs over the vector components and the second runs
C                          over the time derivatives. (m, m/sec, m/sec**2)
C         6. EPBASE(3,2) - The J2000.0 baseline position and velocity vectors.
C                          (m, m/sec)
C         7. SITEA(3,2)  - The J2000.0 geocentric acceleration vectors of each 
C                          observing site. (m/sec**2)
C         8. SITEP(3,2)  - The J2000.0 geocentric position vectors of each
C                          observing site. (m)
C         9. SITEV(3,2)  - The J2000.0 geocentric velocity vectors of each 
C                          observing site. (m/sec)
C        10. SUN(3,2)    - The J2000.0 geocentric Sun position and velocity 
C                          vectors. (m, m/sec)
C        11. XMOON(3,2)  - The J2000.0 geocentric Moon position and velocity
C                          vectors. (m, m/sec)
C        12. STAR(3)     - The J2000.0 source unit vector. (unitless)
C        13. AT          - The Atomic Time fraction of the Atomic Time Day 
C                          (TAI). (days) (Not used)
C
C     Common blocks used -
C
      INCLUDE 'ccon.i'
C       Variables 'from':
C         1.  KTHEC - The Theory routine flow control flag.
C                     (No longer has any function.)
C         2.  KTHED - The theory routine debug control flag.
C         3.  KRELC - The relativity module flow control flag.
C                       0     --> Gravitational bending used.
C                       NOT 0 --> Gravitational bending not used.
C
C   Program specifications -
      INTEGER*4 I
      REAL*8 DATMC(2,2), DAXOC(2,2), DIONC(2), SUN(3,2), EARTH(3,3), 
     .       EPBASE(3,2), SITEA(3,2), STAR(3), SITEP(3,2), SITEV(3,2),
     .       XMOON(3,2), AT, SUNHAT(3), DLPGR
      Real*8 CONDEL(2), CONRAT, delta_t_grav, d_delta_t_grav, 
     .       tg2_tg1, dtg2_tg1, xtg2_tg1, CON_CNTRB(2), CON_PART(2),
     .       delta_t_grav_Sun, d_delta_t_grav_Sun, CONSENSUS(2),
     .       con_cont(2), Sun_cntrb(2), Bend_par(2), Sunplus(2)
C
C 4.2.4 DATA BASE ACCESS -
C
C        'PUT' VARIABLES:
C          1. CONDEL(2)    - The theoretical delay from the Consensus
C                            model in two pieces in units of MICROSECONDS.
C                            The 1st is the integer microseconds and the
C                            2nd is the submicroseconds portion. 
C          2. CONRAT       - The theoretical delay rate from the Consensus
C                            model/ (sec/sec).
C          3. SUN_CNTRB(2) - The solar gravitational bending delay and rate 
C                            terms from the Consensus model. (sec, sec/sec)
C          4. CON_CNTRB(2) - The total gravitational bending delay and rate 
C                            terms from the Consensus model. (sec, sec/sec)
C          5. CON_PART(2)  - The total Consensus delay and rate partials
C                            with respect to Gamma. (sec, sec/sec)
C          6. Sunplus(2)  -  Higher order solar bending delay and rate 
C                            contributions, as defined in IERS Conventions
C                            (1996), page 91, eqn. 14. (sec, sec/sec) It 
C                            has also been added to the total solar bending 
C                            delay and rate contibutions. 
C          7. Bend_par(2)  - The relativistic bending delay and rate partials
C                            with respect to Gamma. (sec, sec/sec)
C
C        ACCESS CODES:
C          1. 'CONSNDEL' - The database access code for the theoretical
C                          delay using the Eubanks' Consensus model.
C          2. 'CONSNRAT' - The database access code for the theoretical
C                          delay rate using the Eubanks' Consensus model.
C          3. 'SUN CONT' - The database access code for the Consensus model
C                          Solar gravitational bending delay and rate terms.
C          4. 'CON CONT' - The database access code for the Consensus model
C                          TOTAL gravitational bending delay and rate terms.
C          5. 'CONSPART' - Database access code for the partial derivatives 
C                          of the Consensus model delays and rates with 
C                          respect to Gamma.
C          6. 'SUN2CONT' - Database access code for the additional solar 
C                          bending due to higher order relativistic effects.
C                          This term is already in the theoretical, so to 
C                          remove it, SUBTRACT the values in this access code. 
C          7. 'BENDPART' - Database access code for the partial derivatives 
C                          of the Consensus model gravitational bending
C                          delay and rate contributions with respect to Gamma.
C
C    Subroutine interface -
C          Caller subroutine:  DRIVR
C          Called subroutines:  PUT4, CONSEN
C
C    Program variables -
C           1. tg2_tg1      - The geometric delay corrected for relativistic 
C                             effects using the Consensus model. 
C           2. dtg2_tg1     - The time derivative of the geometric delay
C                             corrected for relativistic effects using the 
C                             Consensus model. 
C           3. delta_t_grav - The total differential gravitational time delay,
C                             or "bending delay" from the Consensus model.
C           4. d_delta_t_grav-The time derivative of the total differential
C                             gravitational time delay, or "bending delay,"
C                             from the Consensus model.
C           5. SUN_CNTRB(2) - The solar gravitational bending delay and rate 
C                             contributions from the Consensus model. (s, s/s).
C           6. CON_CNTRB(2) - The total gravitational bending delay and rate 
C                             contributions from the Consensus model. (s, s/s).
C           7. CON_PART(2)  - The partial derivatives of the Consensus model
C                             delay and rate with respect to Gamma (s and s/s).
C           8. Sunplus(2)  -  Higher order solar bending delay and rate 
C                             contributions, as defined in IERS Conventions
C                             (1996), page 91, eqn. 14 (sec, sec/sec). It 
C                             has also been added to the total solar bending 
C                             delay and rate contibutions. 
C           9. Bend_par(2)  - The relativistic bending delay and rate partials
C                             with respect to Gamma. (sec, sec/sec)
C
C 4.2.9 PROGRAMMER - DALE MARKHAM 01/17/77
C                  PETER DENATALE 07/20/77
C                  CHOPO MA / DAVID GORDON 04/12/84
C                  DAVID GORDON 06/19/84 REMOVED ATMOSPHERE.
C                  DAVID GORDON 07/31/84 RELATIVISTIC CORRECTIONS.
C                  DAVID GORDON 01/03/85 ADDED ATMOSPHERE AFTER MODS TO 
C                               ATMOSPHERE FLAGS.
C                  SAVITA GOEL  06/03/87 CDS FOR A900.
C                  GREGG COOKE  05/01/89 NEW MODEL FROM I.SHAPIRO.
C                  JIM RYAN     06/06/89 Some bugs fixed and documentation
C                               modified.
C                  Jim Ryan     09/13/89 Robertson code removed and Hellings
C                               added.
C                  Jim Ryan     89.10.05 CPHYS common made an include file
C                  Jim Ryan     89.10.09 Relativity partials and contributions 
C                               moved here.
C                  Jim Ryan     89.11.20 Shapiro algorithm modified to make
C                               it reflect Ryan's memo
C                  Jim Ryan     89.12.12 UNIX-like database interface 
C                               implemented.
C                  Jim Ryan     89.12.14 Addional delay information stored 
C                               and Helling rate implemented.
C                  C Ma         90.08.10 Corrections to documentation.
C                  Jim Ryan     90.11.20 Debug statement fixed and comments
C                               cleaned up.
C                  T. Marshall Eubanks & Brent Archinal 91.05.10 HELL EMS 
C                               fixed, debug statements and comments modified.
C                               A900 and HP-UX versions consolidated except for
C                               dbh calls and ILUOUT.
C                  Jim Ryan     91.05.28 A few, mostly cosmetic changes made.
C                  David Gordon 93.04.27 GMEARTH put into cphys.i and 
C                               defined in cinit.f; definition removed here.
C                  David Gordon 93.08.02 Thery modified to call subroutine
C                               Consen (previously called by DRIVR), and to
C                               do the puts for the Consensus delay and rate. 
C                  David Gordon 93.12.22 Fixed up debug output.
C                  David Gordon 93.12.27 Added access code CONSCONT, correction
C                               to convert Hellings to Comsensus theoreticals.
C                  David Gordon 94.01.21 Added access code SHAPCONT, correction
C                               to convert Hellings to Shapiro theoreticals.
C                  David Gordon 94 Feb/March - Modified axis offset correction
C                               for use here.
C                  David Gordon 94.10.05 Corrected error in computing second
C                               half of Lcode for Shapiro delay contribution.
C                               Many unused variables removed.
C                  David Gordon 95.10.11 Minor correction to Hellings model
C                               when gravitational bending turned OFF (KRELC=1).
C                  David Gordon 96.01.30 Added Consensus model solar bending
C                               term (Sun_cntrb(2) and the L-code 'SUN CONT').
C                               Removed 'CON PART' L-code (Solar bending 
C                               partials w.r.t. Gamma) and replaced it with
C                               'CONSPART' (Total Consensus delay and rate 
C                               partials w.r.t. Gamma).   
C                  David Gordon 98.08.18 Shapiro and Hellings models removed.
C                  David Gordon 98.11.16 Added Sunplus to CALL CONSEN argument 
C                               list. Contains the delay and rate contributions
C                               due to higher order solar bending, as defined
C                               in the 1996 IERS Conventions, p. 91, eqn. 14.
C                               Added PUTR of 'SUN2CONT' to put it in the
C                               data bases. It is already included in the 
C                               theoretical, therefore to remove its effects,
C                               you must SUBTRACT it from the theoretical. 
C                  David Gordon 98.12.16 Added Bend_par(2), the partials of the
C                               gravitional bending contributions with respect
C                               to Gamma.
C
C     THERY program structure
C--------------------------------------------------------------------------
C
C   Call subroutine CONSEN to compute the delay and delay rate based on the 
C   Eubanks' Consensus relativity model. We put it in its own subroutine
C   because it is a very long and detailed set of computations. 93JUL29, DG
C
      Call  CONSEN ( DATMC, EARTH, EPBASE, SITEP, SITEV,
     1      SITEA, SUN, XMOON, STAR, tg2_tg1, dtg2_tg1,
     2      delta_t_grav, d_delta_t_grav, delta_t_grav_Sun,
     3      d_delta_t_grav_Sun, Con_part, Bend_par, Sunplus )
C
C   Add the axis offset corrections
      tg2_tg1  = tg2_tg1  + DAXOC(1,1) + DAXOC(2,1)
      dtg2_tg1 = dtg2_tg1 + DAXOC(1,2) + DAXOC(2,2)
C
      CONSENSUS(1) =  tg2_tg1      ! Consensus delay
      CONSENSUS(2) = dtg2_tg1      ! Consensus rate
C
C   Convert the theoretical delay to microseconds and split the double
C   precision results into an integer microseconds portion and a 
C   submicroseconds portion.
      xtg2_tg1  = tg2_tg1  * 1.D6
      CONDEL(1) = IDINT(xtg2_tg1 )
      CONDEL(2) = xtg2_tg1 - CONDEL(1)
      CONRAT = dtg2_tg1 
C
C   Put the Consensus model theoretical delays and rates into the database.
      CALL PUT4 ('CONSNDEL      ', CONDEL, 2, 1, 1)
      CALL PUT4 ('CONSNRAT      ', CONRAT, 1, 1, 1)
C
C  Compute and store the total gravitational light bending terms
      CON_CNTRB(1) = delta_t_grav
      CON_CNTRB(2) = d_delta_t_grav
C  Compute and store the solar gravitational light bending terms
      Sun_cntrb(1) = delta_t_grav_Sun 
      Sun_cntrb(2) = d_delta_t_grav_Sun
C
C   Note: The Lcode 'CON CONT' will hold the TOTAL gravitational bending delay 
C         and rate from the Sun, Moon, Earth, and planets. 'SUN CONT' will hold
C         the gravitational bending delay from the Sun only. 
C         'CONSPART' will hold the TOTAL Consensus delay and rate partials with
C         respect to Gamma. 
C         
C   Put into the data base
      CALL PUT4 ('CON CONT      ', CON_CNTRB, 2, 1, 1)
      CALL PUT4 ('SUN CONT      ', SUN_CNTRB, 2, 1, 1)
      CALL PUT4 ('CONSPART      ', CON_PART,  2, 1, 1)
      CALL PUT4 ('BENDPART      ', BEND_PAR,  2, 1, 1)
C  The following is the higher order solar bending from the 1999 Conventions.
C   It is already included in the total theoreticals ('CONSNDEL' and 
C   'CONSNRAT') and the total solar bending contribution ('SUN CONT').
      CALL PUT4 ('SUN2CONT      ', Sunplus,   2, 1, 1)
C
C--------------------------------------------------------------------------
C     Check KTHED to determine if debug output is neccessary.
      IF ( KTHED .ne. 0 )  Then
       WRITE (6, 9100 )
 9100  FORMAT (1X, "Debug output for subroutine THERY." )
    8  FORMAT(A,4D25.16/(7X,5D25.16))
       write(6,8)' tg2_tg1, dtg2_tg1 ',tg2_tg1, dtg2_tg1
       write(6,8)' CONDEL, CONRAT ', CONDEL, CONRAT
       write(6,8)' CON_CNTRB ',CON_CNTRB
       write(6,8)' SUN_CNTRB ',SUN_CNTRB
       write(6,8)' CON_PART  ',CON_PART
       write(6,8)' CONSENSUS ',CONSENSUS
       write(6,8)' Sunplus   ',Sunplus
      Endif
C
      Return
      END
C
C-------------------------------------------------------------------------------
      Subroutine CONSEN ( DATMC, EARTH, EPBASE, SITEP, SITEV,
     1           SITEA, SUN, XMOON, STAR, tg2_tg1, dtg2_tg1, 
     2           delta_t_grav, d_delta_t_grav, delta_t_grav_Sun,
     3           d_delta_t_grav_Sun, Con_part, Bend_par, Sunplus )
      Implicit none 
C 
C     Routine CONSEN computes the theoretical values of delay and delay rate 
C     using the Eubanks' Consensus relativity model. 
C
C     References:
C       - Eubanks, T.M., "A Consensus Model for Relativistic Effects in
C          Geodetic VLBI," in "Proceedings of the U.S. Naval Observatory
C          Workshop on Relativistic Models for Use in Space Geodesy,",
C          T.M. Eubanks (editor), USNO, Washington, DC, June, 1991. 
C
C      Input Variables:
C         1.  DATMC(2,2)   -  The contributions to the delay and delay rate due
C                             to tropospheric refraction at each site. (s, s/s)
C         2.  EARTH(3,3)   -  The solar system barycentric Earth position,
C                             velocity, and acceleration vectors. The first
C                             index runs over the vector components and the
C                             second runs over the time derivatives.
C                             (m, m/sec, m/sec**2)
C         3.  EPBASE(3,2)  -  The J2000.0 baseline position and velocity
C                             vectors.  (m, m/sec)
C         4.  SITEP(3,2)   -  The J2000.0 geocentric position vectors of each
C                             observing site. (m)
C         5.  SITEV(3,2)   -  The J2000.0 geocentric velocity vectors of each
C                             observing site. (m/sec)
C         6.  SITEA(3,2)   -  The J2000.0 geocentric acceleration vectors of
C                             each observing site. (m/sec**2)
C         7.  SUN(3,2)     -  The J2000.0 geocentric Sun position and velocity
C                             vectors.  (m, m/sec)
C         8.  XMOON(3,2)   -  The J2000.0 geocentric Moon position and velocity
C                             vectors.  (m, m/sec)
C         9.  STAR(3)      -  The J2000.0 source unit vector. (unitless)
C
C      Output Variables:
C         1. tg2_tg1       -  The geometric delay corrected for relativistic 
C                             effects (but not atmospheric refraction) using
C                             the Consensus model. 
C         2. dtg2_tg1      -  The time derivative of the geometric delay
C                             corrected for relativistic effects using the 
C                             Consensus model. 
C         3. delta_t_grav  -  The total differential gravitational time delay,
C                             or "bending delay" from the Consensus model.
C         4. d_delta_t_grav-  The time derivative of the total differential
C                             gravitational time delay, or "bending delay" from
C                             the Consensus model.
C         5. delta_t_grav_Sun-The Sun's contribution to the "bending delay" 
C                             from the Consensus model. Now includes 
C                             Sunplus(1).
C         6. d_delta_t_grav_Sun-The time derivative of the Sun's contribution
C                             to the "bending delay" from the Consensus model.
C                             Now includes Sunplus(2).
C         7. Con_part      -  Partial derivatives of the Consensus delays and 
C                             rates with respect to Gamma.
C         8. Sunplus(2)    -  Higher order solar bending delay and rate 
C                             contributions, as defined in IERS Conventions
C                             (1996), page 91, eqn. 14. (sec, sec/sec) It 
C                             has also been added to the total solar bending 
C                             delay and rate contibutions. 
C         9. Bend_par(2)   -  The relativistic bending delay and rate partials
C                             with respect to Gamma. (sec, sec/sec)
C
C     Common blocks used -
C
       INCLUDE 'cphys.i'
C          Variables 'from':
C            1. VLIGHT  - The velocity of light in vacuum.  (m/sec)
C            2. VLIGHT2 - The velocity of light squared. (m/sec)**2
C            3. VLIGHT3 - The velocity of light cubed. (m/sec)**3
C            4. GAMMA   - The post Newtonian expansion parameter which
C                         affects light bending. (1.0 used here). (unitless)
C            5. GMSUN, GMMOON, GMEARTH, GMPLANET(7) - Gravitational constant
C                         times the masses of the Sun, Moon, Earth, and the 
C                         other planets except Pluto.
C            6. REARTH  - The equatorial radius of the Earth. (meters)
C
      INCLUDE 'csolsys.i'
C       Variables 'from':
C         1. SPLANET(3,2,7) - The J2000.0 Solar System Barycentric positions
C                             and velocities of all planets except the Earth
C                             and Pluto. (meters, meters/sec) The first index
C                             runs over X,Y, and Z, the second runs over
C                             position and velocity, and the third runs over
C                             the planets, where
C                                    1 = Mercury
C                                    2 = Venus
C                                    3 = Mars
C                                    4 = Jupiter
C                                    5 = Saturn
C                                    6 = Uranus
C                                    7 = Neptune 
C
C         2. GPLANET(3,2,7) - The J2000.0 Geocentric positions and velocities
C                             of all planets except the Earth and Pluto.
C                             (meters, meters/sec) The first index runs over
C                             X,Y, and Z, the second runs over position and
C                             velocity, and the third runs over the planets,
C                             where
C                                    1 = Mercury
C                                    2 = Venus
C                                    3 = Mars
C                                    4 = Jupiter
C                                    5 = Saturn
C                                    6 = Uranus
C                                    7 = Neptune 
C
      Real*8           PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      COMMON / CMATH / PI, TWOPI, HALFPI, CONVD, CONVDS, CONVHS, SECDAY
      EXTERNAL CMATHB
C
       INCLUDE 'ccon.i'
C            Variables 'from':
C              1.  KTHEC  -  The Theory routine flow control flag.
C                            (No longer has any function.)
C              2.  KTHED  -  The theory routine debug control flag.
C              3.  KRELC  -  The relativity module flow control flag.
C                            0     --> Gravitational bending used.
C                            NOT 0 --> Gravitational bending not used.
C
      INCLUDE 'cobsn.i'
C          Variables from:
C            1. Nzero  -  Set to 1 or 2 if station 1 or 2 is at the geocenter,
C                         otherwise equals zero. For correlator usage.
C
C   Program Specifications -
      INTEGER*4 l,k
      REAL*8 DATMC(2,2), SUN(3,2), EARTH(3,3), EPBASE(3,2), SITEA(3,2),
     .       STAR(3), SITEP(3,2), SITEV(3,2), XMOON(3,2), CON_CNTRB(2),
     .       SUN_CNTRB(2), DOTP, VECMG
      Real*8 XsubEarth(3), VsubEarth(3), AsubEarth(3)
      Real*8 x_sub1t1(3), x_sub2t1(3), w_sub1(3), w_sub2(3)
      Real*8 a_sub1(3), a_sub2(3)
      Real*8 unit_K(3), b_nought(3),db_nought(3)
      Real*8 x_subSun(3), x_subMoon(3), v_subSun(3), v_subMoon(3)
      Real*8 XsubSun(3), XsubMoon(3), VsubSun(3), VsubMoon(3)
      Real*8 R_Earth_Sun(3), R_Earth_Moon(3)
      Real*8 V_Earth_Sun(3), V_Earth_Moon(3)   
      Real*8 Xsub1(3), Xsub2(3), t_sub1, t_sub2   
      Real*8 dXsub1(3), dXsub2(3)
      Real*8 x1Sun(3), x1Moon(3), x1Planet(3,7)    
      Real*8 del_t_Sun, del_t_Moon, del_t_Planet(7)
      Real*8 XSunt1J(3), XMoont1J(3), XPlant1J(3,7)    
      Real*8 dXSunt1J(3), dXMoont1J(3), dXPlant1J(3,7)    
      Real*8 R1Sunt1(3), R1Moont1(3), R1Plant1(3,7)
      Real*8 R2Sunt1(3), R2Moont1(3), R2Plant1(3,7)
      Real*8 dR1Sunt1(3), dR1Moont1(3), dR1Plant1(3,7)
      Real*8 dR2Sunt1(3), dR2Moont1(3), dR2Plant1(3,7)
      Real*8 delta_t_grav, delta_t_grav_Sun, delta_t_grav_Moon,
     .       delta_t_grav_Earth, delta_t_grav_Plan(7),
     .       delta_t_grav_Planets 
      Real*8 d_delta_t_grav, d_delta_t_grav_Sun, d_delta_t_grav_Moon,
     .       d_delta_t_grav_Earth, d_delta_t_grav_Plan(7),
     .       d_delta_t_grav_Planets
      Real*8 U, U_Sun, dU, absVEarth
      Real*8 C_Earth, C_Sun, C_Moon, C_Plan(7)
      Real*8 term_a, term_b, term_c, term_d, term_e, term_f,
     .       term_g, term_h 
      Real*8 dterm_a, dterm_b, dterm_c, dterm_d, dterm_e, dterm_f,
     .       dterm_g, dterm_h 
      Real*8 term1, term2a, term2b, term2c, term2d, term2,
     .       term2bcd, term3a, term3b, term3, term4,
     .       term123, vec_sum(3), tv2_tv1
      Real*8 dterm1, dterm2a, dterm2b, dterm2c, dterm2d, dterm2,
     .       dterm2bcd, dterm3a, dterm3b,  dterm3, dterm4,
     .       dterm123, dvec_sum(3), dtv2_tv1
      Real*8 V_w1(3), V_w2(3), K_V_w1, K_V_w2           !!
      Real*8 tg2_tg1, dtg2_tg1, Con_part(2), Bend_par(2)
      Real*8 w2_w1(3), a2_a1(3), K_dotw2w1, dK_dotw2w1 
C     Real*8 k_sub1(3), k_sub2(3) 
      Real*8 KdotB, dKdotB, VdotB, dVdotB, KdotV, dKdotV
      Real*8 delta_t2_t1, d_delta_t2_t1, term10a, term10b, term10c, 
     .       dterm10a, dterm10b, dterm10c, del_del_t(2,2)
      Real*8 x_delta_t2_t1, dx_delta_t2_t1
      Real*8 vecmg1,vecmg2
      Real*8 N_hat(3), dN_hat(3), Vmag_S, CSun1, NplusK(3), V1, dV1, 
     .       V2, dV2, Sunplus(2) 
C
C     Subroutines used:  DOTP, IDINT, PUT4, VECAD, VECMG
C
C     Constants used - VLIGHT      - Speed of light (m/s)
C                      VLIGHT2     - Speed of light squared
C                      VLIGHT3     - Speed of light cubed
C                      GMMOON      - GM of the Moon.
C                      GMEARTH     - GM of the Earth.
C                      GMSUN       - GM of the Sun.
C                      GMPlanet(7) - GM's of all planets except Earth and Pluto
C
C   Program variables -
C
C    1.  XsubEarth(3)  =  Barycentric radius vector of the geocenter (meters).
C    2.  VsubEarth(3)  =  Barycentric velocity vector of the geocenter (m/sec).
C    3.  AsubEarth(3)  =  Barycentric acceleration of the geocenter (m/sec**2).
C
C    4.  x_sub1t1(3)   =  Geocentric radius vector of receivers 1 and 2 at the
C    5.  x_sub2t1(3)        geocentric time t_sub1 (meters).
C
C    6.  w_sub1(3)     =  Geocentric velocity of receiver 1 (m/sec).
C    7.  w_sub2(3)     =  Geocentric velocity of receiver 2 (m/sec).
C
C    8.  a_sub1(3)     =  Geocentric acceleration of receiver 1 (m/sec).
C    9.  a_sub2(3)     =  Geocentric acceleration of receiver 2 (m/sec).
C
C   10.  x_subSun(3)   =  Geocentric radius vector of the Sun (meters).
C   11.  x_subMoon(3)  =  Geocentric radius vector of the Moon (meters).
C   12.  GPLANET(l,1,k)=  Geocentric radius vectors of the planets (meters).
C                          (l = X,Y,Z; k = planet #)
C                       
C   13.  v_subSun(3)   =  Geocentric velocity vector of the Sun (m/sec).
C   14.  v_subMoon(3)  =  Geocentric velocity vector of the Moon (m/sec).
C   15.  GPLANET(l,2,k)=  Geocentric velocity vector of the planets (m/sec).
C                          (l = X,Y,Z; k = planet #)
C
C   16.  Xsub1(3)      =  Barycentric radius vector of receiver 1 (meters). 
C   17.  Xsub2(3)      =  Barycentric radius vector of receiver 2 (meters). 
C   18.  dXsub1(3)     =  Barycentric velocity vector of receiver 1 (m/sec). 
C   19.  dXsub2(3)     =  Barycentric velocity vector of receiver 2 (m/sec). 
C
C   20.  x1Sun(3)      =  Vector from receiver 1 to the Sun at time t_sub1
C   21.  x1Moon(3)     =  Vector from receiver 1 to the Moon at time t_sub1
C   22.  x1Planet(3,7) =  Vector from receiver 1 to the planets at time t_sub1
C
C   23.  XsubSun(3)    =  Barycentric radius vector of the Sun/Moon/planets
C   24.  XsubMoon(3)         
C   25.  SPLANET(l,1,k) 
C
C   26.  VsubSun(3)    =  Barycentric velocity vector of the Sun/Moon/planets
C   27.  VsubMoon(3)         
C   28.  SPLANET(l,2,k) 
C
C   29.  XSunt1J(3)    =  SSBC radius vector to the Sun/Moon/planets
C   30.  XMoont1J(3)         at the time of closest approach.
C   31.  XPlant1J(3,7)              (meters) 
C
C   32.  R_Earth_Sun(3) = Vector from the Sun to the geocenter
C   33.  R_Earth_Moon(3)= Vector from the Moon to the geocenter
C   34.  -GPLANET(l,1,k)= Vector from planets to the geocenter
C
C   35.  V_Earth_Sun(3) = Velocity Vector of the geocenter from the Sun
C
C   36.  R1Sunt1(3)    =  Vector from the Sun/Moon/planets to receiver 
C   37.  R1Moont1(3)   =      1 at the time of closest approach.
C   38.  R1Plant1(3,7) =                (meters) 
C
C   39.  R2Sunt1(3)    =  Vector from the Sun/Moon/planets to receiver 
C   40.  R2Moont1(3)   =      2 at the time of closest approach.
C   41.  R2Plant1(3,7) =              (meters) 
C
C   42.  t_sub1        =  Time of arrival of the signal at receivers 1 and 2.
C   43.  t_sub2            [Note: not needed here so we don't actually define
C                          them.]
C
C   44.  del_t_Sun     =  Difference between arrival time at receiver 1, t_sub1,
C   45.  del_t_Moon         and time of closest approach to Sun/Moon/planets
C   46.  del_t_Planet(7)      ( > 0 if closest approach is before arrival)
C
C   47.  unit_K(3)     =  Barycentric unit vector to the source (in the absense
C                         of gravitational or aberrational bending).
C
C   48.  k_sub1(3)     =  Aberrated unit vector from station 1 to the source.
C   49.  k_sub2(3)     =  Aberrated unit vector from station 2 to the source.
C                          [Note: not computed here, see atmosphere module.]
C
C   50.  b_nought(3)   =  A priori geocentric baseline vector at time t_sub1
C                         (Baseline vector and it's velocity. Defined from
C                         site #1 to site #2 (M,M/S).)
C
C   51.  U             =  Sun's potential
C
C   52.  delta_t_grav_Sun 
C   53.  delta_t_grav_Moon =  The differential gravitational time delay for
C   54.  delta_t_grav_Plan(7)     the Sun/Moon/planets/Earth.
C   55.  delta_t_grav_Earth
C
C   56.  delta_t_grav_Planets = Sum of bending delays for the planets 
C
C   57.  delta_t_grav  =  The total differential gravitational time delay,
C                         or "bending delay."
C
C   58.  tv2_tv1       =  The theoretical vacuum delay computed using the
C                         Consensus model. (sec)
C
C   59.  tg2_tg1       =  The total theoretical geometric delay (doesn't
C                         include tropospheric refraction effects which are
C                         usually added in Solve) computed using the Consensus
C                         model. (sec)
C
C 4.2.9 PROGRAMMER - DAVID GORDON 04/22/93 thru 08/02/93 - Written and debuged 
C                    D. Gordon Nov. 1993 - Modified to use all planets
C                              except Pluto.
C                    D. Gordon Jan-Mar 1994 - Modified for axis offset 
C                              correction. Not using Eubank's Step 10.
C                    D. Gordon 10.05.94 Many unused variable and much unused
C                              code removed.
C                    D. Gordon 96.01.30 Added section to compute partial
C                              derivatives of delay and rate w.r.t. Gamma.
C                    D. Gordon 96.02.09 Corrected typo in Step 2 debug printout.
C                    D. Gordon 98.08.18 Mods for geocenter station.
C                    D. Gordon 98.11.16 Added computation of higher order 
C                              solar bending term, from IERS Conventions
C                              (1996), page 91, eqn. 14. This term is now 
C                              added to the total solar bending term. It 
C                              only becomes significant within about 2 
C                              degrees of the Sun.
C                    D. Gordon 98.12.16 Added computation of Bend_par(2), the
C                              partial derivatives of the gravitational 
C                              bending contributions with respect to Gamma.
C
C     CONSEN program structure:
C
C     Compute the theoretical delay and delay rate using Eubanks's Consensus 
C     Relativity model.
C_____________________________________________________________________________
C
C     Copy CALC variables into variables with names which mimic the variables
C     in the consensus paper. VLIGHT is used for the velocity of light because
C     'C' is a poor variable name that could be easily lost.
C
      Do l=1,3
C
       XsubEarth(l) = EARTH(l,1)     ! SSBC Earth position
       VsubEarth(l) = EARTH(l,2)     !   ditto    velocity
       AsubEarth(l) = EARTH(l,3)     !   ditto    acceleration
C
       x_sub1t1(l) = SITEP(l,1)      ! Site 1 geocentric position
       w_sub1(l)   = SITEV(l,1)      !      ditto        velocity
       a_sub1(l)   = SITEA(l,1)      !      ditto        acceleration
C
       x_sub2t1(l) = SITEP(l,2)      ! Site 2 geocentric position
       w_sub2(l)   = SITEV(l,2)      !      ditto        velocity
       a_sub2(l)   = SITEA(l,2)      !      ditto        acceleration
C
       unit_K(l)    = STAR(l)        ! J2000.0 unit source vector
C
       x_subSun(l)  = SUN(l,1)       ! Geocentric Sun position
       v_subSun(l)  = SUN(l,2)       !       ditto    velocity
C
       x_subMoon(l) = XMOON(l,1)     ! Geocentric Moon position
       v_subMoon(l) = XMOON(l,2)     !       ditto     velocity
C
       b_nought(l)  = -EPBASE(l,1)   ! Baseline vector from site 1 to site 2
       db_nought(l) = -EPBASE(l,2)   ! Time derivative of baseline vector 
C
       R_Earth_Sun(l)  = -SUN(l,1) 
       V_Earth_Sun(l)  = -SUN(l,2)    ! = -v_subSun(l) also 
       R_Earth_Moon(l) = -XMOON(l,1) 
C
C   SSBC radius vectors of Sun and Moon:
       XsubSun(l)  = XsubEarth(l) + x_subSun(l)
       XsubMoon(l) = XsubEarth(l) + x_subMoon(l)
C
C   SSBC velocity vectors of Sun and Moon:
       VsubSun(l)  = VsubEarth(l) + v_subSun(l)
       VsubMoon(l) = VsubEarth(l) + v_subMoon(l)
C
      Enddo
C
C    Atomic time, TAI, at receiver #1:
C      t_sub1 =   ! Not needed here 
C
      If (KTHED .ne. 0) Then
       write(6,'(/,15x,"  Debug output for subroutine CONSEN",/)')
       write(6,8)' GMSUN  ', GMSUN
       write(6,8)' GMMOON ', GMMOON
       write(6,8)' GMEARTH', GMEARTH
       write(6,'("GM-Planets = ",3d25.16,/,5x,4d25.16)') GMPLANET
C      write(6,8)' GAMMA  ', GAMMA
       write(6,'(/,"XsubEarth:",3D23.14)') XsubEarth
       write(6,'("VsubEarth:",3D23.14)') VsubEarth
       write(6,'("AsubEarth:",3D23.14)') AsubEarth
       write(6,'("x_sub1t1:",3D23.14)') x_sub1t1
       write(6,'("w_sub1:",3D23.14)') w_sub1
       write(6,'("a_sub1:",3D23.14)') a_sub1
       write(6,'("x_sub2t1:",3D23.14)') x_sub2t1
       write(6,'("w_sub2:",3D23.14)') w_sub2
       write(6,'("a_sub2:",3D23.14)') a_sub2
       write(6,'("unit_K:",3D23.14)') unit_K 
       write(6,'("x_subSun:",3D23.14)') x_subSun 
       write(6,'("x_subMoon:",3D23.14)') x_subMoon 
       write(6,'("v_subSun:",3D23.14)') v_subSun 
       write(6,'("v_subMoon:",3D23.14)') v_subMoon 
       write(6,'("b_nought:",3D23.14)') b_nought
       write(6,'("db_nought:",3D23.14)') db_nought
       write(6,'("R_Earth_Sun:",3D23.14)') R_Earth_Sun 
       write(6,'("V_Earth_Sun:",3D23.14)') V_Earth_Sun 
       write(6,'("R_Earth_Moon:",3D23.14)') R_Earth_Moon 
       write(6,'("XsubSun:",3D23.14)') XsubSun 
       write(6,'("XsubMoon:",3D23.14)') XsubMoon  
       write(6,'("VsubSun:",3D23.14)') VsubSun  
       write(6,'("VsubMoon:",3D23.14)') VsubMoon 
      Endif
C
C******************************************************************************
C  Step 1: Estimate barycentric radius and velocity vectors for stations 1 
C          and 2 at time t_sub1 (Equation 6 and time derivative of).
C
      Do l=1,3
       Xsub1(l) = XsubEarth(l) + x_sub1t1(l)
       Xsub2(l) = XsubEarth(l) + x_sub2t1(l)
       dXsub1(l) = VsubEarth(l) + w_sub1(l)     !derivative
       dXsub2(l) = VsubEarth(l) + w_sub2(l)     !derivative
      Enddo
C
      If (KTHED .ne. 0) Then
       write(6,'(/,15x,"Step 1 dump:")')
       write(6,8)' Xsub1 ',Xsub1    
       write(6,8)' Xsub2 ',Xsub2   
       write(6,8)' dXsub1 ',dXsub1 
       write(6,8)' dXsub2 ',dXsub2  
      Endif
C
C******************************************************************************
C  Step 2: Estimate the vectors from the Sun, the Moon, and each planet (except
C          Earth and Pluto) to receiver 1.
C
C   Eq. 5a  -  Find time of closet approach to the gravitating body. [Actually
C              we find only how much earlier (or later) the quasar's rays 
C              passed closest to the gravitating body. Then, if earlier, we
C              extrapolate the body's position back to that earlier time. If
C              not earlier we just keep the current position.]
C
C  J2000.0 vector from receiver #1 to the Sun/Moon/planets:
      Do l=1,3
       x1Sun(l)  = XsubSun(l)  - Xsub1(l)
       x1Moon(l) = XsubMoon(l) - Xsub1(l)
      Enddo
      Do k=1,7
       do l=1,3
        x1Planet(l,k)  = SPLANET(l,1,k)  - Xsub1(l)
       enddo
      Enddo
C
C   more good stuff
       KdotB  = DOTP(unit_K,b_nought)
       dKdotB = DOTP(unit_K,db_nought)     !derivative
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  Sun:
       del_t_Sun  = DOTP(unit_K,x1Sun) / Vlight
       If(del_t_Sun .lt. 0.D0) del_t_Sun = 0.0D0 
C MODIFIED CALC
C  Sun:
       del_t_Sun = VECMG(x1Sun) / Vlight
     *  - Vlight**(-2) * DOTP(VsubSun,x1Sun)
C
        Do l=1,3
C    SSBC vector to Sun at time of closest approach and its time derivative:
         XSunt1J(l) = Xsubsun(l) - VsubSun(l)*del_t_Sun
         dXSunt1J(l) = Vsubsun(l)   !Derivative (approx. - no acceleration)
C
C         equation 5b: Vector from the Sun to receiver 1
         R1Sunt1(l) = Xsub1(l) - XSunt1J(l)
         dR1Sunt1(l) = dXsub1(l) - dXSunt1J(l)   !Derivative
C
C         equation 5c: Vector from the Sun to receiver 2
         R2Sunt1(l) = Xsub2(l) - VsubEarth(l)*KdotB/VLIGHT - XSunt1J(l)
         dR2Sunt1(l) = dXsub2(l) -  AsubEarth(l)*KdotB/VLIGHT -
     *                 VsubEarth(l)*dKdotB/VLIGHT - dXSunt1J(l)   !Derivative
        Enddo
C
C  98NOV18 addition, unit vector from Sun to receiver #1
          Call VUNIT (R1Sunt1, N_hat)
          Vmag_S = VECMG(R1Sunt1)
          Do l=1,3
           dN_hat(l) = dR1Sunt1(l)/Vmag_S - 
     *        R1Sunt1(l)*(DOTP(dR1Sunt1,R1Sunt1))/Vmag_S**3 
          Enddo
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  Moon:
       del_t_Moon = DOTP(unit_K,x1Moon) / Vlight
       If(del_t_Moon .lt. 0.D0) del_t_Moon = 0.D0
C MODIFIED CALC
C  Moon:
       del_t_Moon = VECMG(x1Moon) / Vlight
     *  - Vlight**(-2) * DOTP(VsubMoon,x1Moon)
C
        Do l=1,3
C    SSBC vector to Moon at time of closest approach and its time derivative:
         XMoont1J(l) = XsubMoon(l) - VsubMoon(l)*del_t_Moon
         dXMoont1J(l) = VsubMoon(l)    !Derivative (approx. - no acceleration)
C
C        equation 5b: Vector from the Moon to receiver 1
         R1Moont1(l) = Xsub1(l) - XMoont1J(l)
         dR1Moont1(l) = dXsub1(l) - dXMoont1J(l)   !Derivative
C
C        equation 5c: Vector from the Moon to receiver 2
         R2Moont1(l) = Xsub2(l) - VsubEarth(l)* KdotB/VLIGHT -
     .                 XMoont1J(l)
         dR2Moont1(l) = dXsub2(l) - AsubEarth(l)*KdotB/VLIGHT -
     *                VsubEarth(l)*dKdotB/VLIGHT - dXMoont1J(l)  !Derivative
        Enddo
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  Planets:
C      Do k=1,7      !Planet loop
C       del_t_Planet(k)  = DOTP(unit_K,x1Planet(1,k)) / Vlight
C       If(del_t_Planet(k) .lt. 0.D0) del_t_Planet(k) = 0.D0
C MODIFIED CALC
C  Planets:
      Do k=1,7      !Planet loop
       del_t_Planet(k)  = VECMG(x1Planet(1,k)) / Vlight
     *  - Vlight**(-2) * DOTP(SPLANET(1,2,k),x1Planet(1,k))
C
        Do l=1,3
C    SSBC vector to Planet at time of closest approach and its time derivative:
         XPlant1J(l,k) = SPLANET(l,1,k) - SPLANET(l,2,k)*del_t_Planet(k)
         dXPlant1J(l,k) = SPLANET(l,2,k)    ! Derivative  (approximate)
C
C        equation 5b: Vector from Planet to receiver 1
         R1Plant1(l,k) = Xsub1(l) - XPlant1J(l,k)
         dR1Plant1(l,k) = dXsub1(l) - dXPlant1J(l,k)   !Derivative
C
C        equation 5c: Vector from Planet to receiver 2
         R2Plant1(l,k) = Xsub2(l) - VsubEarth(l)*KdotB/VLIGHT -
     .                XPlant1J(l,k)
         dR2Plant1(l,k) = dXsub2(l) - AsubEarth(l)*KdotB/VLIGHT -
     .               VsubEarth(l)*dKdotB/VLIGHT - dXPlant1J(l,k)   !Derivative
        Enddo
      Enddo      !Planet loop
C
C******************* Debug ********************
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 2 dump:")')
       write(6,8)' x1Sun ',x1Sun  
       write(6,8)' x1Moon ',x1Moon   
      Do k=1,7  
         write(6,'("x1Planet(",i1,") = ",3d25.16)') k,x1Planet(1,k),
     .            x1Planet(2,k),x1Planet(3,k)
         write(6,'("SPLANET(",i1,") = ",3d25.16)') k,SPLANET(1,1,k),
     .            SPLANET(2,1,k),SPLANET(3,1,k)
      Enddo 
C
       write(6,8)' KdotB, dKdotB ', KdotB, dKdotB
       write(6,8)' del_t_Sun ',del_t_Sun     
       write(6,8)' XSunt1J ',XSunt1J     
       write(6,8)' dXSunt1J ',dXSunt1J 
       write(6,8)' R1Sunt1 ',R1Sunt1     
       write(6,8)' dR1Sunt1 ',dR1Sunt1     
       write(6,8)' R2Sunt1 ',R2Sunt1    
       write(6,8)' dR2Sunt1 ',dR2Sunt1   
       write(6,8)' del_t_Moon ',del_t_Moon     
       write(6,8)' XMoont1J ',XMoont1J    
       write(6,8)' dXMoont1J ',dXMoont1J 
       write(6,8)' R1Moont1 ',R1Moont1   
       write(6,8)' dR1Moont1 ',dR1Moont1 
       write(6,8)' R2Moont1 ',R2Moont1 
       write(6,8)' dR2Moont1 ',dR2Moont1
       write(6,8)' Vmag_S ',  Vmag_S 
       write(6,8)' N_hat  ',  N_hat  
       write(6,8)' dN_hat ', dN_hat  
C
      Do k=1,7 
         write(6,'("del_t_Planet(",i1,") = ",d25.16)')k,del_t_Planet(k) 
         write(6,'("XPlant1J(",i1,") = ",3d25.16)') k,XPlant1J(1,k),
     .            XPlant1J(2,k),XPlant1J(3,k)
         write(6,'("dXPlant1J(",i1,") = ",3d25.16)') k,dXPlant1J(1,k),
     .            dXPlant1J(2,k),dXPlant1J(3,k)
         write(6,'("R1Plant1(",i1,") = ",3d25.16)') k,R1Plant1(1,k),
     .            R1Plant1(2,k),R1Plant1(3,k)
         write(6,'("dR1Plant1(",i1,") = ",3d25.16)') k,dR1Plant1(1,k),
     .            dR1Plant1(2,k),dR1Plant1(3,k)
         write(6,'("R2Plant1(",i1,") = ",3d25.16)') k,R2Plant1(1,k),
     .            R2Plant1(2,k),R2Plant1(3,k)
         write(6,'("dR2Plant1(",i1,") = ",3d25.16)') k,dR2Plant1(1,k),
     .            dR2Plant1(2,k),dR2Plant1(3,k)
      Enddo 
C
      Endif                               ! Debug
C
C******************************************************************************
C  Step 3: Use Equation 2 to estimate the differential gravitational delay for
C          the Sun, the Moon, and the Planets.
C
       C_Sun = (1.0D0 + gamma) * GMSUN/VLIGHT3
       vecmg1 = VECMG(R1Sunt1)
       term_a = vecmg1 + DOTP(unit_K,R1Sunt1)
       vecmg2 = VECMG(R2Sunt1)
       term_b = vecmg2 + DOTP(unit_K,R2Sunt1)
C    Derivatives:
       dterm_a = Dotp(R1Sunt1,dR1Sunt1)/vecmg1 + DOTP(unit_K,dR1Sunt1)
       dterm_b = Dotp(R2Sunt1,dR2Sunt1)/vecmg2 + DOTP(unit_K,dR2Sunt1)
C
       delta_t_grav_Sun = C_Sun * DLOG(term_a / term_b) 
       d_delta_t_grav_Sun = C_Sun * ( dterm_a/term_a -
     .                      dterm_b/term_b )                 !derivative
C
C  98NOV18 addition: Additional solar gravitional delay term, for observations
C   close to the Sun. IERS Conventions (1996), paged 91, equation 14.
       CSun1 = C_Sun**2 * VLIGHT
        Call VECAD (N_hat, unit_K, NplusK)
        V1 = DOTP(b_nought, NplusK)
       dV1 = DOTP(db_nought, NplusK) + DOTP(b_nought, dN_hat)
        V2 = term_a**2 
       dV2 = 2.D0 * term_a * dterm_a
        Sunplus(1) = CSun1 * V1 / V2
        Sunplus(2) = CSun1*dV1/V2 - CSun1*V1*DV2/V2**2
C  Don't add thess to the solar bending yet, or they will mess up the Gamma
C   partials.
C
C
      CALL PUT4 ('R1SUNT1       ', R1Sunt1,   3, 1, 1)
      CALL PUT4 ('R2SUNT1       ', R2Sunt1,   3, 1, 1)
      CALL PUT4 ('C_SUN         ', C_Sun,     1, 1, 1)
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       C_Moon = (1.0D0 + gamma) * GMMoon/VLIGHT3
       vecmg1 = VECMG(R1Moont1)
       term_c = vecmg1 + DOTP(unit_K,R1Moont1)
       vecmg2 = VECMG(R2Moont1)
       term_d = vecmg2 + DOTP(unit_K,R2Moont1)
C    Derivatives:
       dterm_c = Dotp(R1Moont1,dR1Moont1)/vecmg1 +
     .           DOTP(unit_K,dR1Moont1)
       dterm_d = Dotp(R2Moont1,dR2Moont1)/vecmg2 +
     .           DOTP(unit_K,dR2Moont1)
C
       delta_t_grav_Moon = C_Moon * DLOG(term_c / term_d) 
       d_delta_t_grav_Moon = C_Moon* ( dterm_c/term_c -
     .                      dterm_d/term_d )                   !derivative
C
      CALL PUT4 ('R1MOONT1      ', R1Moont1,   3, 1, 1)
      CALL PUT4 ('R2MOONT1      ', R2Moont1,   3, 1, 1)
      CALL PUT4 ('C_MOON        ', C_Moon,     1, 1, 1)
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 3 dump:")')
       write(6,8)' C_Sun    ',C_Sun 
       write(6,8)' term_a, dterm_a ',term_a, dterm_a 
       write(6,8)' term_b, dterm_b ',term_b, dterm_b 
       write(6,8)' delta_t_grav_Sun, d_delta_t_grav_Sun,  ',
     .             delta_t_grav_Sun, d_delta_t_grav_Sun
       write(6,8)' C_Moon   ',C_Moon
       write(6,8)' term_c, dterm_c ',term_c, dterm_c 
       write(6,8)' term_d, dterm_d ',term_d, dterm_d 
       write(6,8)' delta_t_grav_Moon, d_delta_t_grav_Moon,  ',
     .             delta_t_grav_Moon, d_delta_t_grav_Moon
       write(6,8)' CSun1    ', CSun1
       write(6,8)' NplusK   ', NplusK
       write(6,8)' V1, dV1  ', V1, dV1
       write(6,8)' V2, dV2  ', V2, dV2
       write(6,8)' Sunplus  ', Sunplus
      Endif                               ! Debug
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      Do k=1,7       ! Planet loop
C
       C_Plan(k) = (1.0D0 + gamma) * GMPlanet(k)/VLIGHT3 
       vecmg1 = VECMG(R1Plant1(1,k))
       term_e = vecmg1 + DOTP(unit_K,R1Plant1(1,k))
       vecmg2 = VECMG(R2Plant1(1,k))
       term_f = vecmg2 + DOTP(unit_K,R2Plant1(1,k))
C    Derivatives:
       dterm_e = Dotp(R1Plant1(1,k),dR1Plant1(1,k))/vecmg1 + 
     .           Dotp(unit_K,dR1Plant1(1,k))
       dterm_f = Dotp(R2Plant1(1,k),dR2Plant1(1,k))/vecmg2 + 
     .           Dotp(unit_K,dR2Plant1(1,k))
C
       delta_t_grav_Plan(k) = C_Plan(k) * DLOG(term_e / term_f) 
       d_delta_t_grav_Plan(k) = C_Plan(k) * ( dterm_e/term_e -
     .                      dterm_f/term_f )                 !derivative
C
      CALL PUT4 ('R1PLANT1      ', R1Plant1,   3, 7, 1)
      CALL PUT4 ('R2PLANT1      ', R2Plant1,   3, 7, 1)
      CALL PUT4 ('C_PLAN        ', C_Plan,     7, 1, 1)
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,13) k, C_Plan(k), vecmg1,term_e, dterm_e, vecmg2,
     .             term_f, dterm_f, delta_t_grav_Plan(k),
     .             d_delta_t_grav_Plan(k)
  13   format(" k,C_Plan: ",i5,d25.16,/," vecmg1,term_e,dterm_e: ",
     .        3d25.16,/," vecmg2,term_f,dterm_f: ",3d25.16,/,
     .        " delta_t_grav_Plan, d_delta_t_grav_Plan: ",2d25.16,/) 
      Endif                               ! Debug
C
      Enddo           ! Planet loop
C
C******************************************************************************
C  Step 4: Use Equation 4 to find the differential delay due to the Earth.
C
       C_Earth = (1.0D0 + gamma) * GMEarth/VLIGHT3
C
       IF(Nzero .ne. 1) THEN
        vecmg1 = VECMG(x_sub1t1)
        term_g = vecmg1 + DOTP(unit_K,x_sub1t1)
        dterm_g = Dotp(x_sub1t1,w_sub1)/vecmg1 + DOTP(unit_K,w_sub1)
       ELSE
        vecmg1 =  0.0D0
        term_g =  2.0D0 * REARTH
        dterm_g = 0.0D0
       ENDIF
C
       IF(Nzero .ne. 2) THEN
        vecmg2 = VECMG(x_sub2t1)
        term_h = vecmg2 + DOTP(unit_K,x_sub2t1)
        dterm_h = Dotp(x_sub2t1,w_sub2)/vecmg2 + DOTP(unit_K,w_sub2)
       ELSE
        vecmg2 =  0.0D0
        term_h =  2.0D0 * REARTH
        dterm_h = 0.0D0
       ENDIF
C
       delta_t_grav_Earth = C_Earth * DLOG(term_g / term_h) 
       d_delta_t_grav_Earth = C_Earth*( dterm_g/term_g - 
     .                        dterm_h/term_h )               !derivative
C
      CALL PUT4 ('C_EARTH       ', C_Earth,   1, 1, 1)
      CALL PUT4 ('R1EARTHT1     ', x_sub1t1,  3, 1, 1)
      CALL PUT4 ('R2EARTHT1     ', x_sub2t1,  3, 1, 1)
      CALL PUT4 ('EARTHGRAV     ', delta_t_grav_Earth, 1, 1, 1)
C      
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 4 dump:")')
       write(6,8)' C_Earth  ',C_Earth
       write(6,8)' term_g, dterm_g  ', term_g, dterm_g
       write(6,8)' term_h, dterm_h  ', term_h, dterm_h
       write(6,8)' delta_t_grav_Earth, d_delta_t_grav_Earth ',
     .             delta_t_grav_Earth,d_delta_t_grav_Earth
      Endif                               ! Debug
C
C******************************************************************************
C  Step 5: Add up all components from steps 3 and 4 to get the total
C          differential gravitational delay (equation 7).
C          [Does not include the term for observations close to the Sun.]
C
        delta_t_grav_Planets = 0.D0 
        d_delta_t_grav_Planets = 0.D0 
       do k=1,7
        delta_t_grav_Planets = delta_t_grav_Planets + 
     .                          delta_t_grav_Plan(k) 
        d_delta_t_grav_Planets = d_delta_t_grav_Planets + 
     .                         d_delta_t_grav_Plan(k) 
       enddo
C
       delta_t_grav = delta_t_grav_Sun + delta_t_grav_Moon
     .              + delta_t_grav_Planets + delta_t_grav_Earth
C  derivative
       d_delta_t_grav = d_delta_t_grav_Sun + d_delta_t_grav_Moon
     .              + d_delta_t_grav_Planets + d_delta_t_grav_Earth
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 5 dump:")')
       write(6,8)' delta_t_grav_Sun ', delta_t_grav_Sun
       write(6,8)' delta_t_grav_Moon ', delta_t_grav_Moon
       write(6,8)' delta_t_grav_Earth ', delta_t_grav_Earth
       write(6,8)' delta_t_grav_Planets, d_delta_t_grav_Planets ',
     .             delta_t_grav_Planets, d_delta_t_grav_Planets
       write(6,8)' delta_t_grav, d_delta_t_grav ',
     .             delta_t_grav, d_delta_t_grav
      Endif                               ! Debug
C
C******************************************************************************
C  Step 6: Add the total differential gravitational delay to the rest of the
C          a priori vacuum delay, Equation 9.
C
C  Find Sun's potential:
C
       U_Sun  = GMSUN/VLIGHT2
       vecmg1 = VECMG(R_Earth_Sun)
       U      = U_Sun/vecmg1
C   Derivative:
       dU = -U_Sun * Dotp(R_Earth_Sun,V_Earth_Sun) / vecmg1**3
C
C   Compute individual terms of Eqn. 9
C
C    Check to see if the user wants to use gravitational bending.
      If(KRELC .eq. 0) Then      ! Default, use bending
C       Also add in term for observations close to the Sun
       term1 = delta_t_grav + Sunplus(1)
       dterm1 = d_delta_t_grav + Sunplus(2) 
      Else                       ! Don't use bending
       term1 = 0.D0
       dterm1 = 0.D0
      Endif
C 
       term2a = KdotB/VLIGHT
       dterm2a = dKdotB/VLIGHT                  ! derivative
C
       term2b = 1.D0 - ((1.D0 + gamma) * U)
       dterm2b = -(1.D0 + gamma) * dU           ! derivative
C 
       absVEarth = VECMG(VsubEarth)
       term2c = (absVEarth)**2 / (2.D0*VLIGHT2)
       dterm2c = Dotp(VsubEarth,AsubEarth) /  VLIGHT2        ! derivative
C 
       term2d = DOTP(VsubEarth,w_sub2) / VLIGHT2
       dterm2d = (DOTP(AsubEarth,w_sub2) + DOTP(VsubEarth,a_sub2))
     .            / VLIGHT2                                  ! derivative
C 
C  Combine terms 2b,2c,2d 
       term2bcd = term2b - term2c - term2d
       dterm2bcd = dterm2b - dterm2c - dterm2d               ! derivative
C 
       term2  = term2a * term2bcd
       dterm2  = term2a * dterm2bcd + dterm2a * term2bcd     ! derivative
C 
       VdotB  = DOTP(VsubEarth,b_nought)
       dVdotB = DOTP(AsubEarth,b_nought) + DOTP(VsubEarth,db_nought) !derivative
C
       KdotV  = DOTP(unit_K,VsubEarth)
       dKdotV = DOTP(unit_K,AsubEarth)                       ! derivative
C
       term3a = VdotB/VLIGHT2
       term3b = 1.D0 + KdotV/(2.D0*VLIGHT)
       term3  = term3a * term3b
C 
       dterm3a = dVdotB/VLIGHT2                              ! derivative
       dterm3b = dKdotV/(2.D0*VLIGHT)                        ! derivative
       dterm3  = dterm3a*term3b +  term3a*dterm3b            ! derivative
C 
       call VECAD(VsubEarth,w_sub2,vec_sum)
       call VECAD(AsubEarth,a_sub2,dvec_sum)                 ! derivative
       term4  = 1.D0 + DOTP(unit_K,vec_sum)/VLIGHT
       dterm4  = DOTP(unit_K,dvec_sum)/VLIGHT                ! derivative
C 
       term123  = term1  - term2  - term3 
       dterm123 = dterm1 - dterm2 - dterm3                   ! derivative
C 
       tv2_tv1 = term123 / term4
       dtv2_tv1 = dterm123 / term4  -
     .            term123 * dterm4 / term4**2                ! derivative
C
      CALL PUT4 ('DELTATGRAV    ', delta_t_grav, 1, 1, 1);
      CALL PUT4 ('TERM4         ', term4,     1, 1, 1)
      CALL PUT4 ('R1SUNT1       ', R1Sunt1,   3, 1, 1)
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 6 dump:")')
       write(6,8)' U_Sun, U, dU ',U_Sun, U, dU
       write(6,8)' term1, dterm1  ',term1, dterm1
       write(6,8)' term2a, dterm2a  ',term2a, dterm2a
       write(6,8)' term2b, dterm2b  ',term2b, dterm2b
       write(6,8)' absVEarth',absVEarth 
       write(6,8)' term2c, dterm2c  ',term2c, dterm2c
       write(6,8)' term2d, dterm2d  ',term2d, dterm2d
       write(6,8)' term2bcd, dterm2bcd  ',term2bcd, dterm2bcd
       write(6,8)' term2, dterm2  ',term2, dterm2
       write(6,8)' VdotB, dVdotB  ',VdotB, dVdotB
       write(6,8)' KdotV, dKdotV  ',KdotV, dKdotV 
       write(6,8)' term3a, dterm3a  ',term3a, dterm3a
       write(6,8)' term3b, dterm3b  ',term3b, dterm3b
       write(6,8)' term3, dterm3  ',term3, dterm3
       write(6,8)' vec_sum, dvec_sum ',vec_sum, dvec_sum
       write(6,8)' term4, dterm4  ',term4, dterm4
       write(6,8)' term123, dterm123  ',term123, dterm123
       write(6,8)' tv2_tv1, dtv2_tv1 ',tv2_tv1, dtv2_tv1
      Endif                               ! Debug
C
C******************************************************************************
C  Step 7: Calculate the aberrated source vectors for use in the tropospheric 
C          propogation delay calculation.
C
C      [Note: These calculations are in the atmosphere module, subroutine 
C      ATMG, and do not need to be done here. The aberrated source vectors are
C      used in the atmosphere module to compute the topocentric azimuths and
C      elevations, and in the axis offset module (along with refraction) to
C      compute the vector axis offset.]
C
C     call vecad(VsubEarth,w_sub1,V_w1)
C     K_V_w1 = Dotp(unit_K,V_w1)
C
C     call vecad(VsubEarth,w_sub2,V_w2)
C     K_V_w2 = Dotp(unit_K,V_w2)
C
C     Do l=1,3
C       k_sub1(l) = unit_K(l) + (VsubEarth(l) + w_sub1(l)) / VLIGHT
C    .            - unit_K(l) * K_V_w1 /VLIGHT
C       k_sub2(l) = unit_K(l) + (VsubEarth(l) + w_sub2(l)) / VLIGHT
C    .            - unit_K(l) * K_V_w2 /VLIGHT
C     Enddo
C
C     IF(KTHED .ne. 0) Then               ! Debug
C      write(6,'(/,15x,"Step 7 dump:")')
C      write(6,8)' V_w1, K_V_w1  ',V_w1, K_V_w1
C      write(6,8)' V_w2, K_V_w2  ',V_w2, K_V_w2
C      write(6,8)' k_sub1, k_sub2  ',k_sub1, k_sub2
C     Endif                               ! Debug
C
C******************************************************************************
C  Step 8: Add the geometric part of the tropospheric propogation delay to the 
C          vacuum delay.
C          [Geocenter station: DATMC(Nzero,1) and DATMC(Nzero,2) should 
C          already be zero.]
C
      call vecsb(w_sub2,w_sub1,w2_w1)
      call vecsb(a_sub2,a_sub1,a2_a1)
      K_dotw2w1 = Dotp(unit_K,w2_w1)
      dK_dotw2w1 = Dotp(unit_K,a2_a1)           ! derivative
C
C  Apparent error in earlier versions of following code because station 1
C   atmosphere is negative. Error only at the .01 picosecond level though.  
      tg2_tg1 = tv2_tv1 + -DATMC(1,1) * K_dotw2w1/VLIGHT
      dtg2_tg1 = dtv2_tv1 + -DATMC(1,2) * K_dotw2w1/VLIGHT
     .                    + -DATMC(1,1) * dK_dotw2w1/VLIGHT       ! derivative
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step 8 dump:")')
       write(6,8)' w2_w1, a2_a1   ',w2_w1, a2_a1
       write(6,8)' K_dotw2w1, dK_dotw2w1 ', K_dotw2w1, dK_dotw2w1 
       write(6,8)' DATMC  ', DATMC     
       write(6,8)' tg2_tg1, dtg2_tg1 ', tg2_tg1, dtg2_tg1
      Endif                               ! Debug
C
C******************************************************************************
C  Step 9: Step 9 is to compute the total delay by adding in the "Best"
C          estimate of the troposphere propogation delay for each site. 
C          Normally this will be done 'on the fly' in program SOLVE.
C          However, we add the Niell atmosphere term here if the atmosphere
C          flag, KATMC is 1. The default is not to add it.
C           [Geocenter station: DATMC(Nzero,1) and DATMC(Nzero,2) should 
C            already be zero.]
C
      IF(KATMC.eq.1) then
         tg2_tg1  =  tg2_tg1 + DATMC(1,1) + DATMC(2,1)
         dtg2_tg1 = dtg2_tg1 + DATMC(1,2) + DATMC(2,2)
C
       IF(KTHED .ne. 0) Then               ! Debug
        write(6,'(/,15x,"Step 9 dump:")')
        write(6,8)'(KATMC=1:) tg2_tg1, dtg2_tg1 ', tg2_tg1, dtg2_tg1
       Endif                               ! Debug
C
      Endif
C
C******************************************************************************
C  Step 10: Correct for axis offset: 
C
C      [ These computations are made in the axis offset module. ]
C
C******************************************************************************
C  Step Gamma: Take the partial derivatives of the delay (tg2_tg1) and the 
C              delay rate (dtg2_tg1) with respect to Gamma. 
C              [Does not include the term for observing close to the Sun.]
C
      Con_part(1) = ( delta_t_grav/(1.d0+Gamma) + U*term2a ) / term4
      Con_part(2) =
     .   ( ( d_delta_t_grav/(1.d0+Gamma) + U*dterm2a + term2a*dU )
     .     / term4 ) - 
     .      (dterm4/term4**2) *
     .     ( delta_t_grav/(1.d0+Gamma) + U*term2a )
C
C  Take partial derivative of the gravitational bending part w.r.t. Gamma.
C    [Does not include the term for observing close to the Sun.]
      Bend_par(1) = ( delta_t_grav/(1.d0+Gamma) ) / term4
      Bend_par(2) = ( d_delta_t_grav/(1.d0+Gamma) ) / term4  - 
     .      (dterm4/term4**2) * ( delta_t_grav/(1.d0+Gamma) )
C
      IF(KTHED .ne. 0) Then               ! Debug
       write(6,'(/,15x,"Step Gamma dump:")')
       write(6,8)' Con_part   ', Con_part
       write(6,8)' Bend_par  ', Bend_par
      Endif                               ! Debug
C
C  Cleanup Step: Finish computation of components for PUT's into data base
C
C   Total light bending portion:
      CON_CNTRB(1) = (   delta_t_grav + Sunplus(1) ) / term4
      CON_CNTRB(2) = ( d_delta_t_grav + Sunplus(2) ) / term4 - 
     .        dterm4 * ( delta_t_grav + Sunplus(1) ) / term4**2
c      write(6,8)' delta_t_grav, d_delta_t_grav ',
c    .             delta_t_grav, d_delta_t_grav
c      write(6,8)' CON_CNTRB ', CON_CNTRB
C
C   Solar light bending portion:
      Sun_cntrb(1) = (   delta_t_grav_Sun + Sunplus(1) ) / term4
      Sun_cntrb(2) = ( d_delta_t_grav_Sun + Sunplus(2) ) / term4 - 
     .        dterm4 * ( delta_t_grav_Sun + Sunplus(1) ) / term4**2
c      write(6,8)' delta_t_grav_Sun, d_delta_t_grav_Sun ',
c    .             delta_t_grav_Sun, d_delta_t_grav_Sun
c      write(6,8)' SUN_CNTRB ', SUN_CNTRB
C
C   Close to the Sun additional light bending portion:
c      write(6,8)' Sunplus   ',Sunplus
      Sunplus(1) =  Sunplus(1)/term4
      Sunplus(2) =  Sunplus(2)/term4 - dterm4*Sunplus(1)/term4**2
c      write(6,8)' Sunplus   ',Sunplus
C
C--------------------------------------------------------------------------
C
    8 FORMAT(A,4D25.16/(7X,5D25.16))
C
      RETURN
      END



