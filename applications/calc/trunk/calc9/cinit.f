      SUBROUTINE INITL ( KOUNT )
      IMPLICIT None
C
C 1.    INITL
C
C 1.1   INITL PROGRAM SPECIFICATION
C
C 1.1.1 INITL obtains the mathematical and physical constants from the database
C       and elsewhere and loads them into the common block 'cphys' for their
C       use throughout the program. INITL also calls the input and
C       initialization sections of the model modules and the necessary utility
C       routines. Each section will obtain internally the model module parameter
C       values from the database and initialize all such variables in the local
C       common block. Each section will also put into the header a text message
C       for each model module and necessary utility routine. INITL also
C       initializes a counter which counts the observation number and writes a
C       header text for the observation number, time tag, baseline
C       identification, and source identification.
C       SUBROUTINE INITL IS CALLED ONLY ONCE PER DATA BASE.
C 
C 1.2   INITL PROGRAM INTERFACE 
C 
C 1.2.1 CALLING SEQUENCE -
C             OUTPUT VARIABLES:
C               1. KOUNT - THE VARIABLE WHICH INITIALIZES THE COUNTER
C                          OF THE OBSERVATION ITEMS TO ZERO. (UNITLESS)
C
C 1.2.2 COMMON BLOCKS USED -
C
      INCLUDE 'cphys.i'
C            VARIABLES 'TO':
C              2. EFLAT   - THE FLATTENNING OF THE ELLIPSOID WHICH APPROXIMATES
C                           THE SHAPE OF THE EARTH. (UNITLESS)  (Site module)
C              3. GMMOON  - THE MASS OF THE MOON MULTIPLIED BY THE NEWTONIAN
C                           GRAVITATIONAL CONSTANT. (M**3/SEC**2) 
C              4. GMSUN   - THE MASS OF THE SUN MULTIPLIED BY THE NEWTONIAN
C                           GRAVITATIONAL CONSTANT. (M**3/SEC**2) 
C              5. GMEARTH - THE MASS OF THE EARTH MULTIPLIED BY THE NEWTONIAN
C                           GRAVITATIONAL CONSTANT. (M**3/SEC**2) 
C              6. REARTH  - THE EQUATORIAL RADIUS OF THE EARTH. (M)
C              7. SECPAU  - THE NUMBER OF LIGHT-SECONDS PER ASTRONOMICAL UNIT.
C                           (SEC/A.U.)
C              8. GAMMA   - THE POST NEWTONIAN EXPANSION PARAMETER WHICH AFFECTS
C                           LIGHT BENDING. (1.0 FOR EINSTEIN).
C              9. VLIGHT  - THE VELOCITY OF LIGHT IN VACUUM. (M/SEC)
C             10. VLIGHT2 - THE VELOCITY OF LIGHT SQUARED. ((M/SEC)**2)
C             11. VLIGHT3 - THE VELOCITY OF LIGHT CUBED. ((M/SEC)**3)
C             12. GMPLANET(7)-The masses of 7 of the planets multiplied by the
C                           gravitational constant. (1=Mercury, 2=Venus, 3=Mars,
C                           4=Jupiter, 5=Saturn, 6=Uranus, and 7=Neptune)
C             13. AU_meters-The Astronomical unit. (meters) 
C
      INCLUDE 'ccon.i'
C       Variables 'from':
c              1. ILUOUT  - Output control flag.
c
C 1.2.3 PROGRAM SPECIFICATIONS -
      Integer*2  KERR(12), NDO(3), N, NN, idm1
      Integer*4  KOUNT
C
C 1.2.4 DATA BASE ACCESS -
C
C            'GET' VARIABLES:
C              1. EFLAT, GMMOON, GMSUN, REARTH, SECPAU, VLIGHT, GAMMA
C                               - THE PHYSICAL CONSTANTS.
C
C            ACCESS CODES:
C              1. 'VLIGHT  ' - THE DATABASE ACCESS CODE FOR THE VELOCITY OF
C                              LIGHT IN A VACUUM.
C              3. 'GMSUN   ' - THE DATABASE ACCESS CODE FOR THE MASS OF THE
C                              SUN MULTIPLIED BY THE NEWTONIAN GRAVITATIONAL
C                              CONSTANT. 
C              4. 'GMMOON  ' - THE DATABASE ACCESS CODE FOR THE MASS OF THE
C                              MOON MULTIPLIED BY THE NEWTONIAN GRAVITATIONAL
C                              CONSTANT. 
C              5. 'GMEARTH ' - THE DATABASE ACCESS CODE FOR THE MASS OF THE
C                              EARTH MULTIPLIED BY THE NEWTONIAN GRAVITATIONAL
C                              CONSTANT. 
C              6. 'TSEC/AU ' - THE DATABASE ACCESS CODE FOR THE NUMBER OF
C                              LIGHT-SECONDS PER ASTRONOMICAL UNIT.
C              7. 'EARTHRAD' - THE DATABASE ACCESS CODE FOR THE EQUATORIAL
C                              RADIUS OF THE EARTH.
C              8. 'E-FLAT  ' - THE DATABASE ACCESS CODE FOR THE SQUARE OF THE
C                              ECCENTRICITY OF THE ELLIPSOID WHICH APPROXIMATES
C                              THE SHAPE OF THE EARTH.
C              9. 'REL DATA' - THE DATABASE ACCESS CODE FOR THE POST NEWTONIAN
C                              EXPANSION PARAMETER.
C
C 1.2.5 EXTERNAL INPUT/OUTPUT -
C            OUTPUT VARIABLES:
C              1. THE HEADER TEXT FOR THE OBSERVATION ITEM NUMBER, TIME TAG,
C                 BASELINE IDENTIFICATION, AND SOURCE IDENTIFICATION.
C              2. POSSIBLE ERROR OUTPUT
C
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MAIN
C             CALLED SUBROUTINES: ATIMI, ATMI, AXOI, ETDI, PTDI,
C                                 GET4, KILL, MVREC, NUTI, OCEI,
C                                 PEPI, PREI, RELI, SITI, STRI, THERI,
C                                 UT1I, WOBI, WRIDR, CTIMI, STAI, PLXI
C
C 1.2.8 PROGRAM VARIABLES -
C           1. KERR(3) - THE DATA BASE ERROR RETURN FLAGS.
C           2. NDO(3)  - THE DATA BASE RETURN ARRAY INDICES.
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/12/77
C                    PETER DENATALE 07/27/77
C                    BRUCE SCHUPLER 03/08/78
C                    BRUCE SCHUPLER 09/18/78
C                    BRUCE SCHUPLER 08/26/80
C                    CHOPO MA AND DAVID GORDON 04/09/84
C                    DAVID GORDON   07/13/84   (POLE TIDE)
C                    SAVITA GOEL    06/03/87   (CDS FOR A900)
C                    GREGG COOKE    12/21/88
C                    GREGG COOKE    05/22/89
C                    Jim Ryan 89.07.25 Documentation simplified.
C                    Jim Ryan 89.10.06 CPHYS common made an include file and
C                             GAMMA added to the list. GAMMA now pulled here.
C                    Jim Ryan 89.10.08 Call to RELI removed.
C                    Jim Ryan 89.12.12 UNIX-like database interface
C                             implimented.
C                    David Gordon 93/04/27 GMEARTH added to cphys.i and
C                             defined here.
C                    David Gordon 93/05/06 VLIGHT2 and VLIGHT3 added to
C                             cphys.i and defined here.
C                    David Gordon 93/10/19 GMPLANET(7) added (G times mass of
C                             each planet except Earth and Pluto).
C                    David Gordon 94/04/12 Removed GET4'ing of GAUSS, EMS/MMS,
C                             and U-GRV-CN.
C                    David Gordon 94.04.14 Changed to Implicit None.
C                    David Gordon 94.06.08 Corrected format statement, single
C                                 and double quotes in wrong order. 
C                    David Gordon 95.05.02 Adding AU_meters, Astronomical unit.
C                    David Gordon 96.01.23 Change to take GMEARTH from the
C                                 database. Removal of old L-codes. Cleanup of
C                                 documentation.
C                    David Gordon 98.10.13 Adjusted observation header line
C                                 for change to a 4-digit year.
C                    David Gordon 98.10.14 Removed ACCGRV and its 'GET'
C                                 Also removed ACCGRV from cphys.i.
C                    David Gordon 98.11.12 Removed 'CALL PANI'. Feedbox
C                                 rotation module merged into axis offset
C                                 module. 
C                                 
C   INITL Program Structure.
C
C   Call MVREC to make the header record available and check for db error.
      CALL MVREC ( 1, 1, 1, KERR (1) )
      IF ( KERR (1) .NE. 0 ) CALL CKILL(6HINITL ,1,KERR(1))
C
C   GET the physical constants.
      CALL GET4 ('VLIGHT        ', VLIGHT, 1, 1, 1, NDO, KERR(2) )
        kerr(3) = 0
        kerr(4) = 0
      CALL GET4 ('GMSUN         ', GMSUN,  1, 1, 1, NDO, KERR(5) )
      CALL GET4 ('GMMOON        ', GMMOON, 1, 1, 1, NDO, KERR(6) )
      CALL GET4 ('TSEC/AU       ', SECPAU, 1, 1, 1, NDO, KERR(7) )
      CALL GET4 ('EARTHRAD      ', REARTH, 1, 1, 1, NDO, KERR(8) )
      CALL GET4 ('GMEARTH       ', GMEARTH,1, 1, 1, NDO, KERR(9) )
       if (kerr(9) .ne. 0) then    !old database, insert GM-Earth 
        gmearth = .3986004418d+15 
        kerr(9) = 0
       endif
        kerr(10) = 0
      CALL GET4 ('E-FLAT        ', EFLAT, 1, 1, 1, NDO, KERR(11) )
      CALL GET4 ('REL DATA      ', GAMMA, 1, 1, 1, NDO, KERR(12) )
C  Compute GM of planets. Reciprocal solar mass units from IERS Technical
C   Note 13. 93OCT19, D. Gordon
      GMPLANET(1) = GMSUN / 6023600.D0          ! Mercury 
      GMPLANET(2) = GMSUN /  408523.71D0        ! Venus
      GMPLANET(3) = GMSUN / 3098708.D0          ! Mars
      GMPLANET(4) = GMSUN /    1047.3486D0      ! Jupiter
      GMPLANET(5) = GMSUN /    3497.90D0        ! Saturn
      GMPLANET(6) = GMSUN /   22902.94D0        ! Uranus
      GMPLANET(7) = GMSUN /   19412.24D0        ! Neptune
C  Compute square and cube of velocity of light. 93MAY06, D. Gordon
      VLIGHT2 = VLIGHT * VLIGHT
      VLIGHT3 = VLIGHT2 * VLIGHT
C  Compute the Astronomical unit (meters). DG 95MAY02
      AU_meters = vlight * secpau
C
C  Check for database errors. If an error, KILL
      DO  N = 2,12
        NN = N
        IF ( KERR(N) .NE. 0 ) CALL CKILL (6HINITL ,NN,KERR(NN))
      ENDDO
C
C  Provide for the input and initializations of the model modules and of the
C  necessary utility routines and for the adding to the header of the
C  corresponding text messages.
C
      CALL STAI
      CALL ATMI
      CALL AXOI
      CALL ETDI
      CALL PTDI
      CALL NUTI
      CALL OCEI
      CALL PREI
      CALL SITI
      CALL STRI
      CALL UT1I
      CALL WOBI
      CALL ATIMI
      CALL CTIMI
      CALL PEPI
      CALL THERI
      CALL PLXI
C
C  Write the header record to the output database.
      CALL WRIDR
C
C  Initialize the observation counter to 0.
      KOUNT = 0
C
C  Write the observation banner.
      If(ILUOUT.ne.-1)
     .  WRITE(6,'(/,12X,13("*"),"The observation header",
     .         12("*"),/" Number Year Mn Dy Hr Mn  Sec  ",
     .         "   Baseline       Source  ")')
C
C     Normal conclusion.
      RETURN
      END
