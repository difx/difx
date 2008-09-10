      SUBROUTINE TOCUP 
      IMPLICIT None
C
C 1.    TOCUP
C
C 1.1   TOCUP  PROGRAM SPECIFICATION
C
C 1.1.1 TOCUP calls the 'A' sections of the CALC modules which provide
C       for entries into the database table of contents. TOCUP is called
C       once per data base.
C
C 1.2   TOCUP PROGRAM INTERFACE
C
C 1.2.1 CALLING SEQUENCE -
C             INPUT VARIABLES: NONE
C             OUTPUT VARIABLES: NONE
C
C 1.2.2 COMMON BLOCKS USED - NONE
C
C 1.2.6 SUBROUTINE INTERFACE -
C             CALLER SUBROUTINES: MAIN
C             CALLED SUBROUTINES: ATMA, AXOA, ETDA, PTDA, NUTA, OCEA, 
C                                 PREA, SITA, STRA, UT1A, WOBA, ATIMA,
C                                 CTIMA, PEPA, THERA, STAA, PLXA, 
C                                 DIRNA, M2000A, 
C
C 1.2.7 CONSTANTS USED - NONE
C
C 1.2.8 PROGRAM VARIABLES - None
C
C 1.2.9 PROGRAMMER - DALE MARKHAM  01/12/77
C                    PETER DENATALE 07/26/77
C                    BRUCE SCHUPLER 09/14/78
C                    BRUCE SCHUPLER 01/08/80
C                    BRUCE SCHUPLER 08/26/80
C                    CHOPO MA AND DAVID GORDON 04/09/84
C                    DAVID GORDON   07/12/84  (POLE TIDE)
C                    SAVITA GOEL    06/03/87  (CDS FOR A900)
C                    GREGG COOKE    12/21/88
C                    GREGG COOKE    05/22/89
C                    Jim Ryan       89.07.26 Documentation simplified.
C                    Jim Ryan       89.10.08 Call to RELA removed.
C                    N.Zacharias/D.Gordon 93.10.07 Call to DIRNA
C                                          (equation of equinox)
C                    D. Gordon 94.04.14 Changed to Implicit None
C                    D. Gordon 96.04.02 Added 'Call M2000A', subroutine to do
C                              the ADD for the crust fixed to J2000 rotation
C                              matrix L-code, 'CF2J2000'.
C                    D. Gordon 98.07.23 Removed ASK for 'PEP TAPE'; removed 
C                              unnecessary variables, corrected documentation.
C                    D. Gordon 98.11.12 Removed 'CALL PANA'. Feedbox 
C                              rotation module merged into axis offset module.
C                    D. Gordon 98.11.17 Added subroutine UVA, for 
C                              calculation of missing U,V coordinates.
C                              
C
C     TOCUP Program Structure.
C
C   Provide for the entries to the table of contents for the text messages from
C   the model modules and for the necessary utility routines. Also pass the
C   frequency and the PEP information to the necessary programs.
C
      CALL ATMA
      CALL AXOA
      CALL ETDA
      CALL PTDA
      CALL NUTA
      CALL OCEA
      CALL PREA
      CALL SITA
      CALL STRA
      CALL UT1A
      CALL WOBA
      CALL ATIMA
      CALL CTIMA
      CALL PEPA
      CALL THERA
      CALL STAA
      CALL PLXA
      CALL DIRNA
      CALL M2000A 
      CALL UVA
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   Temporary addition. Remove some Lcodes.
       CALL DELI(1,'PRT FLAG')
       CALL DELI(1,'OBCLFLGS')
       CALL DELI(1,'OBCLLIST')
C
       CALL DELR(2,'CFA22DRY')
       CALL DELR(2,'CFA22WET')
       CALL DELR(2,'CFA PART')
       CALL DELR(2,'LANYIDRY')
       CALL DELR(2,'LANYIWET')
       CALL DELR(2,'LANYINFO')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      END
