
	program fred

	implicit none

C       COMMON AREA FOR CONSTANTS AND POINTERS IN RECORD 1
      Real*8 SS(3),CVAL(400),AU,EMRAT
      INTEGER*4 NCON,IPT(36),DENUM,LPT(3)
C     COMMON/EPHHDR/SS,NCON,CVAL,AU,EMRAT,IPT,DENUM                             
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,IPT,DENUM,LPT 
C
	call ephopn

	type *, ss(1), ss(2), ss(3)

	end



C******************************************************************************
      SUBROUTINE EPHOPN                                                         
      Implicit None
C                                                                               
C     THIS SUBROUTINE OPENS THE JPL PLANETARY EPHEMERIS FILE,                   
C     EXPECTED TO BE 'JPLEPH'.                                                  
C                                                                               
C       Programmers:
C              David Gordon 94.04.18 Converted to Implicit None.
C              David Gordon 94.04.20 Put JPL ephemeris file name into 'param.i'
C                           include file as character string JPL_eph.
C   
      Include 'params.i'
C
C      Variables From:
C        1. JPL_eph - Character string giving the complete path name of the JPL
C                     DE/LE200 ephemeris.
C
      SAVE                                                                      
C                                                                               
C       COMMON AREA FOR CHARACTER DATA IN RECORD 1                              
      CHARACTER*6 TTL(14,3)
      CHARACTER*6 CNAM(400)
      COMMON/CHRHDR/TTL,CNAM
C
C       COMMON AREA FOR CONSTANTS AND POINTERS IN RECORD 1
      Real*8 SS(3),CVAL(400),AU,EMRAT
      INTEGER*4 NCON,IPT(36),DENUM,LPT(3)
C     COMMON/EPHHDR/SS,NCON,CVAL,AU,EMRAT,IPT,DENUM                             
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,IPT,DENUM,LPT 
C
      INTEGER*4 FILE
      COMMON/EPUNIT/FILE
C                                                                               
      LOGICAL FIRST                                                             
      DATA FIRST/.TRUE./                                                        
C                                                                               
C       OPEN FILE AND READ RECORD # 1 ON FIRST ENTRY ONLY                       
      IF(FIRST) THEN                                                            
        FIRST=.FALSE.                                                           
C            OPEN(UNIT=FILE, FILE='/data/super3/jpl/de200/JPLEPH.',
C
C (mjk, 5/12/94)  change RECL from 8000 to 7200 - for UNIX
C
C (wew, 27/2/96)  change RECL from 7200 to 1800 - for Alpha/VMS
C    - for UNFORMATTED files, record length is in longwords, not bytes.
C
        OPEN(UNIT=FILE, FILE=JPL_eph,
C     *       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=8000,
C     *       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1800,
     *       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=EPH_RECL,
     *       STATUS='OLD')
C                                                      
        READ(FILE,REC=1)TTL,CNAM,SS,NCON,CVAL,AU,EMRAT,IPT,DENUM                
      ENDIF                                                                     
C                                                                               
      RETURN                                                                    
      END                                                                       
