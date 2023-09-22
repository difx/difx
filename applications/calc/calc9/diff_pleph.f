1c1,2
< C*************************************************************************
---
> C++++++++++++++++++++++++++
> C
3,5d3
<       Implicit None
< C  The following code is from JPL with only a few necessary modifications:
< C*************************************************************************
6a5
> C++++++++++++++++++++++++++
10,11c9,11
< C  for this inconsistency; in this present version, we use only the four 
< C  necessary arguments and do the testing outside of the subroutine.
---
> C  for this inconsistency; in this present version, we use only the four necessary 
> C  arguments and do the testing outside of the subroutine.
> C
13,14c13,16
< C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS AND GIVES THE 
< C     POSITION AND VELOCITY OF THE POINT 'NTARG' WITH RESPECT TO 'NCENT'.
---
> C
> C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS
> C     AND GIVES THE POSITION AND VELOCITY OF THE POINT 'NTARG'
> C     WITH RESPECT TO 'NCENT'.
18c20,21
< C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION IS WANTED.
---
> C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION
> C            IS WANTED.
21c24,25
< C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE SUBROUTINE STATE
---
> C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE 
> C          SUBROUTINE STATE
23a28
> C
26a32
> C
34c40,41
< C                                     15 = LIBRATIONS, IF ON EPH FILE
---
> C                            15 = LIBRATIONS, IF ON EPH FILE
> C
48,49c55,60
<       REAL*8  RRD(6),ET2Z(2),ET2(2),PV(6,13), ET, AU, EMRAT
<       REAL*8  SS(3),CVAL(400),PVSUN(6)
---
> 
>       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
> 
>       DIMENSION RRD(6),ET2Z(2),ET2(2),PV(6,13)
>       DIMENSION SS(3),CVAL(400),PVSUN(6)
> 
53c64,66
<       INTEGER*4 LIST(12), IPT(39), DENUM, NTARG, NCENT, NCON, I, K
---
> 
>       INTEGER LIST(12),IPT(39),DENUM
> 
55a69
> 
58c72,73
< C  INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
---
> C     INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
> C
61,62c76,77
< C     GO TO 11
< C
---
>       GO TO 11
> 
65,68c80,85
< C     ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)
< C     ET2(1)=ET2Z(1)
< C     ET2(2)=ET2Z(2)
< C
---
> 
>       ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)
> 
>       ET2(1)=ET2Z(1)
>       ET2(2)=ET2Z(2)
> 
72c89
< C
---
> 
75c92
< C
---
> 
77c94
< C
---
> 
81,82c98,100
< C
< C   CHECK FOR NUTATION CALL
---
> 
> C     CHECK FOR NUTATION CALL
> 
93,94c111,113
< C
< C   CHECK FOR LIBRATIONS
---
> 
> C     CHECK FOR LIBRATIONS
> 
108,109c127,129
< C
< C   FORCE BARYCENTRIC OUTPUT BY 'STATE'
---
> 
> C       FORCE BARYCENTRIC OUTPUT BY 'STATE'
> 
112,113c132,134
< C
< C   SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL
---
> 
> C       SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL
> 
122,123c143,145
< C
< C   MAKE CALL TO STATE
---
> 
> C       MAKE CALL TO STATE
> 
125c147
< C
---
> 
131c153
< C
---
> 
137c159
< C
---
> 
143c165
< C
---
> 
150c172
< C
---
> 
156c178
< C
---
> 
162c184
< C
---
> 
166c188
< C
---
> 
168c190
< C
---
> 
170a193
> C+++++++++++++++++++++++++++++++++
172d194
< C**************************************************************************
174c196,197
<       Implicit None
---
> C
> C+++++++++++++++++++++++++++++++++
181a205
> C
182a207
> C
186a212
> C
187a214
> C
188a216
> C
190a219
> C
193a223
> C
194a225
> C
197a229,231
> C
>       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
> C
199,203c233,235
< C WEW - move following line from after to before REAL*8 declaration
<       INTEGER*4 NCF, NCM, NA, IFL, L, NP, NV, I, J
<       REAL*8  BUF(NCF,NCM,*), T(2), PV(NCM,*), PC(18), VC(18),
<      *        DNA, DT1, TEMP, TC, TWOT, VFAC 
< C      INTEGER*4 NCF, NCM, NA, IFL, L, NP, NV, I, J
---
> C
>       DOUBLE PRECISION BUF(NCF,NCM,*),T(2),PV(NCM,*),PC(18),VC(18)
> 
220c252,253
< C   TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)
---
> C         TC IS THE NORMALIZED CHEBYSHEV TIME (-1 .LE. TC .LE. 1)
> 
222c255
< C
---
> 
227c260
< C
---
> 
235,236c268,270
< C  BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
< C  AND ARE STORED IN THE ARRAY 'PC'.
---
> C       BE SURE THAT AT LEAST 'NCF' POLYNOMIALS HAVE BEEN EVALUATED
> C       AND ARE STORED IN THE ARRAY 'PC'.
> C
244c278,279
< C   INTERPOLATE TO GET POSITION FOR EACH COMPONENT
---
> C       INTERPOLATE TO GET POSITION FOR EACH COMPONENT
> C
253,254c288,290
< C  IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
< C  DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
---
> C       IF VELOCITY INTERPOLATION IS WANTED, BE SURE ENOUGH
> C       DERIVATIVE POLYNOMIALS HAVE BEEN GENERATED AND STORED.
> C
264c300,301
< C  INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
---
> C       INTERPOLATE TO GET VELOCITY FOR EACH COMPONENT
> C
273a311
> C
274a313,314
> 
> C+++++++++++++++++++++++++
276d315
< C***********************************************************************
278d316
<       Implicit None
280,281c318,323
< C   THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
< C   AND A D.P. FRACTIONAL PART.
---
> C+++++++++++++++++++++++++
> C
> C     THIS SUBROUTINE BREAKS A D.P. NUMBER INTO A D.P. INTEGER
> C     AND A D.P. FRACTIONAL PART.
> C
> C     CALLING SEQUENCE PARAMETERS:
283d324
< C   CALLING SEQUENCE PARAMETERS:
284a326
> C
286,287c328,330
< C            FR(1) CONTAINS INTEGER PART.
< C            FR(2) CONTAINS FRACTIONAL PART.
---
> C            FR(1) CONTAINS INTEGER PART
> C            FR(2) CONTAINS FRACTIONAL PART
> C
291,292c334
< C   CALLING SEQUENCE DECLARATIONS
<       REAL*8 TT, FR(2)
---
> C       CALLING SEQUENCE DECLARATIONS
294c336,341
< C   MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS
---
>       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
> 
>       DIMENSION FR(2)
> 
> C       MAIN ENTRY -- GET INTEGER AND FRACTIONAL PARTS
> 
297c344
< C
---
> 
299,300c346,348
< C
< C   MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER
---
> 
> C       MAKE ADJUSTMENTS FOR NEGATIVE INPUT NUMBER
> 
303c351
< C
---
> 
304a353
> 
305a355,357
> 
> 
> C++++++++++++++++++++++++++++++++
307d358
< C********************************************************************
309c360,361
<       Implicit None
---
> C
> C++++++++++++++++++++++++++++++++
315a368
> C
318a372
> C
320a375
> C
324a380
> C
327a384
> C
329a387
> C
332a391
> C
333a393
> C
346a407
> C
348c409,410
< C          PV     DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
---
> C
> C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
356c418,419
< C                 ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
---
> C
> C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
359c422,423
< C                 THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
---
> C
> C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
362c426,427
< C                 LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
---
> C
> C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
364c429,430
< C         NUT     DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
---
> C
> C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
366a433
> C
371c438,439
< C           *     STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
---
> C
> C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
373a442
> C
374a444
> C
379a450
> C
384a456
> C
387a460,462
> C
>       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
> 
389,397c464,469
<       REAL*8    ET2(2),PV(6,12),PNUT(4),T(2),PJD(4),BUF(1500),
<      . SS(3),CVAL(400),PVSUN(6,1), AU, EMRAT, S, AUFAC
< C    . SS(3),CVAL(400),PVSUN(3,2), AU, EMRAT, S, AUFAC
< C     INTEGER*4 LIST(12),IPT(3,13), NUMDE, NCON, NRECL, KSIZE, NRFILE,
<       INTEGER*4 LIST(12),IPT(3,13), NUMDE, NCON, NRECL,        NRFILE,
<      *          IRECSZ, NCOEFFS, I, J, K, NRL, NR
<       INTEGER*4 I2, I3
<       DATA I2 /2/
<       DATA I3 /3/
---
> 
>       DIMENSION ET2(2),PV(6,12),PNUT(4),T(2),PJD(4),BUF(1500),
>      . SS(3),CVAL(400),PVSUN(3,2)
> 
>       INTEGER LIST(12),IPT(3,13)
> 
399a472
> 
401c474,475
< C     CHARACTER*80 NAMFIL
---
>       CHARACTER*80 NAMFIL
> 
403,408c477
< C
<       INCLUDE 'param.i'
< C       Variables from:
< C         1. JPL_eph - Character string giving the complete path name of 
< C                      the JPL DE403 ephemeris.
< C
---
> 
412c481,483
<        DATA KM/.TRUE./
---
> 
> C
> C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
414d484
< C   ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
417,431c487,513
< C
<       NRECL=4
<       NRFILE=12
< C     NAMFIL= 'JPL.DE403'
< C     KSIZE = 1796
< C
< C     IRECSZ=NRECL*KSIZE
< C     NCOEFFS=KSIZE/2
<       NCOEFFS = IRECL/NRECL/2
< C
< C     OPEN(NRFILE, FILE=NAMFIL, ACCESS='DIRECT', FORM='UNFORMATTED',
< C    *     RECL=IRECSZ, STATUS='OLD')
<       OPEN(NRFILE, FILE=JPL_eph, ACCESS='DIRECT', FORM='UNFORMATTED',
<      *     RECL=IRECL, STATUS='OLD')
< C
---
> 
> C ************************************************************************
> C ************************************************************************
> 
> C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1
> 
> C ************************************************************************
> 
> C        CALL FSIZER1(NRECL,KSIZE,NRFILE,NAMFIL)
> C        CALL FSIZER2(NRECL,KSIZE,NRFILE,NAMFIL)
>         CALL FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)
> 
>       IF(NRECL .EQ. 0) WRITE(*,*)'  ***** FSIZER IS NOT WORKING *****'
> 
> C ************************************************************************
> C ************************************************************************
> 
>       IRECSZ=NRECL*KSIZE
>       NCOEFFS=KSIZE/2
> 
>         OPEN(NRFILE,
>      *       FILE=NAMFIL,
>      *       ACCESS='DIRECT',
>      *       FORM='UNFORMATTED',
>      *       RECL=IRECSZ,
>      *       STATUS='OLD')
> 
434,439d515
< C
<       READ(NRFILE,REC=2)CVAL
< 
< c wew: Add
<       write(6,'(/" Opened JPL DE200 Ephemeris - MJD range: ",
<      .                  2f12.1/)' ) SS(1), SS(2)
440a517
>       READ(NRFILE,REC=2)CVAL
442,447d518
<        write(6,'("TTL: ", 3(14(a6)/))') TTL
<        write(6,'("CNAM: ",40(10(a6,1x)/))') CNAM
<        write(6,'("SS: ",3d25.16)') SS  
<        write(6,'("NCON, NUMDE: ",2i10)') NCON, NUMDE  
<        write(6,'("AU, EMRAT: ",2d25.16)') AU, EMRAT    
<        write(6,'("IPT: ",3(13i8,/))') IPT 
449c520
< C
---
> 
451c522,523
< C
---
> 
> 
452a525,526
> 
> 
454c528
< C
---
> 
462c536
< C
---
> 
463a538
> 
465c540
< C
---
> 
466a542
> 
470c546
< C
---
> 
471a548
> 
474d550
< C      print *, '!!!!!!!!!! STATE: NR = ', NR
477c553
< C
---
> 
485c561
< C
---
> 
487,488c563,565
< C     CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)
<       CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),I3,IPT(3,11),I2,PVSUN)
---
> 
>       CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)
> 
490c567
<        PVSUN(I,1)=PVSUN(I,1)*AUFAC
---
>       PVSUN(I,1)=PVSUN(I,1)*AUFAC
492c569
< C
---
> 
494c571
< C
---
> 
496,509c573,585
<        IF(LIST(I).EQ.0) GO TO 4
< C      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
< C    &  LIST(I),PV(1,I))
<        CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),I3,IPT(3,I),
<      &  LIST(I),PV(1,I))
< C
<        DO J=1,6
<         IF(I.LE.9 .AND. .NOT.BARY) THEN
<         PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
<         ELSE
<         PV(J,I)=PV(J,I)*AUFAC
<         ENDIF
<        ENDDO
< C
---
>       IF(LIST(I).EQ.0) GO TO 4
> 
>       CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
>      & LIST(I),PV(1,I))
> 
>       DO J=1,6
>        IF(I.LE.9 .AND. .NOT.BARY) THEN
>        PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
>        ELSE
>        PV(J,I)=PV(J,I)*AUFAC
>        ENDIF
>       ENDDO
> 
511,516c587,591
< C
< C   DO NUTATIONS IF REQUESTED (AND IF ON FILE)
<       IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0) THEN
< C      CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
< C    * LIST(11),PNUT)
<        CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),I2,IPT(3,12),
---
> 
> C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)
> 
>       IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0)
>      * CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
518,524c593,597
<       ENDIF
< C
< C   GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
<       IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0) THEN
< C      CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
< C    * LIST(12),PV(1,11))
<        CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),I3,IPT(3,13),
---
> 
> C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
> 
>       IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0)
>      * CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
526,527c599
<       ENDIF
< C
---
> 
529,530c601,602
< C
<   98  WRITE(6,198)ET2(1)+ET2(2),SS(1),SS(2)
---
> 
>   98  WRITE(*,198)ET2(1)+ET2(2),SS(1),SS(2)
533,536c605,609
< C
<       STOP
< C
<    99 WRITE(6,'(2F12.2,"  ERROR RETURN IN STATE")') ET2
---
> 
>       stop
> 
>    99 WRITE(*,'(2F12.2,A80)')ET2,'ERROR RETURN IN STATE'
> 
537a611
> 
