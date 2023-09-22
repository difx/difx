       SUBROUTINE TDATECW(Y,M,D,JD,MNAME,DNAME)
C
C               Input: date (y/m/d) in Y,M,D   (may be 'unnormalized')
C               Output: 'normalized' date in Y/M/D, julian day
C               number at 12 hrs UT (JD), name of month (MNAME),
C               day of week (DNAME)
C
C               TJP  1978 March 8
C               RCW  1991 April 12  Convert to characters for MNAME and
C                                   DNAME
C
        CHARACTER MLIST(12)*3, MNAME*3, DNAME*3, DLIST(7)*3
        INTEGER MLENG(12), Y, M, D, JD, J, NYRM1, IC, NDAYS, LEAP, ML
        DATA MLIST/'Jan','Feb','Mar','Apr','May','Jun',
     1             'Jul','Aug','Sep','Oct','Nov','Dec'/
        DATA MLENG/31,28,31,30,31,30,31,31,30,31,30,31/
        DATA DLIST/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
C
C               JD number at 12 hrs UT on Jan 0 of year Y
C               (Gregorian calendar)
C
	IF(M.GE.1 .AND. M.LE.12) GOTO 1
		Y = Y + (M-1)/12
		M = MOD(M-1,12)+1
		IF(M.LT.0) Y=Y-1
		IF(M.LT.0) M=M+12
    1   NYRM1=Y-1
        IC=NYRM1/100
        JD=1721425 + 365*NYRM1 + NYRM1/4 - IC + IC/4
C
C               Day number; is it a leap year?
C
        NDAYS=D
        IF(M.EQ.1) GOTO 20
        DO 10 J=2,M
   10   NDAYS=NDAYS+MLENG(J-1)
   20   LEAP=0
        IF((MOD(Y,4).EQ.0.AND.MOD(Y,100).NE.0).OR.(MOD(Y,400).EQ.0))
     1          LEAP=1
        IF(M.GT.2) NDAYS=NDAYS+LEAP
C
C               Return JD and DNAME
C
        JD=JD+NDAYS
        DNAME=DLIST(1+MOD(JD,7))
C
C               Convert back to M/D
C
        DO 30 J=1,12
        ML=MLENG(J)
        IF(J.EQ.2) ML=ML+LEAP
        IF(NDAYS.LE.ML) GOTO 40
   30   NDAYS=NDAYS-ML
        Y=Y+1
        M=1
        D=NDAYS
        GOTO 1
C
   40   M=J
        D=NDAYS
        MNAME=MLIST(J)
        RETURN
        END

