      SUBROUTINE GAUFIT( X, Y, SIG, NPTS, NGAU, GAMP, GPOS, GWID,
     1                   AMP, POS, WID, AMPE, POSE, WIDE, FIT )
C
C     A routine to NGAU gaussians to data points (X,Y) with errors SIG.  
C     GAMP, GPOS, and GWID are first guesses for each gaussian.  AMP, 
C     POS, and WID are the fit results with errors AMPE, POSE, and WIDE.
C     FIT is an array of the desired parameters to fit (for example,
C     FIT = 1,3,4,5 causes the program to solve for both amplitudes,
C     the width of the first component, and the position of the second).
C     The widths are the full width half maximum (FWHM)
C     Currently set for a maximum of 4 gaussians.
C
C     Numerical Recipes routine MRQMIN is used for the fit.  Following
C     example program D14R8A closely.
C     Routine names have initial G appended to avoid library conflicts
C     in ROTANAL.  Eventually, I should link with standard library.
C
C     If NGAU=1 and NPTS=3, an analytic solution will be used using
C     subroutine TOP lifted from FITMON.
C
C     If the errors are negative, the fit was bad for some reason
C     and should be ignored.
C
      PARAMETER (MGAU=4)
      PARAMETER (MFIT=MGAU*3)
      INTEGER FIT(MFIT), NGAU, NPTS
      REAL    X(NPTS), Y(NPTS), SIG(NPTS)
      REAL    GAMP(NGAU), GPOS(NGAU), GWID(NGAU)
      REAL    AMP(NGAU),  POS(NGAU),  WID(NGAU)
      REAL    AMPE(NGAU), POSE(NGAU), WIDE(NGAU)
C
      EXTERNAL  FGAUSS
      REAL    COVAR(MFIT,MFIT), ALPHA(MFIT,MFIT), A(MFIT)
      REAL    WIDCON, SIGCON
      PARAMETER  (WIDCON = 0.6005612)  !  Ratio of 1/e width to FWHM
      PARAMETER  (SIGCON = 0.424660900) ! Ratio of sigma to FWHM
C ----------------------------------------------------------------------
C     In most cases, do a least squares fit.
C
      IF( NPTS .LT. 3 * NGAU ) THEN
         WRITE(*,'(A,I3,A,I3,A)' )
     1       ' Too few points (', NPTS,') to fit ',NGAU, ' gaussians.'
         DO I = 1, NGAU
            AMP(I) = 0.0
            POS(I) = 0.0
            WID(I) = 0.0
            AMPE(I) = -99.
            POSE(I) = -99.
            WIDE(I) = -99.
         END DO
      ELSE IF( NGAU .NE. 1 .OR. NPTS .NE. 3 ) THEN
C
C        The following prints seem to be needed in debugging all too
C        often.
C
C         WRITE(*,*) ' In GAUFIT. NGAU = ', NGAU, '  NPTS = ', NPTS
C         DO I = 1, NPTS
C            WRITE(*,*) ' X,Y,SIG ', X(I), Y(I), SIG(I)
C         END DO
C
C        Set up the fit arrays.  Note that FGAUSS is based on the 1/e width,
C        not the FWHM.
C
         NPAR = NGAU * 3
C
         DO I = 1, NGAU
            IFIT = (I-1)*3
            A(IFIT+1) = GAMP(I)
            A(IFIT+2) = GPOS(I)
            A(IFIT+3) = GWID(I) * WIDCON
         END DO
C
         DO I = 1, NPAR
            IF( FIT(I) .GT. 0 ) NFIT = I
         END DO
C
         ALAMDA = -1
         CALL GMRQMIN( X, Y, SIG, NPTS, A, NPAR, FIT, NFIT, COVAR, 
     1        ALPHA, MFIT, CHISQ, FGAUSS, ALAMDA )
C
         ITST = 0
  100    CONTINUE
            OCHISQ = CHISQ
            CALL GMRQMIN( X, Y, SIG, NPTS, A, NPAR, FIT, NFIT, COVAR, 
     1             ALPHA, MFIT, CHISQ, FGAUSS, ALAMDA )
            IF( CHISQ .GT. OCHISQ) THEN
               ITST = 0
            ELSE IF( ABS( OCHISQ-CHISQ ) .LT. 0.1 ) THEN
               ITST = ITST + 1
            END IF
            IF( ITST .LT. 2 ) THEN
               GO TO 100
            END IF
C
         ALAMDA = 0.0
         CALL GMRQMIN( X, Y, SIG, NPTS, A, NPAR, FIT, NFIT, COVAR, 
     1         ALPHA, MFIT, CHISQ, FGAUSS, ALAMDA )
C
C        Now get the answers.
C
         DO I = 1, NGAU
            IFIT = (I-1)*3
            AMP(I) = A(IFIT+1)
            POS(I) = A(IFIT+2)
            WID(I) = A(IFIT+3) / WIDCON
            IF( COVAR(IFIT+1,IFIT+1) .GE. 0.0 .AND.
     1          COVAR(IFIT+2,IFIT+2) .GE. 0.0 .AND.
     2          COVAR(IFIT+3,IFIT+3) .GE. 0.0 ) THEN
               AMPE(I) = SQRT( COVAR(IFIT+1,IFIT+1) )
               POSE(I) = SQRT( COVAR(IFIT+2,IFIT+2) )
               WIDE(I) = SQRT( COVAR(IFIT+3,IFIT+3) ) / WIDCON
            ELSE
               AMPE(I) = -99.
               POSE(I) = -99.
               WIDE(I) = -99.
            END IF
         END DO
C
C         WRITE(*,*) ' 1st component amp, pos, wid, errors ', 
C     1          amp(1), pos(1), wid(1), ampe(1), pose(1), wide(1)
C
      ELSE  !  3 data points, 1 gaussian.
C
         CALL TOP( Y, X, POS(1), WID(1), AMP(1) )
         WID(1) = WID(1) / SIGCON
         AMPE(1) = 0.0
         POSE(1) = 0.0
         WIDE(1) = 0.0 / SIGCON
C         write(*,*) '  just topped ', amp(1), pos(1), wid(1), widcon
C
      END IF
C
      RETURN
      END
C
C     The numerical recipes routines:
C
      SUBROUTINE GMRQMIN(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *    COVAR,ALPHA,NCA,CHISQ,GFUNCS,ALAMDA)
      PARAMETER (MMAX=20)
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),
     *  COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX)
      EXTERNAL GFUNCS
      IF(ALAMDA.LT.0.)THEN
        KK=MFIT+1
        DO 12 J=1,MA
          IHIT=0
          DO 11 K=1,MFIT
            IF(LISTA(K).EQ.J)IHIT=IHIT+1
11        CONTINUE
          IF (IHIT.EQ.0) THEN
            LISTA(KK)=J
            KK=KK+1
          ELSE IF (IHIT.GT.1) THEN
            PAUSE 'Improper permutation in LISTA'
          ENDIF
12      CONTINUE
        IF (KK.NE.(MA+1)) PAUSE 'Improper permutation in LISTA'
        ALAMDA=0.001
        CALL GMRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,
     *GFUNCS)
        OCHISQ=CHISQ
        DO 13 J=1,MA
          ATRY(J)=A(J)
13      CONTINUE
      ENDIF
      DO 15 J=1,MFIT
        DO 14 K=1,MFIT
          COVAR(J,K)=ALPHA(J,K)
14      CONTINUE
        COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)
        DA(J)=BETA(J)
15    CONTINUE
      CALL GGAUSSJ(COVAR,MFIT,NCA,DA,1,1)
      IF(ALAMDA.EQ.0.)THEN
        CALL GCOVSRT(COVAR,NCA,MA,LISTA,MFIT)
        RETURN
      ENDIF
      DO 16 J=1,MFIT
        ATRY(LISTA(J))=A(LISTA(J))+DA(J)
16    CONTINUE
      CALL GMRQCOF(X,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,
     *GFUNCS)
      IF(CHISQ.LT.OCHISQ)THEN
        ALAMDA=0.1*ALAMDA
        OCHISQ=CHISQ
        DO 18 J=1,MFIT
          DO 17 K=1,MFIT
            ALPHA(J,K)=COVAR(J,K)
17        CONTINUE
          BETA(J)=DA(J)
          A(LISTA(J))=ATRY(LISTA(J))
18      CONTINUE
      ELSE
        ALAMDA=10.*ALAMDA
        CHISQ=OCHISQ
      ENDIF
      RETURN
      END
      SUBROUTINE GCOVSRT(COVAR,NCVM,MA,LISTA,MFIT)
      DIMENSION COVAR(NCVM,NCVM),LISTA(MFIT)
      DO 12 J=1,MA-1
        DO 11 I=J+1,MA
          COVAR(I,J)=0.
11      CONTINUE
12    CONTINUE
      DO 14 I=1,MFIT-1
        DO 13 J=I+1,MFIT
          IF(LISTA(J).GT.LISTA(I)) THEN
            COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
          ELSE
            COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
          ENDIF
13      CONTINUE
14    CONTINUE
      SWAP=COVAR(1,1)
      DO 15 J=1,MA
        COVAR(1,J)=COVAR(J,J)
        COVAR(J,J)=0.
15    CONTINUE
      COVAR(LISTA(1),LISTA(1))=SWAP
      DO 16 J=2,MFIT
        COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
16    CONTINUE
      DO 18 J=2,MA
        DO 17 I=1,J-1
          COVAR(I,J)=COVAR(J,I)
17      CONTINUE
18    CONTINUE
      RETURN
      END
      SUBROUTINE FGAUSS(X,A,Y,DYDA,NA)
      DIMENSION A(NA),DYDA(NA)
      Y=0.
      DO 11 I=1,NA-1,3
        ARG=(X-A(I+1))/A(I+2)
        EX=EXP(-ARG**2)
        FAC=A(I)*EX*2.*ARG
        Y=Y+A(I)*EX
        DYDA(I)=EX
        DYDA(I+1)=FAC/A(I+2)
        DYDA(I+2)=FAC*ARG/A(I+2)
11    CONTINUE
      RETURN
      END
      SUBROUTINE GGAUSSJ(A,N,NP,B,M,MP)
      PARAMETER (NMAX=50)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                PAUSE 'Singular matrix'
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN
      END
      SUBROUTINE GMRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NALP,
     *CHISQ,GFUNCS)
      PARAMETER (MMAX=20)
      EXTERNAL GFUNCS
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),ALPHA(NALP,NALP),BETA(MA),
     *    DYDA(MMAX),LISTA(MFIT),A(MA)
      DO 12 J=1,MFIT
        DO 11 K=1,J
          ALPHA(J,K)=0.
11      CONTINUE
        BETA(J)=0.
12    CONTINUE
      CHISQ=0.
      DO 15 I=1,NDATA
        CALL GFUNCS(X(I),A,YMOD,DYDA,MA)
        SIG2I=1./(SIG(I)*SIG(I))
        DY=Y(I)-YMOD
        DO 14 J=1,MFIT
          WT=DYDA(LISTA(J))*SIG2I
          DO 13 K=1,J
            ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
13        CONTINUE
          BETA(J)=BETA(J)+DY*WT
14      CONTINUE
        CHISQ=CHISQ+DY*DY*SIG2I
15    CONTINUE
      DO 17 J=2,MFIT
        DO 16 K=1,J-1
          ALPHA(K,J)=ALPHA(J,K)
16      CONTINUE
17    CONTINUE
      RETURN
      END
      SUBROUTINE TOP( A, X, X0, S, AMP )
C
C     Does the actual solution for the gaussian parameters.
C
      REAL*4    A(3), X(3), X0, S, AMP, LA(3)
      REAL*4    DENOM, NUMER
C
C     Take log base e of amplitudes.  Ta's are always positive.
C
      LA(1) = LOG( A(1) )
      LA(2) = LOG( A(2) )
      LA(3) = LOG( A(3) )
C
C     Get parts for equation for position of peak.
C
      NUMER = (LA(2)-LA(3))*X(1)**2 + (LA(3)-LA(1))*X(2)**2 +
     1        (LA(1)-LA(2))*X(3)**2
      DENOM = (LA(2)-LA(3))*X(1) + (LA(3)-LA(1))*X(2) +
     1        (LA(1)-LA(2))*X(3)
C
C     Protect against divide by zero.
C
      IF( DENOM .EQ. 0.0 .OR. X(1) .EQ. X(2) .OR. X(2) .EQ. X(3) ) THEN
         X0 = X(2)
         S = 1.E10
         AMP = -999. 
      ELSE
         X0 = 0.5 * NUMER / DENOM
         IF( ABS( LA(2) - LA(1) ) .GT. ABS( LA(2) - LA(3) ) ) THEN
            S = -0.5 * ( (X(1)+X(2)-2.0*X0) * (X(2)-X(1)) ) /
     1                     (LA(2)-LA(1))
         ELSE
            S = -0.5 * ( (X(3)+X(2)-2.0*X0) * (X(2)-X(3)) ) /
     1                     (LA(2)-LA(3))
         END IF
         IF( S .LE. 0 ) S = 1.E20
         S = SQRT( S )
         AMP = A(2) * EXP( ((X(2)-X0)**2) / (2.0*S*S) )
      END IF
C
      RETURN
      END
