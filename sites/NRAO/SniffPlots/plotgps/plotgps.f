      PROGRAM PLOTGPS
C
C     Plot Mark IV log gps-fmout.
C     Fit for clock offsets and rates.
C
C     For large offsets, don't attempt to put out data because
C     of the potential for precision problems.
C
C     Adjusting offsets to zero KK.  May 2, 2006.
C
      INTEGER   MPT
      PARAMETER (MPT=2000)
      LOGICAL   NEWFMT
      INTEGER   I, J, IY, ID, IH, IM, IS, NPT, ICH, ICH1, ICH2, IE
      INTEGER   IPT, LIY, MDAY, IMONTH, JD, IP, JCH, NCH, IPCH
      REAL      TIME(MPT), GPS(MPT), SIG(MPT), FTIME(MPT)
      REAL      XMIN, XMAX, YMIN, YMAX, XLO, XHI, YLO, YHI
      REAL      XRNG, YRNG
      REAL      AVG, RATE, SAVG, SRATE, CHI2, Q, JOBRATE, JOBOFF
      REAL      RATESS, SRATESS, TIMEMID, DTIME
      REAL      XL(2), YL(2), RM, RH, NPRT
      DOUBLE PRECISION  RGPS, DCOFF, JOBAVG, MULT
      CHARACTER INFILE*80, PLOTFILE*80, INLINE*128
      CHARACTER CLKFILE*80
      CHARACTER STACODE*2, DATE*19, MONTH*3, DNAME*3
      INTEGER   MOFF
C
C     Add offsets for some stations.  These used to be hardwired,
C     but were converted to use an external file on Nov. 20, 2006.
C     The master copy of the input file is in 
C     /home/noatak/cwalker/geodesy/Clocks/gpsoffsets.dat 
C     and an operations copy is
C     in /users/cwalker/bin/gpsoffsets.dat
C
C     The pre RDV56 values are now in OLDOFFSET and new ones,
C     adjusted for zero at KK have been put in OFFSET.
C
      PARAMETER (MOFF=100)
      INTEGER   NOFF
      CHARACTER OFFSTA(MOFF)*2
      REAL      OFFSET(MOFF), OLDOFFSET(MOFF)
      REAL      SIGN(MOFF)
C
C     Keep the old values around for a while just in case.
C      DATA ( OFFSTA(I), OFFSET(I), OLDOFFSET(I), SIGN(I), I=1,MOFF )
C     1    / 'AP', -0.64E-6,     -1.00E-6,    1.0,
C     1      'GC',  0.0,         -0.55E-6,    1.0,
C     1      'GG',  0.238E-6,    -0.12E-6,    1.0, 
C     1      'HH', -0.132E-6,    -0.65E-6,    1.0,
C     1      'HO',  0.313E-6,     0.0,        1.0,
C     1      'KK',  0.0,         -0.40E-6,    1.0,
C     1      'MA', -0.68E-6,     -1.04E-6,    1.0,
C     1      'MC', -0.78E-7,     -1.00E-6,    1.0,
C     1      'NY', -0.29E-6,     -0.65E-6,    1.0,
C     1      'ON', -0.59E-7,      0.50E-6,    1.0,
C     1      'TC', -0.174E-6,    -0.55E-6,    1.0,
C     1      'TS', -1.39E-6,     -1.75E-6,    1.0,
C     1      'WF', -0.72E-7,     -0.43E-6,    1.0,
C     1      'WZ', -0.259E-6,    -0.65E-6,    1.0 /
C    Aug 23, 2005.  Reversed sign of MC.  RCW.
C    July 12, 2006  Add HO before RDV57.
C    Nov 20, 2006.  Convert to use external clock file.
C ---------------------------------------------------------
C     Open the clock offset file.
C
   10 WRITE(*,*) 'Input clock offset file '
      WRITE(*,*) '  (Default (blank) means '//
     1           '/users/cwalker/bin/gpsoffsets.dat):'
      READ(*,'(A)') CLKFILE
      IF( CLKFILE .EQ. ' ' ) 
     1   CLKFILE = '/users/cwalker/bin/gpsoffsets.dat'
      OPEN( UNIT=10, FILE=CLKFILE, STATUS='OLD', FORM='FORMATTED',
     1      ERR=50 )
C
C     Read the clock lines and fill the arrays.
C
      NOFF = 0
   15    CONTINUE
         READ(10,'(A)',END=20) INLINE
         IF( INLINE(1:1) .EQ. '#' ) GO TO 15
         IF( INLINE .EQ. ' ' ) GO TO 15
         NOFF = NOFF + 1
         IF( NOFF .GT. MOFF ) THEN
            WRITE(*,*) 'Too many clock offsets, max ', MOFF
            STOP
         END IF
         READ(INLINE,*) OFFSTA(NOFF), OFFSET(NOFF), SIGN(NOFF)
         WRITE(*,*) NOFF, '  ', OFFSTA(NOFF), OFFSET(NOFF), SIGN(NOFF)
         GO TO 15
   20 CONTINUE
C
C     Open the data file.
C
   50 WRITE(*,*) 'Input SNAP (MarkIV) log file:'
      READ(*,'(A)') INFILE
      OPEN( UNIT=8, FILE=INFILE, STATUS='OLD', FORM='FORMATTED',
     1      ERR=50 )
C
C     Assume that the last two characters are the station name.
C
      ICH = INDEX( INFILE, 'log.' ) + 4
      IF( ICH .GT. 4 ) THEN
         STACODE = INFILE(ICH:ICH+1)
      ELSE
         STACODE = 'XX'
         WRITE(*,*) ' *** No station code.  File name must be of '//
     1       'form ...log.xx where xx is the station code.'
      END IF
      CALL UPCASE( STACODE )
C
C     Open the plot file.
C
      WRITE(*,*) 'Output plot file (/xs for terminal, ',
     1     'name.ps/ps for postscript):'
      READ(*,'(A)') PLOTFILE
      CALL PGBEGIN(0,PLOTFILE,1,1)
C
C     Read the data.  Get the limits while at it.
C
      NPT = 0
      XMIN = 1.E9
      XMAX = -1.E9
      YMIN = 1.E9
      YMAX = -1.E9
      LIY = 0
      NPRT = 1
      WRITE(*,*) 'Some sample modified input lines:'
  100 CONTINUE
         ICH1 = 0
         ICH2 = 0
         READ( 8, '(A)', END=200 ) INLINE
         IF( INDEX( INLINE, 'clock' ) .EQ. 0 ) THEN
            ICH1 = INDEX( INLINE, '/gps-fmout/' )
            ICH2 = INDEX( INLINE, '/fmout-gps/' )
         END IF
         NEWFMT = INLINE(5:5) .EQ. '.'
C
C        Process the right type of line only.
C
         IF( ICH1 .NE. 0 .OR. ICH2 .NE. 0 ) THEN
C
C           Get the range of characters that contain the clock offset.
C
            ICH = MAX( ICH1, ICH2 ) + 11
            NCH = LEN1( INLINE )
C
C           Display a few input lines.
C
            IF( NPRT .LE. 5 ) THEN
               WRITE(*,*) '----Original: ', INLINE(1:NCH)
            END IF
C
C           Read the date and time.  There are two possible formats.
C           Why was the mod(id,100) there?  Precision problem?
C
            IF( NEWFMT ) THEN
               READ( INLINE, '( I4, 1X, I3, 1X, I2, 1X, I2, 1X, I2)' ) 
     1                 IY, ID, IH, IM, IS
C                    ID = MOD( ID, 100 )
            ELSE
               READ( INLINE, '(I2,I3,3I2)' ) IY, ID, IH, IM, IS
               IY = 1900 + IY
            END IF
            IF( LIY .NE. 0 .AND. IY .NE. LIY ) THEN
               WRITE(*,*) ' ****** PROBLEM ********* '
               WRITE(*,*) '  Crossed year boundary. '
               WRITE(*,*) '  This program can''t handle that. '
               WRITE(*,*) '  Break log into single year segments '
               STOP
            END IF
            LIY = IY
C
C           Fix the data format.  There are several bizzare verients.
C
C           Some use different units!  Parse, then purge a "usec" as
C           it confuses the attempt to fix E to D in the real format
C           below.  Also assume that the usec comes after any numbers
C           so blank the rest of the line.
C           Here is a sample line HH line:
C 2006.033.17:42:42.02/fmout-gps/1.120170e-05 11.2 usec (Hmaser leads GPS)
C           Here is a sample NT line:
C 2010.064.02:06:55.88/gps-fmout/RQIR    -1.68 usec  
C
C           After the sequence below, I think there will be nothing 
C           beyond the number, although someone could have a comment.
C
            IE = INDEX( INLINE(ICH:NCH), 'usec' ) 
            IF( IE .NE. 0 ) THEN
               MULT = 1.D-6
               INLINE(ICH+IE-1:NCH) = ' '
               NCH = LEN1( INLINE )
            ELSE
               MULT = 1.D0
            END IF
C
C           Convert nEm form to nDm form.
C
            IE = INDEX( INLINE(ICH:NCH), 'E' )  
            IF( IE .EQ. 0 ) IE = INDEX( INLINE(ICH:NCH), 'e' )  
            IF( IE .NE. 0 ) INLINE(ICH+IE-1:ICH+IE-1) = 'D'
C
C           Get rid of any + signs, "S", or "T" before the exponential.
C           But not all files have an exponential! (NT)
C
            IE = INDEX( INLINE(ICH:NCH), 'D' )
            IF( IE .EQ. 0 ) THEN
               JCH = NCH
            ELSE
               JCH = IE + ICH -1
            END IF
            IE = INDEX( INLINE(ICH:JCH), '+' )  
            IF( IE .NE. 0 ) INLINE(ICH+IE-1:ICH+IE-1) = ' '
            IE = INDEX( INLINE(ICH:JCH), 'S' )  
            IF( IE .NE. 0 ) INLINE(ICH+IE-1:ICH+IE-1) = ' '
            IE = INDEX( INLINE(ICH:JCH), 'T' )  
            IF( IE .NE. 0 ) INLINE(ICH+IE-1:ICH+IE-1) = ' '
            IE = INDEX( INLINE(ICH:JCH), 'RQIR' )
            IF( IE .NE. 0 ) INLINE(ICH+IE-1:ICH+IE+3) = '    '
C
C           Get rid of blanks - there can be blanks between minus
C           signs and data.
C
            I = ICH
            DO WHILE( I .LT. NCH )
C
               IF( INLINE(I:I) .EQ. ' ' ) THEN
                  DO J = I + 1, NCH
                     INLINE(J-1:J-1) = INLINE(J:J)
                  END DO
                  INLINE(NCH:NCH) = ' '
                  NCH = NCH - 1
               ELSE
                  I = I + 1
               END IF
            END DO
C
C           Good old GG threw another ringer at us:
C                2005.181.17:19:08.00/fmout-gps/+5.18.0E-6
C           Try not to get this completely wrong since this showed up
C           after the read as a value near 1 rather than an error.
C
            IPCH = INDEX(INLINE(ICH:NCH), '.')
            IF( IPCH .NE. 0 ) THEN
               IF( INDEX(INLINE((ICH+IPCH):NCH),'.') .NE. 0 ) THEN
                  GO TO 90
               END IF
            END IF
C
C           Ok, now decode the data point.
C
            READ( INLINE(ICH:NCH), *, ERR=90 ) RGPS
C
C           Write a few of the fixed lines and the result.
C
            IF( NPRT .LE. 5 ) THEN
               WRITE(*,*) 'Fixed Format: ', INLINE(1:NCH)
               WRITE(*,*) 'Derived offset, mult, offset: ', RGPS,
     1             MULT, RGPS*MULT
               NPRT = NPRT + 1
            END IF
C
C           Scale the microsecond cases.
C
            RGPS = MULT * RGPS
C
C           Take care of modulo 1 second cases.
C
            IF( RGPS .GT. 0.5D0 ) RGPS = RGPS - 1.0D0
            IF( RGPS .LT. -0.5D0 ) RGPS = RGPS + 1.0D0
            IF( ICH2 .NE. 0 ) RGPS = -RGPS
C           
C           Add the point to the plot arrays.  Allow for times
C           close to one second.
C
            NPT = NPT + 1
            TIME(NPT) = ID + IH/24.0 + IM/1440.0 + IS/86400.0
            GPS(NPT) = RGPS
C
C           Get limits.
C
            XMIN = MIN( XMIN, TIME(NPT) )
            XMAX = MAX( XMAX, TIME(NPT) )
            YMIN = MIN( YMIN, GPS(NPT) )
            YMAX = MAX( YMAX, GPS(NPT) )
C
         END IF
         GO TO 100
C
C        Jump here to deal with a data point read error.
C
   90    CONTINUE
         WRITE(*,*) 'Bad gps line: ', INLINE(1:NCH)

         GO TO 100
C
C     Come here when reading is done.
C
  200 CONTINUE
C
C     Refer the time to the middle, which also means that the
C     fitted constant will be essentially the average.
C
      TIMEMID = ( XMAX + XMIN ) / 2.0
      DO IPT = 1, MPT
         FTIME(IPT) = TIME(IPT) - TIMEMID
      END DO
C
C     Try to make the time for the job script lines.
C
      ID = INT( TIMEMID )
      DTIME = TIMEMID - ID
      RH = DTIME * 24.0
      IH = INT( RH )
      RM = ( RH - IH ) * 60.0
      IM = INT( RM )
      IS = INT( (  RM - IM ) * 60.0 )
      MDAY = ID
      IMONTH = 0
      CALL TDATECW( IY, IMONTH, MDAY, JD, MONTH, DNAME )
      CALL UPCASE( MONTH )
      WRITE( DATE, '( I4, A3, I2.2, A1, I2.2, A1, I2.2, A1, I2.2, A )' )
     1       IY, MONTH, MDAY, '_', IH, 'h', IM, 'm', IS, 's'
C
C     Make a linear fit to the data.
C
      CALL FIT( FTIME, GPS, NPT, SIG, 0, AVG, RATE, SAVG, SRATE, 
     1          CHI2, Q )
C
C     Convert rate to seconds per second.
C
      RATESS = RATE / ( 24.0 * 3600.0 )
      SRATESS = SRATE / ( 24.0 * 3600.0 )
C
C     Write result
C
      WRITE(*,*) '----------------------------------------------'
      WRITE(*,*) 'Mid time: ', TIMEMID
      WRITE(*, '( A, 1P, E15.5, A, E15.5 )' ) 
     1       'Average: ', AVG, ' +- ', SAVG
      WRITE(*,'( A, 1P, E15.5, A, E15.5 )' ) 
     1       'Rate:    ', RATESS, ' +- ', SRATESS
C
C     Write MC warning
C 
      IF( STACODE .EQ. 'MC' ) THEN
         WRITE(*,*)
     1     '*** WARNING:  For MC, the sign is traditionally reversed '
         WRITE(*,*)
     1     '              from that of other stations.'
         WRITE(*,*)
     1     '  This program used to reverse the sign.'
         WRITE(*,*)
     1     '  But MC is changing its sign as of August 2005'
         WRITE(*,*)
     1     '  Be very careful that the sign is correct.'
      END IF
C
C     Write a job script clock table set of lines.  Add in station
C     offset.
C
      JOBAVG = AVG
      JOBRATE = RATESS
      JOBOFF = 0.0
      DO I = 1, MOFF
         IF( STACODE .EQ. OFFSTA(I) ) THEN
            JOBAVG = SIGN(I) * AVG + OFFSET(I)
            JOBRATE = SIGN(I) * RATESS
            JOBOFF = OFFSET(I)
            WRITE(*,*)
            WRITE( *, '( A, A, A, 1P, E13.5, A )' )
     1        'The job script value below is adjusted by the '//
     2        'traditional ', OFFSTA(I), 
     3          ' offset of ', OFFSET(I), 'us'
            WRITE(*,*) '   The traditional offset is the typical '//
     1          ' value of the measured clock minus the GPS.'
            WRITE(*,*) '   Note the sign convention for the '//
     1          'traditional offset was reversed in July 2005.'
            IF( SIGN(I) .LT. 0.0 ) THEN
               WRITE(*,*) 'Also the sign of the gps average ',
     1              'and rate are inverted for this station.'
            END IF
            WRITE(*,*) ' '
            WRITE(*,*) '   This version of plotgps uses clock '//
     1         'offsets adjusted for zero at Kokee.'
         END IF
      END DO
      WRITE(*,*)
      WRITE(*,*) '------------- For jobs: ----------------------'
      IF( JOBAVG .GT. 0.01 ) THEN
         WRITE(*,*) 'Average offset large enough that there are '//
     1      'likely to be precision problems.'
         WRITE(*,*) 'Does the raw data have enough precision?'
         WRITE(*,*) 'Think hard about what you are doing.'
      ELSE
         WRITE( *, '( A, A2, A, A, F8.3, A )' )
     1      '!* ', STACODE, ' clock from plotgps fit to log data ',
     2      'with ', JOBOFF * 1.E6, ' us added. *!'
         WRITE( *, '( A, A, A, A, A, 1P, E13.5 )' )
     1      'name = ', STACODE, '   time = ', DATE, 
     2      '   offset = ', JOBAVG
         WRITE( *, '( A, 1P, E13.5, A )' )
     1      '        rate = ', JOBRATE,  '   !row!'
      END IF
      WRITE(*,*) '----------------------------------------------'
      WRITE(*,*)
C
C     Prepare to plot a fit line.
C
      XL(1) = XMIN
      YL(1) = AVG + ( XL(1) - TIMEMID ) * RATE
      XL(2) = XMAX
      YL(2) = AVG + ( XL(2) - TIMEMID ) * RATE
C
C     Add a bit to the edges.
C
      XRNG = XMAX - XMIN
      IF( XRNG .EQ. 0.0 ) XRNG = 0.1 * ( XMIN + XMAX )
      IF( XRNG .EQ. 0.0 ) XRNG = 1.0
      XMIN = XMIN - 0.03 * XRNG
      XMAX = XMAX + 0.03 * XRNG
      YRNG = YMAX - YMIN
      IF( YRNG .EQ. 0.0 ) YRNG = 0.1 * ( YMIN + YMAX )
      IF( YRNG .EQ. 0.0 ) YRNG = 1.0
      YMIN = YMIN - 0.03 * YRNG
      YMAX = YMAX + 0.03 * YRNG
C
C     Make the plot.
C
      CALL PGRNGE( XMIN, XMAX, XLO, XHI )
      CALL PGRNGE( YMIN, YMAX, YLO, YHI )
  300 CONTINUE
         CALL PGENV( XLO, XHI, YLO, YHI, 0, 1 )
         CALL PGLAB( 'Day (DOY)', 'gps-fmt (sec)',
     1        'GPS Offset in ' // INFILE )
         CALL PGPT( NPT, TIME, GPS, 3 )
         CALL PGLINE( 2, XL, YL )
C
C        Ask for new plot ranges.
C
         WRITE(*,*) ' EXIT or new xmin xmax ymin ymax '
         READ(*,'(A)') INLINE
         IF( INDEX( INLINE, 'EXIT') .NE. 0 .OR. 
     1       INDEX( INLINE, 'exit') .NE. 0 ) GO TO 400
         READ( INLINE, * ) XLO, XHI, YLO, YHI
         GO TO 300
C
  400 CONTINUE
      CALL PGEND
      STOP
      END




