       SUBROUTINE GETWEA( NDATA, DLEN, ADDRES, DATA, GOTWEA, 
     1                   TEMP, DEWP, WINDV, WINDD, PRESS, SNOW, RAIN,
     2                   DTEMP, DDEWP, MINT, MAXT, DWIND, GUST )
C
C     Routine to accumulate weather data for FITMON.  
C
      INTEGER        NDATA, DLEN(NDATA), IW
      INTEGER        BIT(16), TOUT, P1, P2, P3, R1, R2
      REAL           TEMP, DEWP, WINDV, WINDD, PRESS, SNOW, RAIN
      REAL           DTEMP, DDEWP, MINT, MAXT, DWIND, GUST
      DOUBLE PRECISION   GETNUM, TEMDAT
      CHARACTER      WEAMON(12)*4
      CHARACTER      DATA(NDATA)*6, ADDRES(NDATA)*4
      LOGICAL        GOTP1, GOTP2, GOTWEA
      DATA  WEAMON / '1803', '1804', '1806', '1807', '1818', 
     1               '1819', '180F', '181F', '181A', '181B',
     2               '181D', '181E' /
C
C     Weather numbers are in order analog Temperature, analog Dew Point, 
C     analog Wind Speed, Wind Direction, digital Pressure (2 words), 
C     snow depth, rain gauge, digital temperature, digital dew point, 
C     min/max temp, and digital wind speed/max gust.
C
C     Accumulate time with digital temperature. 
C
      TEMP = -99.
      DEWP = -99.
      WINDV = -99.
      WINDD = -99.
      PRESS = -99.
      RAIN = -99.
      SNOW = -99.
      DTEMP = -99.
      DDEWP = -99.
      MINT = -99.
      MAXT = -99.
      DWIND = -99.
      GUST = -99.
      GOTP1 = .FALSE.
      GOTP2 = .FALSE.
      GOTWEA = .FALSE.
C
C     Loop through data points.
C
      DO 100 IW = 1, NDATA
C
C        Analog Temperature.
C
         IF( ADDRES(IW) .EQ. WEAMON(1) ) THEN
            TEMP = GETNUM( DATA(IW), 1, DLEN(IW) ) / 327.68
            GOTWEA = .TRUE.
C
C        Analog Dew Point.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(2) ) THEN
            DEWP = GETNUM( DATA(IW), 1, DLEN(IW) ) / 327.68
            GOTWEA = .TRUE.
C
C        Analog Wind Velocity.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(3) ) THEN
            WINDV = GETNUM( DATA(IW), 1, DLEN(IW) ) / 327.68 ! MPH
            GOTWEA = .TRUE.
C
C        Wind direction.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(4) ) THEN
            WINDD = GETNUM( DATA(IW), 1, DLEN(IW) ) / 91.022
C
C        Digital pressure.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(5) ) THEN
            GOTP1 = .TRUE.
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            P1 = BIT(1) + 2*BIT(2) + 4*BIT(3) + 8*BIT(4) + 16*BIT(5) +
     1        32*BIT(6) !  + 64*BIT(7) + 128*BIT(8)
C            P1 = BIT(9) + 2*BIT(10) + 4*BIT(11) + 8*BIT(12) + 16*BIT(13)
C     1          + 32*BIT(14) + 64*BIT(15) + 128*BIT(16)
         ELSE IF( ADDRES(IW) .EQ. WEAMON(6) ) THEN
            GOTP2 = .TRUE.
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            P3 = BIT(1) + 2*BIT(2) + 4*BIT(3) + 8*BIT(4) + 16*BIT(5) +
     1        32*BIT(6) + 64*BIT(7) + 128*BIT(8)
            P2 = BIT(9) + 2*BIT(10) + 4*BIT(11) + 8*BIT(12) + 16*BIT(13)
     1         + 32*BIT(14) + 64*BIT(15) + 128*BIT(16)
C
C        Snow gauge.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(7) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) ) / 3276.8
            SNOW = 226.0 - SQRT( (TEMP+273.0) / 291.0 )
     1               * 21.371 * (10.0-TEMDAT)
C
C        Rain gauge.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(8) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            R2 = BIT(1) + 2*BIT(2) + 4*BIT(3) + 8*BIT(4) + 16*BIT(5) +
     1        32*BIT(6) + 64*BIT(7) + 128*BIT(8)
            R1 = BIT(9) + 2*BIT(10) + 4*BIT(11) + 8*BIT(12) + 16*BIT(13)
     1         + 32*BIT(14) + 64*BIT(15) + 128*BIT(16)
            RAIN = ( R1*256 + R2 ) * 0.01
            GOTWEA = .TRUE.
C
C        Digital temperature.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(9) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            DTEMP = 0.1*BIT(1) + 0.2*BIT(2) + 0.4*BIT(3) + 0.8*BIT(4) + 
     1                1*BIT(5) +   2*BIT(6) +   4*BIT(7) + 8*BIT(8) + 
     2               10*BIT(9) +  20*BIT(10) + 40*BIT(11)
            IF( BIT(9).EQ.1 .AND. BIT(10).EQ.1 .AND. BIT(11).EQ.1 ) 
     1            DTEMP = DTEMP - 70.0
            IF( BIT(5).EQ.1 .AND. BIT(6).EQ.1 .AND. BIT(7).EQ.1 .AND.
     1          BIT(8).EQ.1 ) DTEMP = DTEMP - 15
            IF( BIT(16) .EQ. 1 ) DTEMP = -1.0 * DTEMP
            GOTWEA = .TRUE.
C
C        Digital dew point.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(10) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            DDEWP = 0.1*BIT(1) + 0.2*BIT(2) + 0.4*BIT(3) + 0.8*BIT(4) + 
     1                1*BIT(5) +   2*BIT(6) +   4*BIT(7) + 8*BIT(8) + 
     2               10*BIT(9) +  20*BIT(10) + 40*BIT(11)
            IF( BIT(9).EQ.1 .AND. BIT(10).EQ.1 .AND. BIT(11).EQ.1 ) 
     1            DDEWP = DDEWP - 70.0
            IF( BIT(5).EQ.1 .AND. BIT(6).EQ.1 .AND. BIT(7).EQ.1 .AND.
     1          BIT(8).EQ.1 ) DDEWP = DDEWP - 15
            IF( BIT(16) .EQ. 1 ) DDEWP = -1.0 * DDEWP
            GOTWEA = .TRUE.
C
C        Digital min and max temperatures.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(11) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            MINT = 1*BIT(1) + 2*BIT(2) + 4*BIT(3) + 8*BIT(4) + 
     1            10*BIT(5) +   20*BIT(6) +   40*BIT(7)
            IF( BIT(5).EQ.1 .AND. BIT(6).EQ.1 .AND. BIT(7).EQ.1 ) 
     1            MINT = MINT - 70.0
            IF( BIT(2).EQ.1 .AND. BIT(3).EQ.1 .AND. BIT(4).EQ.1 )
     1            MINT = MINT - 14
            IF( BIT(8) .EQ. 1 ) MINT = -1.0*MINT
C
            MAXT = 1*BIT(9) + 2*BIT(10) + 4*BIT(11) + 8*BIT(12) + 
     1            10*BIT(13) + 20*BIT(14) + 40*BIT(15)
            IF( BIT(13).EQ.1 .AND. BIT(14).EQ.1 .AND. BIT(15).EQ.1 ) 
     1            MAXT = MAXT - 70.0
            IF( BIT(10).EQ.1 .AND. BIT(11).EQ.1 .AND. BIT(12).EQ.1 )
     1            MAXT = MAXT - 14
            IF( BIT(16) .EQ. 1 ) MAXT = -1.0*MAXT
C
C        Digital wind and max gust.
C
         ELSE IF( ADDRES(IW) .EQ. WEAMON(12) ) THEN
            TEMDAT = GETNUM( DATA(IW), 1, DLEN(IW) )
            CALL GETBIT( TEMDAT, BIT, TOUT )
            DWIND = 1*BIT(1) + 2*BIT(2) + 4*BIT(3) + 8*BIT(4) + 
     1            16*BIT(5) +   32*BIT(6) +   64*BIT(7)
            DWIND = DWIND * 100.0 / 256.0
            GUST = 1*BIT(9) + 2*BIT(10) + 4*BIT(11) + 8*BIT(12) + 
     1            16*BIT(13) + 32*BIT(14) + 64*BIT(15)
            GUST = GUST * 100.0 / 256.0
         END IF         
100   CONTINUE
C
C     Put together the pressure.
C
      IF( GOTP1 .AND. GOTP2 ) THEN
         PRESS = ((P1*256 + P2)*256 + P3) / 1000.0
         GOTWEA = .TRUE.
      END IF
C
      RETURN
      END

