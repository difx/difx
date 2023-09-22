C     RDCAT.INC   Include file for reading catalogs.
C
C     Note that unit 24 is used by RDLOC so the calling program should
C     not use that unit number.
C
C     Read source names as Character*16 with up to 10 aliases.  Worry
C     downstream if these need to be restricted.
C
C     For source catalog.
C
C     SRCVER  C20   -  Version (date) for catalog
C     SRCNAM  C16*10-  Source name
C     SRCRA   R8    -  Right Ascension (rad)
C     SRCDEC  R8    -  Declination (rad)
C     SRCRAE  R4    -  Error on RA (mas)
C     SRCDECE R4    -  Error on Dec (mas)
C     SRCEQ   C5    -  Coordinate epoch: B1950, J2000, DATE, or JNNNN
C     SRCEPOT R8    -  Reference Julian epoch for position.
C     SRCCAL  C1    -  Cal code - eg for VLA, perhaps VLBA later.
C     SRCVEL  R4*32 -  LSR Velocities of spectral line channels.
C                   -    If velocity not given, SRCVEL = CONT where
C                   -      DATA  CONT / 'CONT' /
C     SRCDRA  R4    -  Planetary motion dRA/dt in sec of time/day
C     SRCDDEC R4    -  Planetary motion dDEC/dt in sec of arc/day
C     SRCPMY  I4    -  Planetary motion reference epoch year
C     SRCPMM  I4    -                                   month
C     SRCPMD  I4    -                                   day
C     SRCPMT  R8    -                                   time (rad)
C     SRCPMRA R8    -  Proper motion in RA (cos(dec)*dRA/dt) arcsec/yr
C     SRCPMDEC R8   -  Proper motion in DEC (dDec/dt) arcsec/yr
C     SRCPARA R8    -  Paralax (arcsec)
C     SRCRMK  C80   -  Remark (eg. data reference)
C     SRCVREF C1    -  Velocity reference (L, H, or G for LSR,
C                          Heliocentric, or Geocentric.
C     SRCVDEF C1    -  Velocity definition (R, O, Z for radio, 
C                          optical, or z)
C     SRCFLUX R4*30 -  Freq. total flux, compact flux for 10 freqs.
C     SRCFREF C15   -  Reference for fluxes.
C                
C
C     For station catalog.
C
C     STAVER  C20    -  Version (date) for catalog
C     STANAM  C8     -  Station name
C     STACOD  C3     -  Station code (NUG code)
C     STADBN  C10    -  Station name in correlator data base.
C     STADBC  C4     -  Station code in correlator data base.
C     STAFRM  C80    -  Reference frame.
C     STAEL   R8     -  Elevation (m)
C     STALAT  R8     -  Latitude (rad)
C     STALON  R8     -  Longitude (rad)
C     STAX    R8     -  Station coordinate X (m)
C     STAY    R8     -     "        "      Y (m)
C     STAZ    R8     -     "        "      Z (m)
C     STADX   R8     -  Station rate X (m/yr)
C     STADY   R8     -  Station rate Y (m/yr)
C     STADZ   R8     -  Station rate Z (m/yr)
C     STAEPO  I4     -  MJD of XYZ.
C     STACTL  C5     -  Control file format 
C                       Options for first 4 characters:   
C                          (SNAP, SN50, NRAO, VLA, VLBA, VEX, NONE)
C                       5th character 'V' to produce VLBA style file to 
C                       control DAR and tape  only.
C     STADAR  C5     -  Type of data aquisition rack (mostly formatter).
C                         (VLBA, VGEO (VLBA with geodetic wiring), 
C                          MKIV, MKIII, S2, K4)
C     STAREC  C6     -  Type of recorder (VLBA, MKIII, MKIV, S2, K4)
C     STADSK  C6     -  Type of disk system present.
C     STAMD   C6     -  Default media (TAPE, DISK)
C     STANBC  I4     -  Number of BBC's (or VC's)
C     STANDR  I4     -  Number of tape drives.
C     STANHD  I4     -  Number of heads per drive.
C     STAARE  C6     -  Alternate recorder (MARK5A etc)
C     STAHAZ  R4*200 -  Azimuths for horizon data.
C     STAHEL  R4*200 -  Elevations for horizon data.
C     STANHO  I4     -  Number of valid horizon points.
C     STAAX1  R4*6   -  First axis (Az, Ha, X) limits (deg).
C                        For Az, include full wrap (VLBA: -90 to 450)
C     STAAX2  R4*6   -  Second axis (El, Dec, Y) limits (deg).
C     STANAX  I4     -  Number of sets of axis limits.
C     STARA1  R4     -  First axis slew rate (deg/min).
C     STARA2  R4     -  Second axis slew rate (deg/min).
C     STAAC1  R4*2   -  First axis slew acceleration, deceleration (deg/s/s)
C     STAAC2  R4*2   -  Second axis slew acceleration, deceleration (deg/s/s)
C     STAZAL  R4     -  Zenith angle limit (mainly useful for EQUAT
C                         antennas at low dec.
C     STAHAL  R4     -  Hour angle limit (don't use for new code - 
C                         here for backward compatibility.
C     STAMNT  C5     -  Axis type (ALTAZ, EQUAT, XYNS, XYEW).
C     STADBA  C5     -  Axis type (correlator data base).
C     STAOFF  R4     -  Axis offset (m)
C     STASTL  R4     -  "Settling time" in seconds (time to add to slew)
C     STAMSU  R4     -  Minimum time to start new scan.
C     STAMSH  R4     -  Maximum number of slews per hour (for Jodrell).
C     STATLS  R4     -  Time to set analog levels at VLBA and VLA on the
C                       first scan for a setup.
C     STATSC  C4     -  Method of Tsys measurement (GAP, CONT).
C     STADBBCV C8    -  DBBC version (astro, geo, hybrid)
C
C     OBSDATE R8     -  MJD of observations - this is set in the routines
C                       that call the reader.  It is needed because the
C                       station catalog may have multiple, date dependent
C                       entries thanks to earthquakes etc.
C     
C     Note double precisions are at front of commons to prevent 
C     word boundary problems.
C
C     For locations catalog.  Generally other than RDSTA should 
C     not use these parameters other than to set the file name.
C     There are 4 letter station codes in the VLBA data base so
C     I had to bump up the size of DBCODE  Sept 2008. RCW.
C
C     LOCFILE C80    -  Name of locations file.
C     DBNAME  C10     -  Station name.
C     DBCODE  C4     -  Station code in data base.
C     DBX     R8     -  Station coordinate X (m)
C     DBY     R8     -     "        "      Y (m)
C     DBZ     R8     -     "        "      Z (m)
C     DBDX    R8     -  Station rate X (m/yr)
C     DBDY    R8     -  Station rate Y (m/yr)
C     DBDZ    R8     -  Station rate Z (m/yr)
C     DBEPO   I4     -  MJD of XYZ.
C     DBFRM   C80    -  Reference frame.
C     DBDBA   C5     -  Axis type (correlator data base).
C     DBOFF   R4     -  Axis offset (m)
C     DBBEG   R8     -  Begin MJD of applicability.
C     DBEND   R8     -  End MJD of applicability.
C---------------------------------------------------------------------
C
C     For source catalog.
C
      INTEGER           MVEL, MAL
      PARAMETER         (MVEL=32)
      PARAMETER         (MAL=10)
      CHARACTER         SRCVER*20, SRCNAM(MAL)*16
      INTEGER           SRCPMY, SRCPMM, SRCPMD
      REAL              SRCVEL(MVEL), SRCDRA, SRCDDEC
      REAL              SRCRAE, SRCDECE, SRCFLUX(30)
      DOUBLE PRECISION  SRCPMRA, SRCPMDEC, SRCPARA
      DOUBLE PRECISION  SRCRA, SRCDEC, SRCPMT, SRCEPOT
      CHARACTER         SRCCAL*1, SRCEQ*5, SRCRMK*80
      CHARACTER         SRCVREF*1, SRCVDEF*1, SRCFREF*15
C
C     For station catalog.
C
      INTEGER           MHOR
      PARAMETER         (MHOR=200)
      INTEGER           STANHO, STANAX, STANBC, STANDR, STANHD
      INTEGER           STAEPO
      CHARACTER         STAVER*20
      CHARACTER         STANAM*8, STACOD*3, STADBN*10, STADBC*4
      CHARACTER         STAFRM*80, STACTL*5, STAMNT*5, STADBA*5
      CHARACTER         STADAR*5, STAREC*6, STADSK*6, STAMD*6
      CHARACTER         STATSC*4, STADBBCV*8
      REAL              STAHAZ(MHOR), STAHEL(MHOR)
      REAL              STAAX1(6), STAAX2(6)
      REAL              STARA1, STARA2, STAAC1(2), STAAC2(2)
      REAL              STAZAL, STAHAL, STASTL, STAMSU, STAOFF
      REAL              STAMSH, STATLS
      DOUBLE PRECISION  STALON, STALAT, STAEL, STAX, STAY, STAZ
      DOUBLE PRECISION  STADX, STADY, STADZ, OBSDATE
C
C     For the optional locations file.  Make this array size big 
C     enough that we shouldn't hit it for a while.  There are no
C     big 2D arrays, so this should still only be a small amount 
C     of space.
C
      INTEGER           MDB
      PARAMETER         (MDB=1000) 
      DOUBLE PRECISION  DBX(MDB), DBY(MDB), DBZ(MDB)
      DOUBLE PRECISION  DBDX(MDB), DBDY(MDB), DBDZ(MDB)
      DOUBLE PRECISION  DBBEG(MDB), DBEND(MDB)
      INTEGER           DBEPO(MDB), NDB
      REAL              DBOFF(MDB)
      CHARACTER         LOCFILE*80, DBNAME(MDB)*10, DBCODE(MDB)*4
      CHARACTER         DBFRM(MDB)*80, DBDBA(MDB)*5, DBVER*20
C
C     The COMMONS
C
      COMMON /RDCATC/ SRCVER, SRCNAM, SRCCAL, SRCEQ, SRCRMK, SRCVREF, 
     1                SRCVDEF, SRCFREF, STAVER, STANAM, STACOD, 
     2                STADBN, STADBC, 
     3                STAFRM, STACTL, STAMNT, STADBA, STADAR, STAREC,
     4                STADSK, STAMD, STATSC, LOCFILE, DBNAME, DBCODE, 
     5                DBFRM, DBDBA, DBVER
C
      COMMON /RDCATN/ SRCPMRA, SRCPMDEC, SRCPARA, SRCRA, SRCDEC, 
     1                SRCPMT, SRCEPOT, STALON, STALAT, STAEL, 
     2                STAX, STAY, STAZ, STADX,  STADY,  STADZ, OBSDATE, 
     3                DBX, DBY, DBZ, DBDX, DBDY, DBDZ, DBBEG, DBEND,
     4                STAEPO, SRCPMY, SRCPMM, SRCPMD, SRCVEL, 
     5                SRCDRA, SRCDDEC, SRCRAE, SRCDECE, SRCFLUX,
     6                STANHO, STANAX, STANBC, STANDR, STANHD,
     7                STAHAZ, STAHEL, STAAX1, STAAX2, 
     8                STARA1, STARA2, STAAC1, STAAC2,
     9                STAZAL, STAHAL, STASTL, STAMSU, STAOFF,
     A                STAMSH, STATLS, STADBBCV, DBEPO, NDB, DBOFF
C
C   --------  End of RDCAT.INC ------------
