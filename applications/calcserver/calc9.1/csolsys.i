C
C@csolsys.i
C
      Real*8 SPLANET(3,2,7), GPLANET(3,2,7), SJD
      COMMON / CSOLSYS / SPLANET, GPLANET, SJD
C  Mods:
C     98.07.22 - D. Gordon, common block name changed from 'SOLSYS' to 
C                  'CSOLSYS' to avoid conflicts with G. Kaplan's 'NOVAS'
C                  routines at USNO (B. Archinal suggestion).
C
C
C       1. SPLANET(3,2,7)  -  The J2000.0 Solar System Barycentric positions
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
C       2. GPLANET(3,2,7)  -  The J2000.0 Geocentric positions and velocities
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
C       3. SJD            -   Initially the time of the previous ovservation.
C                             After subroutine PEP it is the current time.
C                             (Days)
