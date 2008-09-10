
C
      program ctest
c
c Test delay calc based on LBAREF position
c

      implicit none

c WEW

        external ATMCMB,CMATHB,CTICMB
        external PRECMB,UT1CMB

c        external ETDCMB,PEPCMB,COMDAT

c Added on 4/3/1996 - when BLOCK DATA STACMB was added
        external STACMB

c end WEW

      include 'ccon.i' 
      include 'cphys.i' 

c Testing
      include 'cmxst.i' 
      include 'situvw.i'

      integer*2 UTCTAG(5) 
      character*8 C8SIT(10),C8SRC 
      real*8 UTCSEC, DELTIM , delay(10), drate(10)
      real*8 dx, dy, dz, dx1, dx2, dx3
      real*8 prev_delay, delta_secs, freq, light

c WEW
      integer*4 KOUNT, I, J, K

c      integer*2 IERR,LUN 
c      integer*2 IFLAG(62) 
c      equivalence (IFLAG(1),KATMC) 

c      data UTCTAG / 94, 08, 01, 9, 0 / 
c      data UTCTAG / 95, 06, 30, 16, 50 / 
c      data UTCTAG / 93, 11, 14, 09, 05 / 
c      data UTCTAG / 93, 11, 14, 05, 19 / 
      data UTCTAG / 2004, 11, 16, 09, 30 / 
c      data UTCTAG / 2004, 04, 18, 03, 00 / 

      real*4   t0, dt

c      real*8  wewut1
c      common / wew / wewut1

	character*80	g_string
	integer*4	status

	integer*4	init_calc, load_sites, load_sources
	logical*4	find_source

	character*16  src16
	character*20  src_ra, src_dec

	real*8	uvw(3), duvw(3)
	real*8	uvwdat(24), duvwdat(24)

c BEGIN

      freq = 8420.0D0
      light = 299792458.0D0

      C8SIT(1) = 'PKS     '
      C8SIT(2) = 'DSS43   '
      C8SIT(3) = 'CAT15   '
      C8SIT(4) = 'MOPRA   '
      C8SIT(5) = 'HOB     '
      C8SIT(6) = 'CED     '
      C8SIT(7) = 'HART    '
      C8SIT(8) = 'KAS     '

      C8SRC    = '0537-441'

      src16 = '0537-441'

      UTCSEC   = 0.0D0

      DELTIM   = 2.0D0

      delta_secs = 1.0D0

      j = 20001

c BEGIN

	print 9153, src16
9153	format( ' Print SOURCE name or Q to STOP [', 
	1			A, '] : ', $ )
	read 900, g_string
900	format( A )
	if( g_string(1:1) .eq. 'q' .or. g_string(1:1) .eq. 'Q' ) stop
	if( g_string(1:1) .ne. ' ' ) src16 = g_string

	do i = 1, 6
		print 7153, i, c8sit(i)
7153	format( ' Print STATION ', I1, ' name [', 
	1			A, '] : ', $ )
		read 900, g_string
		if( g_string(1:1) .ne. ' ' ) c8sit(i) = g_string
	end do

	print 8153, utctag
8153	format( ' Print UTCTAG [', 5I5, '] : ', $ )
	read 900, g_string
	if( g_string(1:1) .ne. ' ' ) then
		read( g_string, * ) ( utctag(i), i = 1, 5 )
	end if

	print 6153, delta_secs
6153	format( ' Print TIME INCREMENT (secs.) [', F9.4, '] : ', $ )
	read 900, g_string
	if( g_string(1:1) .ne. ' ' ) then
		read( g_string, * ) delta_secs
	end if

	print 5153, j
5153	format( ' Print NUMBER of INTERVALS [', I6, '] : ', $ )
	read 900, g_string
	if( g_string(1:1) .ne. ' ' ) then
		read( g_string, * ) j
	end if


	status = init_calc( g_string )
        if( status .ne. 0 ) then
           print *, ' From INIT_CALC: ', g_string(1:50)
           stop
        end if
c Initialise

	print *, ' LOAD SITES '
	status = load_sites( g_string )
        if( status .ne. 0 ) then
           print *, ' From LOAD_SITES: ', g_string(1:50)
           stop
        end if

	print *, ' LOAD SOURCES '
c	status = load_sources( 'cor$db:source.tab', g_string )
	status = load_sources( 'source.tab', g_string )
        if( status .ne. 0 ) then
           print *, ' From LOAD_SOURCES: ', g_string(1:50)
           stop
        end if

	if( .not. find_source( src16, src_ra, src_dec ) ) then
	  print *, ' FIND_SOURCE UNSUCCESSFUL - Exit '
	  stop
	else
	  print *, ' Find Source OK - SRC: ', src16, src_ra, src_dec
	end if
 
        call star( i, dx, dy ) 
	print *, ' NSTARS, RA, DEC (rads) = ', i, dx, dy


      call eop_setup( utctag )
      print *, ' EOP_SETUP OK'

      call tocup
      print *, ' TOCUP OK'

      CALL INITL (KOUNT) 

      t0 = secnds( 0.0 )

c      print *, ' BEGIN LOOP '

      UTCSEC   = 0.0D0

	write(9, 888) src16, c8sit(1), c8sit(2), c8sit(3), c8sit(4),  
     .     c8sit(5), c8sit(6)
	write(10, 888) src16, c8sit(1), c8sit(2), c8sit(3), c8sit(4), 
     .       c8sit(5), c8sit(6)
     

887	format( ' SRC =  ', A, ' STATIONS = ', 2A )
888	format( ' SRC =  ', A, ' STATIONS = ', 6A )
 
	write (9, 987) ' START UT = ',UTCTAG,UTCSEC
	write (10, 987) ' START UT = ',UTCTAG,UTCSEC
987	format( A, 5I4, 2X, F13.6 )


      DO 100 I=1,J

	do k = 1, 6
            call v_refcalc( C8SIT(k), src16, UTCTAG, UTCSEC, 
	1		delay(k), drate(k), uvw, duvw )
            uvwdat(k*3-2) = uvw(1)
            uvwdat(k*3-1) = uvw(2)
            uvwdat(k*3-0) = uvw(3)
	end do
        
        write (10,985) utcsec, uvwdat(1), uvwdat(2), uvwdat(3), 
     .       uvwdat(4), uvwdat(5), uvwdat(6),
     .       uvwdat(7), uvwdat(8), uvwdat(9),
     .       uvwdat(10), uvwdat(11), uvwdat(12),
     .       uvwdat(13), uvwdat(14), uvwdat(15),
     .       uvwdat(16), uvwdat(17), uvwdat(18)
c     .       uvwdat(19), uvwdat(20), uvwdat(21)
c     .       uvwdat(22), uvwdat(23), uvwdat(24)

985     format ( F14.6, 24E20.10 )

	dx = delay(1) - delay(2)

c	if( i .eq. 1 ) then
c		dy = 0.0
c	else
c		dz = ( ( dx - prev_delay ) * freq )
c		dy = dz / delta_secs
c	end if
c
c	print 986, UTCSEC, delay(1), delay(2), dx, dz, dy
c986	format( F10.6, 3F14.6, D18.10, F9.3 )


c Following for test output to FOR009.DAT
c	write( 9, 984 ) utcsec, delay(1), drate(1), delay(2), drate(2)
	write( 9, 981 ) utcsec, delay(1), drate(1), delay(2), drate(2), 
     .  delay(3), drate(3), delay(4), drate(4), delay(5), drate(5),
     .  delay(6), drate(6)
984	format( 1x, F14.6, 4( 1x, D23.16 ) )
981	format( 1x, F14.6, 12( 1x, D23.16 ) )


c next interval
          UTCSEC = UTCSEC + delta_secs

          prev_delay = dx

 100  CONTINUE  

      dt = secnds( t0 )

      print *, ' Elapsed time = ', dt, ' secs. '

      END 




