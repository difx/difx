
C
      program wtest
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

      integer*2 UTCTAG(5) 
      character*8 C8SIT(2),C8SRC 
      real*8 UTCSEC, DELTIM , delay(2), drate(2)
      real*8 dx, dy, dz, dx1, dx2, dx3
      real*8 prev_delay, delta_secs, freq

c WEW
      integer*4 KOUNT, I, J, K

c      integer*2 IERR,LUN 
c      integer*2 IFLAG(62) 
c      equivalence (IFLAG(1),KATMC) 

c      data UTCTAG / 94, 08, 01, 9, 0 / 
c      data UTCTAG / 95, 06, 30, 16, 50 / 
c      data UTCTAG / 93, 11, 14, 09, 05 / 
      data UTCTAG / 93, 11, 14, 05, 19 / 

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

c BEGIN

      freq = 8420.0D0

      C8SIT(1) = 'PKS     '
      C8SIT(2) = 'HOB     '

      C8SRC    = '1921-293'  

      src16 = '1921-293'

      UTCSEC   = 0.0D0

      DELTIM   = 2.0D0

      delta_secs = 1.0D0

      j = 100

c BEGIN

	print 9153, src16
9153	format( ' Print SOURCE name or Q to STOP [', 
	1			A, '] : ', $ )
	read 900, g_string
900	format( A )
	if( g_string(1:1) .eq. 'q' .or. g_string(1:1) .eq. 'Q' ) stop
	if( g_string(1:1) .ne. ' ' ) src16 = g_string

	do i = 1, 2
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
6153	format( ' Print TIME INCREMENT (secs.) [', F6.4, '] : ', $ )
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

	write(9, 887) src16, c8sit(1), c8sit(2)
887	format( ' SRC =  ', A, ' STATIONS = ', 2A )
 
	write (9, 987) ' START UT = ',UTCTAG,UTCSEC
987	format( A, 5I4, 2X, F13.6 )


      DO 100 I=1,J

	do k = 1, 2
            call v_refcalc( C8SIT(k), src16, UTCTAG, UTCSEC, 
	1		delay(k), drate(k), uvw, duvw )
	end do

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
	write( 9, 984 ) utcsec, delay(1), drate(1), delay(2), drate(2)
984	format( 1x, F10.4, 4( 1x, D16.9 ) )


c next interval
          UTCSEC = UTCSEC + delta_secs

          prev_delay = dx

 100  CONTINUE  

      dt = secnds( t0 )

      print *, ' Elapsed time = ', dt, ' secs. '

      END 




