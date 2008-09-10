
	subroutine v_refcalc( site, source, utctag, utcsec, delay, drate,
	1				uvw, duvw )

c  Use calc to get delays from station to LBAREF

	implicit	none

	character*(*)	site, source
	integer*2	utctag(*)
	real*8		utcsec, delay, drate, uvw(*), duvw(*)

c COMMON AREAS

c Following is required to extract tropospheric delays
	include		'CALCDB.i'

c Following is required to extract UVW terms
	include		'situvw.i'

c LOCAL
	integer*4	i
	character*8	c8sit(2), c8src
	real*8		del(3)
	real*8		trop_delay, trop_rate

c BEGIN

	c8sit(1) = 'LBAREF  '
	c8sit(2) = site
	c8src = source

	call tbcalc( c8sit, c8src, utctag, utcsec, del, drate )

c	print *, ' REFCALC: dels= ', del(1), del(2), ' Rate= ', drate

c Get tropospheric contributions from CALCDB.I - to site 2 only - in usec
	trop_delay = ( NDRYCONT(2,1) + NWETCONT(2,1) ) * 1.0D06
	trop_rate =  ( NDRYCONT(2,2) + NWETCONT(2,2) ) * 1.0D06

c TEMPORARY - TESTING !!!!!!!!!!!!!!!!!!!!!!!!!!!!
c	trop_delay = 0.0
c	trop_rate = 0.0
c	print *, ' SETTING TROP. Delays/Rates to zero for TESTING'

c Calculate final delays - Negate to get SITE-LBAREF delay, but at LBAREF time

	delay = -( del(1) + del(2) + trop_delay )
	drate = -( drate + trop_rate ) 

c TEMPORARY !!!!!!!! - TESTING
c Print out tropospheric corrections - for site 2 
c	print 1111, NDRYCONT(1,1)*1.D6, NWETCONT(1,1)*1.D6, 
c	1		NDRYCONT(1,2)*1.D12, NWETCONT(1,2)*1.D12
c1111	format( ' TROPO(DRY,WET)-DELAYSus ', 2E10.3, 
c	1			'  -RATESps/s ', 2E10.3 )
c	print 2222, NDRYCONT(2,1)*1.D6, NWETCONT(2,1)*1.D6, 
c	1		NDRYCONT(2,2)*1.D12, NWETCONT(2,2)*1.D12
c2222	format( ' TROPO(DRY,WET)-DELAYSus ', 2F10.6, 
c	1			'  -RATESps/s ', 2F10.6 )
c

c Get site UVWs
	do i = 1, 3
		uvw( i ) = uvwp( i )
		duvw( i ) = uvwv( i )
	end do

	return

	end


