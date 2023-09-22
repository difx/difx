********************************************************

	integer*4 function	load_sites( message )

	implicit none

	character*(*)		message

*  reads the file stations.dat which contains our best description
*  of the station coordinates.

	include 'CALCDB.i'

	integer*4 n, I1, inx
	real*8  R3(3), R1
	character*80 str
	character*8 name

	character*20 site_file
c	data site_file /'cor$db:stations.tab'/
	data site_file /'stations.tab'/

*--------------------------------------

	open ( unit=1, file=site_file, status='old', ERR=930 )
	 
	n = 0
	do while ( .TRUE. )

	  read (1, '(A)', end=900, ERR=910 ) str

	  if (index(str, '#') .gt. 0) goto 100	! comment card

	  n = n + 1
	  if( n .gt. Max_Stat ) go to 940

	  read (str, *, ERR=920 ) R3, I1, R1

	  sitere(1,n) = R3(1)
	  sitere(2,n) = R3(2)
	  sitere(3,n) = R3(3)

	  axisty(n) = I1
	  axisof(n) = R1

	  siteze(n) = 0.7d-18

	  inx = index (str, '$')
	  if( inx .le. 0 ) then
		message = ' FORMAT ERROR - missing $ for SITENAME in  ' 
     1						// site_file
		close (1)
		load_sites = 1
		return
	  end if

	  read (str(inx+1:), '(A)') name

	  sitnam(n) = name

	  nsites = n

 100	end do

 900	close (1)

	load_sites = 0
	return

910	continue
	message = ' ERROR while READING ' // site_file
	close (1)
	load_sites = 1
	return

920	continue
	message = ' ERROR while READING LINE from ' // site_file
	close (1)
	load_sites = 1
	return

930	continue
	message = ' ERROR opening ' // site_file
	load_sites = 1
	return

940	continue
	write( message, 
     1	'( '' TOO MANY SITES (Max. '', I3, '') in '', A )'
     1					) Max_Stat, site_file
	close (1)
	load_sites = 1
	return

	end

