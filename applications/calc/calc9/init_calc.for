
	integer*4 function init_calc( message )

	implicit	none

c PARAMETERS
	character*(*)	message

C	include		'($SSDEF)'
C	include		'($SYSSRVNAM)'

c CALC INCLUDES

	include		'ccon.i' 
	include		'cphys.i' 

	integer*4	IFLAG(62) 
	equivalence 	(IFLAG(1),KATMC) 

	external ATMCMB,CMATHB,CTICMB
        external PRECMB,UT1CMB

c        external ETDCMB,PEPCMB,COMDAT

c Added on 4/3/1996
	external STACMB

c LOCAL VARIABLES

	integer*4	lun, status

c BEGIN

	lun = 1

c Open CALC database file
c	open( UNIT=lun, STATUS='old', FILE='cor$db:CALC.DB', ERR=9766 )
	open( UNIT=lun, STATUS='old', FILE='CALC.DB', ERR=9766 )

	go to 9767

9766	continue
	message = ' ** ERROR - CALC DATABASE file not found ** '
	init_calc = 1
	return

9767	continue

c Disable DEBUG output from CALC
	iluout = -1

c Read control flags from calc database file
	call dbflag( lun, iflag, status )
	if( status .ne. 0 ) then

	    message = ' *** BAD CONTROL FLAG read in DBFLAG '
	    init_calc = 2
	    return

	end if

c Read external values from calc database file
	call dbcom( lun, status )
	if( status .ne. 0 ) then

	    message = ' *** BAD DATABASE READ in DBCOM '
	    init_calc = 3
	    return

	end if

c Close database file
	close( lun )

	init_calc = 0

	return

	end

