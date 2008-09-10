*
*      params.i
*
*  include file associated with EPHOPN
*
*  it identifies the name of the JPL DE200 data file
*
	character*80 JPL_eph
	integer*4    EPH_RECL

c	data JPL_eph /'$disk1:[corr.work.wwilson.mjkcalc.eop]JPLEPH.'/
c	data EPH_RECL /1800/

	data JPL_eph 
     1    /'$disk1:[corr.work.wwilson.jplephem.de200]JPLEPH.DAT'/
	data EPH_RECL /1652/


