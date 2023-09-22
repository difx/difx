***************************************************************

	subroutine eop_setup (UTCTAG)

*  extracts the eop data from the smoothed data file
*  MAJOR CHANGES TO MOVE TO NASA FILE FORMAT - ATD 21/02/2008

	implicit none

	integer*2	UTCTAG(*)

	include 'CALCDB.i'

	integer*4 year, mon, day, ihr, imn, stat

	integer*4 j, k, m, n
	integer*4 mjd_offset

	integer*4 i, inx, yr, nskip
	integer*4 mj_jan, mj_target, offmicrosec
	character*40 eop_file
	character*256 root, str
	character*1  c1

	real*4 a_day(37000), a_x(37000), a_y(37000), a_dut1(37000)
	real*4 xx, ax, bx, ay, by, ad, bd
	real*4 rmsx, rmsy, rmsd

	real*8 mjd0, x, y, dut1
	real*8 sla_dat, sla_dtt
 
	logical more

c 	data root /'$disk1:[corr.work.wwilson.mjkcalc.eop]x.yy'/

*--------------------------------------------------------------

	year = utctag(1)

c	if (year .lt. 100) year = year + 1900
c 10/2/00 - WEW - change to
	if (year .lt. 1900) year = year + 1900

	mon = utctag(2)
	day = utctag(3)
	ihr = utctag(4)
	imn = utctag(5)
	call sla_caldj(year, mon, day, mjd0, stat)
	if (stat .ne. 0) then
	  write (6, 10) year, mon, day
 10	  format ('SLA_caldj error; input (yr/m/d) : ', 3I6)
	  STOP
	end if

	mj_target = mjd0

* choose the appropriate file

c WEW - 10/2/00 - modify for Y2K
	if( year .ge. 2000 ) then
		write (eop_file, 20) (year - 2000)
	else
		write (eop_file, 20) (year - 1900)
	end if

c 20	format ('eop90c04.', I2)
c	open (unit=66, file=eop_file, status='old',
c     :        defaultfile=root)
 20	format ('eopc04.', I2.2)
cccc    Changed to read usno style files
c	open (unit=66, file=eop_file, status='old', ERR=909 )
	open (unit=66, file='usno_finals.erp', status='old', ERR=909 )

** locate the start of data

	more = .true.
c EDITED by ATD to cope with eopc file format changes 11/2007
        read (66, '(A)', end=900) str
        do while (more)
          read (66, '(A)', end=900) str
          if (str(1:1) .ne. "#") then
            more = .false.
          end if
        end do
c END ATD edit - original code below
c	do while (more)
c
c	  read (66, '(A)', end=900) str
c	  if (index (str, 'YEAR') .gt. 0) then
c
c*  correct year ?
c
c	    inx = index (str, '==')
c	    read (str(inx+4:), *) yr
c	    if (yr .ne. year) then
c	      write (6, 30) yr, year
c 30	      format (' EOP_SETUP: Wrong file ? ', 2I5)
c	      STOP
c	    end if
c
c	    read (66, '(A)') c1		! skip blank line before data
c	    more = .false.
c	  end if
c
c	end do


c*  read the first record tp get the mjd at the start of the year
c
c	read (66, '(A)') str
c	read (str(12:), *) mj_jan
c	nskip = mj_target - mj_jan - 2
c
c	if ((nskip .lt. 0) .or. (nskip .gt. 363)) then
c	  write (6, 40) mjd0, mj_target, mj_jan, str
c 40	  format (' wrong year ? - mjd0 : ', F12.1,
c     :            ' mj target/jan : ', 2I10, /, A)
c	  STOP
c	end if
c
c	do i = 1, nskip
c	  read (66, '(A)') c1
c	end do

c WEW mods.
	mj_target = mj_target - 2

	j = 0
	k = 0
	do while( j .ne. mj_target )
c ATD edit - shifted to end of loop since already read first line
c          read (66, '(A)', END=1000 ) str
c	  read( str(12:16), '(I5)' ) j
c          read( str(13:19), '(I7)' ) j
          read( str(1:7), '(I7)' ) j
          j = j-2400000

c	  print *, ' Searching for ', mj_target, ' - got ', j

c Save in case we need for interpolating
	  k = k + 1
	  if( k .eq. 1 ) mjd_offset = j
c Interpolating routine gets into trouble with large x - hence
	  a_day( k ) = j - mjd_offset
c	  read  (str(18:), * ) a_x( k ), a_y( k ), a_dut1( k )
c          read (str(20:53), *) a_x( k ), a_y( k ), a_dut1( k )
c          write(*,*) 'will be working with ', str(11:34)
          read (str(11:35), *) a_x( k ), a_y( k ), offmicrosec
c          write(*,*) 'x is ', a_x( k )
c          write(*,*) 'y is ', a_y( k )
c          write(*,*) 'offset is ', offmicrosec
          a_x( k ) = a_x( k )/10.0
          a_y( k ) = a_y( k )/10.0
          a_dut1(k) = -offmicrosec/1000000.0
c ADDED by ATD
          read (66, '(A)', END=1000 ) str
c          write(*,*) 'next will be ', str
	end do
	go to 2000

1000	continue
c Need to do all 4 interpolations
	i = 1

1001	continue
c Need to do 4-i+1 interpolations to complete set
	j = i

c Got to end of file - interpolate using last 5(?) values
	m = 5
	print *, ' !!!!! EOP FILE NOT UP TO DATE - INTERPOLATING !!!!! '
	k = k - 4
	call flinear_mean_square_fit( a_day( k ), a_x( k ), m, ax, bx,
     - rmsx )
	call flinear_mean_square_fit( a_day( k ), a_y( k ), m, ay, by, 
     - rmsy )
	call flinear_mean_square_fit( a_day( k ), a_dut1( k ), m, ad, 
     - bd,rmsd )

	m = k
	do i = 1, 5
	  n = a_day( m ) + mjd_offset
	  write (6, '( ''  DATA     '', I5, 2F9.5, F10.6 )' )
	1			n, a_x( m ), a_y( m ), a_dut1( m )
	  m = m + 1
	end do

c	write( 6, '( ''  X: '', 3F15.8 )' ) ax, bx, rmsx
c	write( 6, '( ''  Y: '', 3F15.8 )' ) ay, by, rmsy
c	write( 6, '( ''  D: '', 3F15.8 )' ) ad, bd, rmsd

	do i = j, 4
	  n = mj_target + i
	  xx = ( n - mjd_offset )

	  x = ( ax * xx ) + bx
	  fwobxy(1,i) = x * 1000.		! milliarcsec

	  y = ( ay * xx ) + by
	  fwobxy(2,i) = y * 1000.

	  dut1 = ( ad * xx ) + bd
c	  fut1pt(i) = sla_dat(mjd0) - dut1
	  fut1pt(i) = dut1
	  write (6, '( ''  INTERP   '', I5, 2F9.5, F10.6 )')
	1			n, x, y, dut1

 	end do

	go to 3000

2000	continue
c Found correct value

	do i = 1, 4
c          write(*,*) 'About to get the real values'
	  read (66, '(A)', END=1001 ) str
	  write (6, '(A)') str

c REPLACED BY ATD 
c	  read( str(12:16), '(I5)' ) j
c 	  read  (str(18:), *) x, y, dut1
c          read( str(13:19), '(I7)' ) j
c          read (str(20:53), *) x, y, dut1
          read( str(1:7), '(I7)' ) j
          j = j-2400000
          read (str(11:35), *) x, y, offmicrosec
          x = x/10.0
          y = y/10.0
          dut1 = (-offmicrosec)/1000000.0
          write(*,*) 'Offsetmicrosec is ', offmicrosec
          write(*,*) 'x is', x, ', y is ', y, ', dut1 is ', dut1

c Save in case we need for interpolating
	  k = k + 1
	  a_day( k ) = j - mjd_offset
	  a_x( k ) = x
	  a_y( k ) = y
	  a_dut1( k ) = dut1

	  fwobxy(1,i) = x * 1000.		! milliarcsec
	  fwobxy(2,i) = y * 1000.
c	  fut1pt(i) = sla_dat(mjd0) - dut1
	  fut1pt(i) = dut1
          write(*,*) 'fut1pt(i) is ', fut1pt(i)
 	end do

3000	continue

	nwob = 4
	nut1 = 4

 	fwobin(1) = mjd0 - 1.d0 + 2400000.5d0
	fwobin(2) = 1.d0
	fwobin(3) = 4.
	do i = 1, 3
	  fut1in(i) = fwobin(i)
	end do
	fut1in(4) = 1.d0

	taiutc(1) = 0.
	taiutc(2) = sla_dat(mjd0)
c	taiutc(2) = ((500000-offmicrosec)/1000000)
	taiutc(3) = 0.
	write(*,*) 'intger tai-utc is', taiutc(2)

	nepoch = 2
	rotepo(1,1) = fwobin(1)
	rotepo(1,2) = fwobin(1) + 2.
	rotepo(2,1) = 0.
	rotepo(2,2) = 0.

	goto 999


 900	continue		! error exit -----------------
	write (6, '('' EOP file lacks YEAR '')')
	stop

 909	continue
	write (6, '('' ERROR Opening File: '', A )' ) eop_file
	stop

 999	continue

	close( 66 )

	end

