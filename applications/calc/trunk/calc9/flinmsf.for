c
c	Calculates linear least mean square fit to real*4 arrays
c	xaray and yaray
c
c	Form is   y = a*x + b
c
c

	integer*4 function	flinear_mean_square_fit(
	1				xaray, yaray,
	1				length, a, b, rmserror )


	real*4		xaray(*), yaray(*), a, b, rmserror
	integer		length


c LOCAL VARIABLES

	integer		i
	real*4 		x, y, n, sumx, sumy, sumxx, sumxy, sumyy
	real*8		z

	real*4		NEGLIGIBLE
	parameter	( NEGLIGIBLE = 1.0E-15 )

	real*4		ENORMOUS
	parameter	( ENORMOUS = 1.0E15 )

c BEGIN

	a = 0.0
	b = 0.0
	rmserror = 0.0

	sumx = 0.0
	sumy = 0.0
	sumxx = 0.0
	sumxy = 0.0
	sumyy = 0.0

	do i = 1, length

		x = xaray( i )
		y = yaray( i )

		if( abs( x ) .lt. ENORMOUS .and. abs( y ) .lt. ENORMOUS ) then

			sumx = sumx + x
			sumy = sumy + y
			sumxx = sumxx + (x * x)
			sumxy = sumxy + (x * y)
			sumyy = sumyy + (y * y)

		else
			flinear_mean_square_fit = 1
			return
		end if
	end do

	n = length

	x = (n * sumxx) - (sumx * sumx)

	if( abs( x ) .lt. NEGLIGIBLE ) then
		flinear_mean_square_fit = 1
		return
	end if

	a = ((n * sumxy) - (sumx * sumy))/x
	b = ((sumy * sumxx) - (sumx * sumxy))/x

	x = sumyy - 2.0*((a * sumxy) + (b * sumy) - (a * b * sumx))
	1		+ (a * a * sumxx) + (n * b * b)

	if(x .lt. 0.0) then
		rmserror = x
		flinear_mean_square_fit = 2
		return
	end if

	rmserror = sqrt(x/n)

	flinear_mean_square_fit = 0
	return

	end

