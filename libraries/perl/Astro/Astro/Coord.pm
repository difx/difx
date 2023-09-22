package Astro::Coord;
use strict;

=head1 NAME

Astro::Coord - Astronomical coordinate transformations

=head1 SYNOPSIS

    use Astro::Coord;

    ($l, $b) = fk4gal($ra, $dec);
    ($az, $el) = eqazel($ha, $dec, $latitude);

=head1 DESCRIPTION

Astro::Coord contains an assorted set Perl routines for coordinate
conversions, such as hour angle to elevation and J2000 to B1950.

=head1 AUTHOR

Chris Phillips  Chris.Phillips@csiro.au

=head1 FUNCTIONS

=cut


BEGIN {
  use Exporter ();
  use vars qw( $VERSION @ISA @EXPORT @EXPORT_OK @EXPORT_FAIL 
	       $bepoch );
  $VERSION = '1.43';

  @ISA = qw(Exporter);

  @EXPORT      = qw( xy2azel azel2xy eqazel J2000todate
                     fk4fk5 fk5fk4 fk4gal galfk4  j2gal
                     coord_convert
                     haset_ewxy ewxy_tlos haset_azel azel_tlos
                     antenna_rise pol2r r2pol
                     );
  @EXPORT_OK   = qw ( fk4fk5r fk5fk4r fk4galr galfk4r
                      ephem_vars nutate precsn $bepoch );
  @EXPORT_FAIL = qw ( );

  use Carp;
  use POSIX qw( asin acos fmod tan );

  use Astro::Time qw( $PI rad2turn turn2rad mjd2lst );
}

$bepoch = 1950.0;

use constant JULIAN_DAY_J2000 => 2451545.0;
use constant JULIAN_DAYS_IN_CENTURY => 36525.0;

# The E-terms vector for FK4 <--> other coordinate system transforms
# (used in fk4fk5 fk5fk4 fk4gal galfk4)
my @eterm = (-1.62557E-06, -0.31919E-06, -0.13843E-06);

## The precession matrix for FK4 <--> FK5 conversions (used in
## fk4fk5 and fk5fk4)
#my @btoj = ([+0.999925678186902,-0.011182059642247,-0.004857946558960],
#	    [+0.011182059571766,+0.999937478448132,-0.000027176441185],
#	    [+0.004857946721186,-0.000027147426498,+0.999988199738770]);

# The precession matrix for FK4 <--> Galactic conversions (used in
# fk4gal and galfk4)
my @etog = ([-0.066988739415,-0.872755765852,-0.483538914632],
	    [+0.492728466075,-0.450346958020,+0.744584633283],
	    [-0.867600811151,-0.188374601723,+0.460199784784]);

# Values used in SLALIB routines

use constant D2PI => 6.283185307179586476925287;

#  Radians per year to arcsec per century
use constant PMF => 100*60*60*360/D2PI;

#  Small number to avoid arithmetic problems
use constant TINY => 1e-30;

#  Km per sec to AU per tropical century
#  = 86400 * 36524.2198782 / 149597870
use constant  VF => 21.095;

#  Vectors A and Adot, and matrix M
my @a = ( -1.62557e-6,  -0.31919e-6, -0.13843e-6,
	  +1.245e-3, -1.580e-3, -0.659e-3);

my @ad =(+1.245e-3,    -1.580e-3,   -0.659e-3);

my @em = ([+0.9999256782, -0.0111820611, -0.0048579477],
	  [+0.0111820610, +0.9999374784, -0.0000271765],
	  [+0.0048579479, -0.0000271474, +0.9999881997],
	  [-0.000551,	    -0.238565,     +0.435739],
	  [+0.238514,     -0.002667,     -0.008541],
	  [-0.435623,     +0.012254,     +0.002117]);

my @emi = ([+0.9999256795, +0.0111814828, +0.0048590039,
	    -0.00000242389840, -0.00000002710544, -0.00000001177742],
	   [-0.0111814828, +0.9999374849, -0.0000271771,
	    +0.00000002710544, -0.00000242392702, +0.00000000006585],
	   [-0.0048590040, -0.0000271557, +0.9999881946,
	    +0.00000001177742, +0.00000000006585, -0.00000242404995],
	   [-0.000551,     +0.238509,     -0.435614,
	    +0.99990432,       +0.01118145,       +0.00485852],
	   [-0.238560,     -0.002667,     +0.012254,
	    -0.01118145,       +0.99991613,       -0.00002717],
	   [+0.435730,     -0.008541,     +0.002117,
	    -0.00485852,       -0.00002716,       +0.99996684]);

=item B<pol2r>

  ($x, $y, $z) = pol2r($polar1, $polar2);

 Converts a position in polar coordinates into rectangular coordinates
   $polar1, $polar2   The polar coordinates to convert (turns)
   $x, $y, $z         The rectangular coordinates

=cut

sub pol2r ($$) {
  my ($p1, $p2) = @_;

  # Converts polar coordinates into rectangluar
  my @rect;
  $rect[0] = cos(turn2rad($p1))*cos(turn2rad($p2));
  $rect[1] = sin(turn2rad($p1))*cos(turn2rad($p2));
  $rect[2] = sin(turn2rad($p2));
  return(@rect);
}

=item B<r2pol>

  ($polar1, $polar2) = r2pol($x, $y, $z);

 Converts a position in rectangular coordinates into polar coordinates
   $x, $y, $y         The rectangular coordinates to convert
   $polar1, $polar2   The polar coordinates (turns);
 Returns undef if too few or too many arguments are passed.

=cut

sub r2pol (@) {
  # First check that we have 3 arguments
  if (scalar @_ != 3) {
    carp 'Inconsistent arguments';
    return undef ;
  }
  my ($x, $y, $z) = @_;

  # Converts rectangular coordinates to polar
  my ($tmp, $left, $right);
  $tmp = atan2($y, $x)/(2.0*$PI);

  if (ref($tmp) =~ /PDL/ ) {  # Allow to work with PDL
    $tmp -> where($tmp<0.0) .= $tmp -> where($tmp<0.0)  + 1.0;
  } elsif ($tmp < 0.0) {
    $tmp += 1.0;
  }

  $left = $tmp;
  $tmp = sqrt($x*$x + $y*$y + $z*$z);

  if (ref($tmp) =~ /PDL/) { # Allow to work with PDL
    $right = &PDL::Math::asin($z/$tmp)/(2.0*$PI);
  } else {
    $right = asin($z/$tmp)/(2.0*$PI);
  }

  return ($left, $right);
}

=item B<xy2azel>

  ($az, $el) = xy2azel($x, $y);

 Converts a telescope position in X,Y coordinates into Az,El coordinates 
   $x, $y     The X and Y coordinates (turns)
   $az, $el    The azimuth and elevation (turns)

=cut

sub xy2azel ($$) {
  my ($x, $y) = @_;

  # Convert a position in X,Y to Az,El
  my @polar = pol2r($x, $y);
  my $temp = $polar[0];
  $polar[0] = $polar[1];
  $polar[1] = $polar[2];
  $polar[2] = $temp;
  return (r2pol(@polar));
}

=item B<azel2xy>

  ($x, $y) = azel2xy($az, $el);

 Converts a position in Az,El coordinates into X,Y coordinates
   $az, $el    The azimuth and elevation (turns)
   $x, $y      The X and Y coordinate (turns)

=cut

sub azel2xy ($$) {
  my ($az, $el) = @_;

  # Convert a position in Az,El to X,Y
  my @polar = pol2r($az, $el);
  my $temp = $polar[1];
  $polar[1] = $polar[0];
  $polar[0] = $polar[2];
  $polar[2] = $temp;
  my ($x, $y) = r2pol(@polar);
  if ($x>0.5) {
    $x -= 1.0;
  }
  if ($y>0.5) {
    $y -= 1.0;
  }
  return ($x, $y);
}

=item B<eqazel>

  ($ha, $dec) = eqazel($az, $el, $latitude);
  ($az, $el) = eqazel($ha, $dec, $latitude);
  ($ha, $dec) = eqazel($az, $el, $latitude, $allownegative);

 Converts HA/Dec coordinates to Az/El and vice versa. 
   $ha, $dec     Hour angle and declination of source (turns)
   $az, $el      Azimuth and elevation of source (turns)
   $latitude     Latitude of the observatory (turns)
   $allownegative  If true, allow negative $ha or $az on return (Optional)
 Note:
  The ha,dec and az,el conversion is symmetrical

=cut

sub eqazel ($$$;$) {
  my $sphi = sin(turn2rad($_[2]));
  my $cphi = cos(turn2rad($_[2]));
  my $sleft = sin(turn2rad($_[0]));
  my $cleft = cos(turn2rad($_[0]));
  my $sright = sin(turn2rad($_[1]));
  my $cright = cos(turn2rad($_[1]));
  my $left_out = atan2(-$sleft,-$cleft*$sphi+$sright*$cphi/$cright)/(2.0*$PI);
  $left_out = ($left_out < 0.0) ? $left_out + 1.0 : $left_out 
    if (!(defined $_[3] && $_[3]));
  my $right_out= asin($cleft*$cright*$cphi + $sright*$sphi)/(2.0*$PI);

  return($left_out, $right_out);

}

=item B<fk4fk5>

 ($JRA, $JDec) = fk4fk5($BRA, $BDec);
      (@fk5) = fk4fk5(@fk4);

 Converts an FK4 (B1950) position to the equivalent FK5 (J2000) 
 position.
   $BRA,$BDec     fk4/B1950 position (turns)
   $JRA,$Dec      fk5/J2000 position (turns)
   @fk4           fk4/B1950 position (as a 3-vector)
   @fk5           fk5/J2000 position (as a 3-vector)
 Note:
  This code is based on similar routines from the Fortran SLALIB 
  package, so are quite accurate, but subject to a restrictive 
  license (see README).

=cut

sub fk4fk5 (@) {
#     - - - - - -
#      F K 4 5 Z
#     - - - - - -
#
#  Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero
#  proper motion in the FK5 frame (double precision)
#
#  This routine converts stars from the old, Bessel-Newcomb, FK4
#  system to the new, IAU 1976, FK5, Fricke system, in such a
#  way that the FK5 proper motion is zero.  Because such a star
#  has, in general, a non-zero proper motion in the FK4 system,
#  the routine requires the epoch at which the position in the
#  FK4 system was determined.
#
#  The method is from Appendix 2 of Ref 1, but using the constants
#  of Ref 4.
#
#  Given:
#     R1950,D1950     dp    B1950.0 FK4 RA,Dec at epoch (rad)
#     BEPOCH          dp    Besselian epoch (e.g. 1979.3D0)
#
#  Returned:
#     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad)
#
#  Notes:
#
#  1)  The epoch BEPOCH is strictly speaking Besselian, but
#      if a Julian epoch is supplied the result will be
#      affected only to a negligible extent.
#
#  2)  Conversion from Besselian epoch 1950.0 to Julian epoch
#      2000.0 only is provided for.  Conversions involving other
#      epochs will require use of the appropriate precession,
#      proper motion, and E-terms routines before and/or
#      after FK45Z is called.
#
#  3)  In the FK4 catalogue the proper motions of stars within
#      10 degrees of the poles do not embody the differential
#      E-term effect and should, strictly speaking, be handled
#      in a different manner from stars outside these regions.
#      However, given the general lack of homogeneity of the star
#      data available for routine astrometry, the difficulties of
#      handling positions that may have been determined from
#      astrometric fields spanning the polar and non-polar regions,
#      the likelihood that the differential E-terms effect was not
#      taken into account when allowing for proper motion in past
#      astrometry, and the undesirability of a discontinuity in
#      the algorithm, the decision has been made in this routine to
#      include the effect of differential E-terms on the proper
#      motions for all stars, whether polar or not.  At epoch 2000,
#      and measuring on the sky rather than in terms of dRA, the
#      errors resulting from this simplification are less than
#      1 milliarcsecond in position and 1 milliarcsecond per
#      century in proper motion.
#
#  References:
#
#     1  Aoki,S., et al, 1983.  Astron.Astrophys., 128, 263.
#
#     2  Smith, C.A. et al, 1989.  "The transformation of astrometric
#        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
#
#     3  Yallop, B.D. et al, 1989.  "Transformation of mean star places
#        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
#        Astron.J. 97, 274.
#
#     4  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
#        the Astronomical Almanac", ISBN 0-935702-68-7.
#
#  Called:  sla_DCS2C, sla_EPJ, sla_EPB2D, sla_DCC2S, sla_DRANRM
#
#  P.T.Wallace   Starlink   21 September 1998
#
#  Copyright (C) 1998 Rutherford Appleton Laboratory


  my ($rect, $w, $i, $j);
  my (@r0, @a1, @v1, @v2); #  Position and position+velocity vectors

  if (@_==3) { # Rectangular coordinates passed
    @r0 = @_;
    $rect = 1;
  } elsif (@_==2) { # Sperical coordinates
    @r0 = pol2r($_[0],$_[1]); #  Spherical to Cartesian
    $rect = 0;
  } elsif (@_>3) {
    croak "Too many arguments for Astro::fk4fk5 ";
  } else {
    croak "Not enough arguments for Astro::fk4fk5 ";
  }

  #  Adjust vector A to give zero proper motion in FK5
  $w=($bepoch-1950)/PMF;
  for ($i=0; $i<3; $i++) {
    $a1[$i]=$a[$i]+$w*$ad[$i];
  }
  #  Remove e-terms
  $w=$r0[0]*$a1[0]+$r0[1]*$a1[1]+$r0[2]*$a1[2];
  for ($i=0; $i<3; $i++) {
    $v1[$i]=$r0[$i]-$a1[$i]+$w*$r0[$i];
  }

  #  Convert position vector to Fricke system
  for ($i=0; $i<6; $i++) {
    $w=0;
    for ($j=0; $j<3; $j++) {
      #warn "DEBUG: [$i,$j]\n";
      $w=$w+$em[$i][$j]*$v1[$j];
      $v2[$i]=$w
    }
  }

  #  Allow for fictitious proper motion in FK4
  $w=(epj(epb2d($bepoch))-2000)/PMF;
  for ($i=0; $i<3; $i++) {
    $v2[$i]=$v2[$i]+$w*$v2[$i+3];
  }

  if ($rect) {
    return @v2[0..2];
  } else {
    #  Revert to spherical coordinates
    return r2pol(@v2[0..2]);
  }
}

=item B<fk4fk5r>

 @fk5 = fk4fk5r(@fk4);

 Converts an FK4 (B1950) position to the equivalent FK5 (J2000) position.
 Note: Convert equitoral positions to/from 3-vectors using pol2r and r2pol.
   @fk4       fk4 position (as a 3-vector, turns)
   @fk5       fk5 position (as a 3-vector, turns)
 Note:
  Just a wrapper to fk4fk5 which now handler polar and rectangular
  arguments

=cut

sub fk4fk5r (@) {
  return fk4fk5(@_);
}

#sub fk4fk5r (@) {
#  # First check that we have 3 arguments
#  if (scalar @_ < 3) {
#    croak 'Not enough arguments for Astro::Coord::fk4fk5r at ';
#  } elsif (scalar @_ > 3) {
#    croak 'Too many arguments for Astro::Coord::fk4fk5r at ';
#  }
#
#  my ($i, $j, @temp, @fk5);
#  my $w = 0.0;
#
#  # Add the eterms
#  for ($i=0 ; $i<3 ; $i++) {
#    $w += $_[$i] * $eterm[$i];
#  }
#  for ($i=0 ; $i<3 ; $i++) {
#    $temp[$i] = $_[$i] - $eterm[$i] + $w * $_[$i];
#  }
#
#  # Precess from FK4 to FK5
#  for ($i=0 ; $i<3 ; $i++) {
#    $fk5[$i] = 0.0;
#    for ($j=0 ; $j<3 ; $j++) {
#      $fk5[$i] += $btoj[$i][$j] * $temp[$j];
#    }
#  }
#
#  return(@fk5);
#}

=item B<fk5fk4>

 ($JRA, $JDec) = fk4fk5($BRA, $BDec);
       ($@fk5) = fk4fk5(@fk4);

 Converts an FK5 (J2000) position to the equivalent FK4 (B1950) position.
   $JRA,$Dec      fk5/J2000 position (turns)
   $BRA,$BDec     fk4/B1950 position (turns)
   @fk5           fk5/J2000 position (as a 3-vector)
   @fk4           fk4/B1950 position (as a 3-vector)
 Note:
  This code is based on similar routines from the Fortran SLALIB 
  package, so are quite accurate, but subject to a restrictive 
  license (see README).

=cut

sub fk5fk4 (@) {
#+
#     - - - - - -
#      F K 5 2 4
#     - - - - - -
#
#  Convert J2000.0 FK5 star data to B1950.0 FK4 (double precision)
#
#  This routine converts stars from the new, IAU 1976, FK5, Fricke
#  system, to the old, Bessel-Newcomb, FK4 system.  The precepts
#  of Smith et al (Ref 1) are followed, using the implementation
#  by Yallop et al (Ref 2) of a matrix method due to Standish.
#  Kinoshita's development of Andoyer's post-Newcomb precession is
#  used.  The numerical constants from Seidelmann et al (Ref 3) are
#  used canonically.
#
#  Given:  (all J2000.0,FK5)
#     R2000,D2000     dp    J2000.0 RA,Dec (rad)
#     DR2000,DD2000   dp    J2000.0 proper motions (rad/Jul.yr)
#     P2000           dp    parallax (arcsec)
#     V2000           dp    radial velocity (km/s, +ve = moving away)
#
#  Returned:  (all B1950.0,FK4)
#     R1950,D1950     dp    B1950.0 RA,Dec (rad)
#     DR1950,DD1950   dp    B1950.0 proper motions (rad/trop.yr)
#     P1950           dp    parallax (arcsec)
#     V1950           dp    radial velocity (km/s, +ve = moving away)
#
#  Notes:
#
#  1)  The proper motions in RA are dRA/dt rather than
#      cos(Dec)#dRA/dt, and are per year rather than per century.
#
#  2)  Note that conversion from Julian epoch 2000.0 to Besselian
#      epoch 1950.0 only is provided for.  Conversions involving
#      other epochs will require use of the appropriate precession,
#      proper motion, and E-terms routines before and/or after
#      FK524 is called.
#
#  3)  In the FK4 catalogue the proper motions of stars within
#      10 degrees of the poles do not embody the differential
#      E-term effect and should, strictly speaking, be handled
#      in a different manner from stars outside these regions.
#      However, given the general lack of homogeneity of the star
#      data available for routine astrometry, the difficulties of
#      handling positions that may have been determined from
#      astrometric fields spanning the polar and non-polar regions,
#      the likelihood that the differential E-terms effect was not
#      taken into account when allowing for proper motion in past
#      astrometry, and the undesirability of a discontinuity in
#      the algorithm, the decision has been made in this routine to
#      include the effect of differential E-terms on the proper
#      motions for all stars, whether polar or not.  At epoch 2000,
#      and measuring on the sky rather than in terms of dRA, the
#      errors resulting from this simplification are less than
#      1 milliarcsecond in position and 1 milliarcsecond per
#      century in proper motion.
#
#  References:
#
#     1  Smith, C.A. et al, 1989.  "The transformation of astrometric
#        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
#
#     2  Yallop, B.D. et al, 1989.  "Transformation of mean star places
#        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
#        Astron.J. 97, 274.
#
#     3  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
#        the Astronomical Almanac", ISBN 0-935702-68-7.
#
#  P.T.Wallace   Starlink   19 December 1993
#
#  Copyright (C) 1995 Rutherford Appleton Laboratory
#-
  my ($rect, @v1, @v2);
  if (@_==3) { # Rectangular coordinates passed
    @v1 = @_;
    $rect = 1;
  } elsif (@_==2) { # Sperical coordinates
    @v1 = pol2r($_[0],$_[1]); #  Spherical to Cartesian
    $rect = 0;
  } elsif (@_>2) {
    croak "Too many arguments for Astro::fk5fk4 ";
  } else {
    croak "Not enough arguments for Astro::fk5fk4 ";
  }

#  Miscellaneous
  my ($w, $x, $y, $z, $wd, $rxyz);
  my ($ur, $ud, $xd, $yd, $zd);
  my ($i,$j);

  #  Convert position+velocity vector to BN system
  for ($i=0; $i<6; $i++) {
    $w=0.0;
    ##for ($j=0; $j<6; $j++) {
    for ($j=0; $j<3; $j++) {
      $w=$w+$emi[$i][$j]*$v1[$j];
    }
    $v2[$i]=$w;
  }

#  Position vector components and magnitude
  $x=$v2[0];
  $y=$v2[1];
  $z=$v2[2];
  $rxyz=sqrt($x*$x+$y*$y+$z*$z);

#  Apply E-terms to position
  $w=$x*$a[0]+$y*$a[1]+$z*$a[2];
  $x=$x+$a[0]*$rxyz-$w*$x;
  $y=$y+$a[1]*$rxyz-$w*$y;
  $z=$z+$a[2]*$rxyz-$w*$z;

#  Recompute magnitude
  $rxyz=sqrt($x*$x+$y*$y+$z*$z);

#  Apply E-terms to both position and velocity
  $x=$v2[0];
  $y=$v2[1];
  $z=$v2[2];
  $w=$x*$a[0]+$y*$a[1]+$z*$a[2];
  $wd=$x*$a[3]+$y*$a[4]+$z*$a[5];
  $x=$x+$a[0]*$rxyz-$w*$x;
  $y=$y+$a[1]*$rxyz-$w*$y;
  $z=$z+$a[2]*$rxyz-$w*$z;
  $xd=$v2[3]+$a[3]*$rxyz-$wd*$x;
  $yd=$v2[4]+$a[4]*$rxyz-$wd*$y;
  $zd=$v2[5]+$a[5]*$rxyz-$wd*$z;

  my @r;
  if ($rect) {
    @r = ($x, $y, $z);
  } else {
    @r= r2pol($x, $y, $z);
  }

#  my $rxysq =$x*$x+$y*$y;
#  my $rxy = sqrt($rxysq);
#  if ($rxy>TINY) {
#    $ur=($x*$yd-$y*$xd)/$rxysq;
#    $ud=($zd*$rxysq-$z*($x*$xd+$y*$yd))/(($rxysq+$z*$z)*$rxy);
#  }
#
##  Return results
#  my $dr1950=$ur/PMF;
#  my $dd1950=$ud/PMF;

  return(@r);
}


=item B<fk5fk4r>

 @fk4 = fk5fk4r(@fk5);

 Converts an FK5 (J2000) position to the equivalent FK4 (B1950) position.
 Note: Convert equitoral positions to/from 3-vectors using pol2r and r2pol.
   @fk4       fk4 position (as a 3-vector, turns)
   @fk5       fk5 position (as a 3-vector, turns)
 Note:
  Just a wrapper to fk5fk4 which now handler polar and rectangular
  arguments

=cut

sub fk5fk4r (@) {
  return fk5fk4(@_);
}


#sub fk5fk4 (@) {
##     - - - - - -
##      F K 5 4 Z
##     - - - - - -
##
##  Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming
##  zero proper motion and parallax (double precision)
##
##  This routine converts star positions from the new, IAU 1976,
##  FK5, Fricke system to the old, Bessel-Newcomb, FK4 system.
##
##  Given:
##     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad)
##     BEPOCH          dp    Besselian epoch (e.g. 1950D0)
##
##  Returned:
##     R1950,D1950     dp    B1950.0 FK4 RA,Dec (rad) at epoch BEPOCH
##     DR1950,DD1950   dp    B1950.0 FK4 proper motions (rad/trop.yr)
##
##  Notes:
##
##  1)  The proper motion in RA is dRA/dt rather than cos(Dec)#dRA/dt.
##
##  2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
##      only is provided for.  Conversions involving other epochs will
##      require use of the appropriate precession routines before and
##      after this routine is called.
##
##  3)  Unlike in the sla_FK524 routine, the FK5 proper motions, the
##      parallax and the radial velocity are presumed zero.
##
##  4)  It is the intention that FK5 should be a close approximation
##      to an inertial frame, so that distant objects have zero proper
##      motion;  such objects have (in general) non-zero proper motion
##      in FK4, and this routine returns those fictitious proper
##      motions.
##
##  5)  The position returned by this routine is in the B1950
##      reference frame but at Besselian epoch BEPOCH.  For
##      comparison with catalogues the BEPOCH argument will
##      frequently be 1950D0.
##
##  Called:  sla_FK524, sla_PM
##
##  P.T.Wallace   Starlink   10 April 1990
##
##  Copyright (C) 1995 Rutherford Appleton Laboratory
#
#  my $bepoch = 1950.0;
#
#  my $rect;
#  if (@_>3) {
#    croak "Too many arguments for Astro::fk5fk4 ";
#  } elsif (@_<2) {
#    croak "Not enough arguments for Astro::fk5fk4 ";
#  }
#  my @r2000 = @_;
#
#  #  fk5 equinox j2000 (any epoch) to fk4 equinox b1950 epoch b1950
#  my (@r1950) = fk524(@r2000);
#  my $dd1950 = pop @r1950;
#  my $dr1950 = pop @r1950;
#
#  ##  fictitious proper motion to epoch bepoch
#  #my ($r1950, $d1950) = pm($r,$d,$dr1950,$dd1950,0.0,0.0,1950,$bepoch);
#  return @r1950;
#}

#=item B<fk5fk4r>
#
#  @fk4 = fk5fk4r(@fk5);
#
# Converts an FK5 (J2000) position to the equivalent FK4 (B1950) position.
# Note: Convert equitoral positions to/from 3-vectors using pol2r and r2pol.
#   @fk5     fk5 position (as a 3-vector, turns)
#   @fk4     fk4 position (as a  3-vector, turns)
#
#=cut
#
#sub fk5fk4r(@) {
#
#  # First check that we have 3 arguments
#  if (scalar @_ < 3) {
#    croak 'Not enough arguments for Astro::Coord::fk5fk4r at ';
#  } elsif (scalar @_ > 3) {
#    croak 'Too many arguments for Astro::Coord::fk5fk4r at ';
#  }
#
#  my ($i, $j, @fk4);
#  my $w = 0.0;
#
#  # Precess.  Note : the same matrix is used as for the FK4 -> FK5
#  #                  transformation, but we have transposed it within the
#  #                  for loop
#
#  for ($i=0 ; $i<3 ; $i++) {
#    $fk4[$i] = 0.0;
#    for ($j=0 ; $j<3 ; $j++) {
#      $fk4[$i] += $btoj[$j][$i] * $_[$j];
#    }
#  }
#
#  # Allow for e-terms 
#  for ($i=0 ; $i<3 ; $i++) {
#    $w += $_[$i] * $eterm[$i];
#  }
#  $w += 1.0;
#  for ($i=0 ; $i<3 ; $i++) {
#    $fk4[$i] = ($fk4[$i] + $eterm[$i])/$w;
#  }
#
#  return(@fk4);
#}

=item B<fk4galr>

  @gal = fk4galr(@fk4)

 Converts an FK4 position (B1950.0) to the IAU 1958 Galactic
 coordinate system
 Note: convert equitoral positions to/from 3-vectors using pol2r and r2pol.
   @fk4     fk4 position to convert (as a 3-vector, turns)
   @gal     Galactic position (as a 3-vector, turns)
 Returns undef if too few or two many arguments are passed.
 Reference : Blaauw et al., 1960, MNRAS, 121, 123.

=cut

# Within 1e-7 arcsec of SLALIB slaEg50
sub fk4galr(@) {
  # First check that we have 3 arguments
  if (scalar @_ < 3) {
    croak 'Not enough arguments for Astro::Coord::fk4galr at ';
  } elsif (scalar @_ > 3) {
    croak 'Too many arguments for Astro::Coord::fk4galr at ';
  }

  my ($i, $j, @temp, @gal);
  my $w = 0.0;

  # Allow for e-terms
  for ($i=0 ; $i<3 ; $i++) {
    $w += $_[$i] * $eterm[$i];
  }
  for ($i=0 ; $i<3 ; $i++) {
    $temp[$i] = $_[$i] - $eterm[$i] + $w * $_[$i];
  }


  # Precess
  for ($i=0 ; $i<3 ; $i++) {
    $gal[$i] = 0.0;
    for ($j=0 ; $j<3 ; $j++) {
      $gal[$i] += $etog[$i][$j] * $temp[$j];
    }
  }

  return(@gal);
}

=item B<galfk4>

  ($bRA, $bDec) = galfk4($l, $b);
  @fk4 = galfk4(@gal);

 Converts an IAU 1958 Galactic position to the FK4 coordinate system (B1950)
 Notes: Converts equitoral positions to/from 3-vectors using pol2r and r2pol.
   $BRA,$BDec  fk4/B1950 position (turns)
   $l, $b      Galactic longitude and latitude
   @gal        Galactic position (as a 3-vector, turns)
   @fk4        fk4 position (as a  3-vector, turns)
 Reference : Blaauw et al., 1960, MNRAS, 121, 123.

=cut

# Within 1e-7 arcsec of SLALIB slaGe50
sub galfk4(@) {
  my (@r, $rect);

  if (@_==3) { # Rectangular coordinates passed
    @r = @_;
    $rect = 1;
  } elsif (@_==2) { # Sperical coordinates
    @r = pol2r($_[0],$_[1]); #  Spherical to Cartesian
    $rect = 0;
  } elsif (@_>3) {
    croak "Too many arguments for Astro::galfk4 at";
  } else {
    croak "Not enough arguments for Astro::galfk4 at";
  }

  my ($i, $j, @fk4);
  my $w = 0.0;

  # Precess.  Note : the same matrix is used as for the FK4 -> Galactic
  #                  transformation, but we have transposed it within the
  #                  for loop
  for ($i=0 ; $i<3 ; $i++) {
    $fk4[$i] = 0.0;
    for ($j=0 ; $j<3 ; $j++) {
      $fk4[$i] += $etog[$j][$i] * $r[$j];
    }
  }

  # Allow for e-terms */
  for ($i=0 ; $i<3 ; $i++) {
    $w += $r[$i] * $eterm[$i];
  }
  $w += 1.0;
  for ($i=0 ; $i<3 ; $i++) {
    $fk4[$i] = ($fk4[$i] + $eterm[$i])/$w;
  }

  if ($rect) {
    return @fk4;
  } else {
    return r2pol(@fk4);
  }
}

sub galfk4r(@) {galfk4(@_)};

#=item B<fk4fk5>
#
# ($JRA, $JDec) = fk4fk5($BRA, $BDec);
#
# Converts an FK4 (B1950) position to the equivalent FK5 (J2000) position.
#   **LOW PRECISION**
#   $BRA,$BDec     fk4/B1950 position (turns)
#   $JRA,$Dec      fk5/J2000 position (turns)
#
#=cut
#
#sub fk4fk5 ($$) {
#  return r2pol(fk4fk5r(pol2r(shift,shift)));
#}

#=item B<fk5fk4>
#
# ($BRA, $BDec) = fk5fk4($JRA, $JDec);
#
# Converts an FK5 (J2000) position to the equivalent FK4 (B1950) position.
#   **LOW PRECISION**
#   $JRA,$Dec      fk5/J2000 position (turns)
#   $BRA,$BDec     fk4/B1950 position (turns)
#
#=cut
#
#sub fk5fk4 ($$) {
#  return r2pol(fk5fk4r(pol2r(shift,shift)));
#}

=item B<fk4gal>

  ($l, $b) = fk4gal($ra, $dec);

 Converts an FK4 position (B1950) to the IAU 1958 Galactic
 coordinate system
   ($ra, $dec)  fk4 position to convert (turns)
   ($l, $b)     Galactic position (turns)
 Reference : Blaauw et al., 1960, MNRAS, 121, 123.

=cut

sub fk4gal ($$) {
  return r2pol(fk4galr(pol2r(shift,shift)));
}

#=item B<galfk4>
#
#  ($ra, $dec) = galfk4($l, $b);
#
# Converts an IAU 1958 Galactic coordinate system position 
# to FK4  (B1950).
#   ($l, $b)    Galactic position (turns)
#  ($ra, $dec)  fk4 position to convert (turns)
#  Reference : Blaauw et al., 1960, MNRAS, 121, 123.
#
#=cut
#
#sub galfk4 ($$) {
#  return r2pol(galfk4r(pol2r(shift,shift)));
#}

=item B<ephem_vars>

  ($omega, $rma, $mlanom, $F, $D, $eps0) = ephem_vars($jd)

  Given the Julian day ($jd) this routine calculates the ephemeris
  values required by the prcmat and nutate routines

  The returned values are :
    $omega  - Longitude of the ascending node of the Moons mean orbit on
              the ecliptic, measured from the mean equinox of date.
    $rma    - Mean anomaly of the Sun.
    $mlanom - Mean anomaly of the Moon.
    $F      - L - omega, where L is the mean longitude of the Moon.
    $D      - Mean elongation of the Moon from the Sun.
    $eps0   - Mean obilquity of the ecliptic.

=cut

=item B<J2000todate>


 ($DRA, $DDec) = J2000todate($JRA, $JDec, $mjd);
 @date = J2000todate(@J2000, $mjd);

 Converts an J2000 position date coordinate

   $DRA,$DDec     Date coordinate (turns)
   $JRA,$Dec      J2000 position (turns)
   @date          Date coordinate (as a 3-vector)
   @J2000         J2000 position (as a 3-vector)

=cut

# Untested
sub J2000todate(@) {

  my ($rect);
  my (@J2000, @date); #  Position  vectors

  my $mjd = pop @_;
  if (@_==3) { # Rectangular coordinates passed
    @J2000 = @_;
    $rect = 1;
  } elsif (@_==2) { # Sperical coordinates
    @J2000 = pol2r($_[0],$_[1]); #  Spherical to Cartesian
    $rect = 0;
  } elsif (@_>3) {
    croak "Too many arguments for Astro::Coord::J2000todate ";
  } else {
    croak "Not enough arguments for Astro::Coord::J2000todate ";
  }

  # compute the general precession matrix.
  my @gp = precsn(JULIAN_DAY_J2000, $mjd+2400000.5);

  # Determine ephemeris quantities
  my ($deps, $dpsi);
  my @nu = ();
  my ($omega, $rma, $mlanom, $F, $D, $eps0) = ephem_vars($mjd+2400000.5);
  ($deps, $dpsi, @nu) = nutate($omega, $F, $D, $rma, $mlanom, $eps0);

  my @prcmat = ();
  for (my $i=0 ; $i<3 ; $i++) {
    for (my $j=0 ; $j<3 ; $j++) {
      my $xx = 0.0;
      for (my $k=0 ; $k<3 ; $k++) {
	$xx = $xx + $gp[$i][$k] * $nu[$k][$j];
      }
      $prcmat[$i][$j] = $xx;
    }
  }

  for (my $i=0 ; $i<3 ; $i++) {
    $date[$i] = 0.0;
    for (my $j=0 ; $j<3 ; $j++) {
      $date[$i] += $prcmat[$i][$j] * $J2000[$j];
    }
  }

  if ($rect) {
    return @date;
  } else {
    #  Revert to spherical coordinates
    return r2pol(@date);
  }
}

sub ephem_vars ($) {
  my $epoch = shift;

  # Calculates values required internally by prcmat and for nutate from
  # the passed Julian Day

  # Calculate the interval to/from J2000 in Julian Centuries
  my $jcents = ($epoch-(JULIAN_DAY_J2000))/JULIAN_DAYS_IN_CENTURY;

  # Calculate the longitude of the mean ascending node of the lunar
  # orbit on the ecliptic [A.A. Suppl. 1984, p S26]
  my $omega = (((0.000000039 * $jcents + 0.000036143) *
		$jcents - 33.757045934) *
	       $jcents + 2.182438624)/(2.0*$PI);
  $omega = fmod($omega,1.0);
  if ($omega < 0.0) {
    $omega += 1.0;
  }

  # Calculate the mean anomaly. [A.A suppl. 1984, p S26]
  my $manom = (6.240035939 - ((5.818e-8 * $jcents +2.797e-6) * $jcents - 
			      628.301956024) * $jcents)/(2.0*$PI);
  $manom = fmod($manom,1.0);
  if ($manom < 0.0) {
    $manom += 1.0;
  }

  # Calculate the mean anomaly of the Moon. [A.A. Suppl, 1984, p S26]
  my $mlanom = (((0.000000310 * $jcents + 0.000151795) * $jcents
		 +8328.691422884) * $jcents + 2.355548394)/(2.0*$PI);
  $mlanom = fmod($mlanom,1.0);
  if ($mlanom < 0.0) {
    $mlanom += 1.0;
  }

  # Calculate the longitude of the moon from ascending node.
  # [A.A. Suppl, 1984, p S26]
  my $F = (((0.000000053 * $jcents - 0.000064272) * $jcents + 8433.466158318) 
	   * $jcents + 1.627901934)/(2.0*$PI);
  $F = fmod($F,1.0);
  if ($F < 0.0) {
    $F += 1.0;
  }

  # Calculate the mean elongation of the moon from the sun.
  # [A.A suppl. 1984, p S26]
  my $D = (((0.000000092 * $jcents + 0.000033409) * $jcents + 7771.377146171) 
	   * $jcents + 5.198469514)/(2.0*$PI);
  $D = fmod($D,1.0); 
  if ($D < 0.0) {
    $D += 1.0;
  }

  # Calculate the mean obliquity of the ecliptic = mean obliquity.
  # [A.A suppl. 1984, p S26]
  my $eps0 = (((0.000000009 * $jcents - 0.000000003) * $jcents - 0.000226966) 
	      * $jcents + 0.409092804)/(2.0*$PI);
  return($omega, $manom, $mlanom, $F, $D, $eps0)
}

=item B<nutate>

  ($deps, $dpsi, @nu) = nutate($omega, $F, $D, $rma, $mlanom, $eps0);

  To calculate the nutation in longitude and obliquity according to
  the 1980 IAU Theory of Nutation including terms with amplitudes
  greater than 0.01 arcsecond.  The nutation matrix is used to
  compute true place from mean place: true vector = N x mean place
  vector where the three components of each vector are the direction
  cosines wrt the mean equinox and equator.

       /   1          -dpsi.cos(eps)    -dpsi.sin(eps)  \
      |                                                  |
  N = |  dpsi.cos(eps)      1               -deps        |
      |                                                  |
       \ dpsi.sin(eps)    deps                 1        /

  The required inputs are : (NOTE: these are the values returned by ephem_vars)
    $omega  - Longitude of the ascending node of the Moons mean orbit on 
              the ecliptic, measured from the mean equinox of date.
    $rma    - Mean anomaly of the Sun.
    $mlanom - Mean anomaly of the Moon.
    $F      - L - omega, where L is the mean longitude of the Moon.
    $D      - Mean elongation of the Moon from the Sun.
    $eps0   - Mean obilquity of the ecliptic.

  The returned values are :
    $deps - nutation in obliquity
    $dpsi - nutation in longitude (scalar)
    @nu   - nutation matrix (array [3][3])

=cut

sub nutate ($$$$$$) {
  my ($omega, $F, $D, $manom, $mlanom, $eps0) = @_;

  my $arg1 = $omega;
  my $arg2 = 2.0 * $omega;
  my $arg9 = 2.0 * ($F-$D+$omega);
  my $arg10 = $manom;
  my $arg11 = $arg9 + $arg10;
  my $arg12 = $arg9 - $arg10;
  my $arg13 = $arg9 - $arg1;
  my $arg31 = 2.0 * ($F+$omega);
  my $arg32 = $mlanom;
  my $arg33 = $arg31 - $arg1;
  my $arg34 = $arg31 + $arg32;
  my $arg35 = $mlanom - 2.0 * $D;
  my $arg36 = $arg31 - $arg32;

  my $dpsi = (-0.000083386 * sin($arg1*2.0*$PI)
	      +0.000001000 * sin($arg2*2.0*$PI)
	      -0.000006393 * sin($arg9*2.0*$PI)
	      +0.000000691 * sin($arg10*2.0*$PI)
	      -0.000000251 * sin($arg11*2.0*$PI)
	      +0.000000105 * sin($arg12*2.0*$PI)
	      +0.000000063 * sin($arg13*2.0*$PI)
	      -0.000001102 * sin($arg31*2.0*$PI)
	      +0.000000345 * sin($arg32*2.0*$PI)
	      -0.000000187 * sin($arg33*2.0*$PI)
	      -0.000000146 * sin($arg34*2.0*$PI)
	      -0.000000077 * sin($arg35*2.0*$PI)
	      +0.000000060 * sin($arg36*2.0*$PI))/(2.0*$PI);

  my $deps = ( 0.000044615 * cos($arg1*2.0*$PI)
	       -0.000000434 * cos($arg2*2.0*$PI)
	       +0.000002781 * cos($arg9*2.0*$PI)
	       +0.000000109 * cos($arg11*2.0*$PI)
	       +0.000000474 * cos($arg31*2.0*$PI)
	       +0.000000097 * cos($arg33*2.0*$PI)
	       +0.000000063 * cos($arg34*2.0*$PI))/(2.0*$PI);
  my $eps = $eps0 + $deps;

  my @N = ([1.0,  -($dpsi)*(2.0*$PI)*cos($eps*2.0*$PI), 
	    -($dpsi)*(2.0*$PI)*sin($eps*2.0*$PI)],
	   [0.0, 1.0, -($deps)*(2.0*$PI)],
	   [0.0, ($deps)*(2.0*$PI), 1.0]);
  $N[1][0] = -1.0*$N[0][1];
  $N[2][0] = -1.0*$N[0][2];
  return($deps, $dpsi, @N);
}

=item B<precsn>

  @gp = precsn($jd_start, $jd_stop);

  To calculate the precession matrix P for dates AFTER 1984.0 (JD =
  2445700.5) Given the position of an object referred to the equator
  and equinox of the epoch $jd_start its position referred to the
  equator and equinox of epoch $jd_stop can be calculated as follows :

  1) Express the position as a direction cosine 3-vector (V1)
     (use pol2r to do this).
  2) The corresponding vector V2 for epoch jd_end is V2 = P.V1

  The required inputs are :
    $jd_start - The Julian day of the current epoch of the coordinates.
    $jd_end   - The Julian day at the required epoch for the conversion.

  The returned values are :
    @gp - The required precession matrix (array [3][3])

=cut

sub precsn ($$) {
  my ($jd_start, $jd_end) = @_;

  my @a = (0.011180860865024,
	   0.000006770713945,
	   -0.000000000673891,
	   0.000001463555541,
	   -0.000000001667759,
	   0.000000087256766);
  my @b = (0.011180860865024,
	   0.000006770713945,
	   -0.000000000673891,
	   0.000005307158404,
	   0.000000000319977,
	   0.000000088250634);
  my @d = (0.009717173455170,
	   -0.000004136915141,
	   -0.000000001052046,
	   0.000002068457570,
	   0.000000001052046,
	   -0.000000202812107);

  my $t  = ($jd_start - JULIAN_DAY_J2000)/JULIAN_DAYS_IN_CENTURY;
  my $st = ($jd_end - $jd_start)/JULIAN_DAYS_IN_CENTURY;
  my $t2 = $t * $t;
  my $st2 = $st * $st;
  my $st3 = $st2 * $st;

  # Calculate the Equatorial precession parameters
  # (ref.   USNO Circular no. 163      1981,
  #         Lieske et al., Astron. & Astrophys., 58, 1 1977)

  my $zeta = ($a[0] + $a[1]*$t + $a[2]*$t2) * $st + 
    ($a[3] + $a[4]*$t) * $st2 + $a[5] * $st3;
  my $z = ($b[0] + $b[1]*$t + $b[2]*$t2) * $st + 
    ($b[3] + $b[4]*$t) * $st2 + $b[5] * $st3;
  my $theta = ($d[0] + $d[1]*$t + $d[2]*$t2) * $st - 
    ($d[3] + $d[4]*$t) * $st2 + $d[5] * $st3;

  # Calculate the P matrix 
  my @precession = ([0.0, 0.0, 0.0],
		    [0.0, 0.0, 0.0],
		    [0.0, 0.0, 0.0]);
  $precession[0][0] =  cos($zeta)*cos($z)*cos($theta) - sin($zeta)*sin($z);
  $precession[0][1] = -sin($zeta)*cos($z)*cos($theta) - cos($zeta)*sin($z);
  $precession[0][2] = -cos($z)*sin($theta);
  $precession[1][0] =  cos($zeta)*sin($z)*cos($theta) + sin($zeta)*cos($z);
  $precession[1][1] = -sin($zeta)*sin($z)*cos($theta) + cos($zeta)*cos($z);
  $precession[1][2] = -sin($z)*sin($theta);
  $precession[2][0] =  cos($zeta)*sin($theta);
  $precession[2][1] = -sin($zeta)*sin($theta);
  $precession[2][2] =  cos($theta);

  return(@precession);
}

=item B<coord_convert>

  ($output_left, $output_right) = coord_convert($input_left, $input_right,
                                                $input_mode, $output_mode,
                                                $mjd, $longitude, $latitude,
						$ref0);

  A routine for converting between any of the following coordinate systems :
        Coordinate system                               input/output mode
        -----------------                               -----------------
    X, Y (East-West mounted)                                    0
    Azimuth, Elevation                                          1
    Hour Angle, Declination                                     2
    Right Ascension, Declination (date, J2000 or B1950)       3,4,5
    Galactic (B1950)                                            6

  The last four parameters in the call ($mjd, $longitude, $latitude
  and $ref0) are not always required for the coordinate conversion.
  In particular if the conversion is between two coordinate systems
  which are fixed with respect to the celestial sphere (RA/Dec J2000,
  B1950 or Galactic), or two coordinate systems which are fixed with
  respect to the antenna (X/Y and Az/El) then these parameters are not
  used (NOTE: they must always be passed, even if they only hold 0 or
  undef as the routine will return undef if it is not passed 8
  parameters).  The RA/Dec date system is defined with respect to the
  celestial sphere, but varies with time.  The table below shows which
  of $mjd, $longitude, $latitude and $ref0 are used for a given
  conversion.  If in doubt you should determing the correct values for
  all input parameters, no checking is done in the routine that the
  passed values are sensible.

                Conversion                 $mjd $longitude $latitude $ref0
  ------------------------------------------------------------------------
  Galactic,             Galactic,
  RA/Dec J2000,B1950 <->RA/Dec J2000, B1950  N       N         N       N

  Galactic,
  RA/Dec J2000,B1950 <->RA/Dec date          Y       N         N       N

  Galactic,
  RA/Dec J2000,B1950,<->HA/Dec               Y       Y         N       N
  date

  Galactic,
  RA/Dec J2000,B1950,<->X/Y, Az/El           Y       Y         Y       Y
  date

  X/Y, Az/El         <->X/Y, Az/El           N       N         N       N

  X/Y, Az/El         <->HA/Dec               N       N         Y       Y


  NOTE :  The method used for refraction correction is asymptotic at
	  an elevation of 0 degrees.

  The required inputs are :
    $input_left   - The left/longitude input coordinate (turns)
    $input_right  - The right/latitude input coordinate (turns)
    $input_mode   - The mode of the input coordinates (0-6)
    $output_mode  - The mode to convert the coordinates to.
    $mjd          - The time as modified Julian day (if necessary) at
                    which to perform the conversion
    $longitude    - The longitude of the location/observatory (if necessary)
                    at which to perform the conversion (turns)
    $latitude     - The latitude of the location/observatory (if necessary)
                    at which to perform the conversion (turns)
    $ref0         - The refraction constant (if in doubt use 0.00005).

  The returned values are :
    $output_left  - The left/longitude output coordinate (turns)
    $output_right - The right/latitude output coordinate (turns)

=cut

sub coord_convert ($$$$;$$$$) {
  my ($input_left, $input_right, $input_mode, $output_mode, $mjd, $longitude,
      $latitude, $ref0) = @_;

  # Some required constants
  my ($EWXY, $AZEL, $HADEC, $DATE, $J2000, $B1950, $GALACTIC) = 0..6;

  # First check what the input and output modes are.
  if (($input_mode < $EWXY) || ($input_mode > $GALACTIC)) {
    carp "Invalid input coordinate mode : $input_mode\n".
      "Valid inputs are numbers in the range 0-6, which corrspond to X/Y, ".
	"Az/El,\n HA/Dec, RA/Dec (date), RA/Dec (J2000), RA/Dec (B1950), ".
	  "Galactic.";
    return undef;
  }
  if (($output_mode < $EWXY) || ($output_mode > $GALACTIC)) {
    carp "Invalid output coordinate mode : $output_mode\n".
      "Valid outputs are numbers in the range 0-6, which corrspond to X/Y, ".
	"Az/El,\n HA/Dec, RA/Dec (date), RA/Dec (J2000), RA/Dec (B1950), ".
	  "Galactic.";
    return undef;
  }

  # Check we have the correct parameters passed

  # Need mjd
  if ((($input_mode>=$DATE && $output_mode<=$DATE) ||
     ($input_mode<=$DATE && $output_mode>=$DATE)) &&
     !(defined($mjd))) {
    carp '$mjd parametr missing';
    return undef;
  }
  # Need longitude
  if ((($input_mode>=$HADEC && $output_mode<=$AZEL) ||
     ($input_mode<=$HADEC && $output_mode>=$HADEC)) &&
     !(defined($longitude))) {
    carp '$longitude parametr missing';
    return undef;
  }
  # Need latitude
  if ((($input_mode>=$HADEC && $output_mode<$HADEC) ||
     ($input_mode<=$AZEL && $output_mode>$AZEL)) &&
     !(defined($latitude))) {
    carp '$latitude parameter missing';
    return undef;
  }
  # Need ref0
  if ((($input_mode>=$HADEC && $output_mode<$HADEC) ||
     ($input_mode<=$AZEL && $output_mode>$AZEL)) &&
     !(defined($ref0))) {
    carp '$ref0 parameter missing';
    return undef;
  }

  # If necessary determine ephemeris quantities (if either of the modes are
  # date, HA/Dec, AzEl or EWXY).
  my ($omega, $rma, $mlanom, $F, $D, $eps0, $deps, $dpsi);
  my @nu = ();

  if (($input_mode<=$DATE && $output_mode>=$DATE) ||
     ($input_mode>=$DATE && $output_mode<=$DATE)) {
    ($omega, $rma, $mlanom, $F, $D, $eps0) = ephem_vars($mjd+2400000.5);
    ($deps, $dpsi, @nu) = nutate($omega, $F, $D, $rma, $mlanom, $eps0);
  }

  my @vonc = ();
  if (($input_mode<=$HADEC && $output_mode>=$DATE) ||
     ($input_mode>=$DATE && $output_mode<=$HADEC)) {

    # Calculate the interval to/from J2000 in Julian Centuries
    my $jcents = ($mjd+2400000.5-(JULIAN_DAY_J2000))/JULIAN_DAYS_IN_CENTURY;

    # Compute the eccentricity of the Earth's orbit (in radians)
    # [Explanatory supplement to the Astronomical Ephemeris 1961, p 98]
    my $e = (-0.000000126 * $jcents - 0.00004205) * $jcents + 0.016709114;

    # Compute the eccentric anomaly, by iteratively solving :
    #   ea = e*sin(ea) - rma
    my $ea = $rma;
    my $xx;
    do {
      $xx = $ea;
      $ea = $xx + ($rma - $xx + $e*sin($xx)) / (1.0 - $e*cos($xx));
    } while (abs($ea -$xx) > 1.0e-9);

    # Compute the mean longitude of perihelion, in radians
    # (reference as for `e').
    my $perihl = ((0.00000005817764*$jcents + 0.000008077) * $jcents
	       + 0.030010190) * $jcents + 1.796613066;

    # Calculate the equation of the equinoxes
    #my $eqenx = $dpsi * cos(($eps0+$deps)*2.0*$PI);

    # Compute the abberation vector
    my $eps = $eps0 + $deps;
    $xx = 0.00009936508 / (1.0 - $e*cos($ea));
    my $efac = sqrt(1.0 - $e*$e);
    $vonc[0] = $xx * (-cos($perihl)*sin($ea) - $efac*sin($perihl)*cos($ea));
    $vonc[1] = $xx * (-sin($perihl)*cos($eps)*sin($ea) + 
		      $efac*cos($perihl)*cos($eps)*cos($ea));
    $vonc[2] = $xx * (-sin($perihl)*sin($eps)*sin($ea) +
		      $efac*cos($perihl)*sin($eps)*cos($ea));

  }

  my @prcmat = ();
  if (($input_mode<=$DATE && $output_mode>=$J2000) ||
      ($input_mode>=$J2000 && $output_mode<=$DATE)) {

    # compute the general precession matrix. */
    my @gp = precsn(JULIAN_DAY_J2000, $mjd+2400000.5);

    # The matrices returned from nutate (nu) and precsn (gp) can be used
    # to convert J2000 coordinates to date by :
    # (coords at date) = gp * nu * (coords at J2000)
    # gp and nu can be combined to give the required precession matrix

    for (my $i=0 ; $i<3 ; $i++) {
      for (my $j=0 ; $j<3 ; $j++) {
	my $xx = 0.0;
	for (my $k=0 ; $k<3 ; $k++) {
	  $xx = $xx + $gp[$i][$k] * $nu[$k][$j];
	}
	$prcmat[$i][$j] = $xx;
      }
    }
  }

  my $lmst;
  if (($input_mode<=$HADEC && $output_mode>=$DATE) ||
      ($output_mode<=$HADEC && $input_mode>=$DATE)) {
    $lmst = mjd2lst($mjd, $longitude);
  }

  # Perform the conversion
  my (@lb, @b1950, @j2000, @date, $ra, $ha, $dec, $az, $el, $x, $y);
  if ($input_mode == $GALACTIC) {
    @lb = pol2r($input_left, $input_right);
  } elsif ($input_mode == $B1950) {
    @b1950 = pol2r($input_left, $input_right);
  } elsif ($input_mode == $J2000) {
    @j2000 = pol2r($input_left, $input_right);
  } elsif ($input_mode == $DATE) {
    @date = pol2r($input_left, $input_right);
  } elsif ($input_mode == $HADEC) {
    $ha = $input_left;
    $dec = $input_right;
  } elsif ($input_mode == $AZEL) {
    $az = $input_left;
    $el = $input_right;
  } else {
    $x = $input_left;
    $y = $input_right;
  }

  # Conversion is to a "lower" mode
  if ($output_mode < $input_mode) {

    # Convert from Galactic to B1950
    if ($input_mode == $GALACTIC) {
      @b1950 = galfk4r(@lb);
    }

    # Convert from B1950 to J2000
    if (($input_mode >= $B1950) && ($output_mode < $B1950)) {
      @j2000 = fk4fk5r(@b1950);
    }

    # Precess from J2000 to date
    if (($input_mode >= $J2000) && ($output_mode < $J2000)) {
      for (my $i=0 ; $i<3 ; $i++) {
	$date[$i] = 0.0;
	for (my $j=0 ; $j<3 ; $j++) {
	  $date[$i] += $prcmat[$i][$j] * $j2000[$j];
	}
      }
    }

    # Convert from date to HA/Dec
    if (($input_mode >= $DATE) && ($output_mode < $DATE)) {

      # Convert to geometrical equitorial coordinates
      for (my $i=0 ; $i<3 ; $i++) {
	$date[$i] += $vonc[$i];
      }

      # Convert from retangular back to polar coordinates
      ($ra, $dec) = r2pol(@date);

      # Convert to hour angle
      $ha = $lmst - $ra;
      if ($ha < 0.0) {
	$ha += 1.0;
      }
    }

    # Convert from HA/Dec to Az/El
    if (($input_mode >= $HADEC) && ($output_mode < $HADEC)) {
      ($az, $el) = eqazel($ha, $dec, $latitude);

      # Correct for refraction
      $el += $ref0/tan($el*2.0*$PI);
    }

    # Convert from Az/El to X/Y
    if (($input_mode >= $AZEL) && ($output_mode < $AZEL)) {
      ($x, $y) = azel2xy($az, $el);
    }
  } else {
    # Convert from X/Y to Az/El
    if (($input_mode == $EWXY) && ($output_mode > $EWXY)) {
      ($az, $el) = xy2azel($x, $y);
    }

    # Convert from Az/El to HA/Dec
    if (($input_mode <= $AZEL) && ($output_mode > $AZEL)) {

      # First numerically invert the refraction correction
      my $upper = $el - $ref0/tan($el*2.0*$PI);
      my $lower = $el - 1.5*$ref0/tan($el*2.0*$PI);
      my $root = ($lower+$upper)/2.0;
      my $niter = 0;
      do {
	if ($root + $ref0/tan($root*2.0*$PI) - $el > 0.0) {
	  $upper = $root;
	} else {
	  $lower = $root;
	}
	$root = ($lower+$upper)/2.0;
	$niter++;
      } while (($niter <= 10) && (($upper-$root) > 7.0e-8));
      $el = $root;

      # Now do the conversion
      ($ha, $dec) = eqazel($az, $el, $latitude);
    }

    # Convert from HA/Dec to date
    if (($input_mode <= $HADEC) && ($output_mode > $HADEC)) {
      $ra = $lmst - $ha;
      if ($ra < 0.0) {
	$ra += 1.0;
      }
      @date = pol2r($ra, $dec);

      # Remove the abberation vector
      for (my $i=0 ; $i<3 ; $i++) {
	$date[$i] -= $vonc[$i];
      }
    }

    # precess from date to J2000
    if (($input_mode <= $DATE) && ($output_mode > $DATE)) {
      for (my $i=0 ; $i<3 ; $i++) {
	$j2000[$i] = 0.0;
	for (my $j=0 ; $j<3 ; $j++) {
	  $j2000[$i] += $prcmat[$j][$i] * $date[$j];
	}
      }
    }

    # Convert from J2000 to B1950
    if (($input_mode <= $J2000) && ($output_mode > $J2000)) {
      @b1950 = fk5fk4(@j2000);
    }

    # Convert from B1950 to Galactic
    if (($input_mode <= $B1950) && ($output_mode >= $B1950)) {
      @lb = fk4galr(@b1950);
    }
  }

  if ($output_mode == $EWXY) {
    return($x, $y);
  } elsif ($output_mode == $AZEL) {
    return($az, $el);
  } elsif ($output_mode == $HADEC) {
    return($ha, $dec);
  } elsif ($output_mode == $DATE) {
    return(r2pol(@date));
  } elsif ($output_mode == $J2000) {
    return(r2pol(@j2000));
  } elsif ($output_mode == $B1950) {
    return(r2pol(@b1950));
  } elsif ($output_mode == $GALACTIC) {
    return(r2pol(@lb));
  }
}

=item B<haset_ewxy>

  $haset = haset_ewxy($declination, $latitude, %limits);

   This routine takes the $declination of the source, and the $latitude of the
   EWXY mounted antenna and calculates the hour angle at which the source 
   will set.  It is then trivial to calculate the time until the source
   sets, simply by subtracting the current hour angle of the source from
   the hour angle at which it sets.

  The required inputs are :
    $declination - The declination of the source (turns)
    $latitude    - The latitude of the observatory (turns)
    %limits     - A reference to a hash holding the EWXY antenna limits
                   The following keys must be defined XLOW, XLOW_KEYHOLE,
		   XHIGH, XHIGH_KEYHOLE, YLOW, YLOW_KEYHOLE, YHIGH, 
		   YHIGH_KEYHOLE (all values shoule be in turns)

  The returned value is :
    $haset       - The hour angle (turns) at which a source at this 
                   declination sets for an EWXY mounted antenna with the 
                   given limits at the given latitude

  NOTE: returns undef if %limits hash is missing any of the required keys

=cut

sub haset_ewxy($$\%) {

  my ($declination, $latitude, $limitsref) = @_;

  # Check that all the required keys are present
  if ((!exists $limitsref->{XLOW}) || (!exists $limitsref->{XLOW_KEYHOLE}) ||
      (!exists $limitsref->{XHIGH}) || (!exists $limitsref->{XHIGH_KEYHOLE}) ||
      (!exists $limitsref->{YLOW}) || (!exists $limitsref->{YLOW_KEYHOLE}) ||
      (!exists $limitsref->{YHIGH}) || (!exists $limitsref->{YHIGH_KEYHOLE})) {
    carp 'Missing key in %limits';
   return undef;
  }

  # Local variables
  my ($pole, $pxlim, $exlim, $hix, $hixk, $lowx, $lowxk);

  if ($latitude < 0.0) {
    $pole = -90.0/360.0;
    $pxlim = $limitsref->{XLOW};
    $exlim = $limitsref->{XHIGH};
  } else {
    $pole = 90.0/360.0;
    $pxlim = $limitsref->{XHIGH};
    $exlim = $limitsref->{XLOW};
  }
  my $dec_never = $latitude + $exlim;
  my $dec_always = $pole - ($latitude + $pxlim - $pole);

  if ((($latitude < 0.0) && ($declination > $dec_never)) ||
      (($latitude > 0.0) && ($declination < $dec_never))) {

    # Source is never up
    return(0.0);
  } elsif ((($latitude < 0.0) && ($declination < $dec_always)) ||
	     (($latitude > 0.0) && ($declination > $dec_always))) {

    # Source is always up
    return(1.0);
  } else {

    # Up some of the time - calculate the ghastly constants and
    # do everything in radians from here on.
    $declination = 2.0 * $PI * $declination;
    $latitude = 2.0 * $PI * $latitude;
    my $k0 = -cos($declination);
    my $k1 = sin($declination)*cos($latitude);
    my $k2 = sin($declination)*sin($latitude);
    my $k3 = cos($declination)*sin($latitude);
    my $k4 = cos($declination)*cos($latitude);
    my $k5 = $k4 * $k1 + $k2 * $k3;
    my $x = 2.0 * $PI * $limitsref->{XLOW_KEYHOLE};
    my $dec_split = asin(cos(2.0 * $PI * $limitsref->{YLOW}) *
			 (cos($x) * sin($latitude) + sin($x) * 
			  cos($latitude)));
    if ($latitude > 0.0) {
	
      # Set up for northern antenna
      $hix = $limitsref->{XLOW};
      $hixk = $limitsref->{XLOW_KEYHOLE};
      $lowx = $limitsref->{XHIGH};
      $lowxk = $limitsref->{XHIGH_KEYHOLE};
	
    } else {
      
      # Set up for southern antenna
      $hix = $limitsref->{XHIGH};
      $hixk = $limitsref->{XHIGH_KEYHOLE};
      $lowx = $limitsref->{XLOW};
      $lowxk = $limitsref->{XLOW_KEYHOLE};
    }

    if ((($declination > $dec_split) && ($latitude < 0.0)) || 
	(($declination < $dec_split) && ($latitude > 0.0))) {
      
      # We are on the equatorial side
      my $x = 2.0 * $PI * $hix;
      my $y = -1.0 * abs(acos($k5 / ($k4 * sin($x) + $k3 * cos($x))));
      if (abs($y) < abs(2.0 * $PI * $limitsref->{YLOW_KEYHOLE})) {
	return(acos(($k1 - $k2 + cos($x) * cos($y) - sin($x) * cos($y))/
		    ($k3 + $k4))/(2.0 * $PI));
      } else {
	my $x = 2.0 * $PI * $hixk;
	my $y = -1.0 * abs(acos($k5 / ($k4 * sin($x) + $k3 * cos($x))));
	if (abs($y) < abs(2.0 * $PI * $limitsref->{YLOW_KEYHOLE})) {
	  return(asin(sin(2.0 * $PI * $limitsref->{YLOW_KEYHOLE}) / 
		      $k0)/(2.0 * $PI));
	} elsif (abs($y) < abs(2.0 * $PI * $limitsref->{YLOW})) {
	  return(acos(($k1 - $k2 + cos($x) * cos($y) - sin($x) * cos($y)) /
		      ($k3 + $k4))/(2.0 * $PI));
	} else {
	  return(asin(sin(2.0 * $PI*$limitsref->{YLOW}) / $k0) /
		 (2.0 * $PI));
	}
      }
    } else {
      
      # We are on the polar side
      my $x = 2.0 * $PI * $lowx;
      my $y = abs(acos($k5 / ($k4 * sin($x) + $k3 * cos($x))));
      if (abs($y) < abs(2.0 * $PI * $limitsref->{YLOW_KEYHOLE})) {
	return(acos(($k1 - $k2 + cos($x) * cos($y) - sin($x) * cos($y)) /
		    ($k3 + $k4))/(2.0 * $PI));
      } else {
	my $x = 2.0 * $PI * $lowxk;
	my $y = -1.0 * abs(acos($k5 /($k4 * sin($x) + $k3 * cos($x))));
	if (abs($y) < abs(2.0 * $PI* $limitsref->{YLOW_KEYHOLE})) {
	   return(asin(sin(2.0 * $PI * $limitsref->{YLOW_KEYHOLE}) / 
		       $k0)/(2.0 * $PI));
	} elsif (abs($y) < abs(2.0 * $PI * $limitsref->{YLOW})) {
	  return(acos(($k1 - $k2 + cos($x) * cos($y) - sin($x) * cos($y)) /
		       ($k3 + $k4))/(2.0 * $PI));
	} else {
	  return(asin(sin(2.0 * $PI * $limitsref->{YLOW}) / $k0)/
		 (2.0 * $PI));
	}
      }
    }
  }
}

=item B<ewxy_tlos>

  $tlos = ewxy_tlos($hour_angle, $declination, $latitude, %limits);

  This routine calculates the time left on-source (tlos) for a source
  at $hour_angle, $declination for an EWXY mount antenna at $latitude.

  The required inputs are :
    $hour_angle  - The current hour angle of the source (turns)
    $declination - The declination of the source (turns)
    $latitude    - The latitude of the observatory (turns)
    \%limits     - A reference to a hash holding the EWXY antenna limits
                   The following keys must be defined XLOW, XLOW_KEYHOLE,
		   XHIGH, XHIGH_KEYHOLE, YLOW, YLOW_KEYHOLE, YHIGH,
		   YHIGH_KEYHOLE (all values should be in turns)

  The returned value is :
    $tlos        - The time left on-source (turns)

=cut

sub ewxy_tlos($$$\%) {

  my ($hour_angle, $declination, $latitude, $limitsref) = @_;

  my $haset = haset_ewxy($declination, $latitude, %$limitsref);
  return(undef) if (!defined $haset);
  $haset -= $hour_angle if (($haset > 0.0) && ($haset < 1.0));
  $haset += 1.0 if ($haset < 0.0);

  return $haset;
}

=item B<haset_azel>

  $haset = haset_azel($declination, $latitude, %limits);

   This routine takes the $declination of the source, and the
   $latitude of the Az/El mounted antenna and calculates the hour
   angle at which the source will set.  It is then trivial to
   calculate the time until the source sets, simply by subtracting the
   current hour angle of the source from the hour angle at which it
   sets.  This routine assumes that the antenna is able to rotate
   through 360 degrees in azimuth.

  The required inputs are :
    $declination - The declination of the source (turns)
    $latitude    - The latitude of the observatory (turns)
    \%limits     - A reference to a hash holding the Az/El antenna limits
                   The following keys must be defined ELLOW (all values should
                   be in turns)

  The returned value is :
    $haset       - The hour angle (turns) at which a source at this
                   declination sets for an Az/El mounted antenna with the
                   given limits at the given latitude

  NOTE: returns undef if the %limits hash is missing any of the required keys

=cut

sub haset_azel($$\%) {

  my ($declination,  $latitude, $limitsref) = @_;

  # Check that all the required keys are present
  if (!exists $limitsref->{ELLOW}) {
    carp 'Missing key in %limits';
    return undef ;
  }

  my $cos_haset = (cos($PI / 2.0 - $limitsref->{ELLOW} * 2.0 *
		       $PI) - sin($latitude * 2.0 * $PI) *
		   sin($declination * 2.0 * $PI))/
		     (cos($declination * 2.0 * $PI)
		      *cos($latitude * 2.0 * $PI));
  if ($cos_haset > 1.0) {

    # The source never rises
    return(0.0);
  } elsif ($cos_haset < -1.0) {

    # The source never sets
    return(1.0);
  } else {

    return(acos($cos_haset)/(2.0*$PI));
  }
}

=item B<azel_tlos>

  $tlos = azel_tlos($hour_angle, $declination, $latitude, \%limits);

  This routine calculates the time left on-source (tlos) for a source
  at $hour_angle, $declination for an Az/El mount antenna at $latitude.

  The required inputs are :
    $hour_angle  - The current hour angle of the source (turns)
    $declination - The declination of the source (turns)
    $latitude    - The latitude of the observatory (turns)
    %limits     - A reference to a hash holding the Az/El antenna limits
                   The following keys must be defined ELLOW (all values
                   should be in turns)

  The returned value is :
    $tlos        - The time left on-source (turns)

=cut

sub azel_tlos($$$\%) {
  my ($hour_angle, $declination, $latitude, $limitsref) = @_;

  # Calculate the time left onsource
  my $haset = haset_azel($declination, $latitude, %$limitsref);
  if (!defined $haset) {return(undef)};
  if (($haset > 0.0) && ($haset < 1.0)) { $haset -= $hour_angle; }
  if ($haset < 0.0) { $haset += 1.0; }

  return($haset);
}

=item B<antenna_rise>

  $ha_set = antenna_rise($declination, $latitude, $mount, \%limits);

   Given the $declination of the source, the $latitude of the antenna,
   the type of the antenna $mount and a reference to a hash holding
   information on the antenna limits, this routine calculates the hour
   angle at which the source sets for the antenna.  The hour angle at
   which it rises is simply the negative of that at which it sets.
   These values in turn can be used to calculate the LMST at which the
   source rises/sets and from that the UT at which the source
   rises/sets on a given day, or to calculate the native coordinates
   at which the source rises/sets.

   If you want to calculate source rise times above arbitrary elevation,
   use the routine rise.

  The required inputs are :
    $declination - The declination of the source (turns)
    $latitude    - The latitude of the observatory (turns)
    $mount       - The type of antenna mount, 0 => EWXY mount, 1 => Az/El,
                   any other number will cause the routine to return 
	           undef
    %limits     - A reference to a hash holding the antenna limits
                   For an EWXY antenna there must be keys for all the
                   limits (i.e.  XLOW, XLOW_KEYHOLE, XHIGH, XHIGH_KEYHOLE, 
                   YLOW, YLOW_KEYHOLE, YHIGH, YHIGH_KEYHOLE).  For an Az/El
	           antenna there must be a key for ELLOW (all values should
                   be in turns).

  The returned values are :
    $ha_set  - The hour angle at which the source sets (turns).  The hour
               angle at which the source rises is simply the negative of this
               value.

=cut

sub antenna_rise($$$$) {

  my ($declination, $latitude, $mount, $limitsref) = @_;

  # Check that the mount type is either EWXY (0) or AZEL (1)
  if (($mount != 0) && ($mount != 1)) {
    carp 'mount must equal 0 or 1';
    return undef;
  }

  if ($mount == 0) {
    return(haset_ewxy($declination, $latitude, %$limitsref));
  } elsif ($mount == 1) {
    return(haset_azel($declination, $latitude, %$limitsref));
  }
}

my @b2g = ([-0.054875539726,  0.494109453312, -0.867666135858],
	   [-0.873437108010, -0.444829589425, -0.198076386122],
	   [-0.483834985808,  0.746982251810,  0.455983795705]);

#my @b2g = ([ -0.0548777621, +0.4941083214, -0.8676666398],
#	   [ -0.8734369591, -0.4448308610, -0.1980741871],
#	   [ -0.4838350026, +0.7469822433, +0.4559837919]);

sub j2gal($$) {
  my ($ra,$dec) = @_;
  my @r = pol2r($ra,$dec);
  my @g = (0,0,0);
  for (my $i=0; $i<3; $i++) {
    for (my $j=0; $j<3; $j++) {
      $g[$i]+= $b2g[$j][$i] * $r[$j];
    }
  }
  return r2pol(@g);
}

# SLALIB support routines

sub epb2d ($) {
#     - - - - - -
#      E P B 2 D
#     - - - - - -
#
#  Conversion of Besselian Epoch to Modified Julian Date
#  (double precision)
#
#  Given:
#     EPB      dp       Besselian Epoch
#
#  The result is the Modified Julian Date (JD - 2400000.5).
#
#  Reference:
#     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
#
#  P.T.Wallace   Starlink   February 1984
#
#  Copyright (C) 1995 Rutherford Appleton Laboratory

  my $epb = shift;

  return 15019.81352 + ($epb-1900)*365.242198781;
}

sub epj ($) {
#     - - - -
#      E P J
#     - - - -
#
#  Conversion of Modified Julian Date to Julian Epoch (double precision)
#
#  Given:
#     DATE     dp       Modified Julian Date (JD - 2400000.5)
#
#  The result is the Julian Epoch.
#
#  Reference:
#     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
#
#  P.T.Wallace   Starlink   February 1984
#
#  Copyright (C) 1995 Rutherford Appleton Laboratory
  my $date = shift;

  return 2000 + ($date-51544.5)/365.25;
}

sub pm  ($$$$$$$$$$) {
#     - - -
#      P M
#     - - -
#
#  Apply corrections for proper motion to a star RA,Dec
#  (double precision)
#
#  References:
#     1984 Astronomical Almanac, pp B39-B41.
#     (also Lederle & Schwan, Astron. Astrophys. 134,
#      1-6, 1984)
#
#  Given:
#     R0,D0    dp     RA,Dec at epoch EP0 (rad)
#     PR,PD    dp     proper motions:  RA,Dec changes per year of epoch
#     EP0      dp     start epoch in years (e.g. Julian epoch)
#     EP1      dp     end epoch in years (same system as EP0)
#
#  Returned:
#     R1,D1    dp     RA,Dec at epoch EP1 (rad)
#
#  Called:
#     sla_DCS2C       spherical to Cartesian
#     sla_DCC2S       Cartesian to spherical
#     sla_DRANRM      normalize angle 0-2Pi
#
#  Note:
#     The proper motions in RA are dRA/dt rather than
#     cos(Dec)*dRA/dt, and are in the same coordinate
#     system as R0,D0.
#
#  P.T.Wallace   Starlink   23 August 1996
#
#  Copyright (C) 1996 Rutherford Appleton Laboratory

  my ($r0, $d0, $pr, $pd, $ep0, $ep1) = @_;

  #  Km/s to AU/year multiplied by arc seconds to radians
  use constant VFR => 0.21094502*0.484813681109535994e-5;

  my (@em, $t);

  #  Spherical to Cartesian
  my @p = pol2r($r0,$d0);

  #  Space motion (radians per year)
  $em[0]=-$pr*$p[1]-$pd*cos($r0)*sin($d0);
  $em[1]= $pr*$p[0]-$pd*sin($r0)*sin($d0);
  $em[2]=           $pd*cos($d0);

  #  Apply the motion
  $t=$ep1-$ep0;
  for (my $i = 0; $i<3; $i++) {
    $p[$i]=$p[$i]+$t*$em[$i];
  }

  # Cartesian to spherical
  return r2pol(@p);
}


1;

__END__

