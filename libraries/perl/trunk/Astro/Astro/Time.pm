package Astro::Time;
use strict;

=head1 NAME

Astro::Time - Time based astronomical routines

=head1 SYNOPSIS

    use Astro::Time;

    $dayno = cal2dayno($day, $month, $year);
    print "It's a leap year!\n" if (leap($year));
    $lmst = mjd2lst($mjd, $longitude, $dUT1);
    $turns = str2turn($string, 'H');
    $str = turn2str($turn, 'D', $sig);

=head1 DESCRIPTION

Astro::Time contains an assorted set Perl routines for time based
conversions, such as conversion between calendar dates and Modified
Julian day and conversion of UT to local sidereal time. Include are
routines for conversion between numerical and string representation of
angles.

=head1 AUTHOR

Chris Phillips  Chris.Phillips@csiro.au

=head1 FUNCTIONS

=cut


BEGIN {
  use Exporter ();
  use vars qw($VERSION @ISA @EXPORT @EXPORT_OK @EXPORT_FAIL
              $PI $StrSep $StrZero $Quiet );
  $VERSION = '1.22';
  @ISA = qw(Exporter);

  @EXPORT      = qw( cal2dayno dayno2cal leap yesterday tomorrow
                     mjd2cal cal2mjd mjd2dayno dayno2mjd now2mjd mjd2epoch
                     jd2mjd mjd2jd mjd2time mjd2vextime mjd2weekday mjd2weekdaystr
                     gst mjd2lst cal2lst dayno2lst rise lst2mjd
                     turn2str deg2str rad2str str2turn str2deg str2rad
                     hms2time time2hms month2str str2month
                     deg2rad rad2deg turn2rad rad2turn turn2deg deg2turn
                     $PI );
  @EXPORT_OK   = qw ( daynoOK monthOK dayOK utOK nint $StrSep $StrZero
		      $Quiet);
  @EXPORT_FAIL = qw ( @days @MonthShortStr @MonthStr @WeekShortStr @WeekStr);

  use Carp;
  use POSIX qw( fmod floor ceil acos );
}

$PI = 3.1415926535897932384626433832795028841971693993751;

$StrZero = 0;
$StrSep = ':';

my $debug = 0; # Used for debugging str2turn

$Quiet = 0;

my @days = (31,28,31,30,31,30,31,31,30,31,30,31);

my @MonthShortStr = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug',
		     'Sep', 'Oct', 'Nov', 'Dec');
my @MonthStr = ('January', 'February', 'March', 'April', 'May', 'June', 'July',
		'August', 'September','October', 'November', 'December');

my @WeekShortStr = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun');
my @WeekStr = ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
	       'Saturday', 'Sunday');

# Is the dayno valid?
sub daynoOK ($$) {
  my ($dayno, $year) = @_;

  if ($dayno<1 || $dayno>366 || ($dayno>365 && !leap($year))) {
    carp '$dayno out of range' if (!$Quiet);
    return 0;
  } else {
    return 1;
  }
}

# Is the month valid?
sub monthOK ($) {
  my $month = shift;

  if ($month > 12 || $month < 1) {
    carp '$month out of range' if (!$Quiet);
    return 0;
  } else {
    return 1
  }
}

# IS the day of month OK? (assumes month IS ok - should be checked first)
sub dayOK ($$$) {
  my ($day, $month, $year) = @_;

  $month--;             # For array indexing
  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  if ($day < 1 || $day > $days[$month]) {
    carp '$day out of range' if (!$Quiet);
    return 0;
  } else {
    return 1;
  }
}

# Is the day fraction OK?
sub utOK ($) {
  my $ut = shift;

  if ($ut < 0.0 || $ut >= 1.0) {
    carp '$ut out of range' if (!$Quiet);
    return 0;
  } else {
    return 1;
  }
}

# Return the nearest integer (ie round)
sub nint ($) {
  my ($x) = @_;
  ($x<0.0) ? return(ceil($x-0.5)) : return(floor($x+0.5))
}

=over 4

=item B<turn2str>

  $str = turn2str($turn, $mode, $sig);
  $str = turn2str($turn, $mode, $sig, $strsep);

 Convert fraction of a turn into string representation
   $turn   Angle in turns
   $mode   Mode of string to convert to:
            'H' for hours
            'D' for degrees
   $sig    number of significant figures
   $strsep String separator (override for default $Astro::Time::StrSep)
Note:
 The behavior can be modified by the following two variables:
  $Astro::Time::StrZero   Minimum number of leading digits (zero padded 
                          if needed)
  $Astro::Time::StrSep    (Overridden by optional fourth argument)
    Deliminator used in string (Default ':')
    This may also equal one of a number of special values:
      'HMS'           12H45M12.3S or 170D34M56.2S
      'hms'           12h45m12.3s or 170d34m56.2s
      'deg'           170d34'56.2"

=cut

sub turn2str ($$$;$) {
  my($turn, $mode, $sig, $strsep) = @_;
  $mode = uc $mode;
  if (($mode ne 'H') && ($mode ne 'D')) {
    carp 'turn2str: $mode must equal \'H\' or \'D\'';
    return undef;
  }
  $strsep = $StrSep if (!defined $strsep);

  my ($angle, $str, $sign, $wholesec, $secfract, $min);

  if ($mode eq 'H') {
    $angle = $turn * 24;
  } else {
    $angle = $turn * 360;
  }

  if ($angle < 0.0) {
    $sign = -1;
    $angle = -$angle;
  } else {
    $sign = 1;
  }

  my $wholeangle = (int $angle);

  $angle -= $wholeangle;
  $angle *= 3600;

  # Get second fraction
  $wholesec = int $angle;
  $secfract = $angle - $wholesec;

  $wholesec %= 60;
  $min = ($angle-$wholesec - $secfract)/60.0;
  $secfract = int ($secfract * 10**$sig + 0.5); # Add 0.5 to ensure rounding

  # Check we have not rounded too far
  if ($secfract >= 10**$sig) {
    $secfract -= 10**$sig;
    $wholesec++;
    if ($wholesec >= 60.0) {
      $wholesec -= 60;
      $min++;
      if ($min >= 60.0) {
	$min -= 60;
	$wholeangle++;
      }
    }
  }

  my $angleform;
  if ($StrZero > 0) {
    $angleform = "%0$StrZero";
  } else {
    $angleform = '%';
  }

  my ($sep1, $sep2, $sep3);
  if ($strsep eq 'HMS') {
    if ($mode eq 'H') {
      $sep1 = 'H';
    } else {
      $sep1 = 'D';
    }
    $sep2 = 'M';
    $sep3 = 'S';
  } elsif ($strsep eq 'hms') {
    if ($mode eq 'H') {
      $sep1 = 'h';
    } else {
      $sep1 = 'd';
    }
    $sep2 = 'm';
    $sep3 = 's';
  } elsif ($strsep eq 'deg') { # What if $mode eq 'H'??
    $sep1 = 'd';
    $sep2 = "'";
    $sep3 = '"';
  } else {
    $sep1 = $sep2 = $strsep;
    $sep3 = '';
  }

  if ($sig > 0) {
    $str = sprintf("${angleform}d$sep1%02d".
		   "$sep2%02d.%0${sig}d$sep3", 
		   $wholeangle, $min, $wholesec, $secfract);
  } else {
    $str = sprintf("${angleform}d$sep1%02d".
		   "$sep2%02d$sep3", 
		   $wholeangle, $min, $wholesec);
  }

  if ($sign == -1) {
    $str = '-'.$str;
  }
  return $str;
}

=item B<deg2str>

  $str=deg2str($deg, $mode, $sig);

 Convert degrees into string representation
   $deg   angle in degrees
   $mode  mode of string to convert to:
           'H' for hours
           'D' for degrees
   $sig   number of significant figures
 See note for turn2str

=cut

sub deg2str ($$$;$) {

  my($deg, $mode, $sig, $strsep) = @_;
  return turn2str($deg/(360), $mode, $sig, $strsep);
}

=item B<rad2str>

  $str=rad2str($rad, $mode, $sig);

 Convert radians into string representation
   $rad    angle in radians
   $mode   mode of string to convert to:
            'H' for hours
            'D' for degrees
   $sig is number of significant figures
 See note for turn2str

=cut

sub rad2str ($$$;$) {

  my($rad, $mode, $sig, $strsep) = @_;
  return turn2str($rad/(2*$PI), $mode, $sig, $strsep);
}

=item B<str2turn>

  $turns = str2turn($string,$mode);

 Convert angle from string representation into fraction of a turn
   $string    a : or space delimited angle
   $mode      type of angle
               'H' if $string is in hours,min,sec
               'D' if $string is in deg,arcmin,arcsec
 The format of $string can be fairly flexible e.g.:
    12.2
    12:34
    12:34:45.1
   -23 34 12.3
   -34 34.3

 Note: You cannot mix spaces and :

=cut

sub str2turn ($$) {
  my($str,$mode) = @_;

  if (! defined $str) {
    carp 'Use of uninitialized value at';
    return undef;
  }
  $mode = uc $mode;
  if (($mode ne "H") && ($mode ne "D")) {
    carp 'str2turn: $mode must equal "H" or "D"';
    return undef;
  }

  my $sign = 1.0;
  my $angle = 0.0;
  my $min = 0.0;
  my $sec = 0.0;

  # Does it match dd:dd:dd.d form
  if ($str =~ /^\s*(?:([+-])\s*)?      # Sign (optional)
	         (\d*):                # Hours degrees
	         (\d{0,2})(?::         # Minutes
	         (\d{0,2}(?:\.\d*)?))? # Seconds and fractions (both optional)
	        /x) {
    print STDERR "Matches dd:dd:dd.d\n" if $debug;
    $sign = -1 if (defined($1) && $1 eq "-");
    $angle = $2 if ($2);
    $min = $3 if ($3);
    $sec = $4 if ($4);

  # Does it match hms form 12h33m34.6s
  } elsif ($str =~ /^\s*(?:([+-])\s*)?   # Sign (optional)
	            (\d+)\s*(h)\s*       # Hours
	            (?:(\d{0,2})\s*m\s*  # Minutes optional
	            (?:(\d{0,2}          # Seconds and fractions (optional)
                    (?:\.\d*)?)\s*s)?)?

	           /x) {
    print STDERR "Matches hms\n" if $debug;
    $sign = -1 if (defined($1) && $1 eq "-");
    $angle = $2 if ($2);
    $mode = 'H';
    $min = $4 if ($4);
    $sec = $5 if ($5);

    # Does it match dms form 12d33m34.6s or 12d33'34.6"
  } elsif ($str =~ /^\s*(?:([+-])\s*)?  # Sign (optional)
                   (\d+)\s*([d])\s*     # Degrees
                   (?:(\d{0,2})\s*[m']\s*  # Minutes optional
                   (?:(\d{0,2}          # Seconds and fractions (optional)
                     (?:\.\d*)?)\s*[s"])?)?
                  /x) {
    print STDERR "Matches dms\n" if $debug;
    $sign = -1 if (defined($1) && $1 eq "-");
    $angle = $2 if ($2);
    #$mode = uc($3);
    $mode = 'D';
    $min = $4 if ($4);
    $sec = $5 if ($5);

    # Does is match dd dd dd.d form
  } elsif ($str =~ /^\s*(?:([+-])\s*)?   # Sign (optional)
	            (\d+)\s+             # Hours degrees
	            (\d{0,2})(?:\s+      # Minutes
	            (\d{0,2}(?:\.\d*)?))? # Seconds and fractions
	        /x) {
    print STDERR "Matches dd dd dd.d\n" if $debug;
    $sign = -1 if (defined($1) && $1 eq "-");
    $angle = $2 if ($2);
    $min = $3 if ($3);
    $sec = $4 if ($4);

  # Does it match dd.d form
  } elsif ($str =~ /^\s*(?:([+-])\s*)?(\d+(?:\.\d*)?)/) {
    print STDERR "Matches dd.d\n" if $debug;
    $sign = -1 if (defined($1) && $1 eq "-");
    $angle = $2 if ($2);
  } else {
    return undef;
  }

  my $factor;
  if ($mode eq "H") {
    $factor = 24.0;
  } else {
    $factor = 360.0;
  }

  print "Got ($sign) $angle/$min/$sec  [$mode]\n" if $debug;

  return $sign * (($angle + ($min + $sec/60.0)/60.0)/ $factor);
}

=item B<str2deg>

  $degrees=str2deg($string,$mode);

 Convert angle from string representation into degrees
   $string   a : or space delimited angle
   $mode     'H' if $string is in hours,min,sec
             'D' if $string is in deg,arcmin,arcsec
 See note for str2turn

=cut

sub str2deg ($$) {
  my($str, $mode) = @_;
  return str2turn($str, $mode) * 360;
}

=item B<str2rad>

  $radians=str2rad($string,$mode);

 Convert angle from string representation into radians
   $string   a : or space delimited angle
   $mode     'H' if $string is in hours,min,sec
             'D' if $string is in deg,arcmin,arcsec
 See note for str2turn

=cut

sub str2rad ($$) {
  my($str, $mode) = @_;
  return str2turn($str, $mode) * 2*$PI;
}

=item B<hms2time>

  ($time) = hms2time($hour, $minute, $second);
  ($time) = hms2time($hour, $minute, $second, $mode);

 Returns the day fraction given hours minutes and seconds (or degrees)
   $time        Day fraction
   $hour        Hours
   $minutes     Minutes
   $second      Seconds
   $mode        'H' or 'D' to interpret as hours or degrees (default 
                hours)

=cut

sub hms2time ($$$;$) {

  my($hour, $minute, $second, $mode) = @_;
  $mode = 'H' if (!defined $mode);

  my $factor;
  if ($mode eq 'H' || $mode eq 'h') {
    $factor = 24.0;
  } elsif ($mode eq 'D' || $mode eq 'd') {
    $factor = 360.0;
  } else {
    carp 'Illegal $mode value';
    return undef;
  }

  return (($second/60 + $minute)/60 + $hour)/$factor;
}

=item B<time2hms>

  ($sign, $hour, $minute, $second) = time2hms($time, $mode, $sig);

 Returns hours (or degrees), minutes and seconds given the day fraction
   $sign      Sign of angle ('+' or '-')
   $hour      Hours
   $minutes   Minutes
   $second    Seconds
   $time      Day fraction
   $mode      Return degrees or Hours?
               'H' for hours
               'D' for degrees
   $sig       Number of significant digits for $second

=cut

sub time2hms ($$$) {

  my($turn, $mode, $sig) = @_;
  $mode = uc $mode;
  if (($mode ne 'H') && ($mode ne 'D')) {
    carp 'time2hms: $mode must equal \'H\' or \'D\'';
    return undef;
  }

  my ($angle, $str, $sign, $wholesec, $secfract, $min);

  if ($mode eq 'H') {
    $angle = $turn * 24;
  } else {
    $angle = $turn * 360;
  }

  if ($angle < 0.0) {
    $sign = '-';
    $angle = -$angle;
  } else {
    $sign = '+';
  }

  my $wholeangle = (int $angle);

  $angle -= $wholeangle;
  $angle *= 3600;

  # Get second fraction
  $wholesec = int $angle;
  $secfract = $angle - $wholesec;

  $wholesec %= 60;
  $min = ($angle-$wholesec - $secfract)/60.0;
  $secfract = int ($secfract * 10**$sig + 0.5); # Add 0.5 to ensure rounding

  # Check we have not rounded too far
  if ($secfract >= 10**$sig) {
    $secfract -= 10**$sig;
    $wholesec++;
    if ($wholesec >= 60.0) {
      $wholesec -= 60;
      $min++;
      if ($min >= 60.0) {
	$min -= 60;
	$wholeangle++;
      }
    }
  }
  my $format = sprintf '%%0%dd', $sig;
  $secfract = sprintf($format, $secfract);

  if ($sig > 0) {
    return($sign, $wholeangle, $min, "$wholesec.$secfract");
  } else {
    return($sign, $wholeangle, $min, $wholesec);
  }
}

=item B<deg2rad>

  $rad=deg2rad($deg);
 Convert degrees to radians

=cut

sub deg2rad ($) {
  return $_[0] / 180*$PI;
}

=item B<rad2deg>

  $deg=rad2deg($rad);
 Convert radians to degrees

=cut

sub rad2deg ($) {
  return $_[0] * 180/$PI;
}

=item B<turn2rad>

  $rad=turn2rad($turn);
 Convert turns to radians

=cut

#sub turn2rad ($) {
#  return $_[0] * 2*$PI;
#}

sub turn2rad {
  my @ret;
  foreach (@_) {
    push @ret, $_ * 2*$PI;
  }
  if (@ret==1) {
    return $ret[0];
  } elsif (@ret==0) {
    return undef;
  } else {
    return @ret;
  }
}

=item B<rad2turn>

  $turn=rad2turn($rad);
 Convert radians to turns

=cut

#sub rad2turn ($) {
#  return $_[0] / (2*$PI);
#}

sub rad2turn {
  my @ret;
  foreach (@_) {
    push @ret, $_/(2*$PI);
  }
  if (@ret==1) {
    return $ret[0];
  } elsif (@ret==0) {
    return undef;
  } else {
    return @ret;
  }
}

=item B<turn2deg>

  $deg=turn2deg($turn);
 Convert turns to radians

=cut

#sub turn2deg ($) {
#  return $_[0] * 360.0;
#}

sub turn2deg {
  my @ret;
  foreach (@_) {
    push @ret, $_*360.0;
  }
  if (@ret==1) {
    return $ret[0];
  } elsif (@ret==0) {
    return undef;
  } else {
    return @ret;
  }
}

=item B<deg2turn>

  $turn=deg2turn($deg);
 Convert degrees to turns

=cut

#sub deg2turn ($) {
#  return $_[0] / 360.0;
#}

sub deg2turn {
  my @ret;
  foreach (@_) {
    push @ret, $_/360.0;
  }
  if (@ret==1) {
    return $ret[0];
  } elsif (@ret==0) {
    return undef;
  } else {
    return @ret;
  }
}

=item B<cal2dayno>

  $dayno = cal2dayno($day, $month, $year);

 Returns the day number corresponding to $day of $month in $year.

=cut

# VERIFYED
sub cal2dayno ($$$) {

  my ($day, $month, $year) = @_;
  return undef if (!monthOK($month));
  return undef if (!dayOK($day, $month, $year));

  $month--; # For array indexing

  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  my $mon;
  my $dayno = $day;
  for ($mon=0; $mon<$month; $mon++) {
    $dayno += $days[$mon];
  }

  return($dayno);
}

=item B<dayno2cal>

  ($day, $month) = dayno2cal($dayno, $year);

 Return the $day and $month corresponding to $dayno of $year.

=cut

# Verified
sub dayno2cal ($$) {

  my($dayno, $year) = @_;
  return undef if (!daynoOK($dayno, $year));

  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  my $month = 0;
  my $end = $days[$month];
  while ($dayno>$end) {
    $month++;
    $end+= $days[$month];
  }
  $end -= $days[$month];
  my $day = $dayno - $end;
  $month++;

  return($day, $month);
}

=item B<leap>

  $isleapyear = leap($year);

 Returns true if $year is a leap year.
   $year    year in full

=cut

# NOT Verified
sub leap ($) {
  my $year = shift;
  return (((!($year%4))&&($year%100))||(!($year%400)));
}

=item B<yesterday>

  ($dayno, $year) = yesterday($dayno, $year);
  ($day, $month, $year) = yesterday($day, $month, $year);

 Winds back the day number by one, taking account of year wraps.
   $dayno       Day number of year
   $year        Year
   $month       Month
   $day         Day of month

=cut

# Verified
sub yesterday($$;$) {

  my ($day, $month, $dayno, $year);
  
  if (scalar(@_)==2) {
    ($dayno, $year) = @_;
    return undef if (!daynoOK($dayno, $year));
  } else {
    ($day, $month, $year) = @_;
    return undef if (!monthOK($month));
    return undef if (!dayOK($day, $month, $year));
    $dayno = cal2dayno($day, $month, $year);
  }

  $dayno--;
  if ($dayno==0) {
    $year--;
    if (leap($year)) {
      $dayno = 366;
    } else {
      $dayno = 365;
    }
  }

  if (scalar(@_)==2) {
    return ($dayno, $year);
  } else {
    ($day, $month) = dayno2cal($dayno, $year);
    return($day, $month, $year);
  }
}

=item B<tomorrow>

  ($dayno, $year) = tomorrow($dayno, $year);
  ($day, $month, $year) = tomorrow($day, $month, $year);

 Advances the day number by one, taking account of year wraps.
   $dayno       Day number of year
   $year        Year
   $month       Month
   $day         Day of month

=cut

# Verified
sub tomorrow($$;$) {

  my ($day, $month, $dayno, $year);

  if (scalar(@_)==2) {
    ($dayno, $year) = @_;
    return undef if (!daynoOK($dayno, $year));
  } else {
    ($day, $month, $year) = @_;
    return undef if (!monthOK($month));
    return undef if (!dayOK($day, $month, $year));
    $dayno = cal2dayno($day, $month, $year);
  }

  $dayno++;
  if (($dayno==366 && !leap($year)) || $dayno==367) {
    $year++;
    $dayno = 1;
  }

  if (scalar(@_)==2) {
    return ($dayno, $year);
  } else {
    ($day, $month) = dayno2cal($dayno, $year);
    return($day, $month, $year);
  }
}

=item B<mjd2cal>

  ($day, $month, $year, $ut) = mjd2cal($mjd);

 Converts a modified Julian day number into calendar date (universal 
 time). (based on the slalib routine sla_djcl).
    $mjd     Modified Julian day (JD-2400000.5)
    $day     Day of the month.
    $month   Month of the year.
    $year    Year
    $ut      UT day fraction

=cut

# VERIFIED
sub mjd2cal($) {

  my $mjd = shift;

  my $ut = fmod($mjd,1.0);
  if ($ut<0.0) {
    $ut += 1.0;
    $mjd -= 1;
  }

  use integer;  # Calculations require integer division and modulation
  # Get the integral Julian Day number
  my $jd = nint($mjd + 2400001);

  # Do some rather cryptic calculations

  my $temp1 = 4*($jd+((6*(((4*$jd-17918)/146097)))/4+1)/2-37);
  my $temp2 = 10*((($temp1-237)%1461)/4)+5;

  my $year = $temp1/1461-4712;
  my $month =(($temp2/306+2)%12)+1;
  my $day = ($temp2%306)/10+1;

  return($day, $month, $year, $ut);
}

=item B<cal2mjd>

  $mjd = cal2mjd($day, $month, $year, $ut);

 Converts a calendar date (universal time) into modified Julian day 
 number.
    $day     Day of the month.
    $month   Month of the year.
    $year    Year
    $ut      UT dayfraction
    $mjd     Modified Julian day (JD-2400000.5)

=cut

# Verified
sub cal2mjd($$$;$) {
  my($day, $month, $year, $ut) = @_;
  $ut = 0.0 if (!defined $ut);

  return undef if (!monthOK($month));
  return undef if (!dayOK($day, $month, $year));
  return undef if (!utOK($ut));

  my ($m, $y);
  if ($month <= 2) {
    $m = int($month+9);
    $y = int($year-1);
  } else {
    $m = int($month-3);
    $y = int($year);
  }
  my $c = int($y/100);
  $y = $y-$c*100;
  my $x1 = int(146097.0*$c/4.0);
  my $x2 = int(1461.0*$y/4.0);
  my $x3 = int((153.0*$m+2.0)/5.0);
  return($x1+$x2+$x3+$day-678882+$ut);
}

=item B<mjd2dayno>

  ($dayno, $year, $ut) = mjd2dayno($mjd);

 Converts a modified Julian day number into year and dayno (universal 
 time).
    $mjd     Modified Julian day (JD-2400000.5)
    $year    Year
    $dayno   Dayno of year

=cut

# NOT Verified
sub mjd2dayno($) {

  my $mjd = shift;
  my ($day, $month, $year, $ut) = mjd2cal($mjd);

  return  (cal2dayno($day,$month,$year), $year, $ut);
}

=item B<dayno2mjd>

  $mjd = dayno2mjd($dayno, $year, $ut);

 Converts a dayno and year to modified Julian day
    $mjd     Modified Julian day (JD-2400000.5)
    $year    Year
    $dayno   Dayno of year

=cut

# Not verified - wrapper to cal2mjd
sub dayno2mjd($$;$) {

  my ($dayno, $year, $ut) = @_;
  $ut = 0.0 if (!defined $ut);

  return undef if (!daynoOK($dayno,$year));
  return undef if (!utOK($ut));

  my ($day, $month) = dayno2cal($dayno, $year);

  return cal2mjd($day, $month, $year, $ut);
}

=item B<now2mjd>

  $mjd = now2mjd()

=cut

# Not verified - just wrapper
sub now2mjd() {
  my ($s, $m, $h, $day, $month, $year) = gmtime();
  $month++;
  $year += 1900;
  return(cal2mjd($day, $month, $year, ((($s/60+$m)/60+$h)/24)));
}

=item B<jd2mjd>

  $mjd = jd2mjd($jd);

 Converts a Julian day to Modified Julian day
    $jd      Julian day
    $mjd     Modified Julian day

=cut

sub jd2mjd($) {
  return (shift)-2400000.5;
}

=item B<mjd2jd>

  $jd = mjd2jd($mjd);

 Converts a Modified Julian day to Julian day
    $mjd     Modified Julian day
    $jd      Julian day

=cut

sub mjd2jd($) {
  return (shift)+2400000.5;
}

=item B<mjd2time>

  $str = mjd2time($mjd);
  $str = mjd2time($mjd, $np);

 Converts a Modified Julian day to a formatted string
    $mjd     Modified Julian day
    $str     Formatted time
    $np      Number of significant digits for fraction of a sec. Default 0

=cut

sub mjd2time($;$) {
  my ($dayno, $year, $ut) = mjd2dayno(shift);
  my $np = shift;
  $np = 0 if (! defined $np);
  return sprintf("$year %03d/%s", $dayno, turn2str($ut, 'H', $np));
}

=item B<mjd2vextime>

  $str = mjd2vextime($mjd);
  $str = mjd2vextime($mjd, $np);

 Converts a Modified Julian day to a vex formatted string
    $mjd     Modified Julian day
    $str     Formatted time
    $np      Number of significant digits for fraction of a sec. Default 0

=cut

sub mjd2vextime($;$) {
  my ($dayno, $year, $ut) = mjd2dayno(shift);
  my $np = shift;
  $np = 0 if (! defined $np);
  return sprintf("%dy%03dd%s", $year, $dayno, turn2str($ut, 'H', $np, 'hms'));
}

=item B<mjd2epoch>

  $time = mjd2epoch($mjd);

 Converts a Modified Julian day to unix Epoch (seconds sinve 1 Jan 1970)
 Rounded to the nearest second
    $mjd     Modified Julian day
    $tie     Seconds since 1 Jan 1970

=cut

sub mjd2epoch($) {
  my $mjd = shift;
  my $epoch = ($mjd - 40587)*24*60*60;
  return int($epoch + $epoch/abs($epoch*2)); # Work even if epoch is negative
}

=item B<gst>

  $gst  = gst($mjd);
  $gmst = gst($mjd, $dUT1);
  $gtst = gst($mjd, $dUT1, $eqenx);

 Converts a modified Julian day number to Greenwich sidereal time
   $mjd     modified Julian day (JD-2400000.5)
   $dUT1    difference between UTC and UT1 (UT1 = UTC + dUT1) (seconds)
   $eqenx   Equation of the equinoxes (not yet supported)
   $gst     Greenwich sidereal time (turns)
   $gmst    Greenwich mean sidereal time (turns)
   $gtst    Greenwich true sidereal time (turns)

=cut

# Verified
sub gst($;$$) {
  my ($mjd, $dUT1, $eqenx) = @_;
  $dUT1 = 0.0 if (! defined $dUT1);
  if ($dUT1 > 0.5 || $dUT1 < -0.5) {
    carp '$dUT1 out of range';
    return undef;
  }
  if (defined $eqenx) {
    croak '$eqenx is not supported yet';
  }

  my $JULIAN_DAY_J2000          = 2451545.0;
  my $JULIAN_DAYS_IN_CENTURY    = 36525.0;
  my $SOLAR_TO_SIDEREAL         = 1.002737909350795;

  my $a=101.0 + 24110.54841/86400.0;
  my $b=8640184.812866/86400.0;
  my $e=0.093104/86400.0;
  my $d=0.0000062/86400.0;
  my $tu = (int($mjd)-($JULIAN_DAY_J2000-2400000.5))/$JULIAN_DAYS_IN_CENTURY;
  my $sidtim = $a + $tu*($b + $tu*($e - $tu*$d));
  $sidtim -= int($sidtim);
  if ($sidtim < 0.0) {$sidtim += 1.0};
  my $gmst = $sidtim + ($mjd - int($mjd) + $dUT1/86400.0)*$SOLAR_TO_SIDEREAL;
  while ($gmst<0.0) {
    $gmst += 1.0;
  }
  while ($gmst>1.0) {
    $gmst -= 1.0;
  }

  return $gmst;
}

# Not verified - wrapper to gmst

=item B<mjd2lst>

  $lst = mjd2lst($mjd, $longitude);
  $lmst = mjd2lst($mjd, $longitude, $dUT1);
  $ltst = mjd2lst($mjd, $longitude, $dUT1, $eqenx);

 Converts a modified Julian day number into local sidereal time (lst),
 local mean sidereal time (lmst) or local true sidereal time (ltst).
 Unless high precisions is required dUT1 can be omitted (it will always 
 be in the range -0.5 to 0.5 seconds).
   $mjd         Modified Julian day (JD-2400000.5)
   $longitude   Longitude for which the LST is required (turns)
   $dUT1        Difference between UTC and UT1 (UT1 = UTC + dUT1)(seconds)
   $eqenx       Equation of the equinoxes (not yet supported)
   $lst         Local sidereal time (turns)
   $lmst        Local mean sidereal time (turns)
   $ltst        Local true sidereal time (turns)

=cut

sub mjd2lst($$;$$) {
  my ($mjd, $longitude, $dUT1, $eqenx) = @_;

  my $lst = gst($mjd, $dUT1, $eqenx);
  return undef if (!defined $lst);
  $lst += $longitude;

  while ($lst>1.0) {
    $lst-= 1;
  }
  while ($lst < 0.0) {
    $lst += 1;
  }
  return $lst;
}

=item B<cal2lst>

  $lst = cal2lst($day, $month, $year, $ut, $longitude);
  $lmst = cal2lst($day, $month, $year, $ut, $longitude, $dUT1);
  $ltst = cal2lst($day, $month, $year, $ut, $longitude, $dUT1, $eqenx);

 Wrapper to mjd2lst using calendar date rather than mjd

=cut

sub cal2lst($$$$$;$$) {
  my ($day, $month, $year, $ut, $longitude, $dUT1, $eqenx) = @_;
  my $mjd = cal2mjd($day, $month, $year, $ut);
  return undef if (!defined $mjd);

  return mjd2lst($mjd, $longitude, $dUT1, $eqenx);
}

=item B<dayno2lst>

  $lst = dayno2lst($dayno, $year, $ut, $longitude);
  $lmst = dayno2lst($dayno, $year, $ut, $longitude, $dUT1);
  $ltst = dayno2lst($dayno, $year, $ut, $longitude, $dUT1, $eqenx);

 Wrapper to mjd2lst using calendar date rather than mjd

=cut

sub dayno2lst($$$$;$$) {
  my ($dayno, $year, $ut, $longitude, $dUT1, $eqenx) = @_;
  my $mjd = dayno2mjd($dayno, $year, $ut);
  return undef if (!defined $mjd);

  return mjd2lst($mjd, $longitude,  $dUT1, $eqenx);
}

# Not verified

=item B<rise>

  ($lst_rise, $lst_set) = rise($ra, $dec, $obslat, $el_limit);

 Return the lst rise and set time of the given source
   $lst_rise, $lst_set  Rise and set time (turns)
   $ra, $dec            RA and Dec of source (turns)
   $obslat              Latitude of observatory (turns)
   $el_limit            Elevation limit of observatory
                                          (turns, 0 horizontal)
 Returns 'Circumpolar'  if source circumpolar
 Returns  undef         if source never rises

 Uses the formula:
   cos $z_limit = sin $obslat * sin $dec + cos $obslat * cos $dec 
                  * cos $HA
   where:
    $z_limit is the zenith angle limit corresponding to $el_limit
    $HA is the Hour Angle of the source
NOTE: For maximum accuracy source coordinated should be precessed to
      the current date.

=cut

sub rise ($$$$) {
  #print "rise: Got @_\n";
  my ($ra, $dec, $obslat, $el_limit) = @_;
  $ra = turn2rad($ra);
  $dec = turn2rad($dec);
  $obslat = turn2rad($obslat);
  $el_limit = turn2rad($el_limit);

  my $z_limit = $PI/2-$el_limit;

  #print "Check it\n";

  # Check whether the source ever rises or is circumpolar
  my $z = acos(sin($obslat)*sin($dec) + cos($obslat)*cos($dec)); # Highest point
  return (undef) if ($z>$z_limit);

  #print "Got here\n";

  $z = acos(sin($obslat)*sin($dec) - cos($obslat)*cos($dec)); # Lowest point

  return ('Circumpolar') if ($z<$z_limit);

  my $cos_ha = (cos($z_limit) - sin($obslat)*sin($dec))
    /(cos($obslat)*cos($dec));
  my $ha = acos($cos_ha);

  my $lst_rise =  $ra - $ha;
  my $lst_set = $ra + $ha;

  $lst_rise += 2*$PI if ($lst_rise < 0.0);
  $lst_set -= 2*$PI if ($lst_set >= 2*$PI);

  return rad2turn($lst_rise), rad2turn($lst_set);
}

=item B<lst2mjd>

  $mjd = lst2mjd($lmst, $dayno, $year, $longitude);
  $mjd = lst2mjd($lmst, $dayno, $year, $longitude, $dUT1);

  This routine calculates the modified Julian day number corresponding
  to the local mean sidereal time $lmst at $longitude, on a given UT
  day number ($dayno).  Unless high precision is required dUT1 can be
  omitted.

  The required inputs are :
    $lmst      - The local mean sidereal time (turns)
    $dayno     - The UT day of year for which to do the conversion
    $year      - The year for which to do the conversion
    $longitude - The longitude of the observatory (turns)
    $dUT1      - Difference between UTC and UT1 (UT1 = UTC + dUT1) 
                                                                (seconds)
    $mjd         The modified Julian day corresponding to $lmst on $dayno

=cut

sub lst2mjd($$$$;$) {
  my ($lmst, $dayno, $year, $longitude, $dUT1) = @_;
  $dUT1 = 0.0 if (!defined $dUT1);

  my $SOLAR_TO_SIDEREAL = 1.002737909350795;

  my $mjd = dayno2mjd($dayno, $year, $dUT1);

  # Time in turns from passed lmst to lmst at the start of $dayno
  my $delay = $lmst-mjd2lst($mjd, $longitude);

  if ($delay < 0.0) {
    $delay += 1.0;
  }

  return($mjd + $delay/$SOLAR_TO_SIDEREAL);
}

=item B<month2str>

  $monthstr = month2str($month);
  $longmonthstr = month2str($month,1);

  This routine returns the name of the given month (as a number 1..12), 
  where 1 is January. The default is a 3 character version of the month
  ('Jan', 'Feb', etc) in the second form the full month is returned


  The required inputs are :
    $month      - The month in question with 1 == January.

=cut

sub month2str($;$) {
  my ($mon, $long) = @_;

  return undef if (!monthOK($mon));

  if ($long) {
    return $MonthStr[$mon-1];
  } else {
    return $MonthShortStr[$mon-1];
  }
}

=item B<mjd2weekday>

  $weekday = mjd2weekday($mjd);

 Returns the weekday correspondig to the given MJD.
 0 ==> Monday. May not work for historical dates.

    $mjd     Modified Julian day (JD-2400000.5)

=cut



sub mjd2weekday ($) {
  my $mjd = int floor ((shift)+0.00001);  # MJD as an int...
  return ($mjd-5) % 7;
}

=item B<mjd2weekdaystr>

  $weekdaystr = mjd2weekdaystr($mjd);

 Returns the name of the weekday correspondig to the given MJD.
 May not work for historical dates.

    $mjd     Modified Julian day (JD-2400000.5)

=cut


sub mjd2weekdaystr($;$) {
  my ($mjd, $long) = @_;
  my $dow = mjd2weekday($mjd);
  if ($long) {
    return $WeekStr[$dow];
  } else {
    return $WeekShortStr[$dow];
  }
}

=item B<str2month>

  $month = month2str($monthstr);

  Given the name of a month (in English), this routine returns the
  an integer between 1 and 12, where 1 is January. Full month names of
  3 character abbreviations are acceptable. Minumum matching (e.g. "Marc")
  is not supported.

  The required inputs are :
    $month      - Name of the month ('Jan', 'January', 'Feb', 'February' etc)

=cut

sub str2month($) {
  my $month = uc(shift);

  for (my $i=0; $i<12; $i++) {
    if ($month eq uc($MonthStr[$i]) || $month eq uc($MonthShortStr[$i])) {
      return $i+1;
    }
  }
  return undef;
}

1;

__END__

