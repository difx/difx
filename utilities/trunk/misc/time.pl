#!/usr/bin/perl -w

use strict;

use Astro::Time;
use Getopt::Long;

my $day = undef;
my $dayno = undef;
my $month = undef;
my $year = undef;
my $mjd = undef;

my $ut;

GetOptions('year=i'=>\$year, 'dayno=i'=>\$dayno, 'month=i'=>\$month,
	   'day=i'=>\$day, 'mjd=f'=>\$mjd);

my ($nowsec,$nowmin,$nowhour,$nowday,$nowmonth,$nowyear,$nowwday,$nowdayno) 
  = gmtime(time);

$nowyear +=1900;
$nowmonth++;
#my $nowut= ((($nowsec/60 + $nowmin)/60 + $nowhour)/24);

$year = $nowyear if (!defined $year);

if (defined $dayno) {
  ($day, $month) = dayno2cal($dayno, $year);
  exit(0) if (!(defined $day && defined $month));
}

$day = $nowday if (!defined $day);
$month = $nowmonth if (!defined $month);

($day, $month, $year, $ut) = mjd2cal($mjd) if (defined $mjd);

$mjd = cal2mjd($day, $month, $year);
exit(0) if (! defined $mjd);

$dayno = cal2dayno($day, $month, $year);
exit(0) if (! defined $dayno);

printf("%02d/%02d/%04d  doy %d\n", $day, $month, $year, $dayno);
printf("  MJD %d\n", $mjd);
